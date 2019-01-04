import sys, threading, time, logging, wx
try:
    from wx.lib.pubsub import Publisher
except ImportError:
    from wx.lib.pubsub import pub as Publisher

import uhal

from uhal.gui.utilities.utilities import HwReadyEvent


class HardwareMonitoring(threading.Thread):
        
    def __init__(self, parent, file_name):  
        """
        Thread that checks the HW. The thread is set to be a thread daemon, so that it finishes when the main thread does. 
        __is_running: controls the thread execution
        __parent: the main thread
        __hw_complete: map of IP end points' names to map of node names to node values. It's updated after every iteration of the thread
        __hw_tree: is built only once in the constructor. Contains the HW information in a hierarchical/recursive way. This structure is defined:
            __hw_tree := {item - value}, where item is [IPEndPoint, Node] and value is [__hw_tree, node value] 
        __cm: uHAL ConnectionManager
        __devices: contains all IP End Points present in the address table file
        """
        self.__logger = logging.getLogger('uhal.gui.utilities.hardware_monitoring')
        
        threading.Thread.__init__(self)
        self.setDaemon(True)
        
        #members
        self.__is_running = True        
        self.__parent = parent      
        self.__hw_complete = {}
        self.__hw_tree = {}
                
        self.__cm = None
        self.__devices = None
        
        try:
            self.__init_hw(file_name)      
            self.__build_hw_tree()
            
        except Exception as e:
            raise                                 



    def run(self):    
        """
        Thread activity. Infinite loop reading the HW registers. The reading operations have been restricted to:
        1. Nodes that are alive (reply to ping)
        2. End nodes. For example, SUBMODULE or SUBMODULE.SUBSYSTEM will not be read, whereas SUBMODULE.SUBSYSTEM.REG or SUBMODULE.SUBSYSTEM.MEM will
        This is due to the fact that uHAL always displays 0 value for those nodes up in the hierarchy regardless of what is written in the HW
        3. Nodes that are readable
        At the end of every iteration, the read values are copied in the __hw_complete structure and the publisher writes this information
        """                    
        
        while self.__is_running:                        
            self.__logger.debug('Start!')    
            self.__update_devices()
            
            map_devs_to_map_nodes_to_values = {}
           
            for name, dev in self.__devices.items():
                '''
                if dev.get_status() is not "OK":
                    continue
                '''                
                nodes = dev.getNodes()
                nodes_vs_values = {}
                for node in nodes:
                    
                    try: 
                        node_object = dev.getNode(node)
                        # Only reading end nodes
                        if node_object.getNodes():
                            continue
                        
                        # Only reading readable nodes (DUH!)
                        permission = str(node_object.getPermission())
                        if "READ" not in permission:
                            continue
                                               
                        nodes_vs_values[node] = node_object.read()                       
                                                        
                    except Exception as e:
                        self.__logger.warning('Exception while reading node %s from device %s: %s', node , name, str(e))                        
                
                # Temporarily add try/except block to cope with the fact that IP End point check status (PING) is not offered right now
                try:              
                    dev.dispatch()                    
                    map_devs_to_map_nodes_to_values[name] = nodes_vs_values
                except Exception as e:
                    self.__logger.warning('Dispatch operation for device %s failed: %s', dev.id(), str(e))
                                                                                    
            
            wx.CallAfter(Publisher().sendMessage, "HW POLL", map_devs_to_map_nodes_to_values)              
            time.sleep(10)     



    def set_thread_running(self, is_running):
        """
        Sets the thread to run or not to run
        """
        self.__is_running = is_running
        
        
        
    def get_hw_tree(self):
        return self.__hw_tree
                
                    
            
    def __init_hw(self, file_name):
        """
        Initializes the __hw_tree, __hw_complete and __devices objects
        """
        
        self.__cm = uhal.ConnectionManager(str("file://" + file_name))
        self.__devices = {}
        
        for device in self.__cm.getDevices():
            device_object = self.__cm.getDevice(device)
            
            self.__hw_tree[device_object] = {}            
            self.__devices[device] = device_object            
            self.__hw_complete[device] = {}
            
            node_vs_value = {}
            
            for node in device_object.getNodes():
                node_vs_value[node] = 0
            
            self.__hw_complete[device] = node_vs_value   
            
            
            
    def __build_hw_tree(self):
        """
        This method and __build_tree construct the __hw_tree object.    
        """
        for k in self.__hw_tree.keys():
            self.__logger.debug('Going to build tree for device %s', k.id())
            self.__build_tree(k, self.__hw_tree)
    
            
            
    def __build_tree(self, item, parent):
        """
        Builds the __hw_tree object recursively
        """        
        for node in item.getNodes():
            node_object = item.getNode(node)
            # node is not one of the first level nodes
            if node not in item.getNodes("[^.]*"):
                continue
                        
            # node has no children
            if not item.getNode(node).getNodes():
                parent[item][node_object] = 0
            else:
                parent[item][node_object] = {}
                self.__build_tree(node_object, parent[item])
    
    
    
    def __update_devices(self):        
        """
        This method gets executed at the beginning of the background thread. Its goal is to update the list of IP End Points with 
        just the ones that continue being up and running
        """
        for device in self.__cm.getDevices():
            device_object = self.__cm.getDevice(device)
            self.__devices[device] = device_object
                                
            
