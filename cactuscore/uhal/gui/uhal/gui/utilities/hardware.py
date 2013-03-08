import threading
import time

import pycohal


class AbstractNode:

    def __init__(self, node):

        self.name = node.getId()
        self.address = node.getAddress()
        self.mask = node.getMask()
        self.value = None
        self.mode = node.getMode()        
        self.permission = node.getPermission()
        self.size = node.getSize()
        self.tags = node.getTags()


    def update(self):
        pass
        
        
        
    def getId(self):
        return self.name

    def getAddress(self):
        return self.address

    def getMask(self):
        return self.mask

    def getPermission(self):
        return self.permission


class RegNode(AbstractNode):
    pass


class MemNode(AbstractNode):
    pass


class FifoNode(AbstractNode):
    pass



class IPEndPoint:

    def __init__(self, device, status = "Undefined"):

        self.hw = device
        self.id = device.id()
        self.uri = device.uri()
        self.node_list = device.getNodes()
        self.status = status
        
        self.__nodes_dict = {}

        self.__fill_nodes_dict()


    def __fill_nodes_dict(self):

        for node in self.node_list:
            if node not in self.__nodes_dict.keys():
                self.__nodes_dict[node] = AbstractNode(self.hw.getNode(node))
            else:
                print "Duplicated node: ", node, " won't be inserted in node list"

                    
        
    def update(self):

        #Update IP END POINT status before the loop

        #Update nodes' values
        for node in self.__nodes_dict.keys():
            
            try:
                node = self.hw.getNode(node).read()
                self.hw.dispatch()
                print node
            except pycohal.exception, e:
                print "Tried to read only-write node: ", str(e)

       
class Hardware(threading.Thread):


    def __init__(self, connection_file = ""):

        threading.Thread.__init__(self)

        self.__auto_refresh = True
        self.__refresh_period = 60.0
        self.__stop_event = threading.Event()
        
        self.__connection_file = connection_file
        self.__ip_end_points_dict = {}
        self.__hw_manager = pycohal.ConnectionManager(str("file://" + self.__connection_file))

        self.__load_hardware(self.__hw_manager)
                
        
    def run(self):

        if self.__auto_refresh:
            
            while not self.__stop_event.isSet():
                try:
                    self.__update()
                    self.__stop_event.wait(self.__refresh_period)                    
                except:
                    print "Exception while updating HW!"
            time.sleep(self.__refresh_period)

        else:            
            try:
                self.__update()                
            except:
                print "Exception while updating HW!"



    def join(self, timeout=None):

        self.__stop_event.set()
        threading.Thread.join(self, timeout)



    def set_auto_refresh(self, auto_refresh):
        self.__auto_refresh = auto_refresh
        


    def __load_hardware(self, hw_man):
        
        """ Tries to load all HW items present in the connection file.
        If there is an exception while getting the device, the IP end point
        will not be added to the monitoring list"""
        
        for dev_name in hw_man.getDevices():

            try:
                device = hw_man.getDevice(dev_name)
                    
                if dev_name not in self.__ip_end_points_dict:
                    self.__ip_end_points_dict[dev_name] = IPEndPoint(device)
            
            except pycohal.exception, e:
                print "Pycohal exception while getting device %s: %s" % (dev_name, e)
                    
        
        
    def __update(self):

        for name in self.__ip_end_points_dict.keys():          
            print "Updating ip end point ", name
            self.__ip_end_points_dict[name].update()
