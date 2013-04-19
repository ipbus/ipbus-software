import sys
import threading
import time

import uhal
from uhal.gui.utilities.hardware import HardwareStruct, IPEndPoint, Node


class HardwareMonitoring(threading.Thread):

    def __init__(self, connection_file="", id="", uri="", address_table=""):

        threading.Thread.__init__(self)

        self.__conn_file = connection_file
        self.__id = id
        self.__uri = uri
        self.__add_table = address_table
 
        self.__hw_struct = None

        if self.__conn_file != "":
            self.__hw_manager = uhal.ConnectionManager(str("file://" + self.__conn_file))
        else:
            self.__hw_manager = uhal.ConnectionManager(self.id, self.uri, self.address_table)

        if self.__hw_manager:
            self.__load_hardware()
        else:
            print "ERROR: could not start HW Manager, cannot start default GUI"


    def __load_hardware(self):

        """ Tries to load all HW items present in the connection file.
        If there is an exception while getting the device, the IP end point
        will not be added to the monitoring list """

        self.__hw_struct = HardwareStruct()

        for dev_name in self.__hw_manager.getDevices():        

            dev_object = self.__hw_manager.getDevice(dev_name)
            ip_end_point = IPEndPoint(dev_object)

            
            for n in self.__get_parent_nodes(dev_object):

                node_object = dev_object.getNode(n)
                node = Node(node_object)
                ip_end_point.add_node(node)
            
            
            self.__hw_struct.add_ip_end_point(ip_end_point)            
            
        self.__print_hardware()


    def __get_parent_nodes(self, device):
        return device.getNodes("[^.]*")

    
    def __print_hardware(self):

        for i in self.__hw_struct.get_ip_end_points():       
            i.print_ip_end_point()

                
    def run(self):
       
        for i in self.__hw_struct.get_ip_end_points():
            print "Updating IP End Point %s" % i.get_id()
            ##### get the status here, or make the IP end point object to find it out #####
            i.set_status("Undefined")

            for n in i.get_nodes():
                print "Updating node %s" % n.get_name()
                self.__update(i.get_id(), n)
    
        
            

    def __update(self, ip_ep_name, node):
        
        ip_ep = self.__hw_manager.getDevice(ip_ep_name)
        
        if not node.has_kids():
            
            try:
                new_value = ip_ep.getNode(node.get_name()).read()
                ip_ep.dispatch()
                print "Setting value %s in node %s, parent %s" % (new_value, node.get_name(), node.get_parent())
                node.set_value(new_value)

            except Exception, e:
                print "ERROR: %s" % str(e)
        
        for n in node.get_kids():
            self.__update(ip_ep_name, n)
        

    def stop(self):    
        self.__stop.set()


    def __is_stopped(self):
        return self.__stop.isSet()
