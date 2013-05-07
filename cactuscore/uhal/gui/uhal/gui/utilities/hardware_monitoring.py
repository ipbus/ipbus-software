import sys
import threading
import time

import wx

import uhal

from uhal.gui.utilities.utilities import HwReadyEvent
from uhal.gui.utilities.hardware import HardwareStruct, IPEndPoint, Node


class HardwareMonitoring(threading.Thread):

    myEVT_HWREADY = wx.NewEventType()
    EVT_HWREADY = wx.PyEventBinder(myEVT_HWREADY, 1)
    
    def __init__(self, parent, hw):

        threading.Thread.__init__(self)
        self.__parent = parent      
        self.__hw = hw


    def run(self):
        print "DEBUG: Checking HW... start!"
        '''
        cm = self.__hw.get_hw_manager()
        for dev in cm.getDevices():
            dev_object = cm.getDevice(dev)
            
            for n in dev_object.getNodes():
                value = dev_object.getNode(n).read()
                dev_object.dispatch()
                self.__hw.update_node_value(dev, n, value)
        '''         
        
        
        for i in self.__hw.get_ip_end_points():            
            
            print "DEBUG: Updating IP End Point %s" % i.get_id()
            ##### get the status here, or make the IP end point object to find it out #####
            i.set_status("Undefined")
            
            # Do the following ONLY if status is OK:
            '''
            if i.get_status() != "OK":
                continue
            '''
            for n in i.get_nodes():                                             
                print "DEBUG: Updating node %s" % n.get_id()
                self.__update(i.get_id(), n)
      
                
        
        evt = HwReadyEvent(self.myEVT_HWREADY, -1, self.__hw)
        wx.PostEvent(self.__parent, evt)

            
            
    def __update(self, ip_ep_name, node):      
        
        hw_man = self.__hw.get_hw_manager()
        ip_ep = hw_man.getDevice(ip_ep_name)
        new_value = ""
        
        try:
        # Before reading, check that node permission's allow to read
            if node.has_no_children() and ("READ" in node.get_permission()):            
                node_id = node.get_id()
                new_value = ip_ep.getNode(node_id).read()
                ip_ep.dispatch()
                print "DEBUG: Setting value %s in node %s, parent %s" % (new_value, node_id, node.get_parent())
                node.set_value(new_value)
        except Exception, e:
            print "ERROR: updating the node %s from HW device %s" %(node_id, ip_ep_name)
                            
        
        for n in node.get_children():           
            self.__update(ip_ep_name, n)        
    
    
    def __print_hardware(self):

        for i in self.__hw.get_ip_end_points():       
            i.print_ip_end_point()