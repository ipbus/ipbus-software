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
        
        
        for i in self.__hw.get_ip_end_points():
            time.sleep(20)
            '''
            print "DEBUG: Updating IP End Point %s" % i.get_id()
            ##### get the status here, or make the IP end point object to find it out #####
            i.set_status("Undefined")

            for n in i.get_nodes():                             
                                                 
                print "DEBUG: Updating node %s" % n.get_id()
                reg_value = self.__update(i.get_id(), n)
                print "DEBUG: returned value is %s " % reg_value
                
            '''
        evt = HwReadyEvent(self.myEVT_HWREADY, -1, self.__hw)
        wx.PostEvent(self.__parent, evt)

            
            
    def __update(self, ip_ep_name, node):
        
        hw_man = self.__hw.get_hw_manager()
        ip_ep = hw_man.getDevice(ip_ep_name)
        new_value = ""
        
        if node.has_no_children():
            
            try:
                new_value = ip_ep.getNode(node.get_id()).read()
                ip_ep.dispatch()
                print "DEBUG: Setting value %s in node %s, parent %s" % (new_value, node.get_id(), node.get_parent())
                node.set_value(new_value)
                

            except Exception, e:
                print "ERROR: %s" % str(e)
        
        for n in node.get_children():
            self.__update(ip_ep_name, n)
            
        return new_value
    
    
    def __print_hardware(self):

        for i in self.__hw.get_ip_end_points():       
            i.print_ip_end_point()