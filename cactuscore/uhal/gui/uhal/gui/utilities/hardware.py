import os

import pycohal

class AbstractNode:

    def __init__(self):
        pass

    def update(self):
        pass


class MemoryNode(AbstractNode):

    def __init__(self):
        pass

    def update(self):
        pass


    
class RegisterNode(AbstractNode):

    def __init__(self):

        self.id = id
        self.address = address
        self.permission = permission
        self.tag = tag
        self.mask = mask

    def update(self):
        pass
    



class IPEndPoint(AbstractNode):

    def __init__(self, device, status = "Undefined"):

        print "Instantiating end point ", device.id()
        self.uri = device.uri()
        self.id = device.id()
        self.nodeList = device.getNodes()
        self.status = status
    
    def update(self):
        print "update in IP end point ", self.id, " ", self.uri, " was called"


        

class Hardware(IPEndPoint):

    def __init__(self, connection_file = ""):

        self.connection_file = connection_file
        self.ip_end_points_dict = {}
        self.hw_manager = pycohal.ConnectionManager(str("file://" + connection_file))

        self._load_hardware(self.hw_manager)
        
        # start thread to update ???
        

    def _load_hardware(self, hw_man):

        for dev_name in hw_man.getDevices():
            
            device = hw_man.getDevice(dev_name)
           
            if dev_name not in self.ip_end_points_dict:
                self.ip_end_points_dict[dev_name] = IPEndPoint(device)
            
        

    def update(self):

        for name in self.ip_end_points_dict.keys():
            print "Updating ip end point ", name
            self.ip_end_points_dict[name].update()

            
            
        
