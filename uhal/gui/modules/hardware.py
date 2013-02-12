#!/usr/bin/python

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

    def __init__(self_):

        self.status = status
        self.uri = uri
        self.id = id
        self.address_table = address_table
        self.nodeList = []

    def update(self):
        pass


        

class Hardware(IPEndPoint):

    def __init__(self, connection_file = ""):

        self.connection_file = connection_file
        self.ip_end_points_dict = {}
        self.hw_manager = pycohal.ConnectionManager(str("file://" + connection_file))

        #how to know whether hw_manager was correctly instantiated?
        _load_hardware(self.hw_manager)
        

    def _load_hardware(self, hw_man):

        

        

    def update(self):

        for name in self.ip_end_points_dict.keys():
            self.ip_end_points_dict[name].update()

            
            
        
