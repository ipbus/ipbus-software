from __future__ import print_function

import uhal

########## NODE RELATED CLASSES ##########


class Node:

    def __init__(self, node, parent_id=""):

        if not node: 
            print("ERROR: tried to initialize Node object with invalid uhal Node argument")
            return
               
        self.__id = node.getId()
        if parent_id:
            self.__id = parent_id + "." + self.__id
            
        self.__address = node.getAddress()
        self.__mode = str(node.getMode())
        self.__permission = str(node.getPermission())
        self.__mask = node.getMask()
        self.__value = ""
        self.__size = node.getSize()
        self.__tags = node.getTags()

        print("DEBUG: Creating node %s %s" % (self.__id, self.__permission))
        self.__parent = parent_id
        self.__children = []

        for n in self.__get_parent_nodes(node):
            new_node = node.getNode(n)
            kid_node = Node(new_node, parent_id=self.__id)
            self.__add_kid(kid_node)

            
    def __add_kid(self, node):
        print("DEBUG: Adding kid %s to node %s" % (node.get_id(), self.get_id()))
        self.__children.append(node)

    
    def __get_parent_nodes(self, node):
        return node.getNodes("[^.]*")
    
    def has_no_children(self):
        if not self.__children:
            return True
    
        return False
    
    
    def get_children(self):
        return self.__children
    
    def get_id(self):
        return self.__id

    def get_address(self):
        return self.__address

    def get_mode(self):
        return self.__mode

    def get_permission(self):
        return self.__permission

    def get_mask(self):
        return self.__mask

    def get_value(self):
        return self.__value

    def get_size(self):
        return self.__size

    def get_tags(self):
        return self.__tags    

    def get_parent(self):
        return self.__parent    

    def set_value(self, value):
        self.__value = value


    def print_node(self):
        print("DEBUG: Node: %s %s %s" % (self.__id, self.__mode, self.__permission))
        for c in self.__children:
            print("\t",c.print_node())
     



        
########## HW & IP END POINT CLASSES ##########
    
class IPEndPoint:

    def __init__(self, ip_ep, status="Undefined"):

         if not ip_ep:
             print("ERROR: tried to initialize IPEndPoint object with invalid uhal IP End Point argument")
             return
         
         self.__id = ip_ep.id()
         self.__uri = ip_ep.uri()        
         self.__status = status        
         self.__nodes_list = []
         print("DEBUG: IP End Point instantiated: %s %s %s" %(self.__id, self.__uri, self.__status))
         


    def add_node(self, node):
        print("DEBUG: Adding node %s to the IP End Point %s" % (node.get_id(), self.__id))
        self.__nodes_list.append(node)
        
    def get_nodes(self):
        return self.__nodes_list
        
    def has_no_children(self):
        return not self.__nodes_list
    
    def get_children(self):
        return self.__nodes_list
        
    def get_id(self):
        return self.__id
    
    def set_id(self, id):
        self.__id = id
          
    def get_status(self):
        return self.__status

    def set_status(self, status):
        self.__status = status
         
    def get_uri(self): 
        return self.__uri
    
    def set_uri(self, uri):
        self.__uri = uri

          
    def print_ip_end_point(self):
        print("DEBUG: IP End Point: %s %s %s" % (self.__id, self.__uri, self.__status))
        
        for n in self.__nodes_list:
            n.print_node()
        
            
      
class HardwareStruct:
    """
    The class maps the HW structure (collection of IP end points and derived nodes), 
    and provides methods to update the HW via the Pycohal bindings. 
    """
    
    def __init__(self, connection_file, id=None, uri=None, address_table=None):
        """
        Gets the parameters to build the necessary Pycohal's ConnectionManager object.
        If it succeeds, the HW mapping starts
        """
        
        self.__hw_manager = None
        self.__ip_end_points = []
        # self.__ip_to_nodeId_to_value = {}
        
        if connection_file:            
            self.__connection_file = connection_file
            self.__hw_manager = uhal.ConnectionManager(str("file://" + self.__connection_file))
            
            print("DEBUG: HardwareStruct instantiated with file name %s" % self.__connection_file)
        elif id and uri and address_table:            
            self.__id = id
            self.__uri = uri
            self.__address_table = address_table
            self.__hw_manager = uhal.ConnectionManager(self.__id, self.__uri, self.__address_table)
            
            print("DEBUG: HardwareStruct instantiated with id: %s, uri: %s, address_table: %s" % (self.__id, self.__uri, self.__address_table))
        else:
            print("ERROR: HardwareStruct object not properly instantiated")
        
        
        if self.__hw_manager:
            self.__load_hardware()
        else:
            print("ERROR: could not start uhal.ConnectionManager object. Cannot start the default gui")
        
        

    def __load_hardware(self):

        """ 
        Tries to load all HW items present in the connection file.
        If there is an exception while getting the device, the IP end point
        will not be added to the monitoring list 
        """
        
        for dev_name in self.__hw_manager.getDevices():        

            dev_object = self.__hw_manager.getDevice(dev_name)
            ip_end_point = IPEndPoint(dev_object)
            # self.__ip_to_nodeId_to_value[dev_name] = {}
            
            for n in self.__get_parent_nodes(dev_object):

                node_object = dev_object.getNode(n)
                node = Node(node_object)
                ip_end_point.add_node(node)
                # self.__ip_to_nodeId_to_value[dev_name][n] = 0
                    
            self.add_ip_end_point(ip_end_point)            
            
        #self.__print_hardware()


    def __get_parent_nodes(self, device):
        """
        Given an IP End Point, return the 'first level nodes' of it.
        """
        return device.getNodes("[^.]*")


    def __print_hardware(self):

        for i in self.get_ip_end_points():       
            i.print_ip_end_point()


    ##### PUBLIC API #####
    
    def get_hw_manager(self):
        return self.__hw_manager
    

    
    def add_ip_end_point(self, ip_ep):
        print("DEBUG: Adding IP End Point %s to the IP End Point list" % ip_ep.get_id())
        self.__ip_end_points.append(ip_ep)



    def get_ip_end_points(self):
        return self.__ip_end_points
    
    
    
    def update_node_value(self, dev, node_id, node_value):
        self.__ip_to_nodeId_to_value[dev][node_id] = node_value
        


