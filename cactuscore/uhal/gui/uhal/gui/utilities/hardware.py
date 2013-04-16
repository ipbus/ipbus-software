import threading
import time



########## NODE RELATED CLASSES ##########

    
class Node:

    def __init__(self, node, parent_name=""):

        self.__name = node.getId()
        if parent_name:
            self.__name = parent_name + "." + self.__name
            
        self.__address = node.getAddress()
        self.__mode = str(node.getMode())
        self.__permission = str(node.getPermission())
        self.__mask = node.getMask()
        self.__value = ""
        self.__size = node.getSize()
        self.__tags = node.getTags()

        print "Creating node %s %s" % (self.__name, self.__permission)
        self.__parent = parent_name
        self.__children = []

        for n in self.__get_parent_nodes(node):
            new_node = node.getNode(n)
            kid_node = Node(new_node, parent_name=self.__name)
            self.__add_kid(kid_node)

            
    def __add_kid(self, node):
        print "Adding kid %s to node %s" % (node.get_name(), self.get_name())
        self.__children.append(node)

    
    def __get_parent_nodes(self, node):
        return node.getNodes("[^.]*")

    
    def get_name(self):
        return self.__name

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

    def get_kids(self):
        return self.__children

    def get_parent(self):
        return self.__parent

    def has_kids(self):

        if not self.__children:
            return False

        return True

    def set_value(self, value):
        self.__value = value


    def print_node(self):
        print "Node: %s %s %s" % (self.__name, self.__mode, self.__permission)
        for c in self.__children:
            print "\t",c.print_node()
     



        
########## HW & IP END POINT CLASSES ##########
    
class IPEndPoint:

    def __init__(self, ip_ep, status="Undefined"):

        self.__id = ip_ep.id()
        self.__uri = ip_ep.uri()        
        self.__status = status        

        print "IP End Point instantiated: %s %s %s" %(self.__id, self.__uri, self.__status)
        self.__nodes_list = []


    def add_node(self, node):
        print "Adding node %s to the IP End Point %s" % (node.get_name(), self.__id)
        self.__nodes_list.append(node)


    def get_id(self):
        return self.__id


    def set_status(self, status):
        self.__status = status


    def get_nodes(self):
        return self.__nodes_list


    def print_ip_end_point(self):
        print "IP End Point: %s %s %s" % (self.__id, self.__uri, self.__status)
        for n in self.__nodes_list:
            n.print_node()

            
      
class HardwareStruct:

    def __init__(self):
        self.__ip_end_points_list = []


    def add_ip_end_point(self, ip_ep):

        print "Adding IP End Point %s to the IP End Point list..." % ip_ep.get_id()
        self.__ip_end_points_list.append(ip_ep)


    def get_ip_end_points(self):
        return self.__ip_end_points_list



def get_hardware():
    return HardwareStruct()



