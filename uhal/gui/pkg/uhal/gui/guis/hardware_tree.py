import logging, wx

from uhal.gui.utilities.hardware_monitoring import HardwareMonitoring


class HardwareTree(wx.Frame):

    def __init__(self, parent, hw):
            
        self.__logger = logging.getLogger('uhal.gui.guis.hardware_tree')  
        wx.Frame.__init__(self, parent, title="uTCA hardware", size=(500,400))    
        self.SetName('HardwareTree')     

        # Attributes
        self.__parent = parent
        self.__hw = hw   
        self.__ip_end_points = {}
        
        # GUIs Attributes
        self.__tree = None   
        self.__sizer = None  

        # Layout
        self.__do_layout()        
        
        # Event handlers
        self.Bind(wx.EVT_TREE_ITEM_EXPANDED, self.__on_item_expanded, self.__tree)
        self.Bind(wx.EVT_TREE_ITEM_COLLAPSED, self.__on_item_collapsed, self.__tree)
        self.Bind(wx.EVT_TREE_SEL_CHANGED, self.__on_sel_changed, self.__tree)
        self.Bind(wx.EVT_TREE_ITEM_ACTIVATED, self.__on_item_activated, self.__tree)
        
            

    def __do_layout(self):  
        """
        Adds the tree ctrl root node, and calls __add_tree_nodes to add the rest of them
        """                      
        
        if self.__hw:        
            tree_style = wx.TR_LINES_AT_ROOT | wx.TR_HAS_BUTTONS    
            self.__tree = wx.TreeCtrl(self, pos=wx.DefaultPosition, size=wx.DefaultSize, style=tree_style)
            root = self.__tree.AddRoot("uTCA HW")            
            self.__add_tree_nodes(root, self.__hw)  
                              
            self.__get_ip_points_nodes(root)
            self.__tree.Expand(root)                 
        else:
            self.__logger.error('Cannot add IP end Points to the HW tree!')                
        
        
        
    def __add_tree_nodes(self, parent, items):
        """
        Adds the nodes to the TreeCtrl object recursively
        """
                                        
        for k, v in items.iteritems():
            id = ''
            try:
                # Case node is an IP End point
                id = k.id()
            except AttributeError as e:
                # Case node is a Node
                id = k.getId()
                        
            if type(v) is not dict:
                # The node is a final child                
                self.__tree.AppendItem(parent, id)
            else:
                sub_parent = self.__tree.AppendItem(parent, id)
                self.__add_tree_nodes(sub_parent, v)       
                self.__tree.SortChildren(sub_parent)       
        
        
    
    def __get_ip_points_nodes(self, theroot):
        
        item, cookie = self.__tree.GetFirstChild(theroot)
        while item:
            self.__ip_end_points[self.__tree.GetItemText(item)] = item
            self.__logger.debug('added item id %s', self.__tree.GetItemText(item))
            item, cookie = self.__tree.GetNextChild(theroot, cookie)
            
            
    def __get_item_text(self, item):
        
        if item:
            return self.__tree.GetItemText(item)
        else:
            return ""
        
        
        
    def __on_item_expanded(self, event):
        self.__logger.debug('__on_item_expanded %s', self.__get_item_text(event.GetItem()))



    def __on_item_collapsed(self, event):
        self.__logger.debug('__on_item_collapsed %s', self.__get_item_text(event.GetItem()))       



    def __on_sel_changed(self, event):
        self.__logger.debug('__on_sel_changed %s', self.__get_item_text(event.GetItem()))
        result = []        
        self.__get_complete_id(event.GetItem(), result)               
        self.__logger.debug('Clicked on the item %s', str(result))   
        self.__parent.add_new_widget_to_panel(result)



    def __on_item_activated(self, event):
        self.__logger.debug('__on_item_activated %s', self.__get_item_text(event.GetItem()))
        
        
        
    def __get_complete_id(self, starting_point, result):
        
        if self.__tree.GetItemText(starting_point) == 'uTCA HW':
            return result
               
        result.insert(0, self.__tree.GetItemText(starting_point)) 
        item = self.__tree.GetItemParent(starting_point)        
       
        self.__get_complete_id(item, result)                 
        
    

    
    def update(self, hw):
        """
        Updates the status of the IP End points, changing their text colour
        """    
        self.__logger.debug('Updating HW tree')
        for k, v in self.__ip_end_points.iteritems():            
            self.__tree.SetItemTextColour(v, wx.Colour(0, 255, 127)) 
            if k not in hw:               
                self.__tree.SetItemTextColour(v, wx.Colour(205, 205, 193))
            
        

