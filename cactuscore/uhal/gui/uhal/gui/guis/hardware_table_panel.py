import string, logging, wx

import wx.lib.scrolledpanel as scroll

from uhal.gui.guis.plotreg import Plot
from uhal.gui.guis.regvalues import RegValues
from uhal.gui.utilities.hardware import HardwareStruct



class NodeWidget(wx.Panel):
    
    def __init__(self, parent, name='', address='', mask='', value='', colour='#FFFFFF'):
                
        wx.Panel.__init__(self, parent)
        
        
        # Attributes
        self.__id      = name
        self.__address = address
        self.__mask    = mask
        self.__value   = value
        
        self.__wid_dict = {}
        self.__wid_order = ["id", "address", "mask", "value"]
        
        # Layout attributes
        self.__colour = colour
        self.__plotreg = None
        self.__values_window = None
        self.__sizer = None
        
        self.__id_field        = None
        self.__address_field   = None
        self.__mask_field      = None
        self.__value_field     = None
        
        # Layout
        self.__do_layout()
        
        # Event handlers
        self.__id_field.Bind(wx.EVT_LEFT_DOWN, self.__on_click_regname, self.__id_field)
        self.__value_field.Bind(wx.EVT_LEFT_DOWN, self.__on_click_regvalue, self.__value_field)
        
    
    def __do_layout(self):
        
        self.SetBackgroundColour(self.__colour)
        self.__sizer = wx.BoxSizer(wx.HORIZONTAL)
        
        self.__id_field        = wx.StaticText(self, label=str(self.__id), style=wx.ALIGN_LEFT)
        self.__address_field   = wx.StaticText(self, label=hex(self.__address), style=wx.ALIGN_CENTER_HORIZONTAL)
        self.__mask_field      = wx.StaticText(self, label=hex(self.__mask),  style=wx.ALIGN_CENTER_HORIZONTAL)
        self.__value_field     = wx.StaticText(self, label=str(self.__value), style=wx.ALIGN_CENTER_HORIZONTAL)
        
        self.__wid_dict["id"]      = self.__id_field
        self.__wid_dict["address"] = self.__address_field
        self.__wid_dict["mask"]    = self.__mask_field
        self.__wid_dict["value"]   = self.__value_field
        
        
        
        for w in self.__wid_order:
            self.__wid_dict[w].SetFont(wx.Font(8, wx.MODERN, wx.NORMAL, wx.NORMAL)) 
            self.__sizer.Add(self.__wid_dict[w], 1, wx.ALL, 1)
        
        self.SetSizer(self.__sizer)
        self.SetAutoLayout(True)
        self.__sizer.Fit(self)
        
    
            
    def update(self):
        pass
        '''
        try:
            value_to_set = hex(value)
        except TypeError:
            value_to_set = value
        
        self.__value = value_to_set
        self.__wid_dict["value"].SetLabel(self.__value)
        
        if self.__plotreg:
            
            self.__plotreg.add_pair(self.__value)
            self.__plotreg.plot()
            
        if self.__values_window:
            self.__values_window.update(self.__value)
        '''
        
        
    
    def __on_click_regname(self, event):
        #self.__logger.debug('Plotting regname %s...', self.__id)
        self.__plotreg = Plot(self, self.__id)
        self.__plotreg.Show()
        
    
    def __on_click_regvalue(self, event):
        pass
        '''
        print "DEBUG: Showing values in different formats for register %s and value %s..." % (self.__id, self.__value)
        regvalue = RegValues(self, self.__id, self.__value)            
        regvalue.Show()
        '''    

        
    
    

class StaticFields(wx.Panel):
            
    def __init__(self, parent):
        
        print "DEBUG: Init StaticFields"
        wx.Panel.__init__(self, parent)     
        
        # Attributes
        self.__wid_dict = {}
        self.__wid_order = ["name", "address", "mask", "value"]            
    
        self.__sizer = None
        
        # Layout
        self.__do_layout()
        
        
        
    def __do_layout(self):
        
        self.SetBackgroundColour('#6C7B8B')
        self.__sizer = wx.BoxSizer(wx.HORIZONTAL)
        
        self.__wid_dict["name"]    = wx.StaticText(self, label="NAME", style=wx.ALIGN_CENTER_HORIZONTAL)
        self.__wid_dict["address"] = wx.StaticText(self, label="ADDRESS", style=wx.ALIGN_CENTER_HORIZONTAL)
        self.__wid_dict["mask"]    = wx.StaticText(self, label="MASK", style=wx.ALIGN_CENTER_HORIZONTAL)
        self.__wid_dict["value"]   = wx.StaticText(self, label="VALUE", style=wx.ALIGN_CENTER_HORIZONTAL)
        
        for name in self.__wid_order:
            self.__wid_dict[name].SetFont(wx.Font(8, wx.MODERN, wx.NORMAL, wx.NORMAL)) 
            self.__sizer.Add(self.__wid_dict[name], 1, wx.ALL, 1)
               
               
        self.SetSizer(self.__sizer)
        self.SetAutoLayout(True)
        self.__sizer.Fit(self)
        
                     
    
    
class Widget(wx.Panel):
    
    def __init__(self, parent, id='', status='OK'):
        
        self.__logger = logging.getLogger('uhal.gui.guis.hw_table_panel.Widget')
        wx.Panel.__init__(self, parent)    
        
        # Attributes                            
        self.__id = id
        self.__row_colour = '#99CCFF'
        self.__nodes_dict = {}
           
        # Layout
        self.__do_layout()
        
        self.__logger.debug('Widget created with id %s', self.__id)
    
    
    
    def __do_layout(self):                
                
        self.SetBackgroundColour(wx.WHITE)
        box = wx.StaticBox(self, -1)
        self.__sizer = wx.StaticBoxSizer(box, wx.VERTICAL)
            
        borders = wx.ALL | wx.ALIGN_CENTER 
        self.__id_field = wx.StaticText(self, label = self.__id[:self.__id.find('_')].upper()) 
        self.__id_field.SetFont(wx.Font(10, wx.MODERN, wx.NORMAL, wx.FONTWEIGHT_BOLD))
        
        self.__sizer.Add(self.__id_field, 1, borders, 1)   
        
        borders =  wx.ALL | wx.EXPAND          
        self.__static_field = StaticFields(self)
        self.__sizer.Add(self.__static_field, 1, borders, 1)  
        
        self.SetSizer(self.__sizer)
        self.SetAutoLayout(True)
        self.__sizer.Fit(self)
        
        
        
    def add_row(self, name, address, mask, value):
                            
        self.__row_colour = (self.__row_colour == '#B9D3EE') and '#9FB6CD' or '#B9D3EE'
                  
        borders = wx.ALL | wx.EXPAND   
        node = NodeWidget(self, name, address, mask, value, self.__row_colour)       
        self.__nodes_dict[name] = node       
        self.__sizer.Add(self.__nodes_dict[name], 1, borders, 1)
        
        self.__sizer.Layout()
        
        
    
    def update(self):
        for k, v in self.__nodes_dict.iteritems():
            self.__logger.debug('Updating node %s', k)
            v.update()        
    
    


class HardwareTablePanel(scroll.ScrolledPanel):
    """
    Main class. It is a panel object with scroll bars that hosts all the widgets.
    The widgets are added both to a sizer object and a dictionary, which is later used to update their values
    """
    def __init__(self, parent):
                
        self.__logger = logging.getLogger('uhal.gui.guis.hw_table_panel')
        
        self.__logger.debug('HardwareTablePanel instantiated')
        scroll.ScrolledPanel.__init__(self, parent, -1)  
        
        # Attributes
        self.__global_sizer = None
        self.__widget_sizer = None
        self.__children = {}
        
        # Layout
        self.__do_layout()
        
        # Event handlers
        
    
        
    def __do_layout(self):
        
        """
        The self.__global_sizer has been added just because I wanted it to be a StaticBox
        The self.__widget_sizer is the sizer object hosting the widgets. 
        """
        self.SetBackgroundColour(wx.Colour(222, 222, 222))
        box = wx.StaticBox(self, -1, "Hardware Panel")        
        self.__global_sizer = wx.StaticBoxSizer(box, wx.VERTICAL)  
        self.__widget_sizer = wx.FlexGridSizer(0, 3, 0, 0)         
        self.__global_sizer.Add(self.__widget_sizer)
        self.SetSizer(self.__global_sizer)
        
        # SetAutoLayout tells the window to use the sizer to position and size the components
        self.SetAutoLayout(True)
        self.SetupScrolling()
        self.__global_sizer.Fit(self)
       
    
    
    def add_new_widget(self, nodes, hw_tree):                       
        """
        Adds new widget to the panel. 
        The widget ID consists on the IP end point name + the number of widget on the panel 
        The self.__fill_widget method is called to fill the widget
        """
        
        self.SetBackgroundColour(wx.WHITE)
        id = nodes[0] + '_' + str(len(self.__children))
        widget = Widget(self, id)
        self.__children[id] = widget
                        
        self.__fill_widget(nodes, hw_tree, widget) 
        self.__widget_sizer.AddWindow(widget) 
        self.__widget_sizer.FitInside(self)  
                        
            
            
            
    def update(self, hw_info):
        """
        The method is called from the defaultgui class every time the HW has been read. 
        Loops through all the widgets, calling their update method
        """
        for k, v in self.__children.iteritems():
            self.__logger.debug('Updating widget %s ', k)
            v.update()
          
          
          
    def clear(self):
        """
        The method is called from the defaultgui class' menu bar. It erases 'graphically' the widgets that 
        had been added to the panel. It also removes them from the self.__children dictionary (to prevent them from being updated)
        """
        self.__children.clear()
        self.__widget_sizer.DeleteWindows()  
        self.SetBackgroundColour(wx.Colour(222, 222, 180))  
         
    
    
    
    def __fill_widget(self, nodes, hw_tree, widget):
        """
        nodes: list of node names that are needed to traverse the tree until the node that was selected from the tree window is reached
        hw_tree: HW representation in tree-shape
        widget: the brand new widget that has to be fulfilled
        
        The method first traverses the tree until getting to the node that has to be represented. Then calls self.__fill_widget_nodes to fill the widget
        with the this node's information and children
        """   
         
        id = ''
        start_item = None
        
        for n in nodes:
                        
            for k, v in hw_tree.iteritems():
              
                try:
                    # Item is a Node object 
                    id = k.getId()
                except AttributeError, e:
                    # The item is an IP End point object (HardwareInterface)
                    id = k.id()
                
                if id != n:                    
                    continue
                
                start_item = k                
                hw_tree = v
                break   
                       
        self.__fill_widget_nodes(start_item, hw_tree, widget)  
                                
       
        
    def __fill_widget_nodes(self, start_item, hw_tree, widget):
        """
        The method fills the widget
        """
        
        # We have arrived to a tree leaf (register - value)
        if type(hw_tree) is not dict:              
            value = str(hw_tree)
            widget.add_row(start_item.getId(), start_item.getAddress(), start_item.getMask(), value)
            return 
        
        # The item being analyzed is a Node (uhal Node object) composed of more nodes
        # In such a case, we add the node info + N/A in its value
        if 'HwInterface' not in str(type(start_item)):
            widget.add_row(start_item.getId(), start_item.getAddress(), start_item.getMask(), 'N/A')
        
        # Whether the item being analyzed is a Node that has children or an IP end point (uhal HwInterface object), we call the algorithm recursively 
        for k, v in hw_tree.iteritems():            
            self.__fill_widget_nodes(k, v, widget)                       
                                     