import string

import wx
import wx.lib.scrolledpanel as scroll

from uhal.gui.guis.plotreg import Plot
from uhal.gui.guis.regvalues import RegValues
from uhal.gui.utilities.hardware import HardwareStruct



class NodeWidget(wx.Panel):
    
    def __init__(self, parent, n, colour):
        
        print "DEBUG: Init NodeWidget with id %s" % n.get_id()
        wx.Panel.__init__(self, parent)
        
        # Attributes
        self.__id      = n.get_id()
        self.__address = n.get_address()
        self.__mask    = n.get_mask()
        if not n.get_value():
            self.__value = "Unknown"
                 
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
        
        self.__id_field        = wx.StaticText(self, label=str(self.__id))
        self.__address_field   = wx.StaticText(self, label=hex(self.__address))
        self.__mask_field      = wx.StaticText(self, label=hex(self.__mask))
        self.__value_field     = wx.StaticText(self, label=str(self.__value))
        
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
        
        
    def update_value(self, value):
                
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
        
        
        
    
    def __on_click_regname(self, event):
        print "DEBUG: Plotting regname %s..." % self.__id
        self.__plotreg = Plot(self, self.__id)
        self.__plotreg.Show()
        
    
    def __on_click_regvalue(self, event):
        pass
        '''
        print "DEBUG: Showing values in different formats for register %s and value %s..." % (self.__id, self.__value)
        regvalue = RegValues(self, self.__id, self.__value)            
        regvalue.Show()
        '''
    
    
    def get_value_field(self):
        return self.__value_field


        
    
    

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
        
        self.SetBackgroundColour('#CCFFFF')
        self.__sizer = wx.BoxSizer(wx.HORIZONTAL)
        
        self.__wid_dict["name"]    = wx.StaticText(self, label="NAME")
        self.__wid_dict["address"] = wx.StaticText(self, label="ADDRESS")
        self.__wid_dict["mask"]    = wx.StaticText(self, label="MASK")
        self.__wid_dict["value"]   = wx.StaticText(self, label="VALUE")
        
        for name in self.__wid_order:
            self.__wid_dict[name].SetFont(wx.Font(8, wx.MODERN, wx.NORMAL, wx.NORMAL)) 
            self.__sizer.Add(self.__wid_dict[name], 1, wx.ALL, 1)
               
               
        self.SetSizer(self.__sizer)
        self.SetAutoLayout(True)
        self.__sizer.Fit(self)
        
         
                
                
class IpEndPointWidget(wx.Panel):
        
        
    def __init__(self, parent, name="", status=""):
        
        print "DEBUG: IPEndPointWidget created"
        wx.Panel.__init__(self, parent)    
        
        # Attributes      
        self.__wid_dict = {}
        self.__wid_order = ["name", "static"] 
        #self.__wid_order = ["name", "status", "static"]
        self.__nodes_dict = {}
        self.__id = name
           
        # Layout
        self.__do_layout()
        
        # Event handlers
        
        
    def __do_layout(self):
        
        self.__borders = wx.ALL       
                
        box = wx.StaticBox(self, -1, self.__id)
        self.__widget_sizer = wx.StaticBoxSizer(box, wx.VERTICAL)
            
        self.__wid_dict["name"]   = wx.StaticText(self, label="Name")         
        # self.__wid_dict["status"] = wx.StaticText(self, label="Status")
        self.__wid_dict["static"] = StaticFields(self)
        
        
        for name in self.__wid_order:
            borders = self.__borders
            
            if name != "static":
                borders = borders | wx.ALIGN_CENTER
                self.__wid_dict[name].SetFont(wx.Font(10, wx.MODERN, wx.NORMAL, wx.NORMAL))
            else:
                borders = borders | wx.EXPAND
            
            self.__widget_sizer.Add(self.__wid_dict[name], 1, borders , 1)       
        
        self.SetSizer(self.__widget_sizer)
        self.SetAutoLayout(True)
        self.__widget_sizer.Fit(self)
        
    
    def set_name(self, name):
        self.__wid_dict["name"].SetLabel(unicode(str(name)))
        
    '''    
    def set_status(self, status):
        self.__wid_dict["status"].SetLabel(unicode(str(status)))
    ''' 
        
    def add_node_row(self, n, colour):
        borders = self.__borders | wx.EXPAND
        id = n.get_id()
        print "Adding node row for node %s " % id
        self.__nodes_dict[id] = NodeWidget(self, n, colour) 
        print "Node widget for node %s is ready" % id   
        self.__widget_sizer.Add(self.__nodes_dict[id], 1, borders, 1)
        
        for child in n.get_children():
            self.add_node_row(child, colour)
        
    
    def update_node_value(self, n):
        print "DEBUG: Updating node %s with value %s" %(n.get_id(), n.get_value()) 
        self.__nodes_dict[n.get_id()].update_value(n.get_value())
        
        for child in n.get_children():
            self.update_node_value(child)        
    




class HardwareTablePanel(scroll.ScrolledPanel):
     
    def __init__(self, parent):
        print "DEBUG: HardwareTablePanel instantiated"
        scroll.ScrolledPanel.__init__(self, parent, -1)  
        
        # Attributes
        self.__global_sizer = None
        self.__widget_dict = {}
        
        # Layout
        self.__do_layout()
        
        # Event handlers
        
    
        
    def __do_layout(self):
        
        
        # self.SetBackgroundColour('Pink')
        box = wx.StaticBox(self, -1, "Register tables")
        self.__global_sizer = wx.StaticBoxSizer(box, wx.VERTICAL)        
        self.SetSizer(self.__global_sizer)
        
        # SetAutoLayout tells the window to use the sizer to position and size the components
        self.SetAutoLayout(True)
        self.SetupScrolling()
        self.__global_sizer.Fit(self)
        
    
    
    def draw_hw_naked_tables(self, hw):
        
        bck_colour = "#99CCFF"
        for ep in hw.get_ip_end_points():
            new_widget = IpEndPointWidget(self)
            
            ep_id = ep.get_id()
            new_widget.set_name(string.upper(ep_id))
            # new_widget.set_status(ep.get_status())
            
            for n in ep.get_nodes():                
                new_widget.add_node_row(n, bck_colour)
                
            if not self.__widget_dict.has_key(ep_id):
                self.__widget_dict[ep_id] = new_widget
        
                print "DEBUG: widget added in dictionary with name %s" % ep_id
                self.__global_sizer.Add(new_widget, 1, wx.EXPAND, 2)
                print "DEBUG: widget added to global sizer"
        
        self.__global_sizer.Layout()
    
    
    def on_hw_ready(self, hw):
        print "DEBUG: HW ready in HardwareTablePanel"
        self.__dress_up_tables(hw)
        
    
    
    def __dress_up_tables(self, hw):
        
        for ep in hw.get_ip_end_points():
            ep_id = ep.get_id()
            new_status = ep.get_status()
            
            w = self.__widget_dict[ep_id]
            # w.set_status(new_status)
            
            for n in ep.get_nodes():
                # Pass here 'address' instead of 'name' to identify the node because there could be name collisions (?)
                w.update_node_value(n)
        