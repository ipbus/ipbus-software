import wx

from uhal.gui.utilities.hardware import HardwareStruct, IPEndPoint, Node
from uhal.gui.utilities.hardware_monitoring import HardwareMonitoring


class HardwareTree(wx.Frame):

    def __init__(self, parent, hw):
        print "DEBUG: Hardware tree instantiated"
        wx.Frame.__init__(self, parent, title="uTCA hardware", size=(500,400))

        # Attributes
        self.__hw = hw
        
        # GUIs Attributes
        self.__tree = None     

        # Layout
        self.__do_layout()
        
        
        # Event handlers
        self.Bind(wx.EVT_TREE_ITEM_EXPANDED, self.on_item_expanded, self.__tree)
        self.Bind(wx.EVT_TREE_ITEM_COLLAPSED, self.on_item_collapsed, self.__tree)
        self.Bind(wx.EVT_TREE_SEL_CHANGED, self.on_sel_changed, self.__tree)
        self.Bind(wx.EVT_TREE_ITEM_ACTIVATED, self.on_item_activated, self.__tree)
        
            

    def __do_layout(self):
        
        ip_end_pts = self.__hw.get_ip_end_points()
        
        if ip_end_pts:
            self.__tree = wx.TreeCtrl(self, pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.TR_DEFAULT_STYLE)
            root = self.__tree.AddRoot("uTCA HW")
            self.add_tree_nodes(root, ip_end_pts)
            self.__tree.Expand(root)
        else:
            print "ERROR: cannot add IP END POINTS TO HW tree!"
        
        
    def add_tree_nodes(self, parent, items):
        
        
        for item in items:
            if item.has_no_children():
                self.__tree.AppendItem(parent, item.get_id())
            else:
                sub_parent = self.__tree.AppendItem(parent, item.get_id())
                self.add_tree_nodes(sub_parent, item.get_children())
        
        


    def get_item_text(self, item):
        
        if item:
            return self.__tree.GetItemText(item)
        else:
            return ""
        
        
    def on_item_expanded(self, event):
        print "on_item_expanded", self.get_item_text(event.GetItem())


    def on_item_collapsed(self, event):
        print "on_item_collapsed", self.get_item_text(event.GetItem())


    def on_sel_changed(self, event):
        print "on_sel_changed", self.get_item_text(event.GetItem())


    def on_item_activated(self, event):
        print "on_item_activated", self.get_item_text(event.GetItem())
    
    
    def redraw(self, hw):
        print "DEBUG: redrawing upon HW update"

