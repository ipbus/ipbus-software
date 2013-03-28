import wx

from uhal.gui.utilities import hardware


class HardwareTree(wx.Frame):

    def __init__(self, parent):

        wx.Frame.__init__(self, parent, title="uTCA hardware", size=(500,400))
        self.tree = wx.TreeCtrl(self)
        root = self.tree.AddRoot("uTCA HW")

        hw = hardware.HardwareStruct()
        ip_end_points = hw.get_ip_end_points()
        print "IP end points in tree are:", ip_end_points
        self.add_tree_nodes(root)

        self.Bind(wx.EVT_TREE_ITEM_EXPANDED, self.on_item_expanded, self.tree)
        self.Bind(wx.EVT_TREE_ITEM_COLLAPSED, self.on_item_collapsed, self.tree)
        self.Bind(wx.EVT_TREE_SEL_CHANGED, self.on_sel_changed, self.tree)
        self.Bind(wx.EVT_TREE_ITEM_ACTIVATED, self.on_item_activated, self.tree)
        self.tree.Expand(root)


    def add_tree_nodes(self, root):
        pass



    def on_item_expanded(self, event):
        pass


    def on_item_collapsed(self, event):
        pass


    def on_sel_changed(self, event):
        pass


    def on_item_activated(self, event):
        pass

