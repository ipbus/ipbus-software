import wx


class RefreshButtonsPanel(wx.Panel):

    def __init__(self, parent):
        print("DEBUG: Refresh button panel instantiated")
        wx.Panel.__init__(self, parent)


        # Attributes
        self.__parent = parent
        #self.__auto_refresh_box = wx.CheckBox(self, -1, "Auto-refresh")
        self.__refresh_button = wx.Button(self, -1, "Refresh")        
        
        # Layout
        self.__do_layout()
        
        # Event handlers
#        self.Bind(wx.EVT_CHECKBOX, self.__on_click_auto_refresh, self.__auto_refresh_box)
        self.Bind(wx.EVT_BUTTON, self.__on_click_refresh, self.__refresh_button)
        
        
    def __do_layout(self):
        
        # self.SetBackgroundColour('White')
        box = wx.StaticBox(self, -1, "Refresh Controls")
        sizer = wx.StaticBoxSizer(box, wx.HORIZONTAL)
        
        #sizer.Add(self.__auto_refresh_box, 0, wx.ALL, 5)
        sizer.Add(self.__refresh_button, 0, wx.ALL, 5)       
        
        self.SetSizer(sizer)
        self.SetAutoLayout(True)
        sizer.Fit(self)

    def __on_click_refresh(self, event):
        print("DEBUG: Refresh button clicked. Calling default GUI")
        self.__parent.start_hw_thread()        

    '''
    def __on_click_auto_refresh(self, event):
        print "DEBUG: Setting auto-refresh option %s" % self.__auto_refresh_box.GetValue()       
        """ Access HW cache and print values periodically """
    '''