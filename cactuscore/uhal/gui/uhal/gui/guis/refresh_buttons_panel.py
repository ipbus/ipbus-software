import wx


class RefreshButtonsPanel(wx.Panel):

    def __init__(self, parent):
        print "DEBUG: Refresh button panel instantiated"
        wx.Panel.__init__(self, parent)


        # Attributes
        self.__parent = parent
        self.__auto_refresh_box = wx.CheckBox(self, -1, "Auto-refresh")
        self.__refresh_button = wx.Button(self, -1, "Refresh")
        self.__register = wx.StaticText(self, label="REG")
        self.__register.SetFont(wx.Font(16, wx.MODERN, wx.NORMAL, wx.NORMAL))
        self.__regvalue = wx.StaticText(self, label="Undefined")
        self.__regvalue.SetFont(wx.Font(16, wx.MODERN, wx.NORMAL, wx.NORMAL))        
        
        # Layout
        self.__do_layout()
        
        # Event handlers
        self.Bind(wx.EVT_CHECKBOX, self.__on_click_auto_refresh, self.__auto_refresh_box)
        self.Bind(wx.EVT_BUTTON, self.__on_click_refresh, self.__refresh_button)
        

    def set_hw(self, hw):
        #self.__hw = hw
        #ip_end_point = hw.get_ip_end_points()
        print "DEBUG: HW set in refresh button panel. IP End Points:"
        for i in ip_end_point:
            i.print_ip_end_point()
        
        
    def __do_layout(self):
        
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        
        sizer.Add(self.__auto_refresh_box, 0, wx.ALIGN_CENTER, 10)
        sizer.Add(self.__refresh_button, 0, wx.ALIGN_CENTER, 200)
        sizer.Add(self.__register, 0, wx.ALIGN_CENTER, 300)
        sizer.Add(self.__regvalue, 0, wx.ALIGN_CENTER, 400)
        
        self.SetSizer(sizer)


    def __on_click_refresh(self, event):
        print "DEBUG: Refresh button clicked. Calling default GUI"
        self.__parent.start_hw_thread()        


    def __on_click_auto_refresh(self, event):
        print "DEBUG: Setting auto-refresh option %s" % self.__auto_refresh_box.GetValue()       
        """ Access HW cache and print values periodically """


    def on_hw_ready(self, value):  
        print "DEBUG: HW ready on refresh_buttons_panel"                
        self.__regvalue.SetLabel(unicode(str(value)))
    