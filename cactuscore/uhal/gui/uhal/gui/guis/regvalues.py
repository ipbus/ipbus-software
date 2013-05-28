import wx


class RegValues(wx.Frame):    
    
    def __init__(self, parent, regname, regvalue):        
        
        wx.Frame.__init__(self, parent, wx.ID_ANY, regname, size=(400, 300))
        
        # Attributes        
        self.__id = regname        
        self.__global_sizer = None
        self.__value_reps = {"HEX" : regvalue,
                             "DEC" : "Unknown",
                             "OCT" : "Unknown",
                             "BIN" : "Unknown"}
        
        self.__parent = parent
        
        # Layout
        self.__do_layout()
        
        # Event handlers
        
        
    def __do_layout(self):
                
        box = wx.StaticBox(self, -1, "Register values")
        self.__global_sizer = wx.StaticBoxSizer(box, wx.VERTICAL)    
        
        for k,v in self.__value_reps.items():
            
            line_sizer = wx.BoxSizer(wx.HORIZONTAL) 
            name = wx.StaticText(self, label=k)            
            value = wx.StaticText(self, label=v)
            line_sizer.Add(name, 1, wx.ALL, 1)
            line_sizer.Add(value, 1, wx.ALL, 1)
            
            self.__global_sizer.Add(line_sizer, 1, wx.ALL, 1)  
            
            
        self.SetSizer(self.__global_sizer)
        
        # SetAutoLayout tells the window to use the sizer to position and size the components
        self.SetAutoLayout(True)    
        
    
    def update(self, value):
        pass
                
  