import time

import wx
from wx.lib.plot import PlotCanvas, PlotGraphics, PolyLine


class Plot(wx.Frame):
    
    def __init__(self, parent, regname):
        wx.Frame.__init__(self, parent, wx.ID_ANY, regname, size=(400, 300))
        
        # Attributes
        self.__panel = wx.Panel(self, wx.ID_ANY)        
        self.__id = regname
        self.__data = []
        self.__first_time_point = -1
        
        # Layout attributes
        self.__toggle_grid   = wx.CheckBox(self.__panel, label="Show Grid")
        self.__main_sizer = None
        self.__check_sizer = None
        self.__canvas = None
        
        # Layout
        self.__do_layout()
        
        # Handlers
        self.Bind(wx.EVT_CHECKBOX, self.__on_toggle_grid, self.__toggle_grid)
       
    
    def __do_layout(self):
        
        self.__panel.SetBackgroundColour("Gray")
        
        self.__main_sizer  = wx.BoxSizer(wx.VERTICAL)
        self.__check_sizer = wx.BoxSizer(wx.HORIZONTAL)
        
        self.__canvas = PlotCanvas(self.__panel)  
        
        self.__check_sizer.Add(self.__toggle_grid, 0, wx.ALL, 5)
        self.__main_sizer.Add(self.__canvas, 1, wx.EXPAND)
        self.__main_sizer.Add(self.__check_sizer)
        self.__panel.SetSizer(self.__main_sizer)
        self.__panel.SetAutoLayout(True)
        
        self.plot()
        
    
    def add_pair(self, y):
        
        x = int(time.time())
        if not self.__data:
            self.__first_time_point = x
                            
        # Restrict number of maximum plotted points to 100
        if len(self.__data) >= 100:
        	new_data = self.__data[2:]
        	self.__data = new_data
        	
        time_elapsed = x - self.__first_time_point   
        
        if self.__data:
            previous_y = self.__data[-1][1]
            print("previous y is %s" % previous_y)
            self.__data.append((time_elapsed, previous_y))         
        
        self.__data.append((time_elapsed, y))
              
        
    def plot(self):
        print("DEBUG: plotting with data", str(self.__data))
        self.__canvas.Draw(self.__draw_reg_plot())
    
    
    def __draw_reg_plot(self):
        
        plot_title = "REGISTER %s" % self.__id
        
        if not self.__data:
            plot_title = "NO DATA YET"
            
        x_axis_title = "Time (seconds since first sample)"
        y_axis_title = self.__id
              
        line1 = PolyLine(self.__data, colour='blue', width=3)       
                
        return PlotGraphics([line1],
                        plot_title, 
                        x_axis_title, 
                        y_axis_title)
    
    
    def __on_toggle_grid(self, event):
        self.__canvas.SetEnableGrid(event.IsChecked())
