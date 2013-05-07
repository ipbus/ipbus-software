import wx
from wx.lib.plot import PlotCanvas, PlotGraphics, PolyLine


class Plot(wx.Frame):
    
    def __init__(self, parent, regname):
        wx.Frame.__init__(self, parent, wx.ID_ANY, regname, size=(400, 300))
        
        # Attributes
        self.__panel = wx.Panel(self, wx.ID_ANY)
        self.__panel.SetBackgroundColour("Gray")
        self.__id = regname
        self.__data = [(1,1), (2,0), (3,6), (4,7), (5,5)]
        
        # Layout attributes
        self.__toggle_grid   = wx.CheckBox(self.__panel, label="Show Grid")
        self.__toggle_legend = wx.CheckBox(self.__panel, label="Show Legend")     
        self.__main_sizer = None
        self.__check_sizer = None
        self.__canvas = None
        
        # Layout
        self.__do_layout()
        
        # Handlers
        self.Bind(wx.EVT_CHECKBOX, self.__on_toggle_grid, self.__toggle_grid)
        self.Bind(wx.EVT_CHECKBOX, self.__on_toggle_legend, self.__toggle_legend)
    
    
    def __do_layout(self):
        
        self.__main_sizer  = wx.BoxSizer(wx.VERTICAL)
        self.__check_sizer = wx.BoxSizer(wx.HORIZONTAL)
        
        self.__canvas = PlotCanvas(self.__panel)  
        
        self.__check_sizer.Add(self.__toggle_grid, 0, wx.ALL, 5)
        self.__check_sizer.Add(self.__toggle_legend, 0, wx.ALL, 5)
        self.__main_sizer.Add(self.__canvas, 1, wx.EXPAND)
        self.__main_sizer.Add(self.__check_sizer)
        self.__panel.SetSizer(self.__main_sizer)
        self.__panel.SetAutoLayout(True)
        
        self.plot()
        
    
    def add_pair(self, x, y):
        pass
        
        
    def plot(self):
        self.__canvas.Draw(self.__draw_reg_plot())
    
    
    def __draw_reg_plot(self):
        plot_title = "Dummy plot"
        x_axis_title = "Time"
        y_axis_title = self.__id
              
        line1 = PolyLine(self.__data, colour='green', legend='Feb.', width=5)       
                
        return PlotGraphics([line1],
                        plot_title, 
                        x_axis_title, 
                        y_axis_title)
    
    
    def __on_toggle_grid(self, event):
        self.__canvas.SetEnableGrid(event.IsChecked())
    
    
    def __on_toggle_legend(self, event):
        self.__canvas.SetEnableLegend(event.IsChecked())