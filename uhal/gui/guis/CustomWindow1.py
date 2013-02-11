import wx


class CustomWindow1(wx.Frame):

    def __init__(self, parent, id, title):

        wx.Frame.__init__(self, parent, id, title)

        panel = wx.Panel(self)
        panel.SetBackgroundColour('White')
        self.Bind(wx.EVT_CLOSE, self.onCloseWindow)


    def onCloseWindow(self, event):
        self.Destroy()

        

