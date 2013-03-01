import wx


class CustomWindow1(wx.Frame):

    def __init__(self, parent, id, title):

        wx.Frame.__init__(self, parent, id, title)

        panel = wx.Panel(self)
        panel.SetBackgroundColour('White')
        self.Bind(wx.EVT_CLOSE, self.onCloseWindow)


    def onCloseWindow(self, event):
        self.Destroy()



def start():
    app = wx.PySimpleApp()
    frame = CustomWindow1(None, -1, "CustomWindow1")
    frame.Show(True)
    app.MainLoop()


if __name__ == '__main__':
    start()
    
