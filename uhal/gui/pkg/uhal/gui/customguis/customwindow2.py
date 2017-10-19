import wx

class CustomWindow2(wx.Frame):

    def __init__(self, parent, id, title):

        wx.Frame.__init__(self, parent, id, title)

        panel = wx.Panel(self)
        panel.SetBackgroundColour('White')
        self.Bind(wx.EVT_CLOSE, self.onCloseWindow)

        button = wx.Button(panel, -1, "Close window")
        self.Bind(wx.EVT_BUTTON, self.onClickButton, button)


    def onClickButton(self, event):
        self.Close(True)

    def onCloseWindow(self, event):
        self.Destroy()


def start():

    app = wx.PySimpleApp()
    frame = CustomWindow2(None, -1, "CustomWindow2")
    frame.Show(True)
    app.MainLoop()

if __name__ == '__main__':
    start()
