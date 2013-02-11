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
