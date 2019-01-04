import wx
from uhal.gui.guis import defaultgui

class CustomWindow1(defaultgui.DefaultGui):

    def __init__(self, parent, id, title):
        wx.Frame.__init__(self, parent, id, title)


def start():
    app = wx.PySimpleApp()
    frame = CustomWindow1(None, -1, "CustomWindow1")
    frame.Show(True)
    app.MainLoop()


if __name__ == '__main__':
    start()
    
