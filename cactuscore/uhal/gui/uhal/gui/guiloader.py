import wx
import os, sys

from uhal.gui.utilities.utilities import dynamic_loader



class GuiLoader:

    def __init__(self, default='yes', listguis=[]):

        self.d = default
        self.guilist = listguis


    def start(self):

        if self.d == 'yes':
            dynamic_loader('DefaultGui')

        for gui in self.guilist:
             dynamic_loader(gui)

        print 'GuiLoader was called with default= ', self.d, ' and guilist = ', self.guilist
            

'''
class MainApplication(wx.App):

    def __init__(self, redirect=True, filename=None):
        
        wx.App.__init__(self, redirect, filename)


    def OnInit(self):

        """ Method to initiate the main frame"""
        
        self.frame = uhal.gui.guis.defaultGUI.uHalGuiFrame(parent=None, id=-1, title="uHAL")
        self.frame.Show()
        self.SetTopWindow(self.frame)
        
        return True


def start():

    outputToWindow = False

    app = MainApplication(outputToWindow) 
    app.MainLoop()


#if __name__ == __main__:
    start()
'''
