#!/usr/bin/python


import wx
import os, sys

import guis.defaultGUI

    


class MainApplication(wx.App):

    def __init__(self, redirect=True, filename=None):
        
        wx.App.__init__(self, redirect, filename)


    def OnInit(self):

        """ Method to initiate the main frame"""
        
        self.frame = guis.defaultGUI.uHalGuiFrame(parent=None, id=-1, title="MainGUI")
        self.frame.Show()
        self.SetTopWindow(self.frame)
        
        return True


def main():

    outputToWindow = False

    app = MainApplication(outputToWindow) 
    app.MainLoop()


if __name__ == '__main__':
    main()
