import wx

from uhal.gui.utilities.utilities import dynamic_loader



class MainApplication(wx.App):

    def __init__(self, guilist, redirect=True, file_name=None):

        self.guilist = guilist
        wx.App.__init__(self, redirect, file_name)


    def OnInit(self):
        """ Load dynamically all GUIs that are needed.
        None of them is set to be the parent one, so that all of them can be independent from each other"""
        
        for gui in self.guilist:

            class_object = dynamic_loader(gui)[0]
            gui_instance = class_object(None, -1, gui)
            gui_instance.Show(True)


        return True
            



glist = []
testmode = ""

def gui_loader(default='yes', guilist=[], test_mode='no'):

    global glist, testmode    
    
    d = default
    glist = guilist
    testmode = test_mode
    
    if d == 'yes' and glist.count('DefaultGui') == 0:
          glist.insert(0, 'DefaultGui')

    if d == 'no' and glist.count('DefaultGui') == 1:
          glist.remove('DefaultGui')



def start():

    global glist, testmode
    
    output_to_window = False
    app = MainApplication(glist, output_to_window)
    app.MainLoop()


def start_test():

    for gui in glist:
        status = dynamic_loader(gui)[1]
        if status == 'FAILED':
            return status


    return 'OK'
