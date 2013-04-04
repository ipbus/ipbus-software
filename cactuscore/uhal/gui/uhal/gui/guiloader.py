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
            


class GuiLoader:

    def __init__(self, guilist):
        self.gui_list = guilist    


    def start(self):

        output_to_window = False
        app = MainApplication(self.gui_list, output_to_window)
        app.MainLoop()


    


def loader(default=True, guilist=[], test_mode='no'):
    
    if default == True and guilist.count('DefaultGui') == 0:
          guilist.insert(0, 'DefaultGui')

    if default == False and guilist.count('DefaultGui') == 1:
          guilist.remove('DefaultGui')

    g = GuiLoader(guilist)
    return g



def start():
    pass


def start_test():
    pass
"""
    for gui in GLIST:
        status = dynamic_loader(gui)[1]
        if status == 'FAILED':
            return status


    return 'OK'
"""
