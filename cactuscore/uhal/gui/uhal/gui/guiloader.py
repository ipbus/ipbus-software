import wx

from uhal.gui.utilities.utilities import dynamic_loader



class MainApplication(wx.App):

    def __init__(self, guilist, testmode, redirect=True, file_name=None):

        self.guilist = guilist
        self.testmode = testmode
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
    """ The GuiLoader class provides the user a way to instantiate the necessary GUIs.
    It builds up a list of GUIs that should be instantiated from the arguments list,
    builds the MainApplication object, and calls its MainLoop"""

    def __init__(self, default='yes', guilist=[], test_mode='No'):

        self.d = default
        self.guilist = guilist
        self.testmode = test_mode
        
        if self.d == 'yes' and self.guilist.count('DefaultGui') == 0:
            self.guilist.insert(0, 'DefaultGui')

        if self.d == 'no' and self.guilist.count('DefaultGui') == 1:
            self.guilist.remove('DefaultGui')


        
    def start(self):
        
        output_to_window = False
        
        if self.testmode == 'No':    
            app = MainApplication(self.guilist, self.testmode, output_to_window)        
            app.MainLoop()


        if self.testmode == 'Yes':
            self.start_test()


            
    def start_test(self):

        for gui in self.guilist:
            status = dynamic_loader(gui)[1]
            if status == 'FAILED':
                return 'FAILED'

        return 'OK'

