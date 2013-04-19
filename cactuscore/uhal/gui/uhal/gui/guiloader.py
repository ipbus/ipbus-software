import Queue
import threading
import wx
import time

from uhal.gui.utilities.utilities import dynamic_loader
from uhal.gui.guis import defaultgui


class ThreadClient(wx.Timer):

    def __init__(self, main_app):

        self.main_app = main_app
        self.queue = Queue.Queue()
        self.gui = defaultgui.DefaultGui(None, -1, "DefaultGui: Asynchronous thread test")
        self.gui.Show(True)

        self.running = True
        self.hw_thread = threading.Thread(target = self.hw_worker)
        self.hw_thread.start()

        wx.Timer.__init__(self)
        self.Bind(wx.EVT_TIMER, self.gui_periodic_check)
        self.Start(10000)


    def gui_periodic_check(self, event):

        self.gui.process_incoming()
        
        if not self.running:
            import sys
            sys.exit(1)


    def hw_worker(self):

        while self.running:
            time.sleep(20)
            print "executing hw_worker thread"
            print "putting hw object in queue"
        

    def end_application(self):

        self.running = False

        

class MainApplication(wx.App):

    def __init__(self, guilist, redirect=True, file_name=None):
        
        # Attributes
        self.guilist = guilist
        
        wx.App.__init__(self, redirect, file_name)
   



    def OnInit(self):
        """ Load dynamically all GUIs that are needed.
        None of them is set to be the parent one, so that all of them can be independent from each other"""
        
        for gui in self.guilist:
            class_object = dynamic_loader(gui)[0]
            gui_instance = class_object(None, -1, gui.__name__)
            gui_instance.Show(True)

        return True


            
###################### Interface to the outside world ######################



class GuiLoader:

    def __init__(self, guilist):
        self.gui_list = guilist    


    def start(self):
        
        output_to_window = False
        app = MainApplication(self.gui_list, output_to_window)
        # client = ThreadClient(app)
        app.MainLoop()




def loader(default=True, guilist=[]):

    if default == True:

        try:
            default_mod_obj = __import__('uhal.gui.guis', globals(), locals(), ['defaultgui'])
            def_gui_mod_obj = default_mod_obj.defaultgui
            guilist.append(def_gui_mod_obj)
            
        except ImportError, e:
            print 'FAILED to import defaultgui module', e
            
        
    return GuiLoader(guilist)
    



def test(guilist=[]):

    for gui in guilist:
        status = dynamic_loader(gui)[1]
        if status == 'FAILED':
            return status

    return 'OK'
