import wx, logging

from uhal.gui.utilities.utilities import dynamic_loader
from uhal.gui.guis import defaultgui



        

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
            # First argument is the parent GUI, second argument is the ID, third argument is the GUI title
            gui_instance = class_object(None, wx.ID_ANY, gui.__name__)
            gui_instance.Show(True)

        return True


            
###################### Interface to the outside world ######################



class GuiLoader:

    def __init__(self, guilist):
        self.gui_list = guilist    


    def start(self):
        
        output_to_window = False
        app = MainApplication(self.gui_list, output_to_window)
        logger.info('Starting application main loop...')       
        app.MainLoop()



def loader(default=True, guilist=[]):

    configure_logger()
    
    if default:

        try:
            default_mod_obj = __import__('guis', globals(), locals(), ['defaultgui'])
            def_gui_mod_obj = default_mod_obj.defaultgui
            guilist.append(def_gui_mod_obj)
            logger.info('GUI modules successfully imported')
            
        except ImportError as e:
            logger.critical('Failed to import defaultgui module: %s', str(e))
            
        
    return GuiLoader(guilist)
    


def configure_logger():
    
    global logger
    
    logger = logging.getLogger('uhal.gui')
    logger.setLevel(logging.INFO)
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    formatter = logging.Formatter('%(asctime)s - from %(name)s -- %(levelname)s: %(message)s')
    ch.setFormatter(formatter)
    logger.addHandler(ch)



def test(guilist=[]):

    for gui in guilist:
        status = dynamic_loader(gui)[1]
        if status == 'FAILED':
            return status

    return 'OK'
