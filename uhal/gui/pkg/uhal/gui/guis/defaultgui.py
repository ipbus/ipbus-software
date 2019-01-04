import os, webbrowser, logging

import wx
try:
    from wx.lib.pubsub import Publisher
except ImportError:
    from wx.lib.pubsub import pub as Publisher

from uhal.gui.utilities.hardware_monitoring import HardwareMonitoring
from uhal.gui.guis.hardware_tree import HardwareTree
from uhal.gui.guis.hardware_table_panel import HardwareTablePanel


class DefaultGui(wx.Frame):

    def __init__(self, parent, id, title):

        self.__logger = logging.getLogger('uhal.gui.guis.defaultgui')      
        wx.Frame.__init__(self, parent, id, title)       
        
        # Attributes 
        self.__current_session = None      
        self.__hw = None
        self.__hw_mon = None     
        self.__hw_tree_struct = None 
        
        # GUIs Attributes
        self.__hw_tree_frame = None        
        self.__hw_table_panel = None

        # Layout
        self.__create_menu_bar()
        self.__do_layout()
        self.CreateStatusBar()
        
        # Event handlers        
        Publisher.subscribe(self.__on_hw_ready, "HW POLL")
        self.Bind(wx.EVT_CLOSE, self.__on_close_window) 
        self.__logger.info('DefaultGui instance created')      



    def __do_layout(self):
        
        self.__hw_table_panel = HardwareTablePanel(self)
        
        border_flags = wx.ALL
        
        sizer = wx.BoxSizer(wx.VERTICAL)       
        sizer.Add(self.__hw_table_panel, 1, border_flags | wx.EXPAND, 5)
        
        self.SetSizer(sizer)
        self.SetAutoLayout(True)   
        self.SetMinSize((800, 600))



    
    def __create_menu_bar(self):
        """ Creates the menu bar object and adds items to it using the methods
        __menu_data and __create_menu
        """
        menu_bar = wx.MenuBar()
        for each_menu_data in self.__menu_data():
            menu_label = each_menu_data[0]
            menu_items = each_menu_data[1:]
            menu_bar.Append(self.__create_menu(menu_items), menu_label)

        self.SetMenuBar(menu_bar)



    
    def __menu_data(self):
        """
        Creates the content of the menus in the menu bar. 
        Assigns events and handlers to each one of the items
        """
        return (('&File',
                 ('&LoadHW', 'Load HW', wx.ITEM_NORMAL, self.__on_load_hw),
                 #('&SaveSession', 'Save Session', wx.ITEM_NORMAL, self.__on_save_session),
                 #('&SaveSessionAs', 'Save Session As...', wx.ITEM_NORMAL, self.__on_save_session_as),
                 #('&LoadSession', 'Load Session', wx.ITEM_NORMAL, self.__on_load_session),
                 ('&Quit', 'Quit', wx.ITEM_NORMAL, self.__on_close_window)
                ),                
                ('&View',
                 #('&Compact', 'Compact View', wx.ITEM_CHECK, self.__on_compact_view),
                 #('&ExpandAll', 'Expand All', wx.ITEM_CHECK, self.__on_expand_all),
                 #('&ReadOnly', 'Read Only', wx.ITEM_CHECK, self.__on_read_only),
                 ('&ClearPanel', 'Clear Panel', wx.ITEM_NORMAL, self.__on_clear_panel)
                ),                        
                ('&Help',
                 ('&Documentation', 'Documentation', wx.ITEM_NORMAL, self.__on_click_doc),
                 ('&Support', 'Support', wx.ITEM_NORMAL, self.__on_click_support),
                 ('&About', 'About', wx.ITEM_NORMAL, self.__on_click_about)               
                )
            )



    def __create_menu(self, items):
        """
        Creates individual menu objects
        """
        menu = wx.Menu()
        for each_label, each_status, each_kind, each_handler in items:
            
            if not each_label:
                menu.AppendSeparator()
                continue                

            menu_item = menu.Append(-1, each_label, each_status, kind=each_kind)
            self.Bind(wx.EVT_MENU, each_handler, menu_item)

        return menu
    
    
    
    def add_new_widget_to_panel(self, nodes):
        """
        The method is called from the hardware_tree module when a tree item is clicked on. It passes the tree structure
        and the necessary list of nodes to arrive to the selected item from the tree root
        """
        self.__logger.debug('Add new widget to panel...')
        self.__hw_table_panel.add_new_widget(nodes, self.__hw_tree_struct)



    ########## FILE MENU OPTIONS ##########
    def __on_load_hw(self, event):
        """
        Opens a dialog to choose one connection file so that the HW can be loaded. 
        It will incorporate another method to initiate the HW information, consisting on entering a device ID, URI and address_table (this 
        will cope with uHAL's ConnectionManager('device_id', 'uri', 'address_table') method.
        """    
            
        xml_wildcard = 'XML files (*.xml)|*.xml|' \
                   'All files (*.*)|*.*'
        
        self.__dialog(parent=None, msg='Choose connection file', def_dir=os.getcwd(), def_file='', def_wildcard=xml_wildcard, def_style=wx.FD_OPEN)            

  
  
    def __on_save_session(self, event):
        
        if self.__current_session is None:
            self.__on_save_session_as(event)
            return            
    
    
    
    def __on_save_session_as(self, event):
        
        txt_wildcard = 'TXT files (*.txt)|*.txt|' \
                    'All files (*.*)|*.*'
         
        self.__dialog(parent=None, msg='Save GUI session', def_dir=os.getcwd(), def_file='guiSession', def_wildcard=txt_wildcard, def_style=wx.FD_SAVE)                               
    
        
             
    def __dialog(self, parent, msg, def_dir, def_file, def_wildcard, def_style):
        
        save_dialog = wx.FileDialog(parent, message=msg, defaultDir=def_dir, defaultFile=def_file, wildcard=def_wildcard, style=def_style)
        
        if save_dialog.ShowModal() == wx.ID_OK:
            file_name = save_dialog.GetPath()
            self.__logger.debug('Saving session in file %s', str(file_name))
            
            if def_style == wx.FD_OPEN:
                self.__start_hw_thread(file_name)                                  
                self.__create_hardware_tree(self.__hw_tree_struct)   
            elif def_style == wx.FD_SAVE:  
                fd = open(file_name, 'w')
                fd.write('bla bla')
                fd.close()
                self.__current_session = file_name
        
        
        save_dialog.Destroy()
    
    
    
    def __on_load_session(self, event):
        pass
    
    
    
    def __on_close_window(self, event):
        """
        Sets the HW thread to not run anymore, waits for the tread to finish and closes the window, offering a confirmation dialog beforehand.
        """

        msg = "Do you really want to close this GUI?"
        
        dialog = wx.MessageDialog(self, msg, "Confirm Exit", wx.OK | wx.CANCEL | wx.ICON_QUESTION)
        result = dialog.ShowModal()
        dialog.Destroy()
        
        if self.__hw_mon and self.__hw_mon.isAlive():
            self.__hw_mon.set_thread_running(False)
            self.__hw_mon.join()

        if result == wx.ID_OK:
            self.Destroy()
    
    
    
    def __create_hardware_tree(self, hw):
        """
        Picks up the hierarchical HW structure computed in the HardwareMonitoring class, and passes it to the 
        HardwareTree class, so that the HW tree can be built
        """
        
        self.__logger.info('HW valid; creating HW tree...')
        self.__hw_tree_frame = HardwareTree(self, hw)
        self.__hw_tree_frame.Show()                   



    def __on_hw_ready(self, msg):
        """
        HardwareMonitoring class and the DefaultGui coordinate events using a publisher/subscriber schema. 
        When HardwareMonitoring thread is done checking the HW, publishes an event that the DefaultGui object will catch. Upon the reception 
        of this event, this method is executed, updating the HW tree (only status of the IP End points), and widgets drawn in the main panel.
        """
        
        self.__logger.info('Hw ready event: updating main gui\'s children')
        self.__hw = msg.data          
            
        for i in self.GetChildren():                        
            
            if 'update' in dir(i):
                i.update(self.__hw)
                         
        
                
    def __start_hw_thread(self, file_name):
        """
        Creates the thread object that checks the HW. Picks up from it the object that represents the HW hierarchy. Starts the thread (this could also
        be done in the thread's constructor.
        """  
        try:
               
            self.__hw_mon = HardwareMonitoring(self, file_name)       
            self.__hw_tree_struct = self.__hw_mon.get_hw_tree()           
            self.__logger.info('Starting HW monitoring thread...')
              
        except Exception as e:
            
            self.__logger.critical('Exception while instantiating HW monitoring thread! %s Exiting...', str(e))            
            import sys
            sys.exit(1)
              
                                
        try:
            
            self.__hw_mon.start()
            
        except RuntimeError as e:
            
            self.__logger.critical('Tried to initiate twice the HW monitoring thread! Message %s Exiting...', str(e))
            self.__hw_mon.set_thread_running(False)
            self.__hw_mon.join()
            import sys
            sys.exit(1)       
               
    
    
    ########## VIEW MENU OPTIONS ##########
    def __on_compact_view(self, event):
        pass
    
    
    
    def __on_expand_all(self, event):
        pass
    
    
    
    def __on_read_only(self, event):
        pass
    
    
    
    def __on_clear_panel(self, event):
        self.__hw_table_panel.clear()
        
        
        
    ########## ABOUT MENU OPTIONS ########## 
    def __on_click_doc(self, event):
        webbrowser.open("https://svnweb.cern.ch/trac/cactus/wiki/uhalGuiInstructions")



    def __on_click_support(self, event):
        """
        Opens a web browser loading the CACTUS support website
        """
        
        webbrowser.open("https://svnweb.cern.ch/trac/cactus/newticket")



    def __on_click_about(self, event):    
        """
        Displays a frame with this information
        """    

        description = """uHAL GUI is a Python based graphical user interface 
        written using the graphical library wxPython. It has been designed to provide a simple interface
        for uTCA HW developers which use the uHAL C++ library
        """

        #licence = """uHAL GUI is free software; you can redistribute 
        #it and/or modify it under the terms of the GNU General Public License as 
        #published by the Free Software Foundation; either version 2 of the License, 
        #or (at your option) any later version.

        #uHAL GUI is distributed in the hope that it will be useful, 
        #but WITHOUT ANY WARRANTY; without even the implied warranty of 
        #MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
        #See the GNU General Public License for more details. You should have 
        #received a copy of the GNU General Public License along with File Hunter; 
        #if not, write to the Free Software Foundation, Inc., 59 Temple Place, 
        #Suite 330, Boston, MA  02111-1307  USA
        #"""


        info = wx.AboutDialogInfo()
        info.SetName('uHAL GUI')
        info.SetVersion('1.0.0')
        info.SetDescription(description)
        info.SetCopyright('(C) 2013 Carlos Ghabrous, Marc Magrans de Abril')
        info.SetWebSite('http://svnweb.cern.ch/trac/cactus')
        # info.SetLicence(licence)
        info.AddDeveloper('Carlos Ghabrous')
        info.AddDocWriter('Carlos Ghabrous')

        wx.AboutBox(info)
