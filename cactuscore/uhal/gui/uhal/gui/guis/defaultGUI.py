import os
import webbrowser

import wx

import modules.utilities
import modules.hardware


class uHalGuiFrame(wx.Frame):

    def __init__(self, parent, id, title):

        wx.Frame.__init__(self, parent, id, title)

        panel = wx.Panel(self)
        panel.SetBackgroundColour('White')
        self.Bind(wx.EVT_CLOSE, self.on_close_window)
        self.create_menu_bar()


        # Instantiate custom GUIs
        modules.utilities.dynamic_loader(panel)


    # CREATE THE MENU BAR
    def create_menu_bar(self):

        menu_bar = wx.MenuBar()
        for each_menu_data in self.menu_data():
            menu_label = each_menu_data[0]
            menu_items = each_menu_data[1:]
            menu_bar.Append(self.create_menu(menu_items), menu_label)

        self.SetMenuBar(menu_bar)
        

    # MENU BAR ITEMS
    def menu_data(self):
        
        return (("&File",
                 ("&LoadHW", "Load HW", self.on_load_hw),
                 ("&Quit", "Quit", self.on_close_window)
                 ),            
                ("&Help",
                 ("&Documentation", "Documentation", self.on_click_doc),
                 ("&Support", "Support", self.on_click_support),
                 ("&About", "About", self.on_click_about)               
                 )
                )


    # CREATE INDIVIDUAL MENU ITEMS
    def create_menu(self, items):

        menu = wx.Menu()
        for each_label, each_status, each_handler in items:
            if not each_label:
                menu.AppendSeparator()
                continue

            menu_item = menu.Append(-1, each_label, each_status)
            self.Bind(wx.EVT_MENU, each_handler, menu_item)

        return menu



    # FILE PICKER 
    def on_load_hw(self, event):

        wildcard = "XML files (*.xml)|*.xml|" \
                   "All files (*.*)|*.*"

        
        file_picker = wx.FileDialog(None, style = wx.OPEN)

        file_picker.SetMessage("Choose connection file")
        file_picker.SetDirectory(os.getcwd())
        file_picker.SetWildcard(wildcard)
        file_picker.SetFilename("gui")
    

        if file_picker.ShowModal() == wx.ID_OK:
            modules.hardware.Hardware(file_picker.GetPath())
        
        file_picker.Destroy()


    
    def on_click_doc(self, event):
        webbrowser.open("https://svnweb.cern.ch/trac/cactus")


    def on_click_support(self, event):
        webbrowser.open("https://svnweb.cern.ch/trac/cactus")


    def on_click_about(self, event):        

        description = """uHAL GUI is a Python based graphical user interface 
        written using the graphical library wxPython. Bla bla bla...
        """

        licence = """uHAL GUI is free software; you can redistribute 
        it and/or modify it under the terms of the GNU General Public License as 
        published by the Free Software Foundation; either version 2 of the License, 
        or (at your option) any later version.

        uHAL GUI is distributed in the hope that it will be useful, 
        but WITHOUT ANY WARRANTY; without even the implied warranty of 
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
        See the GNU General Public License for more details. You should have 
        received a copy of the GNU General Public License along with File Hunter; 
        if not, write to the Free Software Foundation, Inc., 59 Temple Place, 
        Suite 330, Boston, MA  02111-1307  USA
        """


        info = wx.AboutDialogInfo()

        #info.SetIcon(wx.Icon('hunter.png', wx.BITMAP_TYPE_PNG))
        info.SetName('uHAL GUI')
        info.SetVersion('1.0.0')
        info.SetDescription(description)
        info.SetCopyright('(C) 2013 Carlos Ghabrous')
        info.SetWebSite('http://svnweb.cern.ch/trac/cactus')
        info.SetLicence(licence)
        info.AddDeveloper('Carlos Ghabrous')
        info.AddDocWriter('Carlos Ghabrous')

        wx.AboutBox(info)

    def on_close_window(self, event):
        self.Destroy()



def start():

    app = wx.PySimpleApp()
    frame = uHalGuiFrame(None, -1, "uHalGui")
    frame.Show(True)
    app.MainLoop()

    
if __name__ == '__main__':
    start()
