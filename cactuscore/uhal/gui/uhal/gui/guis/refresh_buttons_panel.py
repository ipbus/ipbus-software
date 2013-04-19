import threading
import time

import wx


myEVT_HWREADY = wx.NewEventType()
EVT_HWREADY = wx.PyEventBinder(myEVT_HWREADY, 1)

class HwReadyEvent(wx.PyCommandEvent):

    """ Event that signals the HW check is ready """
    
    def __init__(self, etype, eid, value=None):

        """Creates the new event type object"""
        wx.PyCommandEvent.__init__(self, etype, eid)
        self.__value = value


    def GetValue(self):
        """ Returns the value from the event.
        @return: the value of this event
        """
        return self.__value


class HardwareThread(threading.Thread):

    def __init__(self, parent, value):

        threading.Thread.__init__(self)
        self.__parent = parent
        self.__value = value


    def run(self):

        print "Checking HW... start!"
        time.sleep(10) #simulated HW read
        evt = HwReadyEvent(myEVT_HWREADY, -1, self.__value)
        wx.PostEvent(self.__parent, evt)





class RefreshButtonsPanel(wx.Panel):

    def __init__(self, parent):

        wx.Panel.__init__(self, parent)


        # Attributes
        self.__auto_refresh_box = wx.CheckBox(self, -1, "Auto-refresh")
        self.__refresh_button = wx.Button(self, -1, "Refresh")
        self.__counter = wx.StaticText(self, label="0")
        self.__counter.SetFont(wx.Font(16, wx.MODERN, wx.NORMAL, wx.NORMAL))
        
        # Layout
        self.__do_layout()

        
        # Event handlers
        self.Bind(wx.EVT_CHECKBOX, self.__on_click_auto_refresh, self.__auto_refresh_box)
        self.Bind(wx.EVT_BUTTON, self.__on_click_refresh, self.__refresh_button)
        self.Bind(EVT_HWREADY, self.__on_hw_ready)
        

    def __do_layout(self):
        
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        
        sizer.Add(self.__auto_refresh_box, 0, wx.ALIGN_CENTER, 10)
        sizer.Add(self.__refresh_button, 0, wx.ALIGN_CENTER, 200)
        sizer.Add(self.__counter, 0, wx.ALIGN_CENTER, 300)

        self.SetSizer(sizer)


    def __on_click_refresh(self, event):
        print "Refresh button clicked"
        hw_worker = HardwareThread(self, 1)
        hw_worker.start()


    def __on_click_auto_refresh(self, event):
        print "Setting auto-refresh option %s" % self.__auto_refresh_box.GetValue()
        """ Access HW cache and print values periodically """


    def __on_hw_ready(self, event):
        val = int(self.__counter.GetLabel()) + event.GetValue()
        self.__counter.SetLabel(unicode(val))
