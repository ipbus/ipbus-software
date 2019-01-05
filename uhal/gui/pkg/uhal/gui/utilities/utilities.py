from __future__ import print_function
import wx


class HwReadyEvent(wx.PyCommandEvent):
    """ This class creates a new type of event object. Its purpose is to signal the GUI when reading from the HW is done"""
    
    def __init__(self, event_type, event_id, value=None):

        wx.PyCommandEvent.__init__(self, event_type, event_id)
        self.__value = value       


    def get_event_info(self):
        return self.__value
   


def dynamic_loader(module_object):
    """
    This function is used in the guiloder module to dynamically load all necessary GUIs
    """

    import inspect

    for name, obj in inspect.getmembers(module_object):
        if inspect.isclass(obj):
            return (obj, "OK")

    print("FAILED to return class from module %s" % module_object.__name__)


