from _pycohal import *

def exception_to_string(self):
   return self.what

exception.__str__ = exception_to_string

