import os, sys


def dynamic_loader(panel):

    _module_directory = "uhal/gui/guis"

    if not os.path.isdir(_module_directory):
        print "\'", _module_directory, "\' directory not found! Could not load custom GUIs"
        return

     # Get the custom guis 
    _module_list = [os.path.splitext(x)[0] for x in os.listdir(_module_directory) if x != '__init__.py' and x != 'defaultGUI.py' and os.path.splitext(x)[1] == '.py']
    
        
    for _module in _module_list:
        
        _class_name = _module
            
        try:
            # Equivalent to: from guis.moduleName import className
            _module_object = __import__(_module_directory + "." + _module, globals(), locals(), [_class_name])
                
            _class_object = getattr(_module_object, _class_name)
            _class_instance = _class_object(panel, -1, _class_name)

            _class_instance.Show(True)

        except ImportError, e:
            print "Failed to import module ", _module, e
