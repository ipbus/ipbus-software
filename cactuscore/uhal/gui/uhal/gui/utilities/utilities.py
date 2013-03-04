import string

def dynamic_loader(gui_class_name):

    if gui_class_name == "DefaultGui":
        import_prefix = "uhal.gui.guis."    
    else:
        import_prefix = "uhal.gui.customguis."

        
    try:
        
        gui_module = import_prefix + string.lower(gui_class_name)

        # Equivalent to: from gui_names.modulename import ClassName
        module_object = __import__(gui_module, globals(), locals(), [gui_class_name])
                
        class_object = getattr(module_object, gui_class_name)        

        return (class_object, "OK")

    except ImportError, e:
        print "Import FAILED: module ", gui_module, e
        return (None, "FAILED")

   
