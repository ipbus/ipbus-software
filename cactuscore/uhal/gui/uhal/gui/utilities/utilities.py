import string

def dynamic_loader(gui_class_name):

    if gui_class_name == 'DefaultGui':
        import_prefix = 'uhal.gui.guis.'    
    else:
        import_prefix = 'uhal.gui.customguis.'
        
    try:
        # Equivalent to: from gui_names.modulename import ClassName
        gui_module = import_prefix + string.lower(gui_class_name)
        
        module_object = __import__(gui_module, globals(), locals(), [gui_class_name])
                
        class_object = getattr(module_object, gui_class_name)
        print 'Trying to import module %s' % gui_class_name
        
       # gui_name_instance = class_object(panel, -1, gui_name)

       # class_instance.Show(True)

    except ImportError, e:
        print "Failed to import module ", gui_class_name, e
