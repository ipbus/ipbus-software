import inspect

def dynamic_loader(module_object):
    
    for name, obj in inspect.getmembers(module_object):
        if inspect.isclass(obj):
            return (obj, "OK")

    print "FAILED to return class from module %s" % module_object.__name__


   
