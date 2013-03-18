import pycohal as uhal
import math

def __isFIFO(node):
    if str(node.getMode()) == "NON_INCREMENTAL":
        return True
    else:
        return False

def __isModule(node):
    if node.getNodes():
        return True
    else:
        return False

def ipbus_addr_map(fn,verbose=False):
    '''Givne a uHAL compatible XML address table, it returns a map to all the IPBus slaves, its addresses, and bslaves
    '''
    if verbose:
        uhal.setLogLevelTo( uhal.LogLevel.DEBUG)
    else:
        uhal.disableLogging()

    if fn.find("file://") == -1:
        fn = "file://"+fn

    d = uhal.getDevice("dummy","ipbusudp-2.0://localhost:12345",fn)
    print  "id\taddress\tsize\tmode\ttags"
    aa = set()
    for i in d.getNodes():
        n = d.getNode(i)
        
        s = int(math.ceil(math.log(n.getSize(),2)))
        a = n.getAddress()
        #remove duplicate addresses
        if not isModule(n) and a in aa:
            continue
        aa.add(a)

        #Change FIFO size
        if isFIFO(n):
            s = 0
        
        print i,"\t",hex(a),"\t",s,"\t",n.getMode(),"\t",n.getTags()
    
