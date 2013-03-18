import pycohal as uhal
import math
import sys
import re

BUS_REGEX = re.compile("(^|[,])\s*bus\s*([,]|$)");
SLAVE_REGEX = re.compile("(^|[,])\s*slave\s*([,]|$)");

def __isFIFO(node):
    if str(node.getMode()) == "NON_INCREMENTAL":
        return True
    else:
        return False

def __isMemory(node):
    if str(node.getMode()) == "INCREMENTAL":
        return True
    else:
        return False

def __isRegister(node):
    if str(node.getMode()) == "SINGLE":
        return True
    else:
        return False
    
def __getWidth(node):
    if __isFIFO(node) or __isRegister(node):
        return 0
    elif __isMemory(node):
        return int(math.ceil(math.log(n.getSize(),2)))
    elif __isModule(node):
        result = 0
        children = n.getNodes()
        minaddr = min(n.getNode(id).getAddress() for id in children)
        maxaddr = min(n.getNode(id).getAddress() + n.getNode(id).getSize() for id in children)
        return int(math.ceil(math.log(maxaddr-minaddr,2)))
    
def __isModule(node):
    if node.getNodes():
        return True
    else:
        return False

def __isBus(node):
    if BUS_REGEX.search(node.getTags()):
        return True
    else:
        return False
    
def __isSlave(node):
    if SLAVE_REGEX.search(node.getTags()):
        return True
    else:
        return False
        
def __getChildren(n):
    return n.getNodes("[^.]*")

def hex32(num):
    return "0x%s"%("00000000%x"%(n&0xffffffff))[-8:]



def ipbus_addr_map(fn,verbose=False):
    '''
    Returns a vector with all the slaves, addresses, and addresses with for each bus.
    '''
    if verbose:
        uhal.setLogLevelTo(uhal.LogLevel.DEBUG)
    else:
        uhal.disableLogging()

    if fn.find("file://") == -1:
        fn = "file://"+fn

    try:
        d = uhal.getDevice("dummy","ipbusudp-2.0://localhost:12345",fn)
    except e:
        raise Exception("File '%s' does not exist or has incorrect format")
        

    result = []
    buses = ["__root__"]
    addrs = set()
    while (buses):
        bus = buses.pop(0)
        if bus == "__root__":
            n = d
        else:
            n = d.getNode(bus)
            
        children = __getChildren(n)
        slaves = []
        while (children):
            id = children.pop(0)
            print id
            n = d.getNode(id)
            if __isBus(n):
                if __isSlave(n):
                    raise Exception("Node '%s' is tagged as slave and bus at the same time" % i)
                elif not __isModule(n):
                    raise Exception("Node '%s' is tagged as bus but it does not have children" % i)
                else:
                    buses.append(id)
            elif __isSlave(n):
                addr = n.getAddress()

                #remove duplicates (e.g. masks)
                if addr in addrs:
                    continue
                addrs.insert(addr)
                width = __getWidth(n)

                slaves.append((id,addr,width))
                
            elif __isModule(n):
                children += __getChildren(n)

        result.append((bus,slaves))

    return result
    
