import pycohal as uhal
import math
import sys
import re
import unittest

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
        return int(math.ceil(math.log(node.getSize(),2)))
    elif __isModule(node):
        result = 0
        children = node.getNodes()
        minaddr = None
        maxaddr = None
        for id in children:
            if __isSlave(node.getNode(id)):
                raise Exception("Slave '%s' inside '%s' slave" % (id,node.getId()))
            addr = node.getNode(id).getAddress()
            if not minaddr or minaddr>addr:
                minaddr = addr
            if not maxaddr or maxaddr<addr:
                maxaddr = addr

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
    return "0x%s"%("00000000%x"%(num&0xffffffff))[-8:]



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
        d = uhal.getDevice("dummy","ipbusudp-1.3://localhost:12345",fn)
    except Exception:
        raise Exception("File '%s' does not exist or has incorrect format" % fn)
        

    result = []
    buses = ["__root__"]
    addrs = set()
    while (buses):
        bus = buses.pop(0)
        if bus == "__root__":
            parent = d
        else:
            parent = parent.getNode(bus)
            
        children = __getChildren(parent)
        slaves = []
        while (children):
            id = children.pop(0)
            child = parent.getNode(id)
            if __isBus(child):
                if __isSlave(child):
                    raise Exception("Node '%s' is tagged as slave and bus at the same time" % id)
                elif not __isModule(child):
                    raise Exception("Node '%s' is tagged as bus but it does not have children" % id)
                else:
                    buses.append(id)
            elif __isSlave(child):
                addr = child.getAddress()

                #remove duplicates (e.g. masks)
                if addr in addrs:
                    if verbose:
                        print "WARNING: Node '%s' has duplicate address %s. Ignoring slave..." % (id, hex32(addr))
                    continue
                addrs.add(addr)
                width = __getWidth(child)

                slaves.append((id,addr,width))
                
            elif __isModule(child):
                children += map(lambda x: "%s.%s" % (id,x),__getChildren(child))

        #sort by address        
        slaves.sort(lambda x,y: cmp(d.getNode(x[0]).getAddress(),d.getNode(y[0]).getAddress()))
        result.append((bus,slaves))

    return result

class TestSimple(unittest.TestCase):
    def test_simple(self):
        m = ipbus_addr_map("uhal/tools/test_data/simple.xml")

        #just a smoke test
        buses = [bus for bus,slaves in m]
        self.assertTrue(len(buses) == 3)
        self.assertTrue("__root__" in buses)
        self.assertTrue("SUBSYSTEM1" in buses)
        self.assertTrue("SUBSYSTEM2" in buses)

        sroot = dict(((id,(hex32(addr),width)) for id,addr,width in m[0][1]))
        self.assertTrue(len(sroot) == 6)
        self.assertTrue(sroot['REG'][0] == "0x00000001")
        self.assertTrue(sroot['REG'][1] == 0)
        self.assertTrue(sroot['MEM'][0] == "0x00100000")
        self.assertTrue(sroot['MEM'][1] == 18)
        self.assertTrue(sroot['FIFO'][0] == "0x00000100")
        self.assertTrue(sroot['FIFO'][1] == 0)

        sub2 = dict(((id,(hex32(addr),width)) for id,addr,width in m[2][1]))
        self.assertTrue(len(sub2) == 4)
        self.assertTrue(sub2['REG'][0] == "0x00300001")
        self.assertTrue(sub2['REG'][1] == 0)
        
def main():
     unittest.main()
     
if __name__ == '__main__':
    main()
