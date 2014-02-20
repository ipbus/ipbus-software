import uhal
import math
import sys
import unittest
import os
import re

BUS_REGEX = re.compile("(^|[,])\s*bus\s*([,]|$)");
SLAVE_REGEX = re.compile("(^|[,])\s*slave\s*([,]|$)");

def isFIFO(node):
    if str(node.getMode()) == "NON_INCREMENTAL":
        return True
    else:
        return False

def isMemory(node):
    if str(node.getMode()) == "INCREMENTAL":
        return True
    else:
        return False

def isRegister(node):
    if str(node.getMode()) == "SINGLE":
        return True
    else:
        return False
    
def getWidth(node):
    if isFIFO(node) or isRegister(node):
        return 0
    elif isMemory(node):
        return int(math.ceil(math.log(node.getSize(),2)))
    elif isModule(node):
        result = 0
        children = node.getNodes()
        minaddr = None
        maxaddr = None
        for name in children:
            if isSlave(node.getNode(name)):
                raise Exception("Slave '%s' inside '%s' slave" % (name,node.getId()))
            addr = node.getNode(name).getAddress()
            if not minaddr or minaddr>addr:
                minaddr = addr
            if not maxaddr or maxaddr<addr:
                maxaddr = addr

        return int(math.ceil(math.log(maxaddr-minaddr+1,2)))
    else:
        return 0

def getAddresses(node):
    init_addr = node.getAddress()
    width = getWidth(node)
    addrs = set()
    for i in range(2**width):
        addrs.add(init_addr+i)
    
    return addrs
       
def isModule(node):
    if node.getNodes():
        return True
    else:
        return False

def isBus(node):
    if BUS_REGEX.search(node.getTags()):
        return True
    else:
        return False
    
def isSlave(node):
    if SLAVE_REGEX.search(node.getTags()):
        return True
    else:
        return False
        
def getChildren(d,name):
    if name:
        nodes = d.getNode(name).getNodes("[^.]*")
        return map(lambda x: "%s.%s" % (name,x) ,nodes)
    else:
        return d.getNodes("[^.]*")

def hex32(num):
    return "0x%08x" % num



def ipbus_addr_map(fn,verbose=False):
    '''
    Returns a vector with all the slaves, addresses, and addresses for each bus.
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
            parent = ""
        else:
            parent = bus
            
        children = getChildren(d,parent)
        slaves = []
        while (children):
            name = children.pop(0)
            child = d.getNode(name)
            if isBus(child):
                if isSlave(child):
                    raise Exception("Node '%s' is tagged as slave and bus at the same time" % name)
                elif not isModule(child):
                    raise Exception("Node '%s' is tagged as bus but it does not have children" % name)
                else:
                    buses.append(name)
            elif isSlave(child):
                slv_addrs = getAddresses(child)

                #Duplicate slaves are not allowed
                if slv_addrs & addrs:
                    raise Exception("Slave '%s' address %s is duplicated" % (name, hex32(child.getAddress())))
                                    
                addrs = addrs | slv_addrs
                width = getWidth(child)

                slaves.append((name,child.getAddress(),width))
                
            elif isModule(child):
                children += getChildren(d,name)

            else:
                raise Exception("Slave '%s' is not tagged as slave, bus or module" % (name))

        #sort by address        
        slaves.sort(lambda x,y: cmp(d.getNode(x[0]).getAddress(),d.getNode(y[0]).getAddress()))
        result.append((bus,slaves))

    return result

def get_vhdl_template(fn=None):
    if not fn:
        this_dir, this_filename = os.path.split(__file__)
        fn = os.path.join(this_dir, "templates", "ipbus_addr_decode.vhd")

    return open(fn).read()

class TestSimple(unittest.TestCase):
    def test_uhal(self):
        #uHAL address table parsing  
        this_dir, this_filename = os.path.split(__file__)
        simplefn = os.path.join(this_dir, "test_data","simple.xml")
        d = uhal.getDevice("dummy","ipbusudp-1.3://localhost:12345","file://" + simplefn)
        
        x = d.getNode("REG")
        self.assertTrue(isRegister(x) and isSlave(x) and not isModule(x))
        
        x =  d.getNode("REG_UPPER_MASK")
        self.assertTrue(isRegister(x) and isSlave(x) and not isModule(x))
        
        x =  d.getNode("REG_LOWER_MASK")
        self.assertTrue(isRegister(x) and not isSlave(x) and not isModule(x))

        x =  d.getNode("MASKED_REG")
        self.assertTrue(isSlave(x) and isModule(x))

        x =  d.getNode("MASKED_REG.REG_UPPER_MASK")
        self.assertTrue(not isSlave(x) and isRegister(x))

        x =  d.getNode("MASKED_REG.REG_LOWER_MASK")
        self.assertTrue(not isSlave(x) and isRegister(x))

        x =  d.getNode("MULTIPLE_REGS")
        self.assertTrue(isSlave(x) and isModule(x))
        
        x =  d.getNode("MULTIPLE_REGS.REG1")
        self.assertTrue(not isSlave(x) and isRegister(x))

        x =  d.getNode("MULTIPLE_REGS.REG2")
        self.assertTrue(not isSlave(x) and isRegister(x))

        x =  d.getNode("FIFO")
        self.assertTrue(isSlave(x) and isFIFO(x) and not isModule(x) and not isMemory(x))

        x =  d.getNode("MEM")
        self.assertTrue(isSlave(x) and not isFIFO(x) and not isModule(x) and isMemory(x))


    def test_ipbus_addr_map(self):
        this_dir, this_filename = os.path.split(__file__)
        simplefn = os.path.join(this_dir, "test_data","simple.xml")
        m = ipbus_addr_map(simplefn)
 
        #just a smoke test
        buses = [bus for bus,slaves in m]
        self.assertTrue(len(buses) == 3)
        self.assertTrue("__root__" in buses)
        self.assertTrue("SUBSYSTEM1" in buses)
        self.assertTrue("SUBSYSTEM1.SUBSYSTEM2" in buses)

        sroot = dict(((name,(hex32(addr),width)) for name,addr,width in m[0][1]))
        #print sroot
        self.assertTrue(len(sroot) == 6)
        self.assertTrue(sroot['REG'][0] == "0x00000000")
        self.assertTrue(sroot['REG'][1] == 0)
        self.assertTrue(sroot['MEM'][0] == "0x00100000")
        self.assertTrue(sroot['MEM'][1] == 18)
        self.assertTrue(sroot['FIFO'][0] == "0x00000100")
        self.assertTrue(sroot['FIFO'][1] == 0)

        sub2 = dict(((name,(hex32(addr),width)) for name,addr,width in m[2][1]))
        #print sub2
        self.assertTrue(len(sub2) == 5)
        self.assertTrue(sub2['SUBSYSTEM1.SUBSYSTEM2.REG'][0] == "0x00500002")
        self.assertTrue(sub2['SUBSYSTEM1.SUBSYSTEM2.REG'][1] == 0)
        
def test():
     suite = unittest.TestLoader().loadTestsFromTestCase(TestSimple)
     unittest.TextTestRunner().run(suite)
     
if __name__ == '__main__':
    test()
