import uhal
import math
import sys
import unittest
import os
import re
import logging

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
    # TODO: bug alert? not reading the size tag for slaves?
    # see mp7_default/tmt_demux.xml
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
            #if isSlave(node.getNode(name)):
            #    raise Exception("Slave '%s' inside '%s' slave" % (name,node.getId()))
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
        
def getChildren(device,name):
    if name:
        nodes = device.getNode(name).getNodes("[^.]*")
        return map(lambda x: "%s.%s" % (name,x) ,nodes)
    else:
        return device.getNodes("[^.]*")

def hex32(num):
    return "0x%08x" % num


#=============================================================

class Module:
    def __init__(self,moduleName,fileName):
        self.moduleName=moduleName
        self.fileName=fileName
        self.nodeList=[]
        return
#=============================================================


def ipbus_addr_map(fn):
    '''
    Returns a vector with all the slaves, addresses, and addresses for each module.
    '''

    # logging: create child of main log (the dot notation is mandatory!)
    log = logging.getLogger("main.scan")
    # adjust uhal verbosity according to overall log level
    if log.getEffectiveLevel()<=logging.INFO:
        uhal.setLogLevelTo(uhal.LogLevel.DEBUG)
    else:
    	uhal.disableLogging()

    # create uhal dummy device using the address table provided by the user.
    # we can use an arbitrary url to initialize uhal because we are not going to
    # connect to this particular device anyway.
    if fn.find("file://") == -1:
        fn = "file://"+fn
    try:
        device = uhal.getDevice("dummy","ipbusudp-1.3://localhost:12345",fn)
    except Exception:
        raise Exception("File '%s' does not exist or has incorrect format" % fn)

    # start by splitting the entire node tree into modules associated
    # with individual input xml files.
    # in principle the whole address table conversion could be done at
    # this stage, but for clarity of the code we address each module
    # in a separate pass over all member nodes later on.

    result = []
    
    modules = {}
    for nodeName in device.getNodes():
        node = device.getNode(nodeName)
        fileName = node.getModule()
        # find name of the module this node belongs to, defined by parent node
        moduleName=nodeName
        while device.getNode(moduleName).getModule()==fileName:
            dotPos=moduleName.rfind(".")
            if dotPos>0:
                moduleName=moduleName[:dotPos]
            else:
                moduleName="root"
                break
        # create module if it doesn't exist yet
        if not moduleName in modules.keys():
            modules[moduleName] = Module(moduleName,fileName)
        # add this node to the list of nodes associated with this module
        modules[moduleName].nodeList.append(nodeName)

    # intermediate output
    for moduleName in modules.keys():
        module=modules[moduleName]
        log.debug("===============================")
        log.debug("FILE:"+module.fileName)
        log.debug("  MODULE NAME:"+module.moduleName) 
        for entry in module.nodeList:
            log.debug("  NODE:"+entry)
        log.debug("-------------------------------")

    # identify everything tagged as slave or bus, and everything that
    # represents an entire module included at this level.
    # these are the smallest objects we are supposed to resolve when decoding
    # this particular module, and we will remove everything
    # below from the list we are dealing with.
    for moduleName in modules.keys():
        relevantNodes=[]
        for nodeName in modules[moduleName].nodeList:
            node=device.getNode(nodeName)
#           if isSlave(node) or isBus(node) or (nodeName in modules.keys()):
            if isSlave(node) or isBus(node):
                # check we do not put a node and a subnode of it into the list
                for existingNodeName in relevantNodes:
                    if nodeName.find(existingNodeName+".")==0 or\
                       existingNodeName.find(nodeName+".")==0:
                        raise Exception("%s and %s both tagged as slave or bus"%(nodeName,existingNodeName))
                # fine, we can add this to the list
                relevantNodes.append(nodeName)

        # now check for anything that is left over
        #unclassifiedNodes=[]
        #for nodeName in modules[moduleName].nodeList:
        #    if nodeName in relevantNodes: continue
        #    subnode=False
        #    for relevantNode in relevantNodes:
        #        subnode|=(nodeName.find(relevantNode+".")==0)
        #    if not subnode:
        #        unclassifiedNodes.append(nodeName)
        #        log.warning("Node %s assumed to represent a slave even though not tagged"%nodeName)
        # we assume that untagged nodes are to be considered slaves as well
        #relevantNodes+=unclassifiedNodes
        # TODO: prevent node *and* subnode from being added here!

        # assemble list of individual entities to be distinguished,
        # with base address and total width
        slaves = []
        for entry in relevantNodes:
            slaves.append([entry,\
                           device.getNode(entry).getAddress(),\
                           getWidth(device.getNode(entry))])
        # TODO: BUG! if a module is added to the list of relevant nodes,
        # the address of the module is not the offset it is included with,
        # but the address of the first node, which is different if the first
        # register is not at address 0 within the module!

        #sort by address        
        slaves.sort(lambda x,y: cmp(device.getNode(x[0]).getAddress(),
                                    device.getNode(y[0]).getAddress()))

        result.append((moduleName,slaves))
                
        # refined output, including relevant nodes only
        module=modules[moduleName]
        log.debug("===============================")
        log.debug("FILE:"+module.fileName)
        log.debug("  MODULE NAME:"+module.moduleName)
        for entry in relevantNodes:
            log.debug("  RELEVANT NODE: %08x %08x %s"%(device.getNode(entry).getAddress(),getWidth(device.getNode(entry)),entry))
            
        log.debug("-------------------------------")

    return result

#===========================================================================

# get VHDL template file. This function needs to be in this library
# because it looks for the template file in a subdirectory relative
# to the library directory

def get_vhdl_template(fn=None):
    if not fn:
        this_dir, this_filename = os.path.split(__file__)
        fn = os.path.join(this_dir, "templates", "ipbus_addr_decode.vhd")

    return open(fn).read()


#===========================================================================


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
        self.assertTrue("root" in buses)
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
