#!/usr/bin/python
"""
usage: gen_ipus_addr_decode [options] <uhal_address_table.xml>

The script generates the address select logic in the
file for the ipbus system.

The script takes an uHAL compliant XML input file and prints out the vhdl module.

Note that full address decoding is not performed (would be very
inefficient with 32b address space), so slaves will appear at many
locations.

options:
   -v, --verbose                            verbose
   -d  --debug                              debug output
   -t <file>, --template=<file>             uses a different vhdl template file
                                            (default
                                            /opt/cactus/etc/uhal/tools/ipbus_addr_decode.vhd)
"""
import getopt
import sys
import os.path
import time
import logging
import math
import uhal

#===========================================================================================
        
class BitArray:
    
    def __init__(self,value=0):
        self.length = 32
        if (value> (1<<self.length)-1):
            raise Exception("Value '%d' too big. It does not fit in %d bits\n" % (value,self.length))
        
        self.bits = [value>>i & 1 for i in xrange(self.length)]

    def __getitem__(self,index):
        return self.bits[index]
        
    def __and__(self,other):
        if not isinstance(other,BitArray):
            raise Exception("BitArray.__and__ method requires BitArray")

        result = BitArray()
        result.bits = [x & y for (x,y) in zip(self.bits,other.bits)]
        return result

    def __or__(self,other):
        if not isinstance(other,BitArray):
            raise Exception("BitArray.__and__ method requires BitArray")

        result = BitArray()
        result.bits = [x | y for (x,y) in zip(self.bits,other.bits)]
        return result
    
    def __invert__(self):
        result = BitArray()
        result.bits = [(x+1) % 2 for x in self.bits]
        return result
    
    def __str__(self):
        return "".join(map(lambda x: x and "1" or "0",self.bits[::-1]))

    def uint(self):
        result = 0
        for i,x in enumerate(self.bits):
            result += x << i
            
        return result

    def hex(self):
        return "0x%08x" % self.uint()

class node(object):
    
    def __init__(self, name, ref, flag, width = None, desc = None):
        self.name = name
        self.ref = ref
        self.flag = flag
        if width is None:
            self.width = 0
        else:
            self.width = width
        self.desc = list()
        if desc is not None: self.desc.append(desc)
        
    def sort(self):
        self.desc = sorted(self.desc, key = lambda x: x.ref.getAddress())
        
class nodetree(object):
	
    def __init__(self):
        self.root = node("TOP", None, False)
        
    def add(self, n):
        np = self.root
        m = n.name.split('.')
        for i in range(len(m) - 1):
            name = '.'.join(m[: i + 1])
            l = [x.name for x in np.desc]
            if name not in l:
                c = node(name, None, False)
                np.desc.append(c)
                np = c
            else:
                np = np.desc[l.index(name)]
        l = [x.name for x in np.desc]
        if n.name not in l:
            np.desc.append(n)
        else:
            np.desc[l.index(n.name)].ref = n.ref
            np.desc[l.index(n.name)].flag = n.flag
    
    def __str__(self):
        return self.dump(self.root, "")
    
    def dump(self, n, prefix):
        s = ""
        for i in n.desc:
            s = s + prefix + i.name + " " + hex(i.ref.getAddress()) + " " + hex(i.width) + " " + str(i.flag) + "\n"
            if i.desc: s = s + self.dump(i, prefix + "  ")
        return s
    
    def assign_width(self, n):
        n.sort()
        if n.ref is not None:
            addr = n.ref.getAddress()
        else:
            addr = 0
        max_addr = 0
        top_addr = 0
        if n.ref is not None and "width" in n.ref.getFirmwareInfo():
            n.width = int(n.ref.getFirmwareInfo()["width"])
        else:
            if not n.desc:
                log.error("Fatal error - no width parameter found for node: " + n.name)
                raise SystemExit(1)
            for i in n.desc:
                saddr = i.ref.getAddress()
                if saddr < max_addr:
                    log.error("Overlap between nodes detected: " + i.name + " " + hex(saddr) + " "+ hex(max_addr))
                max_addr = saddr + pow(2, self.assign_width(i))
                if max_addr > top_addr: top_addr = max_addr
            n.width = int(math.ceil(math.log(top_addr - addr,2)))
        if addr % pow(2, n.width) != 0:
            log.error("Non-aligned base address detected: " + n.name + " " + hex(addr) + " " + hex(n.width))
        return n.width

    def get_nodes(self, n):
        l = list()
        if n.flag:
            l.append((n.name, n.ref.getAddress(), n.width))
        elif n.desc:
            for i in n.desc:
                l.extend(self.get_nodes(i))
        return l

#===========================================================================================


def main():    

    # configure logger
    global log
    log = logging.getLogger("main")
    formatter = logging.Formatter('%(name)s %(levelname)s: %(message)s')
    handler = logging.StreamHandler(sys.stdout)
    handler.setFormatter(formatter)
    log.addHandler(handler)
    log.setLevel(logging.WARNING)
    uhal.setLogLevelTo(uhal.LogLevel.WARNING)

    # options for verbosity level and VHDL template
    nflag = False
    template_fn = "/opt/cactus/etc/uhal/tools/ipbus_addr_decode.vhd"
    try:
        opts, args = getopt.getopt(sys.argv[1:], "vdnht:", ["verbose","debug","dry-run","help","template="])
    except getopt.GetoptError, err:
        log.critical(__doc__)
        sys.exit(2)
    for o, a in opts:
        if o in ("-v", "--verbose"):
            log.setLevel(logging.INFO)
            uhal.setLogLevelTo(uhal.LogLevel.INFO)
        elif o in ("-d", "--debug"):
            log.setLevel(logging.DEBUG)
            uhal.setLogLevelTo(uhal.LogLevel.DEBUG)
        elif o in ("-h", "--help"):
            print __doc__
            sys.exit(0)
        elif o in ("-t", "--template"):
            template_fn = a
        elif o in ("-n", "--dry-run"):
            nflag = True

    # make sure that exactly one argument was given, later assumed to be the xml file name
    if len(args) != 1:
        log.critical("Incorrect usage - invalid number of arguments! Make sure that options come before argument.\n" + __doc__)
        sys.exit(1)

    try:
        device = uhal.getDevice("dummy","ipbusudp-1.3://localhost:12345","file://" + args[0])
    except Exception:
        raise Exception("File '%s' does not exist or has incorrect format" % args[0])

    t = nodetree()
    for i in device.getNodes():
        d = device.getNode(i)
        f = d.getFirmwareInfo()
        p = "type" in f and f["type"] == "endpoint"
        t.add(node(i,d,p))
    t.assign_width(t.root)
    uhal_slaves = t.get_nodes(t.root)
    moduleName = os.path.splitext(os.path.basename(args[0]))[0]
        
    # retrieve masks and addresses
    or_prod = BitArray(0x0)
    and_prod = BitArray(0xffffffff)

    slaves = []
    for n,(id,addr,width) in enumerate(uhal_slaves):
        addr_bits = BitArray(addr)
        mask_bits = BitArray(2**width - 1)
        log.info("uHAL slave %s,%s,0b%s,0b%s" % (n,id,addr_bits, mask_bits))
        if (addr_bits & mask_bits).uint() != 0:
            log.error("ERROR: Non Aligned address 0b%s with respect to address bit mask 0b%s" % (str(addr_bits),str(mask_bits)))
    
        or_prod = or_prod | (addr_bits & ~mask_bits)
        and_prod = and_prod & (addr_bits | mask_bits)
        slaves.append((n,id,addr_bits,mask_bits))

    addr_mask = or_prod & ~and_prod
    log.info("Significant address bits 0b%s" % str(addr_mask))

# FIXME: this algorithm leads to more than the required number of address bits being
# compared in the VHDL in some circumstances. Could add a loop here to calculate the
# number of address bits needed for a unique match for a given slave.

    if not nflag:

        # generate vhdl snipped with symbolic constants
        snippet1 = "-- START automatically  generated VHDL the %s \n" % time.asctime()
        slaveIds={}
        maxN=0
        for n,id,addr_bits,mask_bits in slaves:
            if n>maxN: maxN=n
            slaveIds[n]="N_SLV_"+id.upper().replace(".","_")
            snippet1 += "  constant "+slaveIds[n]+": integer := %i;\n"%n
        snippet1 += "  constant N_SLAVES: integer := %i;\n"%(maxN+1)
        snippet1 += "-- END automatically generated VHDL\n"
        
        # generate vhdl snippet with selection code
        snippet2 = "-- START automatically  generated VHDL the %s \n" % time.asctime()
        for n,id,addr_bits,mask_bits in slaves:
            mask = addr_mask & ~mask_bits
            if n == 0:
                snippet2 += "    if    "
            else:
                snippet2 += "    elsif "
    
            snippet2 += "std_match(addr, \""
            for i in range(32)[::-1]:
                if not mask[i]:
                    snippet2 += "-"
                elif addr_bits[i]:
                    snippet2 += "1"
                else:
                    snippet2 += "0"
            snippet2 += "\") then\n"
            
            snippet2 += "      sel := ipbus_sel_t(to_unsigned(" + slaveIds[n] + ", IPBUS_SEL_WIDTH)); -- " + str(id) + " / base " + addr_bits.hex() + " / mask " + mask.hex() + "\n"
        snippet2 += "-- END automatically generated VHDL\n"
    
        # print vhdl code
        vhdlfilename = "ipbus_decode_"+moduleName+".vhd"
        vhdlfile=open(vhdlfilename,"w")
        vhdlfile.write(open(template_fn).read().replace("-- INSERT_SYMBOLIC_CONSTANTS_HERE",snippet1)\
                       .replace("-- INSERT_SELECTION_HERE",snippet2)\
                       .replace("PACKAGENAME",moduleName))
        vhdlfile.close()
        print "VHDL decode file saved: ", vhdlfilename
    
if __name__ == '__main__':
    main()
  
