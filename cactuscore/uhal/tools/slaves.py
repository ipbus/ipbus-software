#!/usr/bin/env python

import sys
import uhal
import math

class node(object):
    
    def __init__(self, name, ref, width = None, desc = None):
        self.name = name
        self.ref = ref
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
        self.root = node("TOP", None)
        
    def add(self, n):
        np = self.root
        m = n.name.split('.')
        for i in range(len(m) - 1):
            name = '.'.join(m[: i + 1])
            l = [x.name for x in np.desc]
            if name not in l:
                c = node(name, None)
                np.desc.append(c)
                np = c
            else:
                np = np.desc[l.index(name)]
        l = [x.name for x in np.desc]
        if n.name not in l:
            np.desc.append(n)
        else:
            np.desc[l.index(n.name)].ref = n.ref
    
    def __str__(self):
        return self.dump(self.root, "")
    
    def dump(self, n, prefix):
        s = ""
        for i in n.desc:
            s = s + prefix + i.name + " " + hex(i.ref.getAddress()) + " " + hex(i.width) + "\n"
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
        if n.ref is not None and "width" in n.ref.getParameters():
            n.width = int(n.ref.getParameters()["width"])
        else:
            if not n.desc:
                print "Oops, I don't see a width parameter", n.name
                raise SystemExit()
            s = 0
            for i in n.desc:
                saddr = i.ref.getAddress()
                if saddr < max_addr:
                    print "Oops, overlap", i.name, hex(saddr), hex(max_addr)
                max_addr = saddr + pow(2, self.assign_width(i))
                if max_addr > top_addr: top_addr = max_addr
            n.width = int(math.ceil(math.log(top_addr - addr,2)))
        if addr % pow(2, n.width) != 0:
            print "Oops, non aligned address", n.name, hex(addr), hex(n.width)
        return n.width
        
    def get_nodes(self, n):
        l = list()
        for i in n.desc:
            l.append((i.name, i.ref.getAddress(), i.width))
            if i.desc: l.append(get_nodes(i))
        return l

def main():

    fn = sys.argv[1]
    if fn.find("file://") == -1:
        fn = "file://" + fn
    try:
        device = uhal.getDevice("dummy","ipbusudp-1.3://localhost:12345",fn)
    except Exception:
        raise Exception("File '%s' does not exist or has incorrect format" % fn)
        
    t = nodetree()
    for i in device.getNodes():
        d = device.getNode(i)
        tags = d.getTags().split(',')
        if 'slave' in tags or 'bus' in tags: t.add(node(i, d))

    t.assign_width(t.root)    
    print t
    

if __name__ == '__main__':
    main()
