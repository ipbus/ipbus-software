#!/usr/bin/env python

import sys
import uhal

class nodetree(object):
	
    def __init__(self):
        self.root = dict()
        
    def add(self, child):
        node = self.root
        m = child.split('.')
        n = ""
        for i in m:
            if n != "": n += "."
            n += i
            if n not in node:
                node[n] = dict()
                node = node[n]
            else:
                node = node[n]

    def __repr__(self):
        return repr(self.root)
    
    def __str__(self):
        return self.dump(self.root, "")
    
    def dump(self, node, prefix):
        s = ""
        for i in node:
            s = s + prefix + i + "\n"
            if node[i]: s = s + self.dump(node[i], prefix + "  ")
        return s
        
    def find_slaves(self, node, device):
        s = list()
        for i in node:
            tags = device.getNode(i).getTags().split(',')
            if 'slave' in tags or 'bus' in tags:
                s.append(i)
            elif node[i]:
                s.append(self.find_slaves(node[i][0], device))
        return s
        
def main():

    fn = sys.argv[1]
    if fn.find("file://") == -1:
        fn = "file://" + fn
    try:
        device = uhal.getDevice("dummy","ipbusudp-1.3://localhost:12345",fn)
    except Exception:
        raise Exception("File '%s' does not exist or has incorrect format" % fn)
        
    t = nodetree()
    for i in device.getNodes(): t.add(i)
    
    print t
    for i in t.find_slaves(t.root, device):
        print i

if __name__ == '__main__':
    main()
