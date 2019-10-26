#!/usr/bin/env python

"""
This script prints the values in a remote device, of registers below a specified uHAL node.
"""

from __future__ import print_function

import argparse
import uhal


def snapshot(node, regex):
    '''Returns a list of tuples of the values in all uHAL sub-nodes'''

    regValues = []

    for name in sorted(node.getNodes(regex)):
        n = node.getNode(name)
        if n.getPermission() != uhal.NodePermission.WRITE:
            regValues.append( (name, n.read()) )
    node.getClient().dispatch()

    return regValues


if __name__=="__main__":

    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter, description=__doc__)

    parser.add_argument('device_id', help='Board URI or ID within connections file')
    parser.add_argument('reg_name', help='Register node name')
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('-c','--conn_file', default=None, help='Connections file URI (e.g. file://path/to/file.xml)')
    group.add_argument('-a','--addr_file', default=None, help='Address file URI (e.g. file://path/to/file.xml)')
    parser.add_argument('-r','--regex', default='.*', help='Regex pattern')

    args = parser.parse_args()

    # Create HwInterface and print node values
    uhal.setLogLevelTo( uhal.LogLevel.ERROR ) 
    if args.conn_file is None:
        device = uhal.getDevice("device", args.device_id, args.addr_file)
    else:
        cm = uhal.ConnectionManager(args.conn_file)
        device = cm.getDevice(args.device_id)

    print('Values of registers below node "'+args.reg_name+'" in device "'+args.device_id+'"')

    for name, value in snapshot(device.getNode(args.reg_name), args.regex):
        print(" +", name, ":", hex(value))

