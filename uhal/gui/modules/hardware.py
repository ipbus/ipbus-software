#!/usr/bin/python

import os

import pycohal


class Hardware:

    def __init__(self, connection_file = ""):

        self.connection_file = connection_file
        self.IP_end_points_list = []

        self._load_hardware(self.connection_file)

        print "Hardware class instantiated with file ", self.connection_file


    def _load_hardware(self, conn_file):
        pass

        
