"""Clones a XDAQ setup template and replaces the environment variables
Usage: clone_setup.py xdaq_zone /path/to/setup
"""

import os
import shutil
import sys
from nutils import system, logger

TEST_SETUP_REPLACEMENTS = ['ROOTSYS',
                           'BUILD_HOME',
                           'XDAQ_ROOT',
                           'LD_LIBRARY_PATH',
                           'INSTALL_PATH',
                           'XDAQ_DOCUMENT_ROOT',
                           'XDAQ_SETUP_ROOT',
                           'PWD_PATH',
                           'HOSTNAME',
                           'XDAQ_ZONE']
def generate_dict():
    d = {}
    for r in TEST_SETUP_REPLACEMENTS:
        d["${%s}" % r] = os.environ.get(r,"")
        d["$%s" % r] = os.environ.get(r,"")

    return d

def replace_environ(path):
    '''Replace each environment variable within setup files.'''

    r_dict = generate_dict()

    for root, dirs, files in os.walk(path):
        for fn in files:
            if fn not in ["triggerd","xdaqd","ttcd","gtgmtd","subsystemd"]:
                fn = os.path.join(root, fn)

                text = open(fn, 'r').read()

                for p, r in r_dict.iteritems():
                    text = text.replace(p, r)

                open(fn,'w').write(text)

def clone_setup(setup,from_path):
    os.environ["XDAQ_ZONE"] = setup
    to_path = os.path.join(from_path,'../%s' % setup)

    system("rm -rf %s" % to_path,log=False)
    shutil.copytree(from_path,to_path)
	
    for root, dirs, files in os.walk(to_path, topdown=False):
        if '.svn' in dirs:
			shutil.rmtree(os.path.join(root, '.svn'))
        if 'CVS' in dirs:
			shutil.rmtree(os.path.join(root, 'CVS'))

    replace_environ(to_path)

if __name__== "__main__":
    if len(sys.argv) != 3:
        sys.stderr.write('ERROR: wrong number of arguments\n\n')
        print __doc__
        sys.exit(1)

    clone_setup(sys.argc[1],sys.argv[2])
