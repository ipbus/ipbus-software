from __future__ import print_function
import unittest

from uhal.gui.guiloader import test


class TestGuiImports(unittest.TestCase):

    
    def setUp(self):
        
        def_gui = __import__('uhal.gui.guis', globals(), locals(), ['defaultgui'])
        self.default_gui_mod = def_gui.defaultgui

        custom_guis = __import__('uhal.gui.customguis', globals(), locals(), ['customwindow1', 'customwindow2'])
        self.custom_gui1 = custom_guis.customwindow1
        self.custom_gui2 = custom_guis.customwindow2


    def tearDown(self):
        pass


    def test_default_on(self):
        print('\n********** Simple configuration **********')
        self.assertEqual(test(guilist=[self.default_gui_mod]), 'OK')
        print()

    
    def test_default_off(self):    
        print('\n********** Configuration: default=No, guilist=[] **********')
        self.assertEqual(test(guilist=[]), 'OK')
        print()

    
    def test_default_on_custom_one(self):
        print('\n********** Configuration: default=Yes, guilist=[\'CustomWindow1\'] **********')
        self.assertEqual(test(guilist=[self.default_gui_mod, self.custom_gui1]), 'OK')
        print()

    
    def test_default_off_custom_one(self):
        print('\n********** Configuration: default=No, guilist=[\'CustomWindow1\'] **********')
        self.assertEqual(test(guilist=[self.custom_gui1]), 'OK')
        print()

    
    def test_default_on_custom_one_two(self):
        print('\n********** Configuration: default=Yes, guilist=[\'CustomWindow1\',\'CustomWindow2\']**********')
        self.assertEqual(test(guilist=[self.default_gui_mod, self.custom_gui1, self.custom_gui2]), 'OK')
        print()

    
    def test_default_off_custom_one_two(self):
        print('\n********** Configuration: default=No, guilist=[\'CustomWindow1\',\'CustomWindow2\'] **********')
        self.assertEqual(test(guilist=[self.custom_gui1, self.custom_gui2]), 'OK')
        print()

   
def main():
    suite = unittest.TestLoader().loadTestsFromTestCase(TestGuiImports)
    unittest.TextTestRunner(verbosity=2).run(suite)

if __name__ == '__main__':
    main()
