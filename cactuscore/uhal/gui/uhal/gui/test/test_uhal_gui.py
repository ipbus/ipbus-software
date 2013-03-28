import unittest

import uhal.gui.guiloader


class TestGuiImports(unittest.TestCase):


    def setUp(self):
        pass


    def tearDown(self):
        pass


    def test_default_on(self):
        print '********** Simple configuration **********'
        g = uhal.gui.guiloader.gui_loader(test_mode='Yes')
        self.assertEqual(uhal.gui.guiloader.start_test(), 'OK')
        print


    def test_default_off(self):    
        print '********** Configuration: default=No, guilist=[] **********'
        g = uhal.gui.guiloader.gui_loader(default='No', test_mode='Yes')
        self.assertEqual(uhal.gui.guiloader.start_test(), 'OK')
        print


    def test_default_on_custom_one(self):
        print '********** Configuration: default=Yes, guilist=[\'CustomWindow1\'] **********'
        g = uhal.gui.guiloader.gui_loader(guilist=['CustomWindow1'], test_mode='Yes')
        self.assertEqual(uhal.gui.guiloader.start_test(), 'OK')
        print


    def test_default_off_custom_one(self):
        print '********** Configuration: default=No, guilist=[\'CustomWindow1\'] **********'
        g = uhal.gui.guiloader.gui_loader(default='No', guilist=['CustomWindow1'], test_mode='Yes')
        self.assertEqual(uhal.gui.guiloader.start_test(), 'OK')
        print


    def test_default_on_custom_one_two(self):
        print '********** Configuration: default=Yes, guilist=[\'CustomWindow1\',\'CustomWindow2\']**********'
        g = uhal.gui.guiloader.gui_loader(guilist=['CustomWindow1','CustomWindow2'], test_mode='Yes')
        self.assertEqual(uhal.gui.guiloader.start_test(), 'OK')        
        print


    def test_default_off_custom_one_two(self):
        print '********** Configuration: default=No, guilist=[\'CustomWindow1\',\'CustomWindow2\'] **********'
        g = uhal.gui.guiloader.gui_loader(default='No', guilist=['CustomWindow1','CustomWindow2'], test_mode='Yes')
        self.assertEqual(uhal.gui.guiloader.start_test(), 'OK')
        print

   


def main():
    suite = unittest.TestLoader().loadTestsFromTestCase(TestGuiImports)
    unittest.TextTestRunner(verbosity=2).run(suite)

if __name__ == '__main__':
    main()
