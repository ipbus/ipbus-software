import uhal.gui.guiloader

def main():

    print 'Simple configuration'
    g = uhal.gui.guiloader.GuiLoader()
    g.start()

    print 'Configuration: default=No, []'
    g = uhal.gui.guiloader.GuiLoader(default='No')
    g.start()

    print 'Configuration: default=Yes, [\'CustomWindow1\']'
    g = uhal.gui.guiloader.GuiLoader(['CustomWindow1'])
    g.start()

    
    print 'Configuration: default=No, [\'CustomWindow1\']'
    g = uhal.gui.guiloader.GuiLoader(default='No', listguis=['CustomWindow1'])
    g.start()

    
    print 'Configuration: default=Yes, [\'CustomWindow1\',\'CustomWindow2\']'
    g = uhal.gui.guiloader.GuiLoader(listguis=['CustomWindow1','CustomWindow2'])
    g.start()

    print 'Configuration: default=No, [\'CustomWindow1\',\'CustomWindow2\']'
    g = uhal.gui.guiloader.GuiLoader(default='No', listguis=['CustomWindow1','CustomWindow2'])
    g.start()



    
if __name__ == '__main__':
    main()
