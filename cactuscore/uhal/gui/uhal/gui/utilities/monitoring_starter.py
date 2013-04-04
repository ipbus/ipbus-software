from uhal.gui.utilities import hardware_monitoring


hw_mon = None

def start_monitoring(file_name):

    global hw_mon
    
    print "Instantiating monitoring starter with file %s " % file_name
    hw_mon = hardware_monitoring.HardwareMonitoring(connection_file=file_name)
    hw_mon.start()



def stop_mon(self):

    global hw_mon
    
    hw_mon.join(5)
    hw_mon.stop()
    
