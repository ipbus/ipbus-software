from uhal.gui.utilities import hardware_monitoring


class MonitoringStarter:

    def __init__(self, file_name):
        print "Instantiating monitoring starter with file %s " % file_name
        self.__hw_mon = hardware_monitoring.HardwareMonitoring(connection_file=file_name)
        self.__run()


    def __run(self):

        self.__hw_mon.start()


    def stop_mon(self):
        self.__hw_mon.join(5)
        self.__hw_mon.stop()
    
