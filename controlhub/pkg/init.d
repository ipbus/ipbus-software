#!/bin/bash
#
# controlhub	Starts up the controlhub server daemon
#
# chkconfig: 25 99 01
# description: IPBus is a protocol for hardware access over ethernet. \
#              This service starts up the IPBus ControlHub server daemon.
#
# processname: controlhub

### BEGIN INIT INFO
# Provides: controlhub
# Default-Start: 3 5
# Default-Stop: 0 1 6
# Short-Description: Starts up the controlhub server daemon
# Description:       IPBus is a protocol for hardware access over ethernet. \
#                    This service starts up the IPBus ControlHub server daemon.
#
### END INIT INFO

CACTUSROOT=/opt/cactus

export HOME=$CACTUSROOT


# Default LD_LIBRARY_PATH
# Can be overridden in the package .conf files
#
LD_LIBRARY_PATH=${CACTUSROOT}/lib

start() {
  ${CACTUSROOT}/bin/controlhub_start
}

stop() {
  ${CACTUSROOT}/bin/controlhub_stop
}

restart() {
  ${CACTUSROOT}/bin/controlhub_stop
  ${CACTUSROOT}/bin/controlhub_start
}

status() {
  ${CACTUSROOT}/bin/controlhub_status
}

info() {
  ${CACTUSROOT}/bin/controlhub_info
}

stats() {
  ${CACTUSROOT}/bin/controlhub_stats
}

case "$1" in
  'start')
    start
    ;;
  'stop')
    stop
    ;;
  'restart')
    restart
    ;;
  'status')
    status
    ;;
  'info')
    info
    ;;
  'stats')
    stats
    ;;
  *)
    # usage
    echo "Usage: $0 start|stop|restart|status|info|stats"
    ;;
esac

