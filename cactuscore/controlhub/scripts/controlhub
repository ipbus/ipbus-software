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
  'stats')
    stats
    ;;
  *)
    # usage
    echo "Usage: $0 start|stop|restart|status"
    ;;
esac

