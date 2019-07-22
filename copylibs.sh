LIBUHAL=uhal/uhal/lib/*.2.6
LIBLOG=uhal/log/lib/*.2.6
LIBGRAMMAR=uhal/grammars/lib/*.2.6
LIBPUGI=extern/pugixml/pugixml-1.2/libpugixml.so

APOLLO=apollo:/work/lib/

scp ${LIBUHAL} ${LIBLOG} ${LIBGRAMMAR} ${LIBPUGI} ${APOLLO}
