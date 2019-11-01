source /opt/Xilinx/petalinux/2018.2/settings.sh
make -j8 Set=uhal BUILD_PUGIXML=1 BUILD_UHAL_TESTS=0 BUILD_UHAL_PYCOHAL=0
