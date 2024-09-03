
rm -rf /usr/local/gcc_dev
mkdir build_auto
cd build_auto
../configure --prefix=/usr/local/gcc_dev --disable-bootstrap --program-prefix=dev- --with-dwarf2 

make -j 12

##TODO specific tests we want

make check -j 12
#make install -j 6 


