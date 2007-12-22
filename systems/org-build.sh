#!/bin/sh
V="5.17a"
MAIND=`pwd`
cd /tmp
wget http://orgmode.org/org-${V}.tar.gz
gunzip org-${V}.tar.gz
mkdir org-${V}
cp ${MAIND}/org.pases org-${V}/
tar r --file=org-${V}.tar org-${V}/org.pases
rm org-${V}/org.pases
rmdir org-${V}
gzip org-${V}.tar
mv org-${V}.tar.gz ${MAIND}/org-${V}.pases.tar.gz
