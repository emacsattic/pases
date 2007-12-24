#!/bin/sh
OLDDIR=`pwd`
TMPDIR=`mktemp -d /tmp/pases.XXXXXXXXXX` || exit 1
cd ${TMPDIR}
wget $URL -q
tar xzf ${BASE}-${V}.tar.gz
cp ${OLDDIR}/${BASE}.pases ${BASE}-${V}/
cd ${TMPDIR}/${BASE}-${V}
tar czf ../${BASE}-${V}.pases.tar.gz *
mv ../${BASE}-${V}.pases.tar.gz ${OLDDIR}
cd ${OLDDIR}
rm -rf ${TMPDIR}
echo "Build package from source at ${URL}: ${BASE}-${V}.pases.tar.gz"
