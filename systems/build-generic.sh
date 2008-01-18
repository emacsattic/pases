#!/bin/sh
OLDDIR=`pwd`
TMPDIR=`mktemp -d /tmp/pases.XXXXXXXXXX` || exit 1
if [ ! -e ${OLDDIR}/${BASE}-${V}.tar.gz ] ; then
    wget $URL -q
fi
cd ${TMPDIR}
tar xzf ${OLDDIR}/${BASE}-${V}.tar.gz
cp ${OLDDIR}/${BASE}.pasdef ${TMPDIR}/${BASE}-${V}/
cd ${TMPDIR}/${BASE}-${V}
tar czf ${TMPDIR}/${BASE}-${V}.pases *
mv ${TMPDIR}/${BASE}-${V}.pases ${OLDDIR}
cd ${OLDDIR}
rm -rf ${TMPDIR}
echo "Build package from source at ${URL}: ${BASE}-${V}.pases"
