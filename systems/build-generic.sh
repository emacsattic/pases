#!/bin/sh
OLDDIR=`pwd`
TMPDIR=`mktemp -d /tmp/pases.XXXXXXXXXX` || exit 1
if [ ! -e ${OLDDIR}/${BASE}-${V}.tar.gz ] ; then
    wget $URL -q
fi
cd ${TMPDIR}
tar xzf ${OLDDIR}/${BASE}-${V}.tar.gz
if [ -d ${BASE}-${V} ] ; then
    cd ${BASE}-${V}
elif [ -d ${BASE} ] ; then
    cd ${BASE}
fi
cp ${OLDDIR}/${BASE}.pasdef .
tar cz --owner=nobody --group=users -f ../${BASE}-${V}.pases *
mv ${TMPDIR}/${BASE}-${V}.pases ${OLDDIR}
cd ${OLDDIR}
rm -rf ${TMPDIR}
echo "Built package from source at ${URL}: ${BASE}-${V}.pases"
