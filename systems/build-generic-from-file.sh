#!/bin/sh
OLDDIR=`pwd`
TMPDIR=`mktemp -d /tmp/pases.XXXXXXXXXX` || exit 1
if [ ! -e $BASE.el ] ; then
    wget $URL -q
fi
cd ${TMPDIR}
cp ${OLDDIR}/${BASE}.pasdef ${TMPDIR}
cp ${OLDDIR}/${BASE}.el ${TMPDIR}
tar czf ${TMPDIR}/${BASE}-${V}.pases *
mv ${TMPDIR}/${BASE}-${V}.pases ${OLDDIR}
cd ${OLDDIR}
rm -rf ${TMPDIR}
echo "Built package from file at ${URL}: ${BASE}-${V}.pases"
