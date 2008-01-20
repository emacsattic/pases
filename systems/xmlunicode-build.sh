#!/bin/sh
if [ ! -e unichars.el ] ; then
    wget http://nwalsh.com/emacs/xmlchars/unichars.el -q
fi
if [ ! -e xmlunicode.el ] ; then
    wget http://nwalsh.com/emacs/xmlchars/xmlunicode.el -q
fi
V="1.10"
tar czf xmlunicode-$V.pases xmlunicode.el unichars.el xmlunicode.pasdef 
echo "Built package: xmlunicode-${V}.pases"
