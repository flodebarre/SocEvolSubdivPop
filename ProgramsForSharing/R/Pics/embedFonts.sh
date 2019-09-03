#!/bin/bash

## Paul Johnson
## 2013-05-11
##
## R creates pdf files that do not have fonts embedded. This
## uses ghostscript to fix that. It takes all pdf in a directory
## and embeds fonts.
##
## Todo: Make command line argument smarter

for i in *.pdf; do
    base=`basename $i .pdf`;
    basenew="${base}/newtemp.pdf"

   /usr/bin/gs -o $basenew -dNOPAUSE -dPDFSETTINGS=/prepress     -sDEVICE=pdfwrite $i

    mv -f $basenew $i
done;
