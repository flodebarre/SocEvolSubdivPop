#!/bin/bash

for i in *.pdf; do
	echo $i;
    pdffonts $i
done;
