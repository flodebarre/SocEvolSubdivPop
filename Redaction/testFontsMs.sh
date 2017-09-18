#!/bin/bash

# Separate all the pages
pdftk SocEvolSubdivPop_ms.pdf burst

for i in pg*.pdf; do
	echo $i;
  pdffonts $i | grep "Type 3"
done;
