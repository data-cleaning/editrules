#!/bin/bash

# usage: buildVignette.bash linear
#        buildVignette.bash categorical  

arg=\'editrules-$1\'


R -e "Sweave($arg)"
pdflatex editrules-$1.tex
bibtex editrules-$1
makeindex editrules-$1.idx
pdflatex editrules-$1.tex
pdflatex editrules-$1.tex


