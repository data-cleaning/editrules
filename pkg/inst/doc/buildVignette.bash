#!/bin/bash

# usage: buildVignette.bash linear
#        buildVignette.bash categorical  

arg=\'editrules-$1\'
echo $arg

R -e "Sweave($arg)"
latex editrules-linear.tex
bibtex editrules-linear
latex editrules-linear.tex
pdflatex editrules-linear.tex


