#!/bin/bash

R -e "Sweave('editrules-vignette.Snw')"
latex editrules-linear.tex
bibtex editrules-linear
latex editrules-linear.tex
pdflatex editrules-linear.tex


