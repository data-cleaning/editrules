#!/bin/bash

R -e "Sweave('editrules-vignette.Snw')"
latex editrules-vignette.tex
bibtex editrules-vignette
latex editrules-vignette.tex
pdflatex editrules-vignette.tex


