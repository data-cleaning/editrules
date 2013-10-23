R -e "knitr::knit('editrules-as-mip.Rnw')"
xelatex editrules-as-mip.tex
bibtex editrules-as-mip
xelatex editrules-as-mip.tex
xelatex editrules-as-mip.tex

