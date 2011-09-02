@echo off

echo Removing building information...
rm -rf output

echo Copying DESCRIPTION and NAMESPACE to pkg directory..."
copy build\DESCRIPTION pkg
copy build\NAMESPACE pkg

echo Generate documentation...
Rscript roxygen.R
echo export(`[.editmatrix`) >> pkg/NAMESPACE

md output
cd output
R CMD build ../pkg
FOR %%1 in (*.tar.gz) DO R CMD check %%1
cd ..