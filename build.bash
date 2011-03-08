#!/bin/bash


echo "Removing building information..."
rm -rf output

echo "Generate documentation..."
R -q -f roxygen.R
echo "export(\`[.editmatrix\`)" >> pkg/NAMESPACE


mkdir output
cd output
R CMD build ../pkg
for x in *.tar.gz 
do 
    R CMD check $x
done
