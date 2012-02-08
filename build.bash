#!/bin/bash


echo "######## Removing building information..."
rm -rf output

echo "######## Copying DESCRIPTION and NAMESPACE to pkg directory..."
cp build/DESCRIPTION pkg
cp build/NAMESPACE pkg

echo "######## Generate documentation..."
R -q -f roxygen.R

echo "######## Building package in output..."
mkdir output
cd output
Rdev CMD build ../pkg
echo "######## Testing package..."
for x in *.tar.gz 
do 
    Rdev CMD check $x
done
