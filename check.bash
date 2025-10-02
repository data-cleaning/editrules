#!/bin/bash

echo ""
echo "######## Removing "editrules" installation..."
echo ""
R CMD REMOVE editrules

echo ""
echo "######## Install "editrules" from output directory..."
echo ""
cd output
for x in *.tar.gz 
do 
    R CMD INSTALL $x
done

cd ../../deducorrect
echo ""
echo "######## Building and checking deducorrect..."
echo ""
bash build.bash