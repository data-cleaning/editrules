#!/bin/bash

R=R
CHECKARG=""
while [ $# -gt 0 ] ; do
  case "$1" in 
    -dev)
       R=Rdev
       shift 1 ;;
    *)
       CHECKARG="$CHECKARG $1"
       shift 1 ;;
  esac
done


echo "######## Removing building information..."
rm -rf output


echo "######## Generate documentation..."
$R -e 'devtools::document("./pkg")'

echo "######## Building package in output..."
mkdir output
cd output
$R CMD build ../pkg
echo "######## Testing package with $CHECKARG ..."
for x in *.tar.gz 
do 
    $R CMD check $CHECKARG $x
done

echo "**BUILT USING $R"
$R --version

