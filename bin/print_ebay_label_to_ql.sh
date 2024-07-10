#!/bin/bash
set -ex
if ! command -v pdfScale.sh &> /dev/null
then
  git clone git@github.com:tavinus/pdfScale.git
  cp pdfScale/*.sh ~/.bin/
fi

qpdf $1 rotated.pdf --rotate=-90:1-z
pdfcrop rotated.pdf cropped.pdf
pdfScale.sh  rotated.pdf  out.pdf -r "custom mm 103 164" -s 0.85 --no-fit-to-page --hor-align left --vert-align bottom
lp  -d Brother_QL_1110NWB out.pdf 
