#!/bin/bash
set -ex

qpdf $1 rotated.pdf --rotate=-90:1-z
pdfcrop rotated.pdf cropped.pdf
lp  -d Brother_QL_1110NWB cropped.pdf 
