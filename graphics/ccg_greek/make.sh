#!/bin/sh

TEX=*.tex
for file in *.tex; do xelatex ${file} ; convert -density 300 ${file%%.*}.pdf -quality 90 ${file%%.*}.png ; done
rm *.pdf *.log *.aux
