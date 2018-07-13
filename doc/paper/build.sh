#!/bin/sh

pandoc --filter pandoc-citeproc --bibliography=whitepaper.bib --csl=de-buck.csl --mathjax --toc --number-sections -o whitepaper.pdf whitepaper.pdc
