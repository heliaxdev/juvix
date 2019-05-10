all: whitepaper

whitepaper: whitepaper.pdc whitepaper.bib
	pandoc --pdf-engine=xelatex --filter pandoc-citeproc --bibliography=whitepaper.bib --csl=de-buck.csl --mathjax --toc --number-sections -o whitepaper.pdf whitepaper.pdc

.PHONY: all whitepaper
