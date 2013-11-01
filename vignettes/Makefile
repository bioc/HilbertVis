all:	pdf

pdf:	HilbertVis.tex
	pdflatex HilbertVis
	bibtex HilbertVis
	pdflatex HilbertVis
	pdflatex HilbertVis
	make clean

clean:
	rm -f *.out *.bbl *.aux *.log *.blg *.brf *.toc  *~ *.eps
	rm -f *.nfs*
	rm -f HilbertVis-*.jpg HilbertVis-*.pdf HilbertVis.ps 
#	rm -f *.dvi *.ps Ringo.tex
