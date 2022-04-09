default:
	pdflatex pts.tex
	bibtex pts
	pdflatex pts.tex
	pdflatex pts_ua.tex
	bibtex pts_ua
	pdflatex pts_ua.tex
	return 0

clean:
	rm -f *.toc
	rm -f *.aux
	rm -f *.log
	rm -f *.out
	rm -f *.bbl
	rm -f *.blg

