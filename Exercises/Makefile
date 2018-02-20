# HINTS:
# Use $< to refer to the first dependency of the current rule.

# Makefile for compiling all .Rnw files into .pdf files

.PHONY: all clean cleanall

RNW= $(wildcard *.Rnw)
PDFS= $(RNW:.Rnw=.pdf) 

# cache and figure directories
CACHEDIR = cache
FIGUREDIR = figures

.PHONY: all clean cleanall

# these targets will be made by default
all: $(PDFS)

# TEX files will be generated from Rnw files
TEX = $(RNW:.Rnw=.tex) 

# compile a PDF from a RNoWeb file
%.pdf: %.Rnw
	mkdir -p build
	R -e "library(knitr)" \
	R -e "knitr::opts_chunk[['set']](fig.path='$(FIGUREDIR)/$*-')" \
	R -e "knitr::opts_chunk[['set']](cache.path='$(CACHEDIR)/$*-')" \
	R -e "knitr::knit('$*.Rnw', output='build/$*.tex')"
	pdflatex -halt-on-error -output-directory=build build/$*.tex
	[ -h "$*.pdf" ] || ln -s build/$*.pdf $*.pdf
	
# remove generated files except pdf files
clean:
	rm -rf *.aux
	rm -rf *.log
	rm -rf *.nav
	rm -rf *.out
	rm -rf *.tex
	rm -rf *.synctex.gz
	rm -rf *.snm
	rm -rf *.toc

# run the clean target then remove pdf files too
cleanall: clean
	rm -f $(PDFS)



	



	
