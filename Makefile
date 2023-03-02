xmap : create-xmap.Rmd
	Rscript -e "rmarkdown::render('create-xmap.Rmd')"

clean_xmap:
	rm -r xmap *tar.gz
