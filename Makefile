xmap : create-xmap.Rmd clean_xmap
	Rscript -e "rmarkdown::render('create-xmap.Rmd')"

xmap_render : create-xmap.Rmd xmap
	Rscript -e "devtools::build('xmap')"

.PHONY: clean_xmap
clean_xmap:
	rm -rf xmap *tar.gz
