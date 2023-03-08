xmap : create-xmap.Rmd clean_xmap
	Rscript -e "rmarkdown::render('create-xmap.Rmd')"

xmap_render : create-xmap.Rmd xmap
	Rscript -e "devtools::build('xmap')"
	echo 'git commit -a -m ":package: render {xmap} v.0.0.0.9xxx"

.PHONY: clean_xmap
clean_xmap:
	rm -rf xmap *tar.gz
