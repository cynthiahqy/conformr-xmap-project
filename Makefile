xmap : create-xmap.Rmd xmap_clean 
	Rscript -e "rmarkdown::render('create-xmap.Rmd')"

xmap_build : create-xmap.Rmd xmap
	Rscript -e "devtools::build('xmap')"
	echo 'git commit -a -m ":package: render {xmap} v.0.0.0.9xxx"'
xmap_check : xmap
	Rscript -e "devtools::check('xmap', document = FALSE)"

.PHONY: xmap_clean
xmap_clean:
	rm -rf xmap *tar.gz
