README.md: README.Rmd
	r -e 'rmarkdown::render("$<", output_file = "$@")'
