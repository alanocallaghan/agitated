README.md: README.Rmd $(shell find R/ -name "*.R")
	r -e 'rmarkdown::render("$<", output_file = "$@")'
