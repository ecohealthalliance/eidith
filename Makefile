PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file


readme: README.md

README.md: README.Rmd
	${RSCRIPT} -e "rmarkdown::render(input='$<', output_file = '$@', quiet = TRUE, output_options=(list(html_preview=FALSE)))"

docs:
	${RSCRIPT} -e "library(methods); devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))"

site: docs readme
	${RSCRIPT} -e "library(methods); pkgdown::build_site()"
	mkdir -p docs/inst
	cp -R inst/images docs/inst

test:
	${RSCRIPT} -e "library(methods); devtools::test()"

check:
	${RSCRIPT} -e "library(methods); devtools::check()"

# ADD
# DRAT
# Tests?
