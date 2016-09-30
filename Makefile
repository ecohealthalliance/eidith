PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file


readme: README.md

README.md: README.Rmd
	${RSCRIPT} -e "rmarkdown::render(input='$<', output_file = '$@', quiet = TRUE, output_options=(list(html_preview=FALSE)))"

docs:
	${RSCRIPT} -e "library(methods); devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))"

site: docs
	${RSCRIPT} -e "library(methods); staticdocs::build_site()""

test:
	${RSCRIPT} -e "library(methods); devtools::test()"

check:
	${RSCRIPT} -e "library(methods); devtools::check()"
# ADD
# Roxygenize
# Build staticdocs
# Copy README images to docs
# DRAT
# Tests?
