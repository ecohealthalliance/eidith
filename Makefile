PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

all: site

readme: README.md

README.md: README.Rmd
	${RSCRIPT} -e "rmarkdown::render(input='$<', output_file = '$@', quiet = TRUE, output_options=(list(html_preview=FALSE)))"

docs:
	${RSCRIPT} -e "library(methods); devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))"

vignettes:
	${RSCRIPT} -e "library(methods); devtools::build_vignettes()"

site: docs readme index.Rmd
	${RSCRIPT} -e "library(methods); options(repos = c(CRAN='https://cran.rstudio.com')); pkgdown::build_site()"
	mkdir -p docs/inst
	cp -R inst/images docs/inst
	rm -f index.md index.html

test:
	${RSCRIPT} -e "library(methods); devtools::test()"

check:
	${RSCRIPT} -e "library(methods); devtools::check()"

clean:
	rm -rf docs man
	rm -f index.md index.html README.md vignettes/*.html vignettes/*.Rl
