PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

all: drat

readme: README.md

README.md: README.Rmd
	${RSCRIPT} -e "rmarkdown::render(input='$<', output_file = '$@', quiet = TRUE, output_options=(list(html_preview=FALSE)))"

docs:
	${RSCRIPT} -e "library(methods); devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))"

site: docs readme index.Rmd
	${RSCRIPT} -e "library(methods); options(repos = c(CRAN='https://cran.rstudio.com')); pkgdown::build_site()"
	mkdir -p docs/inst
	cp -R inst/images docs/inst
	rm -f index.md index.html

drat: site
	mkdir -p docs/src/contrib docs/bin/windows/contrib docs/bin/macosx/contrib
	${RSCRIPT} -e "src_pkg <- devtools::build(); drat::insertPackage(src_pkg, 'docs'); unlink(src_pkg)"
	##${RSCRIPT} -e "if(Sys.info()['sysname'] == 'Darwin') { osx_pkg <- devtools::build(binary = TRUE, args = c('--preclean')); drat::insertPackage(osx_pkg, 'docs'); unlink(osx_pkg) }"
  ## WINDOWS CURRENTLY BUILT MANUALLY OR PULLED FROM CI. Then run drat::insertPackage(WINDOWS_ZIP_FILE, 'docs')



test:
	${RSCRIPT} -e "library(methods); devtools::test()"

check:
	${RSCRIPT} -e "library(methods); devtools::check()"

clean:
	rm -rf docs man
	rm -f index.md index.html README.md vignettes/*.html vignettes/*.Rl
