
all: README.md

README.md: README.Rmd
	Rscript -e "rmarkdown::render(input='$<', output_file = '$@', quiet = TRUE, output_options=(list(html_preview=FALSE)))"

# ADD
# Roxygenize
# Build staticdocs
# Copy README images to docs
# DRAT
# Tests?
