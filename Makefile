.DEFAULT_GOAL := help

.PHONY: help d document t test c check v validate schema smoke install

help:  ## Show this help message
	@echo "Available commands:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2}'

document:  ## Regenerate NAMESPACE and the man pages (roxygen2)
	Rscript -e 'roxygen2::roxygenise()'

d: document

test:  ## Run the test suite
	Rscript -e 'testthat::test_local(".")'

t: test

check:  ## Run R CMD check (as in CI)
	Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"), error_on = "warning")'

c: check

validate:  ## Validate the GraphQL operations against production
	python tools/validate_queries.py

v: validate

schema:  ## Save the production GraphQL schema to tools/schema.graphql, for the IDE
	python tools/validate_queries.py --save-schema tools/schema.graphql

smoke:  ## Test against the demo server (needs OPENHEXA_DEMO_TOKEN; see tools/smoke_test.R)
	Rscript tools/smoke_test.R

install:  ## Install the package locally
	R CMD INSTALL .
