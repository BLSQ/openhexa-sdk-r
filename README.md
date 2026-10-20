# OpenHexa R SDK

The `openhexa` package is the R client for [OpenHEXA](https://github.com/BLSQ/openhexa): explore workspaces, load data
into data frames, publish web apps and run pipelines in an easy and efficient manner.

## Example

```r
library(openhexa)

# In OpenHEXA notebooks you are already logged in. Elsewhere, either:
# - set HEXA_SERVER_URL, HEXA_TOKEN and HEXA_WORKSPACE (e.g. in ~/.Renviron) and skip login()
# - or pass them, reading the token from an environment variable to keep it out of the script
# - or omit the token in an interactive session, to be prompted for it
login(
  server_url = "https://api.openhexa.org",
  token = Sys.getenv("OPENHEXA_TOKEN"),
  workspace = "malaria-burkina"
)

# Get data as data frames
list_datasets()
cases <- get_dataset_file("routine-data", "cases.parquet")
monthly <- run_saved_query("monthly-malaria-cases")

# Publish a chart as a web app
chart <- plotly::plot_ly(monthly, x = ~month, y = ~cases, type = "bar") # install.packages("plotly")
publish_webapp("Malaria cases", chart)

# Run a pipeline and wait for the result
run_pipeline("dhis2-extract", config = list(start_date = "2024-01-01"), wait = TRUE)
```

## Installation

The package is preinstalled in OpenHEXA notebooks. Elsewhere, install the latest release from GitHub:

```r
# install.packages("remotes")
remotes::install_github("BLSQ/openhexa-sdk-r")
```

### Installing a specific version

Releases are tagged on GitHub (`v0.1.0`, `v0.2.0`, ...). R keeps a single version of a package per library, so the
version is chosen when installing:

```r
remotes::install_github("BLSQ/openhexa-sdk-r@v0.2.0")
```

To pin the version for a project, use [renv](https://rstudio.github.io/renv/): it records the version in a lockfile
that restores the same environment on another computer, or in an OpenHEXA workspace.

```r
renv::install("BLSQ/openhexa-sdk-r@v0.2.0")
renv::snapshot()
```

A script can also check the installed version:

```r
stopifnot(packageVersion("openhexa") >= "0.2.0")
```

Versions follow [semantic versioning](https://semver.org). Before 1.0.0, breaking changes increase the minor version
(0.1.x to 0.2.0), so pinning the minor version protects you from them.

## Authenticate and explore

In OpenHEXA notebooks and R notebook pipelines, the package is installed and configured: just call
`library(openhexa)`. Elsewhere, copy your API token from your account settings and log in:

```r
library(openhexa)
login(server_url = "https://api.openhexa.org") # prompts for your API token

list_workspaces()
set_workspace("my-workspace")

list_datasets()
list_files("data/")
list_saved_queries()
list_webapps()
list_pipelines()
list_connections()
```

## Get data

```r
# A workspace file (CSV, TSV, Parquet, Excel, JSON, RDS)
population <- get_file("data/population.csv")

# A dataset file, from the latest version or a given one
cases <- get_dataset_file("malaria-cases", "cases.parquet", version = "2024-Q1")

# All the tables of a dataset version, stacked
all_cases <- get_dataset_data("malaria-cases")

# The result of a saved query of the Data Studio
monthly <- run_saved_query("monthly-malaria-cases", max_rows = 10000)

# Raw downloads
download_file("reports/summary.pdf")

# The workspace database, with DBI / dplyr
con <- connect_database()
```

## Publish a web app

```r
publish_webapp("Hello", "<h1>Hello from R</h1>")

# An htmlwidget, e.g. a plotly chart or a leaflet map (install.packages("plotly"))
chart <- plotly::plot_ly(monthly, x = ~month, y = ~cases, type = "bar")
app <- publish_webapp("Malaria cases", chart, public = TRUE)
app$url
```

Publishing again under the same name replaces the content of the web app.

## Create notebooks

```r
create_notebook("analysis/malaria.ipynb", c(
  markdown = "# Malaria cases",
  "library(openhexa)",
  "cases <- run_saved_query('monthly-malaria-cases')",
  "summary(cases)"
))
```

## Pipelines and connections

```r
run <- run_pipeline("dhis2-extract", config = list(start_date = "2024-01-01"), wait = TRUE)
run$messages

dhis2 <- get_dhis2_connection("dhis2-play")
```

See `vignette("openhexa")` for a complete walkthrough. For anything else, `graphql_query()` runs any query against
the OpenHEXA API.

## Development

```sh
make test      # unit tests, with a mocked API
make check     # R CMD check, as in CI
make validate  # check the GraphQL operations against the production schema (Python, graphql-core)
make smoke     # run every tier against the demo server, see tools/smoke_test.R
```

`make smoke` only targets the demo server (https://app.demo.openhexa.org), with a demo API token in
`OPENHEXA_DEMO_TOKEN` and optionally a workspace in `OPENHEXA_DEMO_WORKSPACE`. It is read-only, unless
`OPENHEXA_SMOKE_WRITE=true` (uploads, datasets, web apps, notebooks, prefixed with `smoke-test-`) or
`OPENHEXA_SMOKE_PIPELINE=<code>` (runs that pipeline).

### Testing locally in RStudio

1. Install [R](https://cran.r-project.org) and [RStudio](https://posit.co/download/rstudio-desktop/) (on macOS:
   `brew install r` and `brew install --cask rstudio`).
2. Open the repository as a project: **File → New Project → Existing Directory**, then select the repository folder.
3. Install the development dependencies, once:

   ```r
   install.packages(c("pkgload", "testthat", "withr", "rstudioapi", "shiny", "roxygen2"))
   ```

4. Load the package from source with `pkgload::load_all()` (**Cmd/Ctrl + Shift + L**). Load it again after every
   change; `library(openhexa)` would load the installed version instead.
5. Try it against the demo server. Copy your API token from your account settings on
   [app.demo.openhexa.org](https://app.demo.openhexa.org), then:

   ```r
   login() # opens a dialog: pick "OpenHEXA demo" in the dropdown, then paste the token
   list_workspaces()
   set_workspace("my-workspace")
   list_datasets()
   ```

   To skip the prompts, set `HEXA_SERVER_URL`, `HEXA_TOKEN` and `HEXA_WORKSPACE` in `~/.Renviron`
   (`usethis::edit_r_environ()`), then restart R (**Cmd/Ctrl + Shift + F10**).

6. Run the unit tests with `testthat::test_local()` (**Cmd/Ctrl + Shift + T**), and the demo smoke test with
   `source("tools/smoke_test.R")`, which prompts for a demo token. Restart R first if you logged in by hand.
7. After changing roxygen comments, regenerate the documentation with `roxygen2::roxygenise()`
   (**Cmd/Ctrl + Shift + D**).
