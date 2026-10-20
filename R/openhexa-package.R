#' @section Getting started:
#' In OpenHEXA notebooks, you are already logged in: skip [login()]. The functions below are listed by resource
#' in the next sections.
#'
#' ```r
#' # Log in: opens a dialog in RStudio, or prompts in the console
#' login()
#' # or, without prompts:
#' login(server_url = "https://api.openhexa.org", token = Sys.getenv("OPENHEXA_TOKEN"), workspace = "my-workspace")
#'
#' # Explore the workspace
#' list_workspaces()
#' set_workspace("my-workspace")
#' list_files()
#' list_datasets()
#' list_saved_queries()
#' list_pipelines()
#'
#' # Read data into data frames
#' population <- get_file("data/population.csv")
#' cases <- get_dataset_file("malaria-cases", "cases.parquet")
#' monthly <- run_saved_query("monthly-malaria-cases")
#'
#' # Query the workspace database
#' con <- connect_database()
#' DBI::dbListTables(con)
#' DBI::dbDisconnect(con)
#'
#' # Write data
#' write.csv(population, "population.csv", row.names = FALSE)
#' upload_file("population.csv", "data/population.csv")
#' create_dataset_version("malaria-cases", "2024-Q2")
#' add_dataset_file("malaria-cases", "population.csv")
#'
#' # Publish a chart as a web app
#' chart <- plotly::plot_ly(monthly, x = ~month, y = ~cases, type = "bar")
#' app <- publish_webapp("Malaria cases", chart)
#' app$url
#'
#' # Create a notebook
#' create_notebook("analysis/malaria.ipynb", c(markdown = "# Malaria cases", "library(openhexa)"))
#'
#' # Run a pipeline and wait for the result
#' run <- run_pipeline("dhis2-extract", config = list(start_date = "2024-01-01"), wait = TRUE)
#' run$logs
#'
#' # Get the parameters of a connection
#' dhis2 <- get_dhis2_connection("dhis2-play")
#' ```
#'
#' @section Authentication:
#' * [login()]: log in to an OpenHEXA server.
#' * [logout()]: forget the credentials.
#' * [get_user()]: the user you are logged in as.
#' * [openhexa_client()]: a client for another server, in the same session.
#'
#' @section Workspaces:
#' * [list_workspaces()]: the workspaces you are a member of.
#' * [get_workspace()]: get a workspace.
#' * [set_workspace()]: choose the workspace that the other functions use.
#'
#' @section Files:
#' * [list_files()]: list the files of the workspace.
#' * [get_file()]: read a file (CSV, Parquet, Excel, ...) into a data frame.
#' * [download_file()]: download a file as is.
#' * [upload_file()]: upload a file to the workspace.
#' * [workspace_path()]: the path to a workspace file, in OpenHEXA notebooks.
#' * [workspace_tmp_path()]: a path in the temporary folder of the workspace, in OpenHEXA notebooks.
#'
#' @section Datasets:
#' * [list_datasets()]: list the datasets of the workspace.
#' * [get_dataset()]: get a dataset.
#' * [list_dataset_versions()]: list the versions of a dataset.
#' * [list_dataset_files()]: list the files of a dataset version.
#' * [get_dataset_file()]: read a dataset file into a data frame.
#' * [get_dataset_data()]: read all the tables of a dataset version, stacked.
#' * [download_dataset_file()]: download a dataset file as is.
#' * [create_dataset()]: create a dataset.
#' * [create_dataset_version()]: create a version of a dataset.
#' * [add_dataset_file()]: add a file or a data frame to a dataset version.
#'
#' @section Database and saved queries:
#' * [list_saved_queries()]: list the saved queries of the Data Studio.
#' * [run_saved_query()]: the result of a saved query, as a data frame.
#' * [connect_database()]: a DBI connection to the workspace database.
#' * [get_database_credentials()]: the credentials of the workspace database.
#'
#' @section Web apps:
#' * [list_webapps()]: list the web apps of the workspace.
#' * [get_webapp()]: get a web app.
#' * [publish_webapp()]: publish HTML or an htmlwidget (plotly, leaflet, ...) as a web app.
#' * [delete_webapp()]: delete a web app.
#'
#' @section Notebooks:
#' * [create_notebook()]: create an R notebook in the workspace.
#'
#' @section Pipelines:
#' * [list_pipelines()]: list the pipelines of the workspace.
#' * [get_pipeline()]: get a pipeline.
#' * [run_pipeline()]: run a pipeline, and optionally wait for the result.
#' * [list_pipeline_runs()]: list the runs of a pipeline.
#' * [get_pipeline_run()]: get a run, with its status, messages and logs.
#' * [wait_pipeline_run()]: wait for a run to finish.
#' * [stop_pipeline_run()]: stop a run.
#'
#' @section Connections:
#' * [list_connections()]: list the connections of the workspace to external systems.
#' * [get_connection()]: get the parameters of a connection.
#' * [get_dhis2_connection()]: get a DHIS2 connection.
#' * [get_iaso_connection()]: get an IASO connection.
#' * [get_postgresql_connection()]: get a PostgreSQL connection.
#' * [get_s3_connection()]: get an S3 connection.
#' * [get_gcs_connection()]: get a Google Cloud Storage connection.
#' * [get_custom_connection()]: get a custom connection.
#'
#' @section Anything else:
#' * [graphql_query()]: run any query against the OpenHEXA API.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom rlang %||%
NULL

# Package-level state: credentials set by login() and the cache of loaded GraphQL operations.
.openhexa <- new.env(parent = emptyenv())
