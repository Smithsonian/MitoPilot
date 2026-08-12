#' Open The MitoPilot GUI
#'
#' @param host character. Address to bind the Shiny server to. Use
#'   `"0.0.0.0"` to allow connections over an SSH tunnel from a remote
#'   machine. Default `NULL` lets Shiny choose (loopback).
#' @param port integer. Port for the Shiny server to listen on. Default
#'   `NULL` lets Shiny pick a random port.
#' @param launch.browser logical. Whether to open a local browser when the
#'   server starts. Default `NULL` resolves to `interactive()`, so desktop
#'   sessions auto-open but headless/remote sessions do not.
#' @param ... additional arguments passed to `run_app()`.
#'
#' @export
#'
MitoPilot <- function(host = NULL, port = NULL, launch.browser = NULL, ...) {
  run_app(host = host, port = port, launch.browser = launch.browser, ...)
}

#' Run the Shiny Application
#'
#' @param host character. Address to bind the Shiny server to. Use
#'   `"0.0.0.0"` to allow connections over an SSH tunnel from a remote
#'   machine. Default `NULL` lets Shiny choose (loopback).
#' @param port integer. Port for the Shiny server to listen on. Default
#'   `NULL` lets Shiny pick a random port.
#' @param launch.browser logical. Whether to open a local browser when the
#'   server starts. Default `NULL` resolves to `interactive()`.
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
    onStart = NULL,
    options = NULL,
    enableBookmarking = NULL,
    uiPattern = "/",
    host = NULL,
    port = NULL,
    launch.browser = NULL,
    ...) {

  # Fail early on an unsupported Nextflow (see README "Nextflow compatibility").
  check_nextflow_version("MitoPilot app")

  # assemble Shiny options from headless-friendly args
  if (is.null(options)) {
    options <- list()
  }
  if (is.null(launch.browser)) {
    launch.browser <- interactive()
  }
  # shinyApp(options=) expects the key "launch.browser" (not the global R option
  # name). Set the global option too so shiny's interactive() default is also
  # overridden when this list is bypassed.
  options[["launch.browser"]] <- launch.browser
  options(shiny.launch.browser = launch.browser)

  # Headless = browser not launched. Gates "Run from App" in the run modal.
  options(MitoPilot.headless = !isTRUE(launch.browser))

  if (!is.null(host)) {
    options[["host"]] <- host
  }
  if (!is.null(port)) {
    options[["port"]] <- port
  }

  # headless remote launch: print the SSH tunnel command to copy
  if (!isTRUE(launch.browser) && !is.null(host) && !is.null(port)) {
    tunnel_instructions(port)
  }

  # check if user has provided an assembly directory
  conf <- tryCatch({
    readLines(".config")
  }, error = function(e) {
    stop("Error reading .config file: ", e$message)
  })
  asmbDir <- tryCatch({
    stringr::str_trim(stringr::str_split(stringr::str_split(conf[grep("asmbDir", conf)], "=")[[1]][2], "'")[[1]][2])
  }, error = function(e) {
    stop("Errpr, .config file missing \"asmbDir\": ", e$message)
  })
  # asmbDir == "NA" means no user-supplied assembly directory, i.e. a standard project.
  ui <- function(request) app_ui(request, userAsmb = (asmbDir != "NA"))
  server <- function(input, output, session) {
    app_server(input, output, session, userAsmb = (asmbDir != "NA"))
  }

  with_golem_options(
    app = shinyApp(
      ui = ui,
      server = server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )



}

#' Print SSH tunnel instructions for a headless GUI session
#'
#' Prints, ready to copy, the `ssh -L` command that forwards a local port to
#' the cluster node running the MitoPilot Shiny server, plus the URL to open
#' in a local browser. The node hostname is read from the live session; the
#' login host is unknown to R and emitted as a `<cluster>` placeholder.
#'
#' @param port integer. The port the Shiny server is listening on.
#'
#' @export
tunnel_instructions <- function(port) {
  node <- Sys.info()[["nodename"]]
  message(
    "\n",
    "MitoPilot GUI running headless on node: ", node, "\n",
    "To reach it from your laptop, open a tunnel (substitute <cluster> with\n",
    "your login host and <user> with your username):\n\n",
    "  ssh -N -L ", port, ":", node, ":", port, " <user>@<cluster>\n\n",
    "Then open in your browser:\n\n",
    "  http://localhost:", port, "\n"
  )
  invisible(NULL)
}
