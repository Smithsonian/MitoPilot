.onAttach <- function(libname, pkgname) {
  version <- nf_installed_version()

  if (is.na(version)) {
    if (nf_on_path()) {
      # Installed but `nextflow -version` produced no version: usually Java not on
      # the session PATH, or a not-yet-finished first-run download.
      packageStartupMessage(
        "Nextflow was found but 'nextflow -version' did not report a version."
      )
      err <- nf_probe_error()
      if (nzchar(err)) {
        packageStartupMessage(glue::glue("Nextflow reported:\n{err}"))
      }
      glue::glue(
        "Ensure Java 17+ is on PATH in this R session (or set NXF_JAVA_HOME); if ",
        "you just installed Nextflow, run `nextflow -version` in a terminal to ",
        "finish its download. MitoPilot needs Nextflow {nf_supported_label()}."
      ) |> packageStartupMessage()
    } else {
      packageStartupMessage("A Nextflow installation is needed to run the MitoPilot pipeline.")
      glue::glue(
        "Please install Nextflow {nf_supported_label()} from ",
        "{cli::style_underline('https://www.nextflow.io/')}"
      ) |> packageStartupMessage()
    }
    return(invisible())
  }

  packageStartupMessage(glue::glue("Welcome to {cli::style_bold(cli::col_white('{MitoPilot}'))}!"))
  status <- nf_version_status(version)

  switch(status,
    supported = {
      glue::glue(
        "Using Nextflow {version} {cli::col_green('[supported]')} ",
        "(range {nf_supported_label()})"
      ) |> packageStartupMessage()
    },
    too_new = {
      glue::glue("Using Nextflow {version} {cli::col_yellow('[too new]')}") |>
        packageStartupMessage()
      cli::col_yellow(glue::glue(
        "Nextflow 26+ breaks the nf-sqldb plugin. MitoPilot will pin runs to ",
        "Nextflow {NF_MAX_SUPPORTED} via NXF_VER. For best results, install a ",
        "version in {nf_supported_label()}."
      )) |> packageStartupMessage()
    },
    too_old = {
      glue::glue("Using Nextflow {version} {cli::col_red('[unsupported: too old]')}") |>
        packageStartupMessage()
      msg <- glue::glue(
        "Nextflow {version} is below the minimum supported version ",
        "({NF_MIN_SUPPORTED}). {nf_update_hint()}, then restart R."
      )
      # Refuse to attach for a real library() call, but never during R's own
      # install / check load-test (that would break installation and CI).
      if (in_r_build_context()) {
        packageStartupMessage(cli::col_red(msg))
      } else {
        stop(msg, call. = FALSE)
      }
    }
  )

  invisible()
}
