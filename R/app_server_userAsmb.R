#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny gargoyle dbplyr
#' @noRd
app_server_userAsmb <- function(input, output, session) {
  # db connection ----
  db <- getOption("MitoPilot.db") %||% normalizePath(".sqlite")
  session$userData$dir <- dirname(db)
  if (!file.exists(db)) {
    shinyWidgets::sendSweetAlert(
      title = "Database not found",
      text = "The MitoPilot::gui() app requires a database to run. Please make sure your working directory is set to an active MitoPilot project, or use set the location of the database using, options(MitoPilot.db = 'path/to/the/.sqlite').",
      type = "error"
    )
  }
  session$userData$con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)
  session$onSessionEnded(function() {
    message("Session ended. Closing DB connection.")
    DBI::dbDisconnect(session$userData$con)
  })
  register_app_lifecycle(session)
  message(paste("Database attached:", db))

  # Refuse to open a project from an older MitoPilot; see app_server() for why the
  # migration is not run automatically here.
  gaps <- schema_gaps(session$userData$con)
  if (length(gaps) > 0) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Project database needs updating",
      text = shiny::tags$div(
        shiny::tags$p("This project was created with an older version of MitoPilot:"),
        shiny::tags$ul(lapply(gaps, shiny::tags$li)),
        shiny::tags$p("Close the app, run this in the project directory, then reopen it:"),
        shiny::tags$pre('MitoPilot::backwards_compatibility(update_config = FALSE)'),
        shiny::tags$p(
          "That updates the database only. Pass executor = \"local\" (or your ",
          "cluster profile) instead to also refresh the Nextflow config. Your ",
          "database is copied to .old_sqlite_dbs/ before anything changes."
        )
      ),
      html = TRUE,
      type = "warning"
    )
    return(invisible(NULL))
  }

  # Container version check. Warn only: the project is perfectly usable, it just
  # runs the pipeline code baked into the older image (e.g. a pre-1.5.2 image has
  # no local BLAST database, so every sample goes to the remote search instead).
  cgap <- container_version_gap(dirname(db))
  if (!is.null(cgap)) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Container version does not match MitoPilot",
      text = shiny::tags$div(
        shiny::tags$p("This project runs the pipeline from:"),
        shiny::tags$pre(cgap$configured),
        shiny::tags$p("but the installed MitoPilot package is ",
                      shiny::tags$b(as.character(utils::packageVersion("MitoPilot"))),
                      ", which expects:"),
        shiny::tags$pre(cgap$expected),
        shiny::tags$p(
          "The app works either way, but pipeline runs will use the older ",
          "image's tools and workflow code."
        ),
        shiny::tags$p(
          shiny::tags$b("To switch, edit the container line in the project's .config."),
          " That is a one-line change and leaves the rest of your settings alone."
        ),
        shiny::tags$p(
          "You can instead regenerate the whole config, but note it is rebuilt ",
          "from the template for the executor you name, so any tuned cpus, ",
          "memory or clusterOptions must be re-applied afterwards (the old file ",
          "is backed up alongside it). Name your own profile, not \"local\", if ",
          "this project runs on a cluster:"
        ),
        shiny::tags$pre('MitoPilot::backwards_compatibility(executor = "local")')
      ),
      html = TRUE,
      type = "info"
    )
  }

  # Publish / output directory ----
  dir_out <- readLines(file.path(dirname(db), ".config")) |>
    stringr::str_extract("publishDir.*") |>
    na.omit() |>
    stringr::str_remove("^[^'|^\"]+['\"]") |>
    stringr::str_extract("^[^'|^\"]+")
  session$userData$dir_out <- file.path(dirname(db), dir_out)

  # See app_server(). User-assemble projects fix assemble_opts at 'user' and offer
  # no way to change it, so the only way to get here is missing output.
  tryCatch({
    stale <- stale_assemble_dirs(session$userData$con, session$userData$dir_out)
    if (nrow(stale) > 0) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Assembly output not found",
        text = shiny::tags$div(
          shiny::tags$p(
            "The assembly output for these samples is not on disk, so the ",
            "output folder was moved or deleted:"
          ),
          shiny::tags$ul(stale_assemble_items(stale)),
          shiny::tags$p(
            "Restore the output folder, or re-run Assembly to publish it again."
          ),
          shiny::tags$p(
            "These samples are locked and awaiting annotation, so until then ",
            "Annotation and Curation will fail or will silently run without any ",
            "reference hits."
          )
        ),
        html = TRUE,
        type = "warning"
      )
    }
  }, error = function(e) NULL)

  # No-raw-data mode ----
  # rawDir = 'NA' in .config signals an assembly-only project (no reads/coverage).
  # Used to hide read-derived columns in the Assemble table.
  session$userData$no_raw_data <- readLines(file.path(dirname(db), ".config")) |>
    stringr::str_detect("rawDir\\s*=\\s*['\"]NA['\"]") |>
    any()
  # Genetic code ----
  # Per-sample genetic codes live in samples.genetic_code (auto-selected from
  # each sample's curation ruleset) and drive translation. This project-level
  # value only backstops the annotation editor when a sample's code is missing.
  session$userData$genetic_code <- tryCatch({
    v <- DBI::dbGetQuery(
      session$userData$con,
      "SELECT genetic_code FROM samples WHERE genetic_code IS NOT NULL AND TRIM(genetic_code) != '' LIMIT 1"
    )$genetic_code
    gc <- suppressWarnings(as.integer(v))
    if (length(gc) == 1L && !is.na(gc)) as.character(gc) else "2"
  }, error = function(e) "2")
  # Cache the genetic code lookup table once; called ~30x during codon edits.
  session$userData$gcode <- Biostrings::getGeneticCode(session$userData$genetic_code)

  # View mode ----
  observeEvent(input$mode, {
    session$userData$mode <- input$mode
    if(input$mode == "Export"){
      shinyjs::toggle("export_ctrls", condition = TRUE)
      shinyjs::toggle("asmb_ctrls", condition = FALSE)
      shinyjs::toggle("annot_ctrls", condition = FALSE)
    }else if(input$mode == "Assemble"){
      shinyjs::toggle("export_ctrls", condition = FALSE)
      shinyjs::toggle("asmb_ctrls", condition = TRUE)
      shinyjs::toggle("annot_ctrls", condition = FALSE)
    }else{
      shinyjs::toggle("export_ctrls", condition = FALSE)
      shinyjs::toggle("asmb_ctrls", condition = FALSE)
      shinyjs::toggle("annot_ctrls", condition = TRUE)
    }
  })

  # Reload Data
  observeEvent(input$refresh, {
    trigger(paste0("refresh_", tolower(session$userData$mode)))
  })
  # State
  #init("state")
  observeEvent(input$state, {
    trigger("state")
  })
  # Lock
  #init("lock")
  observeEvent(input$lock, {
    trigger("lock")
  })
  # Run
  init("run_modal")
  observeEvent(input$run_modal, {
    trigger("run_modal")
  })
  # ID_verified
  observeEvent(input$id_verified_top, {
    trigger("id_verified_top")
  })
  # mark problematic
  observeEvent(input$problematic_top, {
    trigger("problematic_top")
  })
  # mark partial
  observeEvent(input$partial_top, {
    trigger("partial_top")
  })
  # Export
  observeEvent(input$group, {
    trigger("group")
  })
  observeEvent(input$export, {
    trigger("export")
  })

  # Cross-tab navigation: outlier review -> Annotate details modal.
  # Initialized here (before sub-modules) so listeners in annotate_server /
  # app_annotate_details exist after the flag is created.
  init("goto_annotate")
  on("goto_annotate", {
    shinyWidgets::updatePickerInput(session, "mode", selected = "Annotate")
  })
  init("reopen_outlier_review")
  on("reopen_outlier_review", {
    shinyWidgets::updatePickerInput(session, "mode", selected = "Export")
  })

  # Sub-modules ----
  pipeline_server_userAsmb("run")
  assemble_server_userAsmb("assemble")
  annotate_server("annotate")
  export_server("export")
  workdir_browser_server("workdir_browser")
}
