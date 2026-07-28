#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny gargoyle dbplyr
#' @noRd
app_server <- function(input, output, session) {
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

  # Refuse to open a project from an older MitoPilot. The Annotate module reads
  # (path, scaffold) everywhere, so a stale database otherwise fails deep inside
  # dbplyr with a raw "no such column" / "no such table" backtrace that gives the
  # user nothing to act on. Migration is not run automatically: it rewrites tables
  # in place, which is not something to do silently to someone's project.
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

  # Migrate: add BLAST result columns to assemble table for pre-existing databases
  existing_fields <- DBI::dbListFields(session$userData$con, "assemble")
  if (!"blast_accession" %in% existing_fields)
    DBI::dbExecute(session$userData$con, "ALTER TABLE assemble ADD COLUMN blast_accession TEXT")
  if (!"blast_species" %in% existing_fields)
    DBI::dbExecute(session$userData$con, "ALTER TABLE assemble ADD COLUMN blast_species TEXT")
  if (!"blast_pident" %in% existing_fields)
    DBI::dbExecute(session$userData$con, "ALTER TABLE assemble ADD COLUMN blast_pident REAL")
  if (!"blast_qcovs" %in% existing_fields)
    DBI::dbExecute(session$userData$con, "ALTER TABLE assemble ADD COLUMN blast_qcovs REAL")
  if (!"blast_evalue" %in% existing_fields)
    DBI::dbExecute(session$userData$con, "ALTER TABLE assemble ADD COLUMN blast_evalue REAL")
  if (!"blast_lineage" %in% existing_fields)
    DBI::dbExecute(session$userData$con, "ALTER TABLE assemble ADD COLUMN blast_lineage TEXT")

  # Migrate: add per-scaffold BLAST result columns to assemblies table
  asmb_fields <- DBI::dbListFields(session$userData$con, "assemblies")
  if (!"blast_accession" %in% asmb_fields)
    DBI::dbExecute(session$userData$con, "ALTER TABLE assemblies ADD COLUMN blast_accession TEXT")
  if (!"blast_species" %in% asmb_fields)
    DBI::dbExecute(session$userData$con, "ALTER TABLE assemblies ADD COLUMN blast_species TEXT")
  if (!"blast_pident" %in% asmb_fields)
    DBI::dbExecute(session$userData$con, "ALTER TABLE assemblies ADD COLUMN blast_pident REAL")
  if (!"blast_qcovs" %in% asmb_fields)
    DBI::dbExecute(session$userData$con, "ALTER TABLE assemblies ADD COLUMN blast_qcovs REAL")
  if (!"blast_evalue" %in% asmb_fields)
    DBI::dbExecute(session$userData$con, "ALTER TABLE assemblies ADD COLUMN blast_evalue REAL")
  if (!"blast_lineage" %in% asmb_fields)
    DBI::dbExecute(session$userData$con, "ALTER TABLE assemblies ADD COLUMN blast_lineage TEXT")
  # Migrate: add edit_positions column to assemblies table for pre-existing databases
  if (!"edit_positions" %in% asmb_fields)
    DBI::dbExecute(session$userData$con, "ALTER TABLE assemblies ADD COLUMN edit_positions TEXT")


  # Publish / output directory ----
  dir_out <- readLines(file.path(dirname(db), ".config")) |>
    stringr::str_extract("publishDir.*") |>
    na.omit() |>
    stringr::str_remove("^[^'|^\"]+['\"]") |>
    stringr::str_extract("^[^'|^\"]+")
  session$userData$dir_out <- file.path(dirname(db), dir_out)

  # The assembly output folder is named after assemble_opts, so a sample that was
  # reassigned after it assembled points at a folder that was never created.
  # Warn, but keep the app open: fixing this requires the app.
  tryCatch({
    stale <- stale_assemble_dirs(session$userData$con, session$userData$dir_out)
    if (nrow(stale) > 0) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Assembly output not found",
        text = shiny::tags$div(
          shiny::tags$p(
            "The assembly output these samples are assigned to is not on disk. ",
            "Either the assembly parameter set was changed after they were ",
            "assembled, or the output was moved or deleted:"
          ),
          shiny::tags$ul(stale_assemble_items(stale)),
          shiny::tags$p("Either:"),
          shiny::tags$ul(
            shiny::tags$li("set the sample's assembly parameter set back to the name that exists on disk, or"),
            shiny::tags$li("re-run Assembly so the output is published under the assigned name.")
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
    # Reload the destination tab's data so changes made in another tab (e.g. a
    # newly locked consensus in Assemble) appear without a manual refresh.
    trigger(paste0("refresh_", tolower(input$mode)))
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
  observeEvent(input$clear_group, {
    trigger("clear_group")
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
  pipeline_server("run")
  assemble_server("assemble")
  annotate_server("annotate")
  export_server("export")
  workdir_browser_server("workdir_browser")
}
