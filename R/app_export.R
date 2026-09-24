# Togglable column groups for the Export table. Cols not listed here
# (sticky cols, action buttons) are always shown.
EXPORT_COL_GROUPS <- list(
  Options  = c("curate_opts"),
  Stats    = c("topology", "structure"),
  BLAST    = c("blast_accession", "blast_ref_status", "blast_species",
               "blast_lineage"),
  # filled at render time from the user's mapping file (export_metadata_cols)
  Metadata = character(0),
  # specimen + the fields ticked in meta_export_fields, filled at render time
  Specimen = c("specimen")
)
EXPORT_COL_GROUP_LOOKUP <- {
  out <- character()
  for (.g in names(EXPORT_COL_GROUPS)) {
    for (.c in EXPORT_COL_GROUPS[[.g]]) out[.c] <- .g
  }
  out
}

# Per-gene signature of one unit's PCG annotations, read straight from the db.
# Covers exactly the fields flag_PCG_outliers aligns on (see
# get_export_PCG_annotations), so two equal signatures mean the alignment for
# that gene cannot have changed. Returns a named character vector (names =
# gene); multi-exon genes collapse order-independently.
unit_pcg_sig <- function(con, ID, path, scaffold) {
  rows <- DBI::dbGetQuery(
    con,
    "SELECT gene, pos1, pos2, direction, translation FROM annotations
      WHERE ID = ? AND path = ? AND scaffold = ? AND type = 'PCG' AND pos1 > 0",
    params = list(as.character(ID), as.integer(path), as.integer(scaffold))
  )
  if (nrow(rows) == 0) return(stats::setNames(character(0), character(0)))
  key <- paste(rows$pos1, rows$pos2, rows$direction, rows$translation, sep = "|")
  vapply(split(key, rows$gene), function(x) paste(sort(x), collapse = ";"), character(1))
}

# Genes whose signature differs between two unit_pcg_sig snapshots (edited,
# added or removed). NULL for either snapshot means "unknown", never "unchanged".
sig_diff <- function(before, now) {
  if (is.null(before) || is.null(now)) return(NULL)
  genes <- union(names(before), names(now))
  if (length(genes) == 0) return(character(0))
  b <- unname(before[genes])
  n <- unname(now[genes])
  genes[!(!is.na(b) & !is.na(n) & b == n)]
}

# Body of the pre-review internal-stop warning: one line per affected record.
internal_stop_alert_text <- function(stops) {
  lines <- paste0(
    "<li>", stops$label, " - ", toupper(stops$gene),
    ifelse(stops$n_stops > 1, paste0(" (", stops$n_stops, " stops)"), ""),
    "</li>"
  )
  htmltools::HTML(paste0(
    "<p>", mp_n(nrow(stops), "record"),
    " in this group still translate", if (nrow(stops) == 1) "s" else "",
    " with an internal stop codon. These will fail NCBI validation.</p>",
    "<ul style=\"text-align: left; max-height: 240px; overflow-y: auto;\">",
    paste(lines, collapse = ""), "</ul>",
    "<p>Continue to the outlier review, or cancel the export?</p>"
  ))
}

#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("col_css")),
    # Row filters first, column visibility last and set off by a rule (T13).
    div(
      class = "mp-filter-row",
      mp_filter_picker(
        ns("export_filter"), "Exported:", ANNOTATE_EXPORT_CHOICES,
        width = "140px"
      ),
      div(
        class = "mp-filter-cols",
        mp_filter_picker(
          ns("col_groups"), "Columns:", names(EXPORT_COL_GROUPS),
          width = "150px"
        )
      )
    ),
    uiOutput(ns("n_selected")),
    div(class = "mp-table-resize", reactableOutput(ns("table"))),
    # mp_csv_download_row() shape with the labels T13 settled on: reactable
    # 0.4.5 cannot report its filtered row set, so neither button claims to.
    div(
      class = "mp-csv-row",
      style = "margin-top: 12px; display: flex; gap: 8px;",
      downloadButton(ns("export_selected"), "Download selected rows",
                     class = "btn-sm btn-default"),
      downloadButton(ns("export_all"), "Download all rows",
                     class = "btn-sm btn-default")
    )
  )
}

#' export Server Functions
#'
#' @noRd
export_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    geome_viewer_server("geome", open = reactive(input$specimen_open),
                        on_change = function() trigger("refresh_export"))

    # Prepare data ----
    rv <- reactiveValues(
      # curate_opts = dplyr::tbl(session$userData$con, "curate_opts") |>
      #  dplyr::collect(),
      data = fetch_export_data(),
      updating = NULL,
      outliers = NULL,    # flags tibble from flag_PCG_outliers()
      review_samples = NULL, # named list (by gene) of every unit in the alignment,
                             # so any sample can be picked for editing (not just flagged)
      review_genes = NULL, # genes still pending review (drives navigation)
      review_idx = 1L,     # cursor into review_genes
      resolved = character(0), # "ID|gene" keys the user marked resolved;
                              # persists across flag/alignment recomputes
      # The (unit, gene) the user jumped out to edit, plus that unit's PCG
      # signature at jump-out. Compared against the db on return to decide what
      # to re-align. Owned here, not by the annotate module, so the decision
      # never depends on the details modal's state surviving the round trip.
      review_focus = NULL,
      review_focus_sig = NULL,
      # Currently selected header-template name, so the modal reopens with it
      export_template = "default",
      # Last-used review options, so the modal reopens with them (not defaults)
      opt_review = TRUE,
      opt_start = 10,
      opt_stop = 10,
      opt_ident = 60,
      # TRUE only while an export is writing, so the gears turn only then
      exporting = FALSE
    )

    # Refresh ----
    init("refresh_export")
    on("refresh_export", {
      rv$data <- fetch_export_data()
      trigger("update_export_table")
    })

    # Mirror the column-group picker so NULL (= user cleared all) is
    # distinguishable from the pre-init state. Default: all groups on.
    col_groups_rv <- reactiveVal(.specimen_default_groups(names(EXPORT_COL_GROUPS), session$userData$con))
    if (!"Specimen" %in% isolate(col_groups_rv())) {
      shinyWidgets::updatePickerInput(session, "col_groups", selected = isolate(col_groups_rv()))
    }
    observeEvent(input$col_groups, {
      col_groups_rv(input$col_groups %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    # Exported Yes/No filter. Rows are tagged with mp-exp-<0/1> (see rowClass)
    # so unselected states hide via CSS, same mechanism as the column picker.
    export_filter_rv <- reactiveVal(unname(ANNOTATE_EXPORT_CHOICES))
    observeEvent(input$export_filter, {
      export_filter_rv(input$export_filter %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    .grp <- function(col) {
      g <- EXPORT_COL_GROUP_LOOKUP[col]
      if (is.na(g)) NULL else paste0("mp-grp-", g)
    }

    # One colDef per data column, with the shared header name and tooltip
    # (T10). `extra_class` adds the col_css hide hooks on top of the group
    # class. `name`/`tip` override the registry where Export means something
    # the other tables do not.
    .cd <- function(col, extra_class = NULL, name = NULL, tip = NULL, ...) {
      nm <- name %||% unname(MP_COL_NAMES[[col]])
      tp <- tip %||% (if (col %in% names(MP_COL_TIPS)) unname(MP_COL_TIPS[[col]]) else NULL)
      cls <- c(.grp(col), extra_class)
      colDef(show = TRUE, name = nm, header = rt_header(nm, tp),
             class = cls, headerClass = cls, ...)
    }

    # CSS hide for unselected groups / exported states; keeps DOM intact so
    # filter/sort/page state survives toggling.
    output$col_css <- renderUI({
      hidden     <- setdiff(names(EXPORT_COL_GROUPS), col_groups_rv())
      hidden_exp <- setdiff(unname(ANNOTATE_EXPORT_CHOICES), export_filter_rv())
      # Hide path/scaffold when every unit shares value 1 (no extra info), mirroring
      # the Annotate table. Reactive on rv$data so they appear as soon as a
      # multi-unit sample is locked.
      d <- rv$data
      hide_path     <- !is.null(d) && nrow(d) > 0 && all(d$path == 1, na.rm = TRUE)
      hide_scaffold <- !is.null(d) && nrow(d) > 0 && all(d$scaffold == 1, na.rm = TRUE)
      # SeqID only carries information once it diverges from the ID
      hide_seqid    <- !is.null(d) && nrow(d) > 0 && all(d$seqid == d$ID, na.rm = TRUE)
      # Scope to THIS module's table so rules don't hit the shared mp-grp /
      # mp-exp classes on the assemble, userAsmb, and annotate tables.
      sel <- paste0("#", ns("table"), " ")
      rules <- c(
        if (hide_path)          paste0(sel, ".mp-col-path { display: none !important; }"),
        if (hide_scaffold)      paste0(sel, ".mp-col-scaffold { display: none !important; }"),
        if (hide_seqid)         paste0(sel, ".mp-col-seqid { display: none !important; }"),
        if (length(hidden))     paste0(sel, ".mp-grp-", hidden, " { display: none !important; }"),
        if (length(hidden_exp)) paste0(sel, ".mp-exp-", hidden_exp, " { display: none !important; }")
      )
      if (length(rules) == 0) return(NULL)
      tags$style(HTML(paste(rules, collapse = "\n")))
    })
    # Style-only output has no size, so Shiny would treat it as hidden and
    # stop re-rendering it after the first pass.
    outputOptions(output, "col_css", suspendWhenHidden = FALSE)

    # colDefs for the user's mapping-file columns, toggled as one group. Read
    # from the samples schema, not rv$data, so the render stays isolated from
    # data refreshes (updateReactable keeps page and selection).
    metadata_col_defs <- function(declared) {
      sample_cols <- tryCatch(colnames(dplyr::tbl(session$userData$con, "samples")),
                              error = function(e) character(0))
      cols <- export_metadata_cols(sample_cols, declared)
      stats::setNames(lapply(cols, function(col) {
        colDef(show = TRUE, name = col, header = rt_header(col, "From your mapping file"),
               class = "mp-grp-Metadata", headerClass = "mp-grp-Metadata",
               html = TRUE, cell = rt_longtext(), minWidth = 120)
      }), cols)
    }

    # colDefs for the GEOME fields ticked in the export field picker, toggled
    # as one group. Bumped by geome_fields_ver() after a save so the table
    # re-renders with the new columns.
    geome_fields_ver <- reactiveVal(0L)
    geome_col_defs <- function() {
      keys <- tryCatch(
        DBI::dbGetQuery(session$userData$con, "SELECT key FROM meta_export_fields")$key,
        error = function(e) character(0))
      cols <- vapply(keys, .meta_key_col, character(1), USE.NAMES = FALSE)
      stats::setNames(lapply(cols, function(col) {
        colDef(show = TRUE, name = col, header = rt_header(col, "From GEOME"),
               class = "mp-grp-Specimen", headerClass = "mp-grp-Specimen",
               html = TRUE, cell = rt_longtext(), minWidth = 120)
      }), cols)
    }

    # GEOME field picker ----
    init("geome_fields")
    on("geome_fields", {
      s <- meta_field_summary(session$userData$con, "GEOME")
      showModal(geome_fields_modal(ns, s))
      raw <- s[s$kind == "raw", ]
      output$geome_raw <- reactable::renderReactable(reactable::reactable(
        raw[, c("level", "field", "n_samples", "example", "col")],
        selection = "multiple", onClick = "select", compact = TRUE, searchable = TRUE,
        defaultSelected = which(raw$selected), defaultPageSize = 15,
        columns = list(
          level = colDef(name = "Level"), field = colDef(name = "Field"),
          n_samples = colDef(name = "Samples", width = 80),
          example = colDef(name = "Example", cell = rt_longtext(), html = TRUE),
          col = colDef(name = "Template token", cell = function(v) paste0("{", v, "}"))
        )
      ))
    })

    observeEvent(input$geome_fields_save, {
      s <- meta_field_summary(session$userData$con, "GEOME")
      raw <- s[s$kind == "raw", ]
      picked <- raw$key[reactable::getReactableState("geome_raw", "selected") %||% integer(0)]
      .meta_save_fields(session$userData$con, c(input$geome_combos, picked))
      removeModal()
      geome_fields_ver(geome_fields_ver() + 1L)
      rv$data <- fetch_export_data()
    })

    # Render table ----
    output$table <- reactable::renderReactable({
      geome_fields_ver()
      declared_cols <- declared_cols_fn()
      metadata_cols <- metadata_col_defs(names(declared_cols))
      geome_cols <- geome_col_defs()
      reactable::reactable(
        isolate(rv$data),
        compact = TRUE,
        language = reactable::reactableLang(
          noData = "No annotations are locked yet. Lock an annotation in Annotate to see it here."
        ),
        defaultPageSize = 100,
        showPageSizeOptions = TRUE,
        onClick = "select",
        selection = "multiple",
        searchable = TRUE,
        resizable = TRUE,
        filterable = TRUE,
        height = "100%",
        wrap = FALSE,
        pageSizeOptions = c(25, 50, 100, 200, 500),
        striped = TRUE,
        rowStyle = rt_highlight_row(),
        # Tag rows exported (mp-exp-1) vs not (mp-exp-0) so the Exported picker
        # can hide unselected states via CSS.
        rowClass = htmlwidgets::JS("function(rowInfo) {
          if (!rowInfo || !rowInfo.values) return '';
          var ets = rowInfo.values['export_time_stamp'];
          return 'mp-exp-' + ((ets != null && ets !== '') ? '1' : '0');
        }"),
        # Alphabetical by ID: Export is a checklist over a fixed set, and the
        # only date column (export_time_stamp) is NA for exactly the rows that
        # still need work.
        defaultSorted = list(ID = "asc"),
        theme = reactable::reactableTheme(
          headerStyle = list(whiteSpace = "normal", lineHeight = "1.2")
        ),
        # A column shows only if it is declared below; the user's mapping-file
        # columns are added as the Metadata group (see cols after this list).
        defaultColDef = colDef(show = FALSE),
        # Render order comes from the data frame, not this list. See
        # fetch_export_data().
        columns = c(declared_cols, metadata_cols, geome_cols)
      )
    })

    # Declared MitoPilot columns; metadata_col_defs() appends the user's own.
    declared_cols_fn <- function() {
        list(
          `.selection` = colDef(show = TRUE, sticky = "left", width = 28, align = "center"),
          # Wide enough for a 16-character ID; the tooltip covers longer ones.
          ID = .cd("ID", minWidth = 160, sticky = "left", html = TRUE,
                   cell = rt_longtext()),
          # One row per assembly unit; the classes let col_css hide these when every
          # unit shares value 1.
          path = .cd("path", extra_class = "mp-col-path", width = 90,
                     align = "center", filterable = FALSE),
          scaffold = .cd("scaffold", extra_class = "mp-col-scaffold", width = 90,
                         align = "center", filterable = FALSE),
          # The GenBank record name this unit exports under; hidden when it is just
          # the ID (no fragmented sample in the project).
          seqid = .cd("seqid", extra_class = "mp-col-seqid", minWidth = 130),
          Taxon = .cd("Taxon", minWidth = 140, html = TRUE, cell = rt_longtext()),
          specimen = specimen_col_def(ns("specimen_open"), class = "mp-grp-Specimen"),
          curate_opts = .cd("curate_opts", width = 110),
          genetic_code = .cd("genetic_code", width = 110, align = "center"),
          blast_ref_status = .cd(
            "blast_ref_status", html = TRUE, minWidth = 130, align = "center",
            cell = rt_blast_ref_status()
          ),
          blast_accession = .cd(
            "blast_accession", html = TRUE, width = 130,
            cell = rt_ncbi_link(auto_col = "blast_accession_auto")
          ),
          blast_accession_auto = colDef(show = FALSE),
          blast_species = .cd("blast_species", html = TRUE, minWidth = 160,
                              cell = rt_longtext()),
          blast_lineage = .cd("blast_lineage", html = TRUE, minWidth = 200,
                              cell = rt_longtext()),
          topology = .cd("topology", width = 125, align = "center", html = TRUE, cell = rt_topology()),
          structure = .cd("structure", html = TRUE, minWidth = 200,
                          cell = rt_longtext()),
          PCGCount = .cd("PCGCount", width = 90, align = "center"),
          tRNACount = .cd("tRNACount", width = 90, align = "center"),
          rRNACount = .cd("rRNACount", width = 90, align = "center"),
          ORFCount = .cd("ORFCount", width = 90, align = "center"),
          missing = .cd("missing", html = TRUE, minWidth = 130, cell = rt_longtext()),
          extra = .cd("extra", html = TRUE, minWidth = 130, cell = rt_longtext()),
          # Stored, not recomputed: annotate.warnings counts warning events at
          # curation time, while the Annotate table recounts them per feature.
          warnings = .cd(
            "warnings", width = 110, na = "0", name = "Stored Warnings",
            align = "center",
            tip = paste(
              "Counted when the assembly was curated; the Annotate table",
              "recounts per feature, so its number can be higher"
            )
          ),
          export_time_stamp = .cd("export_time_stamp", html = TRUE, width = 150,
                                  filterable = FALSE, align = "center", cell = rt_ts_date()),
          export_group = .cd("export_group", sticky = "right", minWidth = 140)
        )
    }

    # update table ----
    init("update_export_table")
    on("update_export_table", {
      reactable::updateReactable(
        "table",
        data = rv$data,
        selected = reactable::getReactableState("table", "selected"),
        page = reactable::getReactableState("table", "page")
      )
    })

    # table selection ----
    # Rows hidden by the Exported filter stay mounted, so drop them from the
    # selection so bulk export only touches visible samples.
    selected <- reactive({
      sel <- reactable::getReactableState("table", "selected")
      if (is.null(sel) || length(sel) == 0) return(sel)
      exp_code <- ifelse(is.na(rv$data$export_time_stamp), "0", "1")
      visible <- exp_code %in% export_filter_rv()
      intersect(sel, which(visible))
    })

    # Rows the Exported picker leaves on screen. reactable's own search and
    # column filters are a browser-side layer R cannot see, so this is the
    # server-visible count, not a live DOM count.
    visible_n <- reactive({
      d <- rv$data
      if (is.null(d) || nrow(d) == 0) return(0L)
      sum(ifelse(is.na(d$export_time_stamp), "0", "1") %in% export_filter_rv())
    })

    output$n_selected <- renderUI({
      assemble_table_status(visible_n(), nrow(rv$data), length(selected()), noun = "assembly")
    })

    # Toolbar buttons that act on the selection are dead without one (T01).
    # A class selector, not an id: these buttons live in the top-level UI.
    observe({
      shinyjs::toggleState(
        selector = "#export_ctrls .mp-needs-selection",
        condition = length(selected()) > 0
      )
    })

    # Publish current selection so the work-dir browser can pre-select this sample
    observe({
      session$userData$wd_selected[["Export"]] <- unique(rv$data$ID[selected()])
    })

    # CSV Export ----
    .export_cols_drop <- c("poor_blast_ref", "blast_accession_auto",
                           "annotate_switch", "specimen", "specimen_message")

    observe({
      shinyjs::toggleState("export_selected", condition = length(selected()) > 0)
    })

    output$export_selected <- downloadHandler(
      filename = function() paste0("export_selected_", Sys.Date(), ".csv"),
      content = function(file) {
        req(length(selected()) > 0)
        rv$data |>
          dplyr::slice(selected()) |>
          dplyr::select(-dplyr::any_of(.export_cols_drop)) |>
          write.csv(file, row.names = FALSE)
      }
    )

    output$export_all <- downloadHandler(
      filename = function() paste0("export_all_", Sys.Date(), ".csv"),
      content = function(file) {
        rv$data |>
          dplyr::select(-dplyr::any_of(.export_cols_drop)) |>
          write.csv(file, row.names = FALSE)
      }
    )

    # Group ----
    # The "already in a group" warning is shown inside the modal, next to the
    # group name, rather than as a confirm before the modal opens (T24).
    init("group")
    on("group", {
      req(session$userData$mode == "Export")
      if (!need_selection(length(selected()))) return()
      trigger("group_modal")
    })

    # Clear Group ----
    # Remove the export_group assignment from any selected samples that currently
    # have one.
    init("clear_group")
    on("clear_group", {
      req(session$userData$mode == "Export")
      if (!need_selection(length(selected()))) return()
      rv$updating <- rv$data |> dplyr::slice(selected())
      if (!any(!is.na(rv$updating$export_group))) {
        mp_toast(
          if (nrow(rv$updating) == 1) {
            "The selected assembly is not in an export group."
          } else {
            sprintf(
              "None of the %s selected assemblies are in an export group.",
              nrow(rv$updating)
            )
          },
          type = "warning"
        )
        return()
      }
      mp_confirm(
        ns("clear_group_confirm"),
        title = "Clear export group",
        text = sprintf(
          paste(
            "%s %s export group. Files already written for that group",
            "are not removed."
          ),
          mp_n(nrow(rv$updating), "assembly", "assemblies"),
          if (nrow(rv$updating) == 1) "leaves its" else "leave their"
        ),
        action_label = "Clear group"
      )
    })
    observeEvent(input$clear_group_confirm, {
      req(input$clear_group_confirm)
      assign_export_group(rep(NA_character_, nrow(rv$updating)))
    })

    init("group_modal")
    on("group_modal", {
      rv$updating <- rv$data |> dplyr::slice(selected())
      topologies <- rv$updating |>
        dplyr::pull(topology) |>
        unique()
      structures <- rv$updating |>
        dplyr::pull(structure) |>
        unique()
      group_current <- rv$updating |>
        dplyr::pull(export_group) |>
        unique()
      already <- sort(group_current[!is.na(group_current)])
      modalDialog(
        title = mp_modal_title(
          "Assign export group",
          subtitle = sprintf(
            "%s selected", mp_n(nrow(rv$updating), "assembly", "assemblies")
          )
        ),
        size = "l",
        easyClose = FALSE,
        p(tags$b("Topology: "), paste(topologies, collapse = ", ")),
        p(tags$b("Gene Order:")),
        list_to_li(structures),
        hr(),
        selectizeInput(
          ns("group_name"),
          label = "Group name:",
          choices = c("", sort(unique(rv$data$export_group))),
          selected = character(0),
          width = "320px",
          options = list(
            create = TRUE,
            maxItems = 1,
            # Refuses to create a name the export path cannot use, at the
            # keystroke rather than after Create (T18).
            createFilter = "^[A-Za-z0-9._-]+$",
            placeholder = "letters, numbers, dot, hyphen, underscore"
          )
        ),
        if (length(already) > 0) {
          p(
            class = "mp-fg-warning",
            icon("triangle-exclamation"), " ",
            sprintf(
              "%s already in %s. A new group does not remove %s from export files already written.",
              if (nrow(rv$updating) == 1) "This assembly is" else "Some of these are",
              paste(sprintf("\"%s\"", already), collapse = ", "),
              if (nrow(rv$updating) == 1) "it" else "them"
            )
          )
        },
        footer = mp_footer(
          primary = actionButton(ns("make_group"), "Create"),
          dismiss = "Cancel"
        )
      ) |> showModal()
    })

    # Make Group ----
    # Persist an export_group assignment for the current selection (rv$updating)
    # and refresh the table. `groups` is a character vector aligned to the rows
    # of rv$updating.
    assign_export_group <- function(groups) {
      rv$updating$export_group <- groups
      unit_key <- c("ID", "path", "scaffold")
      upd <- rv$updating[, c(unit_key, "export_group")]
      rv$data <- rv$data |>
        dplyr::rows_update(upd, by = unit_key)
      # Export state is keyed per unit, so selecting several scaffolds of one sample
      # yields distinct keys rather than duplicate IDs. Upsert, not update: a unit
      # that has never been grouped has no export row yet.
      dplyr::tbl(session$userData$con, "export") |>
        dplyr::rows_upsert(
          upd,
          in_place = TRUE,
          copy = TRUE,
          by = unit_key
        )
      trigger("update_export_table")
      removeModal()
      n <- nrow(upd)
      mp_toast(if (all(is.na(groups))) {
        sprintf("%s removed from %s export group.", mp_n(n, "assembly"), if (n == 1) "its" else "their")
      } else {
        sprintf("%s assigned to group \"%s\".", mp_n(n, "assembly"), groups[1])
      })
    }

    observeEvent(input$make_group, {
      name <- req(input$group_name)
      # Backstop: the selectize createFilter already refuses these keystrokes.
      if (any(!(grepl("^[A-Za-z0-9._-]+$", name)))) {
        mp_alert(
          title = "Invalid group name",
          text = "Use only letters, numbers, dots, hyphens, and underscores.",
          type = "error"
        )
        return()
      }
      # GenBank submissions may contain only complete OR only partial
      # mitogenomes, never both. Warn on a mixed selection and offer to split it
      # into "<name>-complete" and "<name>-partial".
      n_complete <- sum(rv$updating$completeness == "complete genome", na.rm = TRUE)
      n_partial  <- sum(rv$updating$completeness == "partial genome", na.rm = TRUE)
      if (n_complete > 0 && n_partial > 0) {
        rv$pending_group_name <- name
        modalDialog(
          title = mp_modal_title("Mixed complete and partial mitogenomes"),
          size = "m",
          easyClose = FALSE,
          HTML(stringr::str_glue(
            "A GenBank submission may contain only complete <i>or</i> only ",
            "partial mitogenomes, not both. This selection has {n_complete} ",
            "complete and {n_partial} partial. Split into two groups, ",
            "'{name}-complete' and '{name}-partial', or keep them as one group?"
          )),
          # Cancel returns to the group modal, so the typed name is not lost.
          footer = mp_footer(
            primary = actionButton(ns("group_split"), "Split into two groups"),
            dismiss = NULL,
            extra = tagList(
              actionButton(ns("group_back"), "Cancel"),
              actionButton(ns("group_keep_one"), "Keep as one mixed group")
            )
          )
        ) |> showModal()
        return()
      }
      assign_export_group(rep(name, nrow(rv$updating)))
    })

    # Mixed-group warning actions.
    # Split into separate complete / partial export groups.
    observeEvent(input$group_split, {
      name <- req(rv$pending_group_name)
      groups <- ifelse(
        rv$updating$completeness == "complete genome",
        paste0(name, "-complete"),
        paste0(name, "-partial")
      )
      assign_export_group(groups)
      rv$pending_group_name <- NULL
    })
    # Override: keep the mixed selection as a single group.
    observeEvent(input$group_keep_one, {
      name <- req(rv$pending_group_name)
      assign_export_group(rep(name, nrow(rv$updating)))
      rv$pending_group_name <- NULL
    })
    # Cancel: return to the group-name modal so the selection can be adjusted.
    observeEvent(input$group_back, {
      rv$pending_group_name <- NULL
      trigger("group_modal")
    })

    # Export data ----
    init("export")
    on("export", {
      req(nrow(rv$data) > 0)
      choices <- sort(unique(rv$data$export_group))
      if (length(choices) == 0) {
        mp_alert(
          title = "No export group is assigned",
          text = paste(
            "Export writes one group at a time. Select rows in the Export",
            "table, press Assign Group, then press Export Data."
          ),
          type = "info"
        )
        return()
      }
      # Saved templates + the currently selected one's header strings, plus the
      # columns available to reference
      con <- session$userData$con
      tmpl_choices <- list_export_templates(con)
      sel_tmpl <- if (rv$export_template %in% tmpl_choices) rv$export_template else "default"
      rv$export_template <- sel_tmpl
      opts <- get_export_opts(con, sel_tmpl)
      # One collapsed list of usable tokens, split by where the column came
      # from. Bookkeeping fields are not offered (T23).
      bookkeeping <- c("annotate_switch", "blast_accession_auto",
                       "poor_blast_ref", "export_time_stamp")
      sample_cols <- tryCatch(
        colnames(dplyr::tbl(con, "samples")),
        error = function(e) character(0)
      )
      avail <- setdiff(names(rv$data), bookkeeping)
      yours <- sort(intersect(avail, sample_cols))
      ours <- sort(setdiff(avail, yours))
      cols_help <- tags$details(
        tags$summary("Available columns"),
        opts_help(
          "Write a column name in braces to use its value, for example ",
          tags$code("{Taxon}"), ". ", tags$code("{seqid}"), " is the record ",
          "name MitoPilot gives this assembly: the sample ID, or ",
          tags$code("ID_p<path>_s<scaffold>"), " when one sample exports more ",
          "than one record. Columns from your mapping file work here even ",
          "when the table does not show them.",
          nested = TRUE
        ),
        p(tags$b("Your columns: "), paste(yours, collapse = ", ")),
        p(tags$b("MitoPilot columns: "), paste(ours, collapse = ", ")),
        opts_help(
          tags$code("{completeness}"),
          " expands to \"complete genome\" or \"partial genome\", derived from ",
          "each assembly's topology (circular = complete, linear = partial), ",
          "unless the per-sample Partial flag (forces partial) or the curation ",
          "\"linear complete\" setting (forces linear assemblies to complete) ",
          "overrides it. For GenBank, put ", tags$code("{completeness}"),
          " at the end of the header.",
          nested = TRUE
        )
      )
      # The status line describes the box above it, so bind the two (WCAG 3.3.1).
      hdr_box <- function(id, label, value) {
        htmltools::tagQuery(
          textAreaInput(ns(id), label, value, width = "100%")
        )$find("textarea")$addAttrs(
          `aria-describedby` = ns(paste0(id, "_status"))
        )$allTags()
      }
      modalDialog(
        title = mp_modal_title(
          tagList("Export data", uiOutput(ns("export_gears"), inline = TRUE))
        ),
        size = "l",
        class = "mp-modal-form",
        # Export group + header-template selector + Save, one row.
        div(
          style = "display: flex; flex-flow: row nowrap; gap: 1em;",
          div(
            style = "flex: 1; min-width: 0;",
            shinyWidgets::pickerInput(
              ns("export_group"),
              "Export group:",
              choices = choices,
              width = "100%"
            )
          ),
          div(
            style = "flex: 1; min-width: 0;",
            selectizeInput(
              ns("template_select"),
              "Header template:",
              choices = tmpl_choices,
              selected = sel_tmpl,
              width = "100%",
              options = list(
                create = TRUE,
                maxItems = 1,
                placeholder = "select or type a new template name"
              )
            )
          ),
          div(
            class = "mp-opts-checkbox",
            actionButton(ns("save_template"), "Save template",
                         title = "Store the header text below under this template name")
          )
        ),
        opts_help(
          "Export group bundles assemblies into one output set. Header ",
          "template is a reusable, named set of the FASTA header patterns ",
          "below: export uses the text on screen, and Save template keeps it ",
          "for next time."
        ),
        hdr_box("fasta_header", "Mitogenome FASTA header:", opts$fasta_header),
        uiOutput(ns("fasta_header_status")),
        cols_help,
        mp_checkbox(
          ns("include_alignments"),
          "Generate group-level PCG alignment summary",
          value = TRUE
        ),
        opts_help(
          "Writes one HTML page comparing the amino-acid alignment of every ",
          "protein-coding gene in the group. Needs more than one record."
        ),
        mp_checkbox(
          ns("export_genes"),
          "Export individual protein-coding and rRNA genes",
          value = FALSE
        ),
        opts_help(
          "Writes one FASTA and one feature table per gene, into a genes ",
          "folder beside the group files."
        ),
        # The gene header only matters when the genes are being written.
        conditionalPanel(
          condition = "input.export_genes == true",
          ns = ns,
          hdr_box("fasta_header_gene", "Gene FASTA header:",
                  opts$fasta_header_gene),
          uiOutput(ns("fasta_header_gene_status")),
          opts_help("The gene name is added to this header automatically.",
                    nested = TRUE)
        ),
        # PCG outlier review options, separated from the export options above
        tags$hr(style = "border-top: 1px solid var(--mp-border); margin: 1em 0 0.75em;"),
        h4("PCG annotation outlier review", style = "margin-top: 0;"),
        mp_checkbox(
          ns("review_outliers"),
          "Review PCG annotations for outliers",
          value = rv$opt_review
        ),
        conditionalPanel(
          condition = "input.review_outliers == true",
          ns = ns,
          div(
            style = "display: flex; flex-flow: row nowrap; gap: 1em;",
            div(
              style = "flex: 1",
              numericInput(
                ns("start_aa"),
                mp_help_label(
                  "Flag start offset > (aa):",
                  "Flag genes with start position offset by +/- this many amino acids from the core alignment"
                ),
                value = rv$opt_start, min = 1, step = 1, width = "100%"
              )
            ),
            div(
              style = "flex: 1",
              numericInput(
                ns("stop_aa"),
                mp_help_label(
                  "Flag stop offset > (aa):",
                  "Flag genes with stop position offset by +/- this many amino acids from the core alignment"
                ),
                value = rv$opt_stop, min = 1, step = 1, width = "100%"
              )
            ),
            div(
              style = "flex: 1",
              numericInput(
                ns("ident_pct"),
                mp_help_label(
                  "Flag sequence identity < (%):",
                  "Mean % identity threshold to flag a gene versus all other genes in alignment group"
                ),
                value = rv$opt_ident, min = 1, max = 100, step = 1, width = "100%"
              )
            )
          )
        ),
        # What pressing Export will do, in the group currently chosen.
        uiOutput(ns("export_summary")),
        footer = mp_footer(
          primary = actionButton(ns("export_data"), "Export"),
          dismiss = "Cancel"
        )
      ) |> showModal()
    })

    # Live header-template validation -----------------------------------------
    # Debounce so we validate after typing pauses, not on every keystroke.
    hdr_main <- shiny::debounce(reactive(input$fasta_header), 400)
    hdr_gene <- shiny::debounce(reactive(input$fasta_header_gene), 400)

    # Render a green/amber/red status line beneath a header textarea.
    # level: "ok" (green), "warn" (amber, non-blocking), "error" (red, blocks).
    render_hdr_status <- function(res) {
      style_for <- switch(
        res$level %||% if (isTRUE(res$ok)) "ok" else "error",
        ok    = list(cls = "mp-fg-success", ic = "circle-check"),
        warn  = list(cls = "mp-fg-warning", ic = "triangle-exclamation"),
        error = list(cls = "mp-fg-danger", ic = "circle-xmark")
      )
      span(
        class = style_for$cls,
        style = "font-size: var(--mp-fs-meta);",
        shiny::icon(style_for$ic), " ", res$message
      )
    }

    output$fasta_header_status <- renderUI({
      render_hdr_status(validate_fasta_header(hdr_main(), rv$data, require_completeness = TRUE))
    })
    output$fasta_header_gene_status <- renderUI({
      render_hdr_status(validate_fasta_header(hdr_gene(), rv$data))
    })

    # Gears turn only while an export is actually running (T22).
    output$export_gears <- renderUI({
      if (isTRUE(rv$exporting)) span(class = "gears")
    })

    # What Export will write, for the group currently chosen. Recomputed as the
    # group and the two output switches change.
    output$export_summary <- renderUI({
      group <- input$export_group
      req(group)
      n <- sum(rv$data$export_group == group, na.rm = TRUE)
      path <- file.path(session$userData$dir_out, "export", group)
      files <- c(
        paste0(group, ".fasta"), paste0(group, ".tbl"), "GFFs/",
        paste0(group, "_sample_info.csv")
      )
      if (isTRUE(input$export_genes)) files <- c(files, "genes/")
      if (isTRUE(input$include_alignments) && n > 1) {
        files <- c(files, paste0("AA_alignments_", group, ".html"))
      }
      div(
        style = paste(
          "font-size: var(--mp-fs-meta); padding: 8px 12px; margin-top: 12px;",
          "background: var(--mp-surface-alt);",
          "border-left: 3px solid var(--mp-primary);"
        ),
        div(sprintf("%s in group \"%s\".", mp_n(n, "record"), group)),
        div("Written to ", tags$code(class = "mp-path", path), " as: ",
            paste(files, collapse = ", ")),
        if (dir.exists(path)) {
          div(
            class = "mp-fg-warning",
            icon("triangle-exclamation"), " ",
            "This folder already exists. Export deletes it and writes it again."
          )
        }
      )
    })

    # Template selector ----------------------------------------------------
    # Like the analysis-opts parameter-set dropdowns: picking an existing name
    # loads its header strings; typing a new name creates a template from the
    # current boxes and adds it to the dropdown.
    observeEvent(input$template_select, {
      req(input$template_select)
      name <- input$template_select
      rv$export_template <- name
      con <- session$userData$con
      if (name %in% list_export_templates(con)) {
        o <- get_export_opts(con, name)
        updateTextAreaInput(session, "fasta_header", value = o$fasta_header)
        updateTextAreaInput(session, "fasta_header_gene", value = o$fasta_header_gene)
      }
    }, ignoreInit = TRUE)

    # Nothing is written until Save template is pressed: editing the boxes
    # while "default" is selected used to rewrite the project default (T18).
    # The gene box is hidden unless genes are exported, so it must not gate
    # anything the user cannot see.
    observe({
      shinyjs::toggleState(
        "save_template",
        condition = isTRUE(validate_fasta_header(hdr_main(), rv$data)$ok) &&
          (!isTRUE(input$export_genes) ||
             isTRUE(validate_fasta_header(hdr_gene(), rv$data)$ok))
      )
    })

    observeEvent(input$save_template, {
      name <- input$template_select
      if (is.null(name) || !nzchar(name)) {
        mp_alert(
          title = "Name the template first",
          text = "Pick a template name, or type a new one, then press Save template.",
          type = "info"
        )
        return()
      }
      if (!valid_headers_or_alert()) return()
      con <- session$userData$con
      set_export_opts(con, input$fasta_header, input$fasta_header_gene, name = name)
      updateSelectizeInput(
        session, "template_select",
        choices = list_export_templates(con), selected = name,
        options = list(create = TRUE, maxItems = 1)
      )
      rv$export_template <- name
      mp_toast(sprintf("Saved header template \"%s\".", name))
    })

    # Validate both header boxes; show an error alert and return FALSE if either
    # is invalid (so a bad template can never reach export). The gene header is
    # only checked when genes are being exported: its box is hidden otherwise.
    valid_headers_or_alert <- function() {
      v_main <- validate_fasta_header(input$fasta_header, rv$data)
      v_gene <- if (isTRUE(input$export_genes)) {
        validate_fasta_header(input$fasta_header_gene, rv$data)
      } else {
        list(ok = TRUE)
      }
      if (!isTRUE(v_main$ok) || !isTRUE(v_gene$ok)) {
        bad <- if (!isTRUE(v_main$ok)) v_main$message else v_gene$message
        which_t <- if (!isTRUE(v_main$ok)) "mitogenome" else "gene"
        mp_alert(
          title = "Invalid FASTA header template",
          text = stringr::str_glue("The {which_t} header template is invalid: {bad}"),
          type = "error"
        )
        return(FALSE)
      }
      TRUE
    }

    exp_val <- function(name) input[[name]]

    run_export <- function() {
      # The export options modal is still on screen at this point.
      on_screen <- TRUE
      group <- exp_val("export_group")
      # PCG review needs a multi-sample group; flag_PCG_outliers/export_files
      # only review when length(IDs) > 1.
      # Units, not samples: one sample can contribute several records, and the
      # outlier review needs >1 record to compare.
      n_units <- sum(rv$data$export_group == group, na.rm = TRUE)
      do_review <- isTRUE(exp_val("review_outliers")) && n_units > 1

      # Remember params so "Back to Review" can recompute against fresh edits and
      # the deferred write (after review) uses the same options. Stashed because
      # the export modal (and its inputs) is removed once the review modal opens.
      rv$review_group <- group
      rv$review_start <- exp_val("start_aa") %||% 10
      rv$review_stop <- exp_val("stop_aa") %||% 10
      rv$review_ident <- exp_val("ident_pct") %||% 60
      rv$export_params <- list(
        fasta_header = exp_val("fasta_header"),
        fasta_header_gene = exp_val("fasta_header_gene"),
        generateAAalignments = exp_val("include_alignments"),
        gene_export = exp_val("export_genes")
      )
      # Options are captured; the stash has done its job.
      # Where the files will land; surfaced via a popup once the user is done.
      rv$export_done_path <- file.path(
        session$userData$dir_out, "export", group
      )

      # Only touch the export modal's own elements while it is still on screen
      if (on_screen) {
        rv$exporting <- TRUE
        shinyjs::disable("export_data")
      }

      if (do_review) {
        # Review BEFORE writing files: edits made during review must land in the
        # DB first, otherwise the exported .fasta/.tbl/.gff would be stale.
        # Files are written on "Done" (see finalize_export).
        waiter::waiter_show(
          html = tagList(
            waiter::spin_fading_circles(),
            tags$h4(style = "color:white; margin-top:1em;", "Preparing PCG outlier review, hold tight...")
          ),
          color = "rgba(40,40,40,0.85)"
        )
        # Defer one tick so the overlay paints before the blocking review.
        shinyjs::delay(100, {
          review_res <- tryCatch(
            flag_PCG_outliers(
              group = group,
              db = file.path(session$userData$dir, ".sqlite"),
              start_aa = rv$review_start,
              stop_aa = rv$review_stop,
              ident_pct = rv$review_ident
            ),
            finally = waiter::waiter_hide()
          )
          if (on_screen) {
            rv$exporting <- FALSE
            shinyjs::enable("export_data")
          }
          # An internal stop means the record will fail NCBI validation, so warn
          # before the outlier review rather than after the files are written.
          if (nrow(review_res$internal_stops) > 0) {
            pending_review <<- review_res
            mp_confirm(
              ns("internal_stop_confirm"),
              title = "Internal stop codons",
              text = internal_stop_alert_text(review_res$internal_stops),
              action_label = "Continue to review",
              danger = TRUE,
              html = TRUE
            )
          } else {
            present_review(review_res)
          }
        })
      } else {
        # No review: write files immediately, then announce.
        write_export_files()
        if (on_screen) {
          rv$exporting <- FALSE
          shinyjs::enable("export_data")
        }
        show_export_done_alert()
      }
    }

    # Write the export files for the current group using the stashed options,
    # with review off (flagging already happened up front). Shown behind a waiter
    # overlay since this runs after the export modal is gone.
    write_export_files <- function() {
      p <- rv$export_params
      if (is.null(p)) return(invisible(NULL))
      # export_files() can stop() (e.g. a sample still has multiple assembly paths).
      # Catch it so the app shows a clean alert instead of crashing the session.
      ok <- tryCatch({
        export_files(
          group = rv$review_group,
          fasta_header = p$fasta_header,
          fasta_header_gene = p$fasta_header_gene,
          generateAAalignments = p$generateAAalignments,
          out_dir = session$userData$dir_out,
          gene_export = p$gene_export,
          review = FALSE,
          start_aa = rv$review_start,
          stop_aa = rv$review_stop,
          ident_pct = rv$review_ident
        )
        TRUE
      }, error = function(e) {
        waiter::waiter_hide()
        mp_alert(
          title = "Export failed",
          text = conditionMessage(e),
          type = "error"
        )
        FALSE
      })
      if (!isTRUE(ok)) return(invisible(NULL))
      # Refresh the table so the newly-written export_time_stamp shows up.
      trigger("refresh_export")
    }

    # Finish a reviewed export: write files (now that all edits are committed to
    # the DB), then show the export-complete popup. Used both when the user clicks
    # "Done" and when review finds nothing to flag.
    finalize_export <- function(extra = NULL) {
      waiter::waiter_show(
        html = tagList(
          waiter::spin_fading_circles(),
          tags$h4(style = "color:white; margin-top:1em;", "Writing export files, hold tight...")
        ),
        color = "rgba(40,40,40,0.85)"
      )
      on.exit(waiter::waiter_hide())
      write_export_files()
      show_export_done_alert(extra = extra)
    }

    # Popup announcing where files were written. `extra` adds a second line
    # (e.g. the "no outliers flagged" note when review found nothing).
    show_export_done_alert <- function(extra = NULL) {
      path <- rv$export_done_path
      if (is.null(path)) return(invisible(NULL))
      # JS-safe single-quoted string for the clipboard onclick
      path_js <- gsub("'", "\\\\'", gsub("\\\\", "\\\\\\\\", path))
      mp_alert(
        title = "Export complete",
        html = TRUE,
        type = "success",
        text = tagList(
          "Data exported to:",
          tags$div(
            style = paste(
              "display: flex; flex-direction: column; gap: 0.4em;",
              "margin-top: 0.5em;"
            ),
            tags$div(
              style = paste(
                "min-width: 0; background: #000; color: #fff;",
                "font-family: monospace; font-size: var(--mp-fs-meta); padding: 0.5em 0.6em; border-radius: 4px;",
                "white-space: normal; word-break: break-all; text-align: center;"
              ),
              path
            ),
            tags$div(
              style = "display: flex; flex-direction: row; gap: 0.4em; justify-content: center;",
              tags$button(
                type = "button",
                class = "btn btn-default",
                title = "Copy path",
                onclick = sprintf(
                  paste0(
                    "navigator.clipboard.writeText('%s');",
                    "var t=this.querySelector('span');",
                    "if(t){var o=t.innerText;t.innerText='Copied!';",
                    "setTimeout(function(){t.innerText=o;},1200);}"
                  ),
                  path_js
                ),
                shiny::icon("copy"),
                tags$span(
                  # Fixed width sized for "Copied!" so the button does not resize
                  # when the label changes on click.
                  style = "margin-left: 0.3em; display: inline-block; min-width: 4.5em; text-align: center;",
                  "Copy"
                )
              ),
              # Open the export folder in an environment-aware way (see open_path):
              # OS file browser locally, RStudio Files pane on Server (with a
              # notification), or a warning path on headless sessions.
              tags$button(
                type = "button",
                class = "btn btn-default",
                title = "Open export folder",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', Math.random(), {priority: 'event'});",
                  ns("open_export_dir")
                ),
                shiny::icon("folder-open"),
                tags$span(
                  style = "margin-left: 0.3em; display: inline-block; min-width: 4.5em; text-align: center;",
                  "Open"
                )
              )
            )
          ),
          if (!is.null(extra)) tags$p(style = "margin-top: 0.75em;", extra)
        )
      )
    }

    # "Open" button in the export-complete popup: open the export folder in an
    # environment-aware way (open_path handles desktop / RStudio Server / headless
    # and notifies the user accordingly).
    observeEvent(input$open_export_dir, ignoreInit = TRUE, {
      open_path(rv$export_done_path)
    })

    # Aligned AAStringSets for the review modal, keyed by gene, held in a plain
    # environment rather than a reactiveValues field. These are large objects that
    # need no dependency tracking of their own: every write is immediately followed
    # by trigger("outlier_modal"), and aln_nonce() is what invalidates the panel.
    # Keeping them out of reactiveValues also removes the copy-on-write hop that
    # was leaving the panel one edit behind the recompute.
    aln_store <- new.env(parent = emptyenv())
    aln_put <- function(gene, aln) {
      if (is.null(gene)) return(invisible())
      if (is.null(aln)) {
        if (exists(gene, aln_store, inherits = FALSE)) rm(list = gene, envir = aln_store)
      } else {
        assign(gene, aln, envir = aln_store)
      }
    }
    aln_get <- function(gene) {
      if (is.null(gene) || !exists(gene, aln_store, inherits = FALSE)) return(NULL)
      get(gene, envir = aln_store)
    }
    aln_reset <- function(alns = list()) {
      rm(list = ls(aln_store, all.names = TRUE), envir = aln_store)
      for (g in names(alns)) aln_put(g, alns[[g]])
    }

    # Load a review result into rv and open the modal (or report none found).
    # focus_gene: optional gene name to navigate to (e.g. the gene just reviewed
    # via "Back to Review"); falls back to the first gene when absent or no longer
    # flagged.
    # Review state held while the internal-stop warning is on screen. A plain
    # variable, not rv$: writing it from its own observer would retrigger it.
    pending_review <- NULL
    observeEvent(input$internal_stop_confirm, ignoreNULL = TRUE, {
      res <- pending_review
      pending_review <<- NULL
      req(!is.null(res))
      if (isTRUE(input$internal_stop_confirm)) {
        present_review(res)
      } else {
        # Cancelled: nothing has been written yet, so just drop the export.
        removeModal()
      }
    })

    present_review <- function(res, focus_gene = NULL) {
      rv$outliers <- res$flags
      rv$review_samples <- res$samples
      aln_reset(res$alignments)
      flagged_genes <- unique(res$flags$gene)
      if (length(flagged_genes) > 0) {
        rv$review_genes <- flagged_genes
        rv$review_idx <- if (!is.null(focus_gene) && focus_gene %in% flagged_genes) {
          which(flagged_genes == focus_gene)[1]
        } else {
          1L
        }
        removeModal()
        trigger("outlier_modal")
      } else {
        # Nothing to review: write the files now, then announce.
        finalize_export(extra = "No outlier PCG annotations were flagged.")
      }
    }

    # Merge a scoped recompute into the cached review state. Only the genes that
    # actually changed are re-aligned, so replace just their flags/alignments and
    # keep review_genes (and the user's position) stable, then navigate to
    # focus_gene and reopen.
    merge_review <- function(res, genes, focus_gene = NULL) {
      rv$outliers <- dplyr::bind_rows(
        rv$outliers[!rv$outliers$gene %in% genes, , drop = FALSE],
        res$flags
      )
      # res$alignments/res$samples keep a recomputed gene even when the edit
      # cleared its last flag (flag_PCG_outliers retains explicitly-requested
      # genes), so the corrected alignment replaces the stale one instead of the
      # panel freezing on the pre-edit view.
      samps <- rv$review_samples
      for (g in genes) {
        aln_put(g, res$alignments[[g]])
        samps[[g]] <- res$samples[[g]]
      }
      rv$review_samples <- samps
      # review_genes intentionally unchanged: genes stay in the list and are
      # marked resolved rather than removed.
      if (!is.null(focus_gene) && focus_gene %in% rv$review_genes) {
        rv$review_idx <- which(rv$review_genes == focus_gene)[1]
      }
      removeModal()
      trigger("outlier_modal")
    }

    # Remember which unit/gene the user is off to edit and snapshot that unit's
    # current PCG signature, so the return trip can tell exactly what changed.
    set_review_focus <- function(ID, path, scaffold, gene) {
      rv$review_focus <- list(ID = ID, path = path, scaffold = scaffold, gene = gene)
      rv$review_focus_sig <- tryCatch(
        unit_pcg_sig(session$userData$con, ID, path, scaffold),
        error = function(e) NULL
      )
    }

    # Returning from the annotate details modal: re-align whatever the user
    # actually changed so resolved flags drop off, then reopen.
    #
    # What changed is decided HERE, by diffing the focal unit's PCG annotations in
    # the db against the snapshot taken when we jumped out. The db is the same
    # source flag_PCG_outliers reads, so the verdict cannot disagree with the
    # alignment. (An earlier version trusted a flag the annotate module set from
    # its in-memory copy; that flag could survive a rejected close or be wiped by
    # an unrelated modal reload, and a wrong "unchanged" silently showed the
    # pre-edit alignment with no recompute.)
    on("reopen_outlier_review", {
      req(rv$review_group)
      # The (unit, gene) just reviewed: mark it resolved (survives the recompute
      # below) and remember the gene so we can navigate back to it.
      focus <- rv$review_focus
      sig_before <- rv$review_focus_sig
      rv$review_focus <- NULL
      rv$review_focus_sig <- NULL
      focal <- if (!is.null(focus)) focus$gene else NULL
      if (!is.null(focus)) {
        rv$resolved <- union(rv$resolved, paste(focus$ID, focus$gene, sep = "|"))
      }
      have_cache <- !is.null(rv$outliers) && length(rv$review_genes) > 0
      # NULL = we could not read one of the two states, so treat everything as
      # suspect and reload the whole group.
      changed <- if (is.null(focus)) {
        NULL
      } else {
        sig_now <- tryCatch(
          unit_pcg_sig(session$userData$con, focus$ID, focus$path, focus$scaffold),
          error = function(e) NULL
        )
        sig_diff(sig_before, sig_now)
      }
      # Nothing in the db moved: the cached flags/alignments are still valid, so
      # reopen at the focal gene and skip the (expensive) alignment recompute.
      if (!is.null(changed) && length(changed) == 0 && have_cache) {
        if (!is.null(focal) && focal %in% rv$review_genes) {
          rv$review_idx <- which(rv$review_genes == focal)[1]
        }
        removeModal()
        trigger("outlier_modal")
        return()
      }
      # Scope the recompute to the changed genes only when we know them AND have
      # cached state to merge into; otherwise re-align the whole group.
      scope <- if (!is.null(changed) && length(changed) > 0 && have_cache) changed else NULL
      # Show the overlay first, then defer the (blocking) recompute one tick so
      # the "hold tight" message actually paints before alignment starts.
      waiter::waiter_show(
        html = tagList(
          waiter::spin_fading_circles(),
          tags$h4(style = "color:white; margin-top:1em;", "Recomputing alignments, hold tight...")
        ),
        color = "rgba(40,40,40,0.85)"
      )
      shinyjs::delay(100, {
        res <- tryCatch(
          flag_PCG_outliers(
            group = rv$review_group,
            db = file.path(session$userData$dir, ".sqlite"),
            start_aa = rv$review_start %||% 10,
            stop_aa = rv$review_stop %||% 10,
            ident_pct = rv$review_ident %||% 60,
            genes = scope
          ),
          finally = waiter::waiter_hide()
        )
        if (!is.null(scope)) {
          merge_review(res, scope, focus_gene = focal)
        } else {
          present_review(res, focus_gene = focal)
        }
      })
    })

    # Remember review-option edits so the modal reopens with them
    observeEvent(input$review_outliers, rv$opt_review <- isTRUE(input$review_outliers))
    observeEvent(input$start_aa, {
      if (!is.null(input$start_aa) && !is.na(input$start_aa)) rv$opt_start <- input$start_aa
    })
    observeEvent(input$stop_aa, {
      if (!is.null(input$stop_aa) && !is.na(input$stop_aa)) rv$opt_stop <- input$stop_aa
    })
    observeEvent(input$ident_pct, {
      if (!is.null(input$ident_pct) && !is.na(input$ident_pct)) rv$opt_ident <- input$ident_pct
    })

    # Samples in this group whose scaffolds would export as several records, i.e.
    # ONE path split across multiple scaffolds. A single fragmented genome exported
    # this way becomes several incomplete records, so we warn. Multi-PATH samples
    # are a different, blocked case (multi_path_samples()) and are excluded here.
    fragmented_samples <- function(group) {
      d <- rv$data[!is.na(rv$data$export_group) & rv$data$export_group == group, ]
      if (nrow(d) == 0) return(character(0))
      by_id <- split(d, d$ID)
      names(by_id)[vapply(
        by_id,
        function(x) nrow(x) > 1 && length(unique(x$path)) == 1,
        logical(1)
      )]
    }

    # Samples in this group contributing units from more than one assembly path.
    # These cannot be exported (export_files() would reject them, since exporting
    # each path submits duplicate records for one specimen), so they are blocked
    # before export rather than warned about.
    multi_path_samples <- function(group) {
      d <- rv$data[!is.na(rv$data$export_group) & rv$data$export_group == group, ]
      if (nrow(d) == 0) return(character(0))
      by_id <- split(d$path, d$ID)
      names(by_id)[vapply(by_id, function(p) length(unique(p)) > 1, logical(1))]
    }


    check_overwrite_then_export <- function() {
      group <- exp_val("export_group")
      export_path <- file.path(session$userData$dir_out, "export", group)
      if (dir.exists(export_path)) {
        mp_confirm(
          ns("overwrite_confirm"),
          title = "Export already exists",
          text = stringr::str_glue(
            "Export files for group '{group}' are already on disk. Exporting ",
            "deletes that folder and writes it again."
          ),
          action_label = "Overwrite",
          danger = TRUE
        )
        return()
      }
      run_export()
    }

    observeEvent(input$export_data, ignoreInit = T, {
      req(input$export_group)
      # Block export if either header template is invalid (would crash str_glue_data)
      if (!valid_headers_or_alert()) return()
      # Block (not warn) any sample with more than one assembly path: export_files()
      # rejects these, so stop them here with a clear message instead of letting the
      # error surface mid-write. Should be rare, since locking a multi-path sample
      # for annotation is blocked upstream.
      mp <- multi_path_samples(input$export_group)
      if (length(mp) > 0) {
        shown <- paste(utils::head(mp, 8), collapse = ", ")
        if (length(mp) > 8) shown <- paste0(shown, ", and ", length(mp) - 8, " more")
        mp_alert(
          title = "Cannot export samples with multiple assembly paths",
          text = stringr::str_glue(
            "{mp_n(length(mp), 'sample')} still have more than one assembly path: {shown}.\n\n",
            "Assembly paths are alternative resolutions of the same genome, so ",
            "exporting each would submit duplicate records for one specimen. In the ",
            "Assemble module, open the assembly details and 'ignore' all but the ",
            "correct path (or build a consensus Path 0), then export again."
          ),
          type = "error"
        )
        return()
      }
      frag <- fragmented_samples(input$export_group)
      if (length(frag) > 0) {
        shown <- paste(utils::head(frag, 5), collapse = ", ")
        if (length(frag) > 5) shown <- paste0(shown, ", and ", length(frag) - 5, " more")
        mp_confirm(
          ns("fragmented_confirm"),
          title = "Some samples export as multiple records",
          text = stringr::str_glue(
            "{mp_n(length(frag), 'sample')} have more than one assembly and will each ",
            "produce a SEPARATE GenBank record: {shown}.\n\n",
            "That is correct when the scaffolds really are different genomes. If a ",
            "sample is instead ONE genome broken into fragments, each record will ",
            "be submitted as an incomplete genome. Cancel and use consensus ",
            "trimming / scaffold joining to combine them, or 'ignore' all but one ",
            "scaffold."
          ),
          action_label = "Export anyway",
          danger = TRUE
        )
        return()
      }
      check_overwrite_then_export()
    })

    observeEvent(input$fragmented_confirm, ignoreInit = T, {
      req(input$fragmented_confirm)
      check_overwrite_then_export()
    })

    observeEvent(input$overwrite_confirm, ignoreInit = T, {
      req(input$overwrite_confirm)
      run_export()
    })

    # Outlier review ----
    # Gene currently under review, and that gene's flagged samples
    current_gene <- reactive({
      req(rv$review_genes, rv$review_idx)
      req(rv$review_idx <= length(rv$review_genes))
      rv$review_genes[[rv$review_idx]]
    })
    current_flags <- reactive({
      g <- current_gene()
      rv$outliers[rv$outliers$gene == g, , drop = FALSE]
    })
    # Every unit in the current gene's alignment (flagged or not), for the
    # "edit any sample" picker.
    current_samples <- reactive({
      g <- current_gene()
      rv$review_samples[[g]]
    })
    # Sample (by label) to highlight in the MSA; cleared when the gene changes
    highlight_label <- reactiveVal(NULL)

    # Bumped on every (re)open of the review modal so the MSA widget gets a fresh
    # output id and is rebuilt from scratch for the gene currently shown (the focal
    # gene on "Back to Review"). Prev/Next navigate within an already-open modal and
    # reuse the same id (standard reactive update), so only the shown gene rebuilds.
    # This fixes the stale MSA when returning to the same focal gene after an edit.
    aln_nonce <- reactiveVal(0L)

    init("outlier_modal")
    on("outlier_modal", {
      req(length(rv$review_genes) > 0)
      highlight_label(NULL)
      # Bump so review_aln_ui rebuilds the widget from scratch on every (re)open.
      aln_nonce(isolate(aln_nonce()) + 1L)
      modalDialog(
        # No close X: leaving the review is an explicit decision, and both
        # exits below clean up the review state.
        title = mp_modal_title("PCG annotation outlier review", close = FALSE),
        size = "l",
        # One pinned bar: the position the arrows move, and the action on the
        # gene shown. Stays in view while the alignment scrolls.
        div(
          class = "mp-review-bar",
          actionButton(ns("review_prev"), "Prev", icon = icon("chevron-left"),
                       class = "btn-default"),
          div(class = "mp-review-gene", textOutput(ns("review_header"), inline = TRUE)),
          actionButton(ns("review_next"), "Next", icon = icon("chevron-right"),
                       class = "btn-default"),
          div(
            class = "mp-review-resolve",
            actionButton(ns("skip_gene"), "Mark gene resolved", class = "btn-default",
                         title = "Mark every flag for this gene as resolved and move on")
          )
        ),
        opts_help(
          "Review the alignment below to decide whether the flagged samples ",
          "need to be revised. Click 'edit' to jump to the annotation editor ",
          "for a sample, or mark the gene resolved if the flags look benign."
        ),
        uiOutput(ns("review_aln_ui")),
        tags$hr(),
        reactableOutput(ns("review_table")),
        # Edit any sample of this gene, flagged or not (only this gene stays
        # editable in the details modal, same as clicking a flagged sample's 'edit').
        uiOutput(ns("review_sample_picker")),
        footer = mp_footer(
          primary = actionButton(ns("review_done"), "Continue export"),
          dismiss = NULL,
          extra = actionButton(ns("cancel_review"), "Stop without exporting")
        )
      ) |> showModal()
    })

    output$review_header <- renderText({
      req(rv$review_genes)
      sprintf(
        "Gene %d of %d: %s",
        rv$review_idx, length(rv$review_genes), toupper(current_gene())
      )
    })

    # Size the alignment viewport to the number of sequences so small groups
    # don't leave a large blank gap below the rows.
    review_aln_height <- reactive({
      # aln_nonce() so the height tracks the store, which is not reactive itself.
      aln_nonce()
      g <- current_gene()
      aln <- aln_get(g)
      if (is.null(aln)) return(120L)
      min(400L, max(120L, as.integer(length(aln) * 18 + 40)))
    })

    # Render the MSA as the uiOutput content itself (not a msaROutput placeholder
    # filled by a separate renderMsaR). Returning the widget from renderUI makes
    # Shiny REPLACE the container's innerHTML on every (re)open, so the old widget
    # DOM is torn down and a fresh one built from the current alignment store - the
    # msaROutput+dynamic-renderMsaR pattern instead let htmlwidgets reuse a stale
    # binding (and msaR::renderValue appends rather than clearing), which showed the
    # pre-edit alignment after "Back to Review". aln_nonce() forces a rebuild even
    # if the gene is unchanged.
    output$review_aln_ui <- renderUI({
      aln_nonce()
      g <- current_gene()
      aln <- aln_get(g)
      if (is.null(aln)) {
        return(div(style = "color:#666; padding:1em;", "No alignment for this gene."))
      }
      # Move the picked sample to the top and mark it so it stands out
      hl <- highlight_label()
      if (!is.null(hl) && hl %in% names(aln)) {
        aln <- aln[c(which(names(aln) == hl), which(names(aln) != hl))]
        names(aln)[1] <- paste0(">> ", names(aln)[1])
      }
      msaR::msaR(
        aln,
        overviewbox = FALSE,
        seqlogo = FALSE,
        menu = FALSE,
        conservation = TRUE,
        labelNameLength = 150,
        colorscheme = "zappo",
        alignmentHeight = review_aln_height(),
        # The widget box otherwise keeps htmlwidgets' default height and leaves
        # a blank band under a short alignment.
        height = paste0(review_aln_height() + 60L, "px")
      )
    })

    output$review_table <- renderReactable({
      df <- current_flags()
      # Render even with 0 rows (reactable shows an empty table) rather than
      # req(nrow>0)-stopping: after an edit clears a gene's last flag the table
      # must refresh to empty, not freeze on the stale pre-edit rows.
      req(!is.null(df))
      keys <- paste(df$ID, df$gene, sep = "|")
      df <- df |>
        dplyr::transmute(
          Sample = label,
          Issue = issue,
          `Start offset (aa)` = start_offset,
          `Stop offset (aa)` = stop_offset,
          `Identity (%)` = pct_identity,
          `Internal stops` = internal_stops,
          resolved = keys %in% rv$resolved,
          edit = "edit"
        )
      # Show signed offsets with an explicit "+" for positive values
      signed_cell <- htmlwidgets::JS(
        "function(ci){var v=ci.value; if(v===null||v===undefined) return ''; return v>0?('+'+v):(''+v);}"
      )
      # Dim + strike-through rows the user has marked resolved
      resolved_row_style <- htmlwidgets::JS(
        "function(rowInfo){ if(rowInfo && rowInfo.values && rowInfo.values['resolved']) return {opacity:0.5, textDecoration:'line-through'}; }"
      )
      reactable::reactable(
        df,
        sortable = TRUE,
        highlight = TRUE,
        rowStyle = resolved_row_style,
        # Headers carry a help icon, so keep every one on a single line and give
        # the wider labels room; Issue takes what is left.
        defaultColDef = reactable::colDef(
          html = TRUE,
          headerStyle = list(whiteSpace = "nowrap")
        ),
        columns = list(
          Sample = reactable::colDef(
            minWidth = 130,
            cell = rt_link(ns("review_pick"),
                           title = "Highlight this sample in the alignment")
          ),
          Issue = reactable::colDef(minWidth = 120),
          `Start offset (aa)` = reactable::colDef(
            minWidth = 150,
            align = "center",
            cell = signed_cell,
            header = mp_help_label(
              "Start offset (aa)",
              "Number of amino acids this sample's start extends past (+) or falls short of (-) the core alignment."
            )
          ),
          `Stop offset (aa)` = reactable::colDef(
            minWidth = 150,
            align = "center",
            cell = signed_cell,
            header = mp_help_label(
              "Stop offset (aa)",
              "Number of amino acids this sample's stop extends past (+) or falls short of (-) the core alignment."
            )
          ),
          `Identity (%)` = reactable::colDef(
            minWidth = 125,
            align = "center",
            header = mp_help_label(
              "Identity (%)",
              "Mean percent identity of this sample versus rest of samples in alignment group."
            )
          ),
          `Internal stops` = reactable::colDef(
            minWidth = 135,
            align = "center",
            header = mp_help_label(
              "Internal stops",
              "Number of stop codons inside this sample's translation. Any at all will fail NCBI validation."
            )
          ),
          resolved = reactable::colDef(
            name = "Resolved",
            width = 100,
            align = "center",
            sortable = FALSE,
            filterable = FALSE,
            cell = rt_bool_bttn(
              ns("toggle_resolved"),
              "fas fa-circle-check",
              "far fa-circle",
              title_true = "Resolved - click to reopen",
              title_false = "Unresolved - click to mark resolved"
            )
          ),
          edit = reactable::colDef(
            name = "Edit",
            width = 90,
            align = "center",
            sortable = FALSE,
            filterable = FALSE,
            cell = rt_icon_bttn_text(
              ns("goto_annot"), "fas fa-pen-to-square fa-xs",
              label = "Edit",
              title = "Open the annotation editor for this sample"
            )
          )
        )
      )
    })

    # Picker to edit ANY sample of the current gene, flagged or not. The table
    # above only lists flagged samples; this offers the rest of the alignment.
    output$review_sample_picker <- renderUI({
      samp <- current_samples()
      req(!is.null(samp), nrow(samp) > 0)
      div(
        style = "margin-top: 0.75em; display: flex; align-items: flex-end; gap: 0.5em;",
        div(
          style = "flex: 1;",
          selectInput(
            ns("edit_sample_label"),
            label = sprintf("Edit any %s sample", toupper(current_gene())),
            choices = sort(samp$label),
            width = "100%"
          )
        ),
        actionButton(
          ns("edit_sample"), "Edit",
          class = "btn-default", style = "margin-bottom: 15px;",
          title = "Open the annotation editor for the chosen sample"
        )
      )
    })

    # Toggle a (sample, gene) as resolved; kept in rv$resolved so it survives
    # alignment/flag recomputes (Back to Review).
    observeEvent(input$toggle_resolved, {
      fr <- current_flags()[as.integer(input$toggle_resolved), ]
      req(nrow(fr) == 1)
      key <- paste(fr$ID, fr$gene, sep = "|")
      rv$resolved <- if (key %in% rv$resolved) setdiff(rv$resolved, key) else c(rv$resolved, key)
    })

    # Click a sample name -> highlight it (moved to top, marked) in the MSA
    observeEvent(input$review_pick, {
      idx <- as.integer(input$review_pick)
      lbl <- current_flags()$label[idx]
      req(length(lbl) == 1, !is.na(lbl))
      highlight_label(lbl)
    })

    observeEvent(input$review_prev, {
      highlight_label(NULL)
      rv$review_idx <- max(1L, rv$review_idx - 1L)
    })
    observeEvent(input$review_next, {
      highlight_label(NULL)
      rv$review_idx <- min(length(rv$review_genes), rv$review_idx + 1L)
    })
    # Finish review: close the modal, write the files (now that all edits are in
    # the DB), and surface the export-complete popup.
    observeEvent(input$review_done, {
      removeModal()
      finalize_export()
    })
    # Mark gene completed: flag every sample for this gene as resolved (kept in
    # rv$resolved so the rows survive recompute and show struck-through) rather than
    # removing the gene from the review list. Advance to the next gene if any.
    observeEvent(input$skip_gene, {
      highlight_label(NULL)
      fr <- current_flags()
      req(nrow(fr) > 0)
      keys <- paste(fr$ID, fr$gene, sep = "|")
      rv$resolved <- union(rv$resolved, keys)
      if (rv$review_idx < length(rv$review_genes)) {
        rv$review_idx <- rv$review_idx + 1L
      }
    })

    # Jump to the annotate details modal for the chosen flagged sample
    observeEvent(input$goto_annot, {
      fr <- current_flags()[as.integer(input$goto_annot), ]
      req(nrow(fr) == 1)
      # Carry the unit: the flag belongs to one scaffold, and without path/scaffold
      # the Annotate side falls back to the sample's first unit.
      session$userData$goto_annotate_target <- list(
        ID = fr$ID, path = fr$path, scaffold = fr$scaffold,
        gene = fr$gene, issue = fr$issue,
        start_offset = fr$start_offset, stop_offset = fr$stop_offset,
        pct_identity = fr$pct_identity
      )
      set_review_focus(fr$ID, fr$path, fr$scaffold, fr$gene)
      removeModal()
      trigger("goto_annotate")
    })

    # Jump to the annotate details modal for any chosen sample of the current
    # gene, flagged or not. Mirrors goto_annot; the unit comes from the sample
    # roster and there are no outlier offsets (NA -> the editor shows a plain
    # "editing <gene>" banner instead of offset values).
    observeEvent(input$edit_sample, {
      samp <- current_samples()
      req(!is.null(samp))
      row <- samp[samp$label == input$edit_sample_label, , drop = FALSE]
      req(nrow(row) == 1)
      session$userData$goto_annotate_target <- list(
        ID = row$ID, path = row$path, scaffold = row$scaffold,
        gene = current_gene(), issue = NA_character_,
        start_offset = NA_integer_, stop_offset = NA_integer_,
        pct_identity = NA_real_
      )
      set_review_focus(row$ID, row$path, row$scaffold, current_gene())
      removeModal()
      trigger("goto_annotate")
    })

    # Abort the export: close the review modal and clear review state so no files
    # are written and the modal does not reopen.
    observeEvent(input$cancel_review, {
      removeModal()
      highlight_label(NULL)
      rv$outliers <- NULL
      aln_reset()
      rv$review_samples <- NULL
      rv$review_genes <- NULL
      rv$review_idx <- 1L
      rv$resolved <- character(0)
      rv$review_focus <- NULL
      rv$review_focus_sig <- NULL
      mp_toast("Export stopped. Nothing was written.")
    })
  })
}
