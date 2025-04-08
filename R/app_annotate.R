#' annotate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
annotate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tagList(
      reactable::reactableOutput(ns("table"))
    )
  )
}

#' annotate Server Functions
#'
#' @noRd
annotate_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Prepare data ----
    rv <- reactiveValues(
      curate_opts = dplyr::tbl(session$userData$con, "curate_opts") |>
        dplyr::collect(),
      annotate_opts = dplyr::tbl(session$userData$con, "annotate_opts") |>
        dplyr::collect(),
      data = fetch_annotate_data(),
      updating = NULL
    )


    # Refresh ----
    init("refresh_annotate")
    on("refresh_annotate", {
      rv$data <- fetch_annotate_data()
      updateReactable(
        "table",
        data = rv$data
      )
      message(str(rv$data))
    })

    # Render table ----
    output$table <- renderReactable({
      isolate(req(rv$data)) |>
        reactable(
          compact = TRUE,
          language = reactable::reactableLang(
            noData = "No Completed / Locked Assemblies Found"
          ),
          defaultPageSize = 100,
          resizable = TRUE,
          showPageSizeOptions = TRUE,
          onClick = "select",
          selection = "multiple",
          searchable = TRUE,
          filterable = TRUE,
          defaultSorted = list(time_stamp = "desc"),
          height = 650,
          wrap = FALSE,
          pageSizeOptions = c(25, 50, 100, 200, 500),
          rowStyle = rt_highlight_row(),
          defaultColDef = colDef(align = "left", show = FALSE),
          columns = list(
            `.selection` = colDef(show = T, sticky = "left", width = 28),
            annotate_lock = colDef(
              show = TRUE,
              sticky = "left",
              name = "",
              html = TRUE,
              filterable = FALSE,
              width = 32,
              align = "center",
              cell = rt_dynamicIcon(
                c(
                  `0` = "fa fa-lock-open",
                  `1` = "fa fa-lock"
                )
              )
            ),
            annotate_switch = colDef(
              show = TRUE,
              sticky = "left",
              name = "",
              html = TRUE,
              filterable = FALSE,
              width = 30,
              align = "center",
              cell = rt_dynamicIcon(
                c(
                  `0` = "fa fa-hourglass",
                  `1` = "fa fa-person-running",
                  `2` = "fa fa-circle-check",
                  `3` = "fa fa-triangle-exclamation"
                )
              )
            ),
            ID = colDef(
              show = TRUE,
              minWidth = 120,
              sticky = "left",
              html = TRUE,
              cell = rt_longtext()
            ),
            Taxon = colDef(
              show = TRUE,
              minWidth = 140,
              sticky = "left",
              html = TRUE,
              cell = rt_longtext()
            ),
            ID_verified = colDef(
              show = TRUE,
              name = "ID_verified",
              html = TRUE,
              align = "center",
              width = 100,
            ),
            annotate_opts = colDef(
              show = TRUE,
              name = "Annotate Opts.",
              html = TRUE,
              width = 130,
              cell = rt_link(ns("set_annotate_opts"))
            ),
            curate_opts = colDef(
              show = TRUE,
              name = "Curate Opts.",
              html = TRUE,
              width = 110,
              cell = rt_link(ns("set_curate_opts"))
            ),
            length = colDef(
              show = TRUE,
              name = "Length",
              filterable = FALSE,
              html = TRUE,
              cell = rt_longtext()
            ),
            topology = colDef(show = TRUE, align = "center"),
            scaffolds = colDef(show = TRUE, align = "center"),
            PCGCount = colDef(show = TRUE, name = "# PCGs", align = "center"),
            tRNACount = colDef(show = TRUE, name = "# tRNAs", align = "center"),
            rRNACount = colDef(show = TRUE, name = "# rRNAs", align = "center"),
            missing = colDef(show = TRUE, align = "center", html = TRUE, cell = rt_longtext()),
            extra = colDef(show = TRUE, align = "center"),
            warnings = colDef(show = TRUE, align = "center"),
            reviewed = colDef(
              show = TRUE,
              name = "Reviewed",
              html = TRUE,
              align = "center",
              width = 100,
            ),
            problematic = colDef(
              show = TRUE,
              name = "Problematic",
              html = TRUE,
              align = "center",
              width = 100,
            ),
            time_stamp = colDef(
              show = TRUE,
              name = "Last Updated",
              filterable = FALSE,
              html = T,
              width = 150,
              cell = rt_ts_date()
            ),
            annotate_notes = colDef(
              show = TRUE,
              name = "Notes",
              html = TRUE,
              align = "left",
              minWidth = 150,
              #maxWidth = 400,
              cell = rt_longtext()
            ),
            view = colDef(
              show = TRUE,
              sticky = "right",
              filterable = FALSE,
              name = "",
              html = TRUE,
              width = 80,
              align = "center",
              cell = rt_icon_bttn_text(ns("details"), "fas fa-square-arrow-up-right fa-xs")
            ),
            output = colDef(
              show = TRUE,
              sticky = "right",
              filterable = FALSE,
              name = "",
              html = TRUE,
              width = 80,
              align = "center",
              cell = rt_icon_bttn_text(ns("output"), "fas fa-folder-open fa-xs")
            )
          )
        )
    })

    # update table ----
    init("update_annotate_table")
    on("update_annotate_table", {
      reactable::updateReactable(
        "table",
        data = rv$data |>
          dplyr::mutate(
            output = dplyr::case_when(
              annotate_switch > 1 ~ "output",
              .default = NA_character_
            ),
            view = dplyr::case_when(
              annotate_switch > 1 ~ "details",
              .default = NA_character_
            )
          ),
        selected = reactable::getReactableState("table", "selected"),
        page = reactable::getReactableState("table", "page")
      )
    })

    # table selection ----
    selected <- reactive(reactable::getReactableState("table", "selected"))

    # Set State ----
    init("state")
    on("state", {
      req(session$userData$mode == "Annotate")
      req(selected())
      req(all(rv$data$annotate_lock[req(selected())] == 0))
      rv$updating <- rv$data |>
        dplyr::select(ID, annotate_switch) |>
        dplyr::slice(selected())
      current <- character(0)
      if (length(unique(rv$updating$annotate_switch)) == 1) {
        current <- rv$updating$annotate_switch[1]
      }
      showModal(
        modalDialog(
          title = "Select New State:",
          shinyWidgets::prettyRadioButtons(
            ns("new_state"),
            label = NULL,
            choices = c("Pre-Annotate (wait)" = 0, "Ready to Annotate" = 1, "Successful Aannotation" = 2),
            selected = current,
            shape = "square",
            status = "primary"
          ),
          size = "m",
          footer = tagList(
            actionButton(ns("update_state"), "Update"),
            modalButton("Cancel")
          )
        )
      )
    })
    observeEvent(input$update_state, {
      rv$updating$annotate_switch <- as.numeric(input$new_state)
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |>
        dplyr::rows_update(
          rv$updating,
          by = "ID"
        )
      trigger("update_annotate_table")
      removeModal()
    })

    # Toggle lock ----
    init("lock")
    on("lock", {
      req(session$userData$mode == "Annotate")
      req(selected())
      rv$updating <- rv$data |>
        dplyr::select(ID, annotate_lock) |>
        dplyr::slice(selected())
      lock_current <- as.numeric(names(which.max(table(rv$updating$annotate_lock))))
      rv$updating$annotate_lock <- as.numeric(!lock_current)
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |>
        dplyr::rows_update(rv$updating, by = "ID")
      trigger("update_annotate_table")
      trigger("refresh_export")
    })

    # Toggle ID_verified
    init("id_verified")
    on("id_verified", {
      req(session$userData$mode == "Annotate")
      req(selected())
      rv$updating <- rv$data |>
        dplyr::select(ID, ID_verified) |>
        dplyr::slice(selected())
      ID_current <- sort(unique(rv$updating$ID_verified))[1]
      if(is.na(ID_current)){
        rv$updating$ID_verified <- "yes"
      } else if(ID_current == "yes"){
        rv$updating$ID_verified <- "no"
      } else if(ID_current == "no"){
        rv$updating$ID_verified <- "yes"
      }
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |>
        dplyr::rows_update(rv$updating, by = "ID")
      trigger("update_annotate_table")
      trigger("refresh_export")
    })

    # Toggle problematic
    init("problematic")
    on("problematic", {
      req(session$userData$mode == "Annotate")
      req(selected())
      rv$updating <- rv$data |>
        dplyr::select(ID, problematic) |>
        dplyr::slice(selected())
      ID_current <- sort(unique(rv$updating$problematic))[1]
      if(is.na(ID_current)){
        rv$updating$problematic <- "yes"
      } else {
        rv$updating$problematic <- NA_character_
      }
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |>
        dplyr::rows_update(rv$updating, by = "ID")
      trigger("update_annotate_table")
      trigger("refresh_export")
    })

    # Set Annotate Options ----
    observeEvent(input$set_annotate_opts, {
      row <- as.numeric(input$set_annotate_opts)
      if (length(selected()) > 0 && !row %in% selected()) {
        req(F)
      } else {
        selected <- c(row, selected()) |> unique()
      }
      req(all(rv$data$annotate_lock[selected] == 0))
      rv$updating <- rv$data |> dplyr::slice(selected)
      rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      annotate_opts_modal(rv)
    })
    observeEvent(input$annotate_opts, ignoreInit = T, {
      exists <- input$annotate_opts %in% rv$annotate_opts$annotate_opts
      shinyWidgets::updatePrettyCheckbox(
        inputId = "edit_annotate_opts",
        value = !exists
      )
      if (exists) {
        cur <- rv$annotate_opts[rv$annotate_opts$annotate_opts == input$annotate_opts, ]
        cur_params <- rv$curate_opts$params[rv$curate_opts$curate_opts == rv$updating$curate_opts[1]] |>
          jsonlite::fromJSON()

        updateNumericInput(
          inputId = "annotate_opts_cpus",
          value = cur$cpus
        )
        updateNumericInput(
          inputId = "annotate_opts_memory",
          value = cur$memory
        )
        updateTextAreaInput(
          inputId = "mitos_opts",
          value = cur$mitos_opts
        )
        updateSelectizeInput(
          inputId = "mitos_ref_dir",
          selected = cur$ref_dir,
          choices = unique(rv$annotate_opts$ref_dir),
          options = list(
            create = TRUE,
            maxItems = 1
          )
        )
        updateSelectizeInput(
          inputId = "mitos_ref_db",
          selected = cur$ref_db,
          #choices = unique(rv$annotate_opts$ref_db),
          choices = c("Metazoa", "Chordata"),
          options = list(
            create = TRUE,
            maxItems = 1
          )
        )
        updateTextInput(
          inputId = "trnaScan_opts",
          value = cur$trnaScan_opts
        )
        updateSelectizeInput(
          ns("start_gene"),
          label = "starting gene for circular assemblies",
          choices = c(
            "rrnL",
            "rrnS",
            "nad1",
            "nad2",
            "cox1",
            "cox2",
            "atp8",
            "atp6",
            "cox3",
            "nad3",
            "nad4l",
            "nad4",
            "nad5",
            "nad6",
            "cob",
            "trnA",
            "trnC",
            "trnD",
            "trnE",
            "trnF",
            "trnG",
            "trnH",
            "trnI",
            "trnK",
            "trnL",
            "trnM",
            "trnN",
            "trnP",
            "trnQ",
            "trnR",
            "trnS",
            "trnT",
            "trnV",
            "trnW",
            "trnY"
          ), # TODO: get choices from list of genes in curate params rules, tricky
          selected = cur$start_gene %||% character(0),
          options = list(
            create = TRUE,
            maxItems = 1
          )
        )
      }
    })
    observeEvent(input$edit_annotate_opts, ignoreInit = T, {
      shinyjs::toggleState("annotate_opts_cpus", condition = input$edit_annotate_opts)
      shinyjs::toggleState("annotate_opts_memory", condition = input$edit_annotate_opts)
      shinyjs::toggleState("mitos_opts", condition = input$edit_annotate_opts)
      shinyjs::toggleState("mitos_ref_dir", condition = FALSE) # TODO: custom / alt ref db for mitos
      shinyjs::toggleState("mitos_ref_db", condition = input$edit_annotate_opts)
      shinyjs::toggleState("trnaScan_opts", condition = input$edit_annotate_opts)
      # Check if editing opts that apply beyond selection
      if (input$edit_annotate_opts && input$annotate_opts %in% rv$data$annotate_opts) {
        rv$updating_indirect <- rv$data |>
          dplyr::filter(annotate_opts == input$annotate_opts) |>
          dplyr::anti_join(rv$updating, by = "ID")
        # Prevent editing opts that apply to locked samples
        if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$annotate_lock == 1)) {
          shinyWidgets::sendSweetAlert(
            title = "Attempting to edit locked samples",
            text = "Processing parameters associated with locked samples can not be edited.",
            type = "warning"
          )
          shinyWidgets::updatePrettyCheckbox(
            inputId = "edit_annotate_opts",
            value = FALSE
          )
          req(F)
        }
        # Confirm editing opts that apply beyond selection
        if (nrow(rv$updating_indirect) > 0L) {
          shinyWidgets::confirmSweetAlert(
            inputId = "editing_annotate_opts_indirect",
            title = "Editing beyond selection",
            text = "You are attempting to edit assembly options that apply to samples beyond the current selection. Are you sure you want to proceed?",
            btn_colors = c("#0056b3", "#0056b3")
          )
        }
      } else {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      }
    })
    # Confirm editing opts that apply beyond selection
    observeEvent(input$editing_annotate_opts_indirect, ignoreInit = T, {
      if (!input$editing_annotate_opts_indirect) {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
        shinyWidgets::updatePrettyCheckbox(
          inputId = "edit_annotate_opts",
          value = FALSE
        )
      }
    })
    ## Save Changes ----
    observeEvent(input$update_annotate_opts, ignoreInit = T, {
      ## Add to params table if new or editing ----
      if (input$edit_annotate_opts) {
        dplyr::tbl(session$userData$con, "annotate_opts") |>
          dplyr::rows_upsert(
            data.frame(
              annotate_opts = req(input$annotate_opts),
              cpus = req(input$annotate_opts_cpus),
              memory = req(input$annotate_opts_memory),
              mitos_opts = req(input$mitos_opts),
              ref_dir = req(input$mitos_ref_dir),
              ref_db = req(input$mitos_ref_db),
              trnaScan_opts = req(input$trnaScan_opts),
              start_gene = req(input$start_gene)
            ),
            in_place = TRUE,
            copy = TRUE,
            by = "annotate_opts"
          )
        rv$annotate_opts <- dplyr::tbl(session$userData$con, "annotate_opts") |>
          dplyr::collect()
      }
      ## Update Annotate Table ----
      update <- data.frame(
        ID = c(rv$updating$ID, rv$updating_indirect$ID),
        annotate_opts = input$annotate_opts,
        annotate_switch = 1
      )
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          update,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |>
        dplyr::rows_update(
          update,
          by = "ID"
        )
      rv$updating <- rv$updating_indirect <- NULL
      removeModal()
      trigger("update_annotate_table")
    })

    # Set Curate Options ----
    observeEvent(input$set_curate_opts, {
      row <- as.numeric(input$set_curate_opts)
      if (length(selected()) > 0 && !row %in% selected()) {
        req(F)
      } else {
        selected <- c(row, selected()) |> unique()
      }
      req(all(rv$data$annotate_lock[selected] == 0))
      rv$updating <- rv$data |> dplyr::slice(selected)
      rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      curate_opts_modal(rv)
    })
    observeEvent(input$curate_opts, ignoreInit = T, {
      exists <- input$curate_opts %in% rv$curate_opts$curate_opts
      shinyWidgets::updatePrettyCheckbox(
        inputId = "edit_curate_opts",
        value = !exists
      )
      if (exists) {
        cur <- rv$curate_opts[rv$curate_opts$curate_opts == input$curate_opts, ]
        updateNumericInput(
          inputId = "curate_opts_cpus",
          value = cur$cpus
        )
        updateNumericInput(
          inputId = "curate_opts_memory",
          value = cur$memory
        )
        updateSelectizeInput(
          inputId = "target",
          selected = cur$target,
          choices = c("fish_mito", "starfish_mito"),
          options = list(
            create = TRUE,
            maxItems = 1
          )
        )
        rv$params <- cur$params |> jsonlite::fromJSON()
      }
    })
    output$params <- listviewer::renderReactjson({
      listviewer::reactjson(
        req(rv$params),
        "Validataion Parameters",
        theme = "monokai",
        iconStyle = "triangle",
        collapsed = 2,
        enableClipboard = FALSE,
        displayObjectSize = FALSE,
        displayDataTypes = FALSE,
        onEdit = FALSE,
        onAdd = FALSE,
        onDelete = FALSE,
        onSelect = FALSE
      )
    })
    observeEvent(input$edit_curate_opts, ignoreInit = T, {
      shinyjs::toggleState("curate_opts_cpus", condition = input$edit_curate_opts)
      shinyjs::toggleState("curate_opts_memory", condition = input$edit_curate_opts)
      shinyjs::toggleState("target", condition = input$edit_curate_opts)
      shinyjs::toggleState("start_gene", condition = input$edit_curate_opts)
      # Check if editing opts that apply beyond selection
      if (input$edit_curate_opts && input$curate_opts %in% rv$data$curate_opts) {
        rv$updating_indirect <- rv$data |>
          dplyr::filter(curate_opts == input$curate_opts) |>
          dplyr::anti_join(rv$updating, by = "ID")
        # Prevent editing opts that apply to locked samples
        if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$annotate_lock == 1)) {
          shinyWidgets::sendSweetAlert(
            title = "Attempting to edit locked samples",
            text = "Processing parameters associated with locked samples can not be edited.",
            type = "warning"
          )
          shinyWidgets::updatePrettyCheckbox(
            inputId = "edit_curate_opts",
            value = FALSE
          )
          req(F)
        }
        # Confirm editing opts that apply beyond selection
        if (nrow(rv$updating_indirect) > 0L) {
          shinyWidgets::confirmSweetAlert(
            inputId = "editing_curate_opts_indirect",
            title = "Editing beyond selection",
            text = "You are attempting to edit assembly options that apply to samples beyond the current selection. Are you sure you want to proceed?",
            btn_colors = c("#0056b3", "#0056b3")
          )
        }
      } else {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      }
    })
    observeEvent(input$target, {
      rv$params <- do.call(paste0("params_", input$target), list()) |>
        jsonlite::toJSON(auto_unbox = TRUE)
      output$params <- listviewer::renderReactjson({
        listviewer::reactjson(
          req(rv$params),
          "Validataion Parameters",
          theme = "monokai",
          iconStyle = "triangle",
          collapsed = 2,
          enableClipboard = FALSE,
          displayObjectSize = FALSE,
          displayDataTypes = FALSE,
          onEdit = FALSE,
          onAdd = FALSE,
          onDelete = FALSE,
          onSelect = FALSE
        )
      })
    })
    # Confirm editing opts that apply beyond selection
    observeEvent(input$editing_curate_opts_indirect, ignoreInit = T, {
      if (!input$editing_curate_opts_indirect) {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
        shinyWidgets::updatePrettyCheckbox(
          inputId = "edit_curate_opts",
          value = FALSE
        )
      }
    })
    ## Save Changes ----
    observeEvent(input$update_curate_opts, ignoreInit = T, {
      ## Add to params table if new or editing ----
      if (input$edit_curate_opts) {
        params <- do.call(paste0("params_", input$target), list()) |>
          jsonlite::toJSON(auto_unbox = TRUE)
        dplyr::tbl(session$userData$con, "curate_opts") |>
          dplyr::rows_upsert(
            data.frame(
              curate_opts = req(input$curate_opts),
              cpus = req(input$curate_opts_cpus),
              memory = req(input$curate_opts_memory),
              target = req(input$target)
            ),
            in_place = TRUE,
            copy = TRUE,
            by = "curate_opts"
          )
        DBI::dbExecute(
          session$userData$con,
          stringr::str_glue(
            "UPDATE curate_opts SET params = '{params}' WHERE curate_opts = '{input$curate_opts}'"
          )
        )
        rv$curate_opts <- dplyr::tbl(session$userData$con, "curate_opts") |>
          dplyr::collect()
      }
      ## Update Annotate Table ----
      update <- data.frame(
        ID = c(rv$updating$ID, rv$updating_indirect$ID),
        curate_opts = input$curate_opts,
        annotate_switch = 1
      )
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          update,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |>
        dplyr::rows_update(
          update,
          by = "ID"
        )
      rv$updating <- rv$updating_indirect <- NULL
      removeModal()
      trigger("update_annotate_table")
    })
    # Open output folder ----
    observeEvent(input$output, ignoreInit = T, {
      pth <- file.path(
        session$userData$dir_out,
        rv$data$ID[as.numeric(input$output)],
        "annotate"
      )
      req(file.exists(pth))
      if (tolower(Sys.getenv("RSTUDIO_PROGRAM_MODE")) == "server") {
        rstudioapi::filesPaneNavigate(pth)
        req(F)
      } else {
        utils::browseURL(pth)
      }
    })

    # Open annotation details ----
    observeEvent(input$details, {
      rv$updating <- rv$data |> dplyr::slice(as.numeric(input$details))
      trigger("annotations_modal")
    })
    annotations_details_server(ns("annotations"), rv)
  })
}
