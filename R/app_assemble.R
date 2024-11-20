#' assemble UI
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
assemble_ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactableOutput(ns("table"))
  )
}

#' assemble Server
#'
#' @noRd
assemble_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Prepare data ----
    rv <- reactiveValues(
      pre_opts = dplyr::tbl(session$userData$db, "pre_opts") |>
        dplyr::collect(),
      assemble_opts = dplyr::tbl(session$userData$db, "assemble_opts") |>
        dplyr::collect(),
      data = fetch_assemble_data(),
      updating = NULL
    )

    # Refresh ----
    on("refresh", {
      rv$data <- fetch_assemble_data()
      updateReactable(
        "table",
        data = rv$data
      )
    })

    # Render table ----
    output$table <- renderReactable({
      isolate(req(rv$data)) |>
        reactable(
          compact = TRUE,
          defaultPageSize = 100,
          showPageSizeOptions = TRUE,
          onClick = "select",
          selection = "multiple",
          searchable = TRUE,
          defaultSorted = list(time_stamp = "desc"),
          height = 650,
          wrap = FALSE,
          pageSizeOptions = c(25, 50, 100, 200, 500),
          rowStyle = rt_highlight_row(),
          defaultColDef = colDef(align = "left", show = F),
          columns = list(
            `.selection` = colDef(show = T),
            assemble_lock = colDef(
              show = TRUE,
              sticky = "left",
              name = "",
              html = TRUE,
              width = 30,
              align = "center",
              cell = rt_dynamicIcon(
                c(
                  `0` = "fa fa-lock-open",
                  `1` = "fa fa-lock"
                )
              )
            ),
            assemble_switch = colDef(
              show = TRUE,
              sticky = "left",
              name = "",
              html = TRUE,
              width = 30,
              align = "center",
              cell = rt_dynamicIcon(
                c(
                  `0` = "fa fa-circle-notch",
                  `1` = "fa fa-hourglass",
                  `2` = "fa fa-circle-check",
                  `3` = "fa fa-triangle-exclamation"
                )
              )
            ),
            ID = colDef(
              show = T,
              width = 150,
              sticky = "left",
              html = T,
              cell = rt_longtext()
            ),
            Taxon = colDef(
              show = T,
              width = 150,
              sticky = "left",
              html = T,
              cell = rt_longtext()
            ),
            pre_opts = colDef(
              show = T,
              name = "Preprocess Opts.",
              html = T,
              width = 130,
              cell = rt_link(ns("set_pre_opts"))
            ),
            trimmed_reads = colDef(
              show = T,
              name = "Reads",
              width = 100
            ),
            mean_length = colDef(
              show = T,
              name = "Read Length",
              width = 100
            ),
            assemble_opts = colDef(
              show = T,
              name = "Assembly Opts.",
              html = T,
              width = 130,
              cell = rt_link(ns("set_assemble_opts"))
            ),
            topology = colDef(
              show = TRUE,
              width = 100,
              name = "Topology"
            ),
            length = colDef(
              show = TRUE,
              width = 140,
              name = "Assembly Length",
              html = TRUE,
              cell = rt_longtext()
            ),
            paths = colDef(
              show = TRUE, width = 100, name = "# Paths", align = "center",
              cell = JS("function(cellInfo){if(cellInfo.value<0){return -cellInfo.value };return cellInfo.value}"),
              style = JS("function(rowInfo){ if (rowInfo.values.paths < 0) return { backgroundColor: '#00000020' }}")
            ),
            scaffolds = colDef(
              show = TRUE, width = 100, name = "# Scaffolds", align = "center",
              cell = JS("function(cellInfo){if(cellInfo.value<0){return -cellInfo.value };return cellInfo.value}"),
              style = JS("function(rowInfo){ if (rowInfo.values.scaffolds < 0) return { backgroundColor: '#00000020' }}")
            ),
            time_stamp = colDef(
              show = TRUE,
              name = "Last Updated",
              html = T,
              width = 150,
              cell = rt_ts_date()
            ),
            assemble_notes = colDef(
              show = TRUE,
              name = "Notes",
              html = TRUE,
              align = "left",
              minWidth = 150
            ),
            view = colDef(
              show = TRUE,
              sticky = "right",
              name = "",
              html = TRUE,
              width = 40,
              align = "center",
              cell = rt_icon_bttn(ns("details"), "fas fa-square-arrow-up-right")
            )
          )
        )
    })

    # update table ----
    init("update_assemble_table")
    on("update_assemble_table", {
      reactable::updateReactable(
        "table",
        data = rv$data,
        selected = reactable::getReactableState("table", "selected"),
        page = reactable::getReactableState("table", "page")
      )
    })

    # table selection ----
    selected <- reactive(reactable::getReactableState("table", "selected"))

    # Set State ----
    on("state", {
      req(session$userData$mode == "Assemble")
      req(all(rv$data$assemble_lock[req(selected())] == 0))
      rv$updating <- rv$data |>
        dplyr::select(ID, assemble_switch) |>
        dplyr::slice(selected())
      current <- character(0)
      if (length(unique(rv$updating$assemble_switch)) == 1) {
        current <- rv$updating$assemble_switch[1]
      }
      showModal(
        modalDialog(
          title = "Select New State:",
          shinyWidgets::prettyCheckboxGroup(
            ns("new_state"),
            label = NULL,
            choices = c("Pre-Assembly" = 0, "Ready to Assemble" = 1, "Successful Assembly" = 2, "Failed / Problematic Assembly" = 3),
            selected = current,
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
      rv$updating$assemble_switch <- as.numeric(input$new_state)
      dplyr::tbl(session$userData$db, "assemble") |>
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
      trigger("update_assemble_table")
      removeModal()
    })

    # Toggle lock ----
    on("lock", {
      req(session$userData$mode == "Assemble")
      req(selected())
      rv$updating <- rv$data |>
        dplyr::select(ID, assemble_lock) |>
        dplyr::slice(selected())
      lock_current <- as.numeric(names(which.max(table(rv$updating$assemble_lock))))
      rv$updating$assemble_lock <- as.numeric(!lock_current)
      dplyr::tbl(session$userData$db, "assemble") |>
        dplyr::rows_update(
          rv$updating,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |>
        dplyr::rows_update(rv$updating, by = "ID")
      trigger("update_assemble_table")
    })

    # Set Pre-process Opts ----
    observeEvent(input$set_pre_opts, {
      row <- as.numeric(input$set_pre_opts)
      if (length(selected()) > 0 && !row %in% selected()) {
        req(F)
      }else{
        selected <- c(row, selected()) |> unique()
      }
      req(all(rv$data$assemble_lock[selected] == 0))
      rv$updating <- rv$data |> dplyr::slice(selected)
      rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      pre_opts_modal(rv)
    })
    observeEvent(input$pre_opts, ignoreInit = T, {
      exists <- input$pre_opts %in% rv$pre_opts$pre_opts
      shinyWidgets::updatePrettyCheckbox(
        inputId = "edit_pre_opts",
        value = !exists
      )
      if (exists) {
        cur <- rv$pre_opts[rv$pre_opts$pre_opts == input$pre_opts, ]
        updateNumericInput(
          inputId = "pre_opts_cpus",
          value = cur$cpus
        )
        updateNumericInput(
          inputId = "pre_opts_memory",
          value = cur$memory
        )
        updateTextAreaInput(
          inputId = "fastp",
          value = cur$fastp
        )
      }
    })
    observeEvent(input$edit_pre_opts, ignoreInit = T, {
      shinyjs::toggleState("pre_opts_cpus", condition = input$edit_pre_opts)
      shinyjs::toggleState("pre_opts_memory", condition = input$edit_pre_opts)
      shinyjs::toggleState("fastp", condition = input$edit_pre_opts)
      # Check if editing opts that apply beyond selection
      if(input$edit_pre_opts && input$pre_opts %in% rv$data$pre_opts){
        rv$updating_indirect <- rv$data |>
          dplyr::filter(pre_opts == input$pre_opts) |>
          dplyr::anti_join(rv$updating, by = "ID")

        # Prevent editing opts that apply to locked
        if(nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$assemble_lock == 1)){
          shinyWidgets::sendSweetAlert(
            title = "Editing locked samples",
            text = "Processing parameters associated with locked samples can not be edited."
          )
          shinyWidgets::updatePrettyCheckbox(
            inputId = "edit_pre_opts",
            value = FALSE
          )
          req(F)
        }

        if(nrow(rv$updating_indirect) > 0L){
          shinyWidgets::confirmSweetAlert(
            inputId = "editing_opts_indirect",
            title = "Editing beyond selection",
            text = "You are attempting to edit pre-processing options that apply to samples beyond the current selection. Are you sure you want to proceed?",
          )
        }
      }else{
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      }
    })
    # Confirm editing opts that apply beyond selection
    observeEvent(input$editing_opts_indirect, ignoreInit = T, {
      if(!input$editing_opts_indirect){
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
        shinyWidgets::updatePrettyCheckbox(
          inputId = "edit_pre_opts",
          value = FALSE
        )
      }
    })
    observeEvent(input$update_pre_opts, ignoreInit = T, {
      if (input$edit_pre_opts) {
        dplyr::tbl(session$userData$db, "pre_opts") |>
          dplyr::rows_upsert(
            data.frame(
              pre_opts = req(input$pre_opts),
              cpus = req(input$pre_opts_cpus),
              memory = req(input$pre_opts_memory),
              fastp = req(input$fastp)
            ),
            in_place = TRUE,
            copy = TRUE,
            by = "pre_opts"
          )
        rv$pre_opts <- dplyr::tbl(session$userData$db, "pre_opts") |>
          dplyr::collect()
      }
      ## Update Assembly / Pre-processing Tables ----
      update <- data.frame(
        ID = c(rv$updating$ID, rv$updating_indirect$ID),
        pre_opts = input$pre_opts,
        assemble_switch = 1
      )
      dplyr::tbl(session$userData$db, "preprocess") |>
        dplyr::rows_update(
          update[, c("ID", "pre_opts")],
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      dplyr::tbl(session$userData$db, "assemble") |>
        dplyr::rows_update(
          update[, c("ID", "assemble_switch")],
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
      trigger("update_assemble_table")
    })

    # Set Assemble Opts ----
    observeEvent(input$set_assemble_opts, {
      row <- as.numeric(input$set_assemble_opts)
      if (length(selected()) > 0 && !row %in% selected()) {
        req(F)
      }else{
        selected <- c(row, selected()) |> unique()
      }
      req(all(rv$data$assemble_lock[selected] == 0))
      rv$updating <- rv$data |> dplyr::slice(selected)
      rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      assemble_opts_modal(rv)
    })
    observeEvent(input$assemble_opts, ignoreInit = T, {
      exists <- input$assemble_opts %in% rv$assemble_opts$assemble_opts
      shinyWidgets::updatePrettyCheckbox(
        inputId = "edit_assemble_opts",
        value = !exists
      )
      if (exists) {
        cur <- rv$assemble_opts[rv$assemble_opts$assemble_opts == input$assemble_opts, ]
        updateNumericInput(
          inputId = "assemble_opts_cpus",
          value = cur$cpus
        )
        updateNumericInput(
          inputId = "assemble_opts_memory",
          value = cur$memory
        )
        updateTextAreaInput(
          inputId = "getOrganelle",
          value = cur$getOrganelle
        )
        updateTextInput(
          inputId = "seeds_db",
          value = cur$seeds_db
        )
      }
    })
    observeEvent(input$edit_assemble_opts, ignoreInit = T, {
      shinyjs::toggleState("assemble_opts_cpus", condition = !input$edit_assemble_opts)
      shinyjs::toggleState("assemble_opts_memory", condition = !input$edit_assemble_opts)
      shinyjs::toggleState("getOrganelle", condition = !input$edit_assemble_opts)
      # TODO - allow for alt seed database
      # Need to modify nextflow to pass seeds database to worker or
      # the specific path must exist in the workers docker container
      shinyjs::toggleState("seeds_db", condition = FALSE)
      # Check if editing opts that apply beyond selection
      if(input$edit_assemble_opts && input$assemble_opts %in% rv$data$assemble_opts){
        rv$updating_indirect <- rv$data |>
          dplyr::filter(assemble_opts == input$assemble_opts) |>
          dplyr::anti_join(rv$updating, by = "ID")
        # Prevent editing opts that apply to locked samples
        if(nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$assemble_lock == 1)){
          shinyWidgets::sendSweetAlert(
            title = "Editing locked samples",
            text = "Processing parameters associated with locked samples can not be edited."
          )
          shinyWidgets::updatePrettyCheckbox(
            inputId = "edit_assemble_opts",
            value = FALSE
          )
          req(F)
        }
        # Confirm editing opts that apply beyond selection
        if(nrow(rv$updating_indirect) > 0L){
          shinyWidgets::confirmSweetAlert(
            inputId = "editing_assemble_opts_indirect",
            title = "Editing beyond selection",
            text = "You are attempting to edit assembly options that apply to samples beyond the current selection. Are you sure you want to proceed?",
          )
        }
      }else{
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      }
    })
    # Confirm editing opts that apply beyond selection
    observeEvent(input$editing_assemble_opts_indirect, ignoreInit = T, {
      if(!input$editing_assemble_opts_indirect){
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
        shinyWidgets::updatePrettyCheckbox(
          inputId = "edit_assemble_opts",
          value = FALSE
        )
      }
    })
    ## Save Changes ----
    observeEvent(input$update_assemble_opts, ignoreInit = T, {
      ## Add to params table if new ----
      if (input$edit_assemble_opts) {
        dplyr::tbl(session$userData$db, "assemble_opts") |>
          dplyr::rows_iupsert(
            data.frame(
              assemble_opts = req(input$assemble_opts),
              cpus = req(input$assemble_opts_cpus),
              memory = req(input$assemble_opts_memory),
              getOrganelle = req(input$getOrganelle),
              seeds_db = req(input$seeds_db),
              labels_db = rv$assemble_opts$labels_db[1]
            ),
            in_place = TRUE,
            copy = TRUE,
            by = "assemble_opts"
          )
        rv$assemble_opts <- dplyr::tbl(session$userData$db, "assemble_opts") |>
          dplyr::collect()
      }
      ## Update Assembly Table ----
      update <- data.frame(
        ID = c(rv$updating$ID, rv$updating_indirect$ID),
        assemble_opts = input$assemble_opts,
        assemble_switch = 1
      )
      dplyr::tbl(session$userData$db, "assemble") |>
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
      trigger("update_assemble_table")
    })

    # Open Assembly Details ----
    observeEvent(input$details, ignoreInit = T, {
      rv$updating <- rv$data |> dplyr::slice(as.numeric(input$details))
      trigger("coverage_modal")
    })
    mod_assembly_coverage_details_server(ns("coverage_details"), rv)

  })
}
