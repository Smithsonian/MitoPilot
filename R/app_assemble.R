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
              cell = rt_icon_bttn(ns("view"), "fa fa-eye")
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
      showModal(
        modalDialog(
          title = "Select New State:",
          shinyWidgets::pickerInput(
            ns("new_state"),
            label = NULL,
            choices = c("Pre-Assembly" = 0, "Ready to Assemble" = 1, "Successful Assembly" = 2, "Failed / Problematic Assembly" = 3),
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
      }
      if (isTRUE(row %in% selected())) {
        rv$updating <- rv$data |> dplyr::slice(selected())
      } else {
        rv$updating <- rv$data |> dplyr::slice(row)
      }
      req(all(rv$updating$assemble_lock == 0))
      pre_opts_modal(rv)
    })
    observeEvent(input$pre_opts, ignoreInit = T, {
      exists <- input$pre_opts %in% rv$pre_opts$pre_opts
      shinyjs::toggleState("pre_opts_cpus", condition = !exists)
      shinyjs::toggleState("pre_opts_memory", condition = !exists)
      shinyjs::toggleState("fastp", condition = !exists)
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
    observeEvent(input$update_pre_opts, ignoreInit = T, {
      ## Add to params table if new ----
      if (!input$pre_opts %in% rv$pre_opts$pre_opts) {
        dplyr::tbl(session$userData$db, "pre_opts") |>
          dplyr::rows_insert(
            data.frame(
              pre_opts = req(input$pre_opts),
              cpus = req(input$pre_opts_cpus),
              memory = req(input$pre_opts_memory),
              fastp = req(input$fastp)
            ),
            in_place = TRUE,
            conflict = "ignore",
            copy = TRUE,
            by = "pre_opts"
          )
      }
      ## Update Assembly / Preprocessing Tables ----
      update <- data.frame(
        ID = rv$updating$ID,
        pre_opts = input$pre_opts,
        assemble_switch = rv$updating$assemble_switch
      )
      if (input$set_state) {
        update$assemble_switch <- 1
        dplyr::tbl(session$userData$db, "assemble") |>
          dplyr::rows_update(
            update[, c("ID", "assemble_switch")],
            unmatched = "ignore",
            in_place = TRUE,
            copy = TRUE,
            by = "ID"
          )
      }
      dplyr::tbl(session$userData$db, "preprocess") |>
        dplyr::rows_update(
          update[, c("ID", "pre_opts")],
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
      removeModal()
      trigger("update_assemble_table")
    })

    # Set Assemble Opts ----
    observeEvent(input$set_assemble_opts, {
      row <- as.numeric(input$set_assemble_opts)
      if (length(selected()) > 0 && !row %in% selected()) {
        req(F)
      }
      if (isTRUE(row %in% selected())) {
        rv$updating <- rv$data |> dplyr::slice(selected())
      } else {
        rv$updating <- rv$data |> dplyr::slice(row)
      }
      req(all(rv$updating$assemble_lock == 0))
      assemble_opts_modal(rv)
    })
    observeEvent(input$assemble_opts, ignoreInit = T, {
      exists <- input$assemble_opts %in% rv$assemble_opts$assemble_opts
      shinyjs::toggleState("assemble_opts_cpus", condition = !exists)
      shinyjs::toggleState("assemble_opts_memory", condition = !exists)
      shinyjs::toggleState("getOrganelle", condition = !exists)
      shinyjs::toggleState("seeds_db", condition = FALSE) # TODO - allow for alt seed database
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
        # TODO - allow for alt seed database
        # Need to modify nextflow to pass seeds database to worker or
        # the specific path must exist in the workers docker container
        # updateTextInput(
        #   inputId = "seeds_db",
        #   value = cur$seeds_db
        # )
      }
    })
    observeEvent(input$update_assemble_opts, ignoreInit = T, {
      ## Add to params table if new ----
      if (!input$assemble_opts %in% rv$assemble_opts$assemble_opts) {
        dplyr::tbl(session$userData$db, "assemble_opts") |>
          dplyr::rows_insert(
            data.frame(
              assemble_opts = req(input$assemble_opts),
              cpus = req(input$assemble_opts_cpus),
              memory = req(input$assemble_opts_memory),
              getOrganelle = req(input$getOrganelle),
              seeds_db = req(input$seeds_db),
              labels_db = rv$assemble_opts$labels_db[1]
            ),
            in_place = TRUE,
            conflict = "ignore",
            copy = TRUE,
            by = "assemble_opts"
          )
      }
      ## Update Assembly Table ----
      update <- data.frame(
        ID = rv$updating$ID,
        assemble_opts = input$assemble_opts,
        assemble_switch = rv$updating$assemble_switch
      )
      if (input$set_state) {
        update$assemble_switch <- 1
      }
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
      removeModal()
      trigger("update_assemble_table")
    })


    # # Set Params ----
    # observeEvent(input$set_assembly_params, {
    #   row <- as.numeric(input$set_assembly_params)
    #   if(length(selected())>0 & !row %in% selected()){req(F)}
    #   if(row %in% selected()){
    #     rv$updating <- rv$data |> dplyr::slice(selected())
    #   }else{
    #     rv$updating <- rv$data |> dplyr::slice(row)
    #   }
    #   # Prevent update to multiple targets (TODO add popup)
    #   req(length(unique(rv$updating$target))==1)
    #
    #   # Current Values
    #   cpus <- unique(rv$updating$opt_cpus) |> {
    #     \(x) ifelse(length(x)==1, x, numeric())
    #   }()
    #   memory <- unique(rv$updating$opt_memory) |> {
    #     \(x) ifelse(length(x)==1, x, numeric())
    #   }()
    #   assembly_params <- unique(rv$updating$opt_getOrganelle) |> {
    #     \(x) ifelse(length(x)==1, x, character())
    #   }()
    #   assembly_params_opts <- rv$assembly_opts |>
    #     dplyr::filter(target==unique(rv$updating$target)) |>
    #     dplyr::pull(opt_getOrganelle)
    #   showModal(
    #     modalDialog(
    #       title = stringr::str_glue("Set Assembly Parameters for {nrow(rv$updating)} Samples"),
    #       numericInput(ns("set_cpus"), "CPUs:", value=cpus ),
    #       numericInput(ns("set_memory"), "Memory (GB):", value=memory ),
    #       shinyWidgets::pickerInput(
    #         ns("existing_params_selector"), "Existing Parameter Settings",
    #         choices = assembly_params_opts, selected = assembly_params, width="100%"
    #       ),
    #       textAreaInput(
    #         ns("updated_params"), "Updated Assembly Params",
    #         value=assembly_params, width="100%"
    #       ),
    #       shinyWidgets::prettyCheckbox(
    #         ns("assembly_set_state"),
    #         label = "Toggle State",
    #         status='primary',
    #         inline=TRUE,
    #         value = TRUE
    #       ),
    #       shinyWidgets::prettyCheckbox(
    #         ns("save_old_assembly"),
    #         label = "Save Old Assembly",
    #         status='primary',
    #         inline=TRUE,
    #         value = FALSE
    #       ),
    #       size="xl",
    #       footer = tagList(
    #         actionButton(ns("params_update"), "Update"),
    #         modalButton("Cancel")
    #       )
    #     )
    #   )
    # })
    # # Choose from existing
    # observeEvent(input$existing_params_selector, {
    #   req(input$existing_params_selector != input$updated_params)
    #   updateTextAreaInput(
    #     inputId = "updated_params",
    #     value = input$existing_params_selector
    #   )
    # })
    # # Set changes
    # observeEvent(input$params_update, {
    #   updated_data <- rv$updating |>
    #     dplyr::transmute(
    #       ID=ID,
    #       target=target,
    #       hash_old = hash,
    #       opt_cpus = req(input$set_cpus),
    #       opt_memory = req(input$set_memory),
    #       opt_db = opt_db, # TODO - make modifiable for custom seed db
    #       opt_getOrganelle = req(input$updated_params) |>
    #         stringr::str_squish(),
    #       topology=NA_character_,
    #       length=NA_character_,
    #       paths=NA_real_,
    #       scaffolds=NA_real_,
    #       time_stamp=as.integer(Sys.time())
    #     ) |>
    #     tidyr::unite(
    #       hash, sep=" ", remove=FALSE,
    #       dplyr::all_of(c("opt_cpus", "opt_memory", "opt_db", "opt_getOrganelle")),
    #     ) |>
    #     dplyr::mutate(
    #       hash = as.character(openssl::md5(hash))
    #     ) |>
    #     filter(hash_old != hash)
    #   # Stop if nothing to update
    #   if(nrow(updated_data)==0){
    #     removeModal()
    #     req(F)
    #   }
    #   # Set state to '1/assemble'
    #   if(input$assembly_set_state){
    #     updated_data$assemble_switch <- 1
    #   }
    #   # Delete old data
    #   if(!input$save_old_assembly){
    #     out.path <- getOption('out.path')
    #     updated_data |>
    #       select(ID, target, hash_old) |>
    #       purrr::pwalk(function(ID, target, hash_old){
    #         stringr::str_glue(
    #           "-rf {out.path}/{ID}/{target}/{hash_old}"
    #         ) |> system2("rm", args=_)
    #       })
    #   }
    #   updated_data <- updated_data |> select(-hash_old)
    #   update_assembly_table(updated_data)
    #   rv$data <- rv$data |>
    #     dplyr::rows_update(updated_data, by=c("ID", "target"))
    #   reactable::updateReactable(
    #     "table",
    #     data = rv$data,
    #     selected = selected(),
    #     page = reactable::getReactableState("table", "page")
    #   )
    #   rv$assembly_opts <- get_assembly_params()
    #   rv$updating <- NULL
    #   removeModal()
    # })
    #
    # # Toggle State ----
    # on("state", {
    #   req(grv$mode=="Assemble")
    #   req(all(rv$data$lock[req(selected())]==0))
    #   rv$updating <- rv$data |>
    #     select(ID, target, assemble_switch) |>
    #     dplyr::slice(selected())
    #
    #   showModal(
    #     modalDialog(
    #       title = "Select New State:",
    #       shinyWidgets::pickerInput(
    #         ns("new_state"),
    #         label = NULL,
    #         choices = c("Pre-Assembly"=0, "Ready to Assemble"=1, "Successful Assembly"=2, "Failed / Problematic Assembly"=3),
    #         choicesOpt = list(
    #           icon = c("fa fa-circle-notch", "fa fa-hourglass", "fa fa-circle-check", "fa fa-triangle-exclamation")
    #         )
    #       ),
    #       size="m",
    #       footer = tagList(
    #         actionButton(ns("update_state"), "Update"),
    #         modalButton("Cancel")
    #       )
    #     )
    #   )
    # })
    # observeEvent(input$update_state, {
    #   rv$updating$assemble_switch <- as.numeric(input$new_state)
    #   update_assembly_table(rv$updating)
    #   rv$data <- rv$data |>
    #     dplyr::rows_update(rv$updating, by=c("ID", "target"))
    #   trigger("update_asembly_table")
    #   removeModal()
    # })
    #
    # # Toggle Lock ----
    # on("lock", {
    #   req(grv$mode=="Assemble")
    #   req(selected())
    #   rv$updating <- rv$data |>
    #     select(ID, target, lock) |>
    #     dplyr::slice(selected())
    #   lock_current <- as.numeric(names(which.max(table(rv$updating$lock))))
    #   rv$updating$lock <- as.numeric(!lock_current)
    #   update_assembly_table(rv$updating)
    #   rv$data <- rv$data |>
    #     dplyr::rows_update(rv$updating, by=c("ID", "target"))
    #   trigger("update_asembly_table")
    # })
    #
    # # Open Assembly Details ----
    # init("assembly_modal")
    # observeEvent(input$assembly_details, {
    #   row <- as.numeric(input$assembly_details)
    #   reactable::updateReactable(
    #     "table",
    #     selected = row,
    #   )
    #   rv$updating <- rv$data |> slice(row)
    #   trigger("assembly_modal")
    # })
    # mod_assembly_modal_server("assembly_details", rv)
  })
}
