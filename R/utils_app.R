#' Open a directory in an environment-aware way
#'
#' Local desktop opens the OS file browser; RStudio Server navigates the Files pane (and
#' notifies the user, since that is not obvious); headless sessions cannot open folders, so a
#' warning notification with the path is shown instead.
#'
#' @noRd
open_path <- function(pth) {
  if (isTRUE(getOption("MitoPilot.headless"))) {
    showNotification(
      paste0("Headless session: cannot open folders here. Use the Copy button. Path: ", pth),
      type = "warning", duration = 10
    )
    return(invisible(FALSE))
  }
  if (!dir.exists(pth)) {
    showNotification(
      paste0("Folder not found (it may have been cleaned or is on storage this host ",
             "cannot see): ", pth),
      type = "warning", duration = 10
    )
    return(invisible(FALSE))
  }
  if (tolower(Sys.getenv("RSTUDIO_PROGRAM_MODE")) == "server") {
    if (requireNamespace("rstudioapi", quietly = TRUE)) rstudioapi::filesPaneNavigate(pth)
    showNotification(
      "Opened in the RStudio Files pane (bottom-right panel).",
      type = "message", duration = 5
    )
  } else {
    utils::browseURL(pth)
  }
  invisible(TRUE)
}

#' Set state of details element open/closed
#'
#' @noRd
toggleDetails <- function(id, state, session = getDefaultReactiveDomain()) {
  session$sendCustomMessage("toggleDetails", list(id = id, state = state))
}

#' Reverse lookup col -> group for a column-group list
#'
#' @noRd
col_group_lookup <- function(groups) {
  stats::setNames(rep(names(groups), lengths(groups)), unlist(groups, use.names = FALSE))
}

#' CSS class for a togglable column. Added to both the body cell (class) and
#' the header cell (headerClass) so the whole column collapses when the
#' matching group is unselected.
#'
#' @noRd
grp_class <- function(col, groups) {
  g <- col_group_lookup(groups)[col]
  if (is.na(g)) NULL else paste0("mp-grp-", g)
}

#' Observer for a "set <x> opts" table button: fold the clicked row into the
#' current selection, refuse locked rows, stage rv$updating and open the modal.
#'
#' @noRd
open_opts_modal <- function(input, input_id, rv, selected, modal_fn) {
  force(input_id)
  force(modal_fn)
  observeEvent(input[[input_id]], {
    row <- as.numeric(input[[input_id]])
    if (length(selected()) > 0 && !row %in% selected()) {
      req(F)
    } else {
      selected <- c(row, selected()) |> unique()
    }
    req(all(rv$data$assemble_lock[selected] == 0))
    rv$updating <- rv$data |> dplyr::slice(selected)
    rv$updating_indirect <- rv$updating |> dplyr::slice(0)
    modal_fn(rv)
  })
}

#' Guard for editing opts shared with rows outside the current selection.
#'
#' Fills rv$updating_indirect with the rows that would be dragged along, blocks
#' the edit outright if any of them are locked, and otherwise asks the user to
#' confirm. Call from the observer on the "edit <x> opts" checkbox.
#'
#' @noRd
opts_indirect_guard <- function(input, rv, dat, opts_col, edit_id, confirm_id,
                                noun, lock_col, by) {
  opts_val <- input[[opts_col]]
  if (input[[edit_id]] && opts_val %in% dat[[opts_col]]) {
    rv$updating_indirect <- dat[which(dat[[opts_col]] == opts_val), , drop = FALSE] |>
      dplyr::anti_join(rv$updating, by = by)
    if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect[[lock_col]] == 1)) {
      shinyWidgets::sendSweetAlert(
        title = "Attempting to edit locked samples",
        text = "Processing parameters associated with locked samples can not be edited.",
        type = "warning"
      )
      shinyWidgets::updatePrettyCheckbox(inputId = edit_id, value = FALSE)
      req(F)
    }
    if (nrow(rv$updating_indirect) > 0L) {
      shinyWidgets::confirmSweetAlert(
        inputId = confirm_id,
        title = "Editing beyond selection",
        text = paste0(
          "You are attempting to edit ", noun, " that apply to samples beyond ",
          "the current selection. Are you sure you want to proceed?"
        ),
        btn_colors = c("#0056b3", "#0056b3")
      )
    }
  } else {
    rv$updating_indirect <- rv$updating |> dplyr::slice(0)
  }
}

#' Observer for the confirmation raised by opts_indirect_guard(): on "no",
#' drop the indirect rows and untick the edit checkbox.
#'
#' @noRd
opts_indirect_confirm <- function(input, rv, confirm_id, edit_id) {
  force(confirm_id)
  force(edit_id)
  observeEvent(input[[confirm_id]], {
    if (!input[[confirm_id]]) {
      rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      shinyWidgets::updatePrettyCheckbox(inputId = edit_id, value = FALSE)
    }
  }, ignoreInit = TRUE)
}

#' Wire the "export selected" / "export all" CSV download handlers for a table.
#'
#' @noRd
csv_download_outputs <- function(output, rv, selected, prefix, drop_cols) {
  observe({
    shinyjs::toggleState("export_selected", condition = length(selected()) > 0)
  })
  output$export_selected <- downloadHandler(
    filename = function() paste0(prefix, "_selected_", Sys.Date(), ".csv"),
    content = function(file) {
      req(length(selected()) > 0)
      rv$data |>
        dplyr::slice(selected()) |>
        dplyr::select(-dplyr::any_of(drop_cols)) |>
        write.csv(file, row.names = FALSE)
    }
  )
  output$export_all <- downloadHandler(
    filename = function() paste0(prefix, "_all_", Sys.Date(), ".csv"),
    content = function(file) {
      rv$data |>
        dplyr::select(-dplyr::any_of(drop_cols)) |>
        write.csv(file, row.names = FALSE)
    }
  )
}

#' Turn an R list into an HTML list
#'
#' @param list An R list
#' @param class a class for the list
#'
#' @return an HTML list
#' @noRd
#'
#' @examples
#' list_to_li(c("a", "b"))
#'
#' @importFrom shiny tags tagAppendAttributes tagList
list_to_li <- function(list, class = NULL) {
  if (is.null(class)) {
    tagList(
      lapply(
        list,
        tags$li
      )
    )
  } else {
    res <- lapply(
      list,
      tags$li
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MitoPilot"
    ),
    waiter::useWaiter(),
    rclipboard::rclipboardSetup(),
    shinyjs::useShinyjs()
  )
}
