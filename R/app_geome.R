#' Left-join GEOME fetch status onto a lazy samples-keyed table
#'
#' @param tbl a lazy dplyr table keyed by `ID`
#' @param db database connection (source of `geome_status`)
#' @return `tbl` with `geome` ("ok" | "failed" | "none") and `geome_message` added
#' @noRd
.geome_status_join <- function(tbl, db) {
  .geome_ensure_tables(db)
  tbl |>
    dplyr::left_join(
      dplyr::tbl(db, "geome_status") |>
        dplyr::select(ID, geome_status = status, geome_message = message),
      by = "ID"
    ) |>
    dplyr::mutate(geome = dplyr::case_when(
      geome_status == "ok" ~ "ok",
      geome_status == "failed" ~ "failed",
      TRUE ~ "none"
    )) |>
    dplyr::select(-geome_status)
}

#' reactable cell renderer for the GEOME status column
#'
#' Renders a clickable icon; clicking sends the row's ID to `inputId`.
#'
#' @param inputId namespaced Shiny input id to receive the clicked row's ID
#' @noRd
rt_geome <- function(inputId) {
  sprintf(
    "function(cellInfo) {
      var st = cellInfo.value || 'none';
      var row = cellInfo.row || {};
      var esc = function(s) { return String(s).replace(/&/g, '&amp;').replace(/'/g, '&#39;')
        .replace(/\"/g, '&quot;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); };
      var cls = st === 'ok' ? 'fa-solid fa-earth-americas' :
        (st === 'failed' ? 'fa-solid fa-triangle-exclamation mp-fg-warning' : 'fa-regular fa-square-plus text-muted');
      var tip = st === 'ok' ? 'GEOME record fetched. Click to view.' :
        (st === 'failed' ? 'GEOME fetch failed: ' + (row['geome_message'] || 'unknown error') + '. Click to fix or retry.' :
        'No GEOME BCID. Click to add one.');
      return `<a href='#' class='mp-geome-cell' data-id='${esc(row['ID'])}' title='${esc(tip)}' aria-label='${esc(tip)}' ` +
        `onclick=\"event.preventDefault(); event.stopPropagation(); Shiny.setInputValue('%s', this.dataset.id, {priority: 'event'})\">` +
        `<i class='${cls}' aria-hidden='true'></i></a>`;
    }",
    inputId
  ) |>
    htmlwidgets::JS()
}

#' Shared colDef for the GEOME status column
#'
#' @param inputId namespaced Shiny input id to receive the clicked row's ID
#' @noRd
geome_col_def <- function(inputId, sticky = NULL) {
  reactable::colDef(
    show = TRUE, name = "GEOME", sticky = sticky, width = 70, align = "center",
    html = TRUE, filterable = FALSE, sortable = TRUE,
    header = rt_header("GEOME", "GEOME metadata for this sample. Click an icon to view, add, or refresh."),
    cell = rt_geome(inputId)
  )
}
