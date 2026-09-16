# Click-time guards. Each returns TRUE to continue, or explains why the click
# cannot proceed and returns FALSE. Replaces bare req() calls that used to abort
# silently (theme T01).

#' Shorten an ID list for a message: five IDs, then "and N more".
#' @noRd
mp_id_list <- function(ids, max = 5) {
  ids <- unique(ids)
  if (length(ids) <= max) return(paste(ids, collapse = ", "))
  paste0(paste(ids[seq_len(max)], collapse = ", "), ", and ", length(ids) - max, " more")
}

#' At least one row must be selected.
#' @noRd
need_selection <- function(n, session = getDefaultReactiveDomain()) {
  if (length(n) > 0 && !is.na(n[1]) && n[1] > 0) return(TRUE)
  mp_toast("Select one or more rows in the table first.", type = "warning", session = session)
  FALSE
}

#' None of the selected units may be locked. `locked_ids` are the IDs that are.
#' @noRd
need_unlocked <- function(locked_ids, noun = "sample",
                          session = getDefaultReactiveDomain()) {
  if (length(locked_ids) == 0) return(TRUE)
  plural <- sub("^\\S+ ", "", mp_n(2, noun))
  mp_alert(
    title = paste("Locked", plural, "cannot be edited"),
    text = paste0("Unlock these ", plural, " in the Lock column first: ",
                  mp_id_list(locked_ids), "."),
    type = "warning", session = session
  )
  FALSE
}

#' A row clicked outside a non-empty selection is not edited.
#' @noRd
row_in_selection <- function(row, sel, id, noun = "sample",
                             session = getDefaultReactiveDomain()) {
  if (length(sel) == 0 || row %in% sel) return(TRUE)
  mp_toast(
    paste0("Clear the current selection, or include ", id, " in it, to edit this ",
           noun, "'s options."),
    type = "warning", session = session
  )
  FALSE
}
