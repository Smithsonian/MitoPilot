#' Resolve the NCBI genetic code (translation table) for a curation ruleset
#'
#' Genetic code auto-selects from the curation ruleset (`target`). When an
#' explicit override is supplied (the `genetic_code` column of `curate_opts`),
#' it wins; otherwise the code is the default argument of the matching
#' `curate_<target>()` wrapper. Falls back to 2 (Vertebrate Mitochondrial).
#'
#' @param target character ruleset key, e.g. "fish_mito"
#' @param override optional explicit code; `NA`/`NULL` means auto-from-target
#'
#' @return integer genetic code
#' @noRd
resolve_genetic_code <- function(target, override = NA) {
  if (length(override) > 0 && !is.na(override) && nzchar(as.character(override))) {
    return(as.integer(override))
  }
  code <- tryCatch(
    as.integer(eval(formals(get(paste0("curate_", target), mode = "function"))$genetic_code)),
    error = function(e) NA_integer_
  )
  if (is.na(code)) 2L else code
}

#' Build genetic-code selectize choices for the curation options modal
#'
#' "" is the auto-from-ruleset option; its label shows the code the given
#' `target` resolves to. Explicit NCBI tables follow, labelled by
#' `genetic_code_name()`.
#'
#' @param target character ruleset key used to label the "Auto" option
#'
#' @return named character vector (labels -> values) for `selectizeInput`
#' @noRd
gcode_choices <- function(target = "fish_mito") {
  codes <- c(1L, 2L, 3L, 4L, 5L, 9L, 13L, 14L, 21L, 24L)
  auto_code <- resolve_genetic_code(target, NA)
  vals <- c("", as.character(codes))
  labs <- c(
    paste0("Auto (from ruleset: ", auto_code, " - ", genetic_code_name(auto_code), ")"),
    vapply(codes, function(x) paste0(x, " - ", genetic_code_name(x)), character(1))
  )
  stats::setNames(vals, labs)
}

#' Recompute samples.genetic_code from each sample's curation ruleset
#'
#' `samples.genetic_code` is the canonical per-sample cache read by assembly,
#' export, the pipeline, and the app. This resolves it from the sample's
#' assigned `curate_opts` (target + optional override) and writes it back.
#' Call it whenever a target/override changes or a sample is reassigned to a
#' different curate_opts set.
#'
#' @param con DBI connection to the project database
#' @param ids optional character vector of sample IDs to update (default: all)
#'
#' @return invisibly, the updated data.frame of (ID, genetic_code)
#' @noRd
.sync_sample_genetic_codes <- function(con, ids = NULL) {
  annotate <- dplyr::tbl(con, "annotate") |>
    dplyr::select("ID", "curate_opts") |>
    dplyr::collect()
  if (!is.null(ids)) {
    annotate <- annotate[annotate$ID %in% ids, , drop = FALSE]
  }
  if (nrow(annotate) == 0) {
    return(invisible(NULL))
  }
  copts <- dplyr::tbl(con, "curate_opts") |>
    dplyr::select("curate_opts", "target", "genetic_code") |>
    dplyr::collect()

  merged <- dplyr::left_join(annotate, copts, by = "curate_opts")
  merged$genetic_code <- vapply(
    seq_len(nrow(merged)),
    function(i) resolve_genetic_code(merged$target[i], merged$genetic_code[i]),
    integer(1)
  )

  update <- data.frame(
    ID = merged$ID,
    genetic_code = as.integer(merged$genetic_code)
  )
  dplyr::tbl(con, "samples") |>
    dplyr::rows_update(
      update,
      unmatched = "ignore",
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )
  invisible(update)
}
