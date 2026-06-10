#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
"%||%" <- function(x, y) {
  if (length(x) == 0L) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
`%nin%` <- Negate(`%in%`)
not_null <- Negate(is.null)
not_na <- Negate(is.na)

#' Collapse strings with a semicolon
#'
#' @param cur current string
#' @param new new string to append
#'
#' @noRd
semicolon_paste <- function(cur, new) {
  if (length(cur) > 1L) {
    return({
      purrr::map_chr(cur, ~ semicolon_paste(.x, new))
    })
  }
  if (length(cur) == 0L || is.na(cur) || nchar(cur) == 0L) {
    return(new)
  }
  paste(c(cur, new), collapse = "; ")
}

#' Lightweight missing/extra gene tally from curation count rules
#'
#' Mirrors only the per-gene count checks in validate_* (a gene with fewer than
#' its minimum expected count is "missing"; more than its maximum is "extra").
#' Does not re-run validation. Used to keep the annotate/export count columns
#' current after in-app annotation edits.
#'
#' @param annotations data frame of (non-deleted) annotations
#' @param rules named list of per-gene rules (from curation params)
#' @param default_rules per-type default rules (from curation params)
#'
#' @return list(missing, extra), each a "; "-joined string or NA
#' @noRd
compute_missing_extra <- function(annotations, rules, default_rules) {
  if (length(rules) == 0L) {
    return(list(missing = NA_character_, extra = NA_character_))
  }
  eff <- rules |>
    purrr::map(~ utils::modifyList(default_rules[[.x$type]] %||% list(), .x))
  missing <- NA_character_
  extra <- NA_character_
  for (g in names(eff)) {
    n <- sum(annotations$gene == g, na.rm = TRUE)
    cnt <- eff[[g]]$count %||% 1
    if (n < min(cnt)) {
      missing <- semicolon_paste(missing, g)
    } else if (n > max(cnt)) {
      extra <- semicolon_paste(extra, g)
    }
  }
  list(missing = missing, extra = extra)
}

#' toJSON wrapper
#'
#' @param x a data.frame or tibble
#' @param dataframe whether to parse by columns (default) or rows
#' @param simplify_df simplify the json by removing columns with only `NA` and
#'   dereplicating values when all values are the same
#'
#' @noRd
#'
json_string <- function(x, dataframe = "columns", simplify_df = TRUE) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NULL)
  }
  if ("data.frame" %in% class(x) && simplify_df) {
    x <- x |>
      dplyr::select(dplyr::where(~ !all(is.na(.)))) |>
      purrr::map(~ {
        if (all(is.na(.x))) {
          return(NULL)
        }
        if (length(unique(.x)) == 1) {
          return(unique(.x))
        }
        return(.x)
      })
  }
  if (class(x) == "character") {
    x <- as.list(x)
  }
  jsonlite::toJSON(x, dataframe = dataframe, auto_unbox = T, null = "null") |>
    as.character()
}

#' Parse JSON and handle empty input
#'
#' @param .x a string to check
#'
#' @noRd
json_parse <- function(.x, tibble = F) {
  out <- tryCatch(
    jsonlite::fromJSON(.x %|NA|% "{}"),
    error = function(e) {
      .x
    }
  )
  if (tibble) {
    # Empty ("{}"), blank, or unparseable JSON -> empty data frame so callers
    # can rely on nrow()/df ops (instead of a bare character that breaks them).
    if (length(out) == 0 || is.null(names(out))) {
      return(data.frame())
    }
    out <- purrr::map_dfc(out, ~.)
  }
  return(out)
}

#' Add columns
#'
#' Will add empty columns if they do not exist
#'
#' @param dat a data frame or tibble
#' @param cols a named list of column types where names will be the columns
#'
#' @noRd
add_cols <- function(dat, cols, .after = dplyr::everything()) {
  if (length(cols) == 0L) {
    return(dat)
  }
  dat |>
    dplyr::bind_rows(dplyr::tibble(!!!cols[!names(cols) %in% names(dat)], .rows = 0)) |>
    dplyr::relocate(names(cols), .after = .after)
}


#' Recursively modify nested list
#'
#' @param l named list to modify
#' @param alt named list of modifications
#'
#' @noRd
modify_list_recursive <- function(l, alt) {
  purrr::iwalk(alt, ~ {
    if (is.list(.x)) {
      l[[.y]] <<- modify_list_recursive(l[[.y]], .x)
    } else {
      l[[.y]] <<- .x
    }
  })
  return(l)
}
