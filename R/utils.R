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

#' Inverted version of in
#'
#' @noRd
#'
`%nin%` <- Negate(`%in%`)

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
#' Parses by columns, and simplifies the json by removing columns with only
#' `NA` and dereplicating values when all values are the same.
#'
#' @param x a data.frame or tibble
#'
#' @noRd
#'
json_string <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NULL)
  }
  if ("data.frame" %in% class(x)) {
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
  jsonlite::toJSON(x, dataframe = "columns", auto_unbox = T, null = "null") |>
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
add_cols <- function(dat, cols) {
  if (length(cols) == 0L) {
    return(dat)
  }
  dat |>
    dplyr::bind_rows(dplyr::tibble(!!!cols[!names(cols) %in% names(dat)], .rows = 0)) |>
    dplyr::relocate(names(cols), .after = dplyr::everything())
}


#' Back up the project SQLite database
#'
#' Copies `.sqlite` into `.old_sqlite_dbs/.sqlite.N`, incrementing `N` past any
#' existing backup.
#'
#' @param path project directory
#'
#' @noRd
backup_project_db <- function(path) {
  backup_dir <- file.path(path, ".old_sqlite_dbs")
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
    num <- 1
  } else {
    backups <- list.files(backup_dir, pattern = ".sqlite.*", full.names = FALSE, all.files = TRUE)
    num <- max(as.numeric(sapply(strsplit(backups, "[.]"), "[", 3))) + 1
  }
  backup <- file.path(backup_dir, paste0(".sqlite.", num))
  file.copy(file.path(path, ".sqlite"), backup)
  message("Backed up old SQLite database to: ", backup)
  invisible(backup)
}

#' Add any missing mapping columns to the samples table
#'
#' Returns `sample_table` with the new columns added as `NA`.
#'
#' @param con project database connection
#' @param mapping updated mapping data frame
#' @param sample_table existing samples table
#'
#' @noRd
add_missing_sample_cols <- function(con, mapping, sample_table) {
  new_cols <- subset(colnames(mapping), !(colnames(mapping) %in% colnames(sample_table)))
  if (length(new_cols) > 0) {
    for (col in new_cols) { # need to loop because SQLite doesn't allow multiple columns to be added in same statement
      glue::glue_sql(
        "ALTER TABLE samples
        ADD COLUMN {col}",
        col = col,
        .con = con
      ) |> DBI::dbExecute(con, statement = _)
      sample_table[, col] <- as.character(rep(NA, nrow(sample_table))) # add new column with NA values to dataframe object
    }
  }
  sample_table
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
