EXPORT_TOKEN_BASICS <- c("seqid", "ID", "Taxon", "genetic_code", "topology", "completeness",
                         "path", "scaffold")
EXPORT_TOKEN_REFERENCE <- c("blast_accession", "blast_ref_status", "blast_species", "blast_lineage")
EXPORT_TOKEN_ASSEMBLY <- c("length", "structure", "PCGCount", "tRNACount", "rRNACount", "ORFCount",
                           "missing", "extra", "warnings", "partial", "curate_opts")

#' Header-template text a metadata key inserts: `{col}` for a raw key, or
#' `[mod={col}]` for a combo (mod defaults to the combo name)
#' @noRd
meta_export_token <- function(keys) {
  vapply(keys, function(k) {
    col <- .meta_key_col(k)
    p <- strsplit(k, ":", fixed = TRUE)[[1]]
    if (p[2] != "combo") paste0("{", col, "}")
    else paste0("[", .meta_combos(p[1])[[p[3]]]$mod %||% p[3], "={", col, "}]")
  }, character(1), USE.NAMES = FALSE)
}

#' Group the columns usable in a header template for the Export Data modal
#'
#' @param data rows about to be exported (first row supplies the hover example;
#'   `export_group`, when present, gives the per-group missing counts)
#' @param sample_cols column names of the `samples` table
#' @param ticked_keys keys from `meta_export_fields`
#' @return named list of groups, each `list(open, tokens, hint)`; tokens carry
#'   `missing`, a JSON object of missing-value counts per export group
#' @noRd
export_token_groups <- function(data, sample_cols, ticked_keys) {
  ex <- function(col) {
    if (!nrow(data) || !col %in% names(data)) return("")
    v <- data[[col]][1]
    if (is.na(v)) "" else as.character(v)
  }
  grp <- if ("export_group" %in% names(data)) as.character(data$export_group) else rep("", nrow(data))
  miss <- function(col) {
    # a blank missing/extra gene list means none, not missing data
    if (!nrow(data) || !col %in% names(data) || col %in% c("missing", "extra")) return("{}")
    v <- trimws(as.character(data[[col]]))
    jsonlite::toJSON(as.list(tapply(is.na(v) | !nzchar(v), grp, sum)), auto_unbox = TRUE)
  }
  tok <- function(cols, insert) {
    data.frame(token = cols, insert = insert,
               example = vapply(cols, ex, character(1), USE.NAMES = FALSE),
               missing = vapply(cols, miss, character(1), USE.NAMES = FALSE))
  }
  plain <- function(cols) {
    cols <- cols[cols %in% names(data)]
    if (!length(cols)) return(tok(character(), character()))
    tok(cols, paste0("{", cols, "}"))
  }
  meta <- function(prefix, id_col) {
    keys <- ticked_keys[startsWith(ticked_keys, paste0(prefix, ":"))]
    cols <- vapply(keys, .meta_key_col, character(1), USE.NAMES = FALSE)
    keep <- cols %in% names(data)
    keys <- keys[keep]
    cols <- cols[keep]
    ticked <- if (length(cols)) {
      tok(cols, meta_export_token(keys))
    } else {
      tok(character(), character())
    }
    # the ID column joins its database group only when that group is in use;
    # otherwise it stays with the mapping columns
    list(tokens = if (nrow(ticked)) rbind(plain(id_col), ticked) else ticked,
         n = nrow(ticked), id = if (nrow(ticked)) id_col)
  }
  pick_hint <- "Nothing ticked yet. Use Choose fields, or the Export column of the Metadata button."
  csv <- setdiff(export_metadata_cols(sample_cols, character()), EXPORT_TOKEN_BASICS)
  geome <- meta("geome", "GEOME_BCID")
  gbif <- meta("gbif", "GBIF_ID")
  ncbi <- meta("ncbi", "BioSample")
  csv_tokens <- plain(setdiff(csv, c(geome$id, gbif$id, ncbi$id)))
  list(
    Basics = list(open = TRUE, tokens = plain(EXPORT_TOKEN_BASICS), hint = NULL),
    `Your mapfile columns` = list(
      open = TRUE, tokens = csv_tokens,
      hint = if (!nrow(csv_tokens)) "Your mapping file has no extra columns." else NULL),
    GEOME = list(open = geome$n > 0, tokens = geome$tokens,
                 hint = if (!geome$n) pick_hint else NULL),
    GBIF = list(open = gbif$n > 0, tokens = gbif$tokens,
                hint = if (!gbif$n) pick_hint else NULL),
    NCBI = list(open = ncbi$n > 0, tokens = ncbi$tokens,
                hint = if (!ncbi$n) pick_hint else NULL),
    `Reference (BLAST)` = list(open = FALSE, tokens = plain(EXPORT_TOKEN_REFERENCE), hint = NULL),
    `Assembly and annotation` = list(open = FALSE, tokens = plain(EXPORT_TOKEN_ASSEMBLY), hint = NULL)
  )
}

#' Summary line for the Available metadata panel, with the field count
#' @noRd
meta_panel_summary <- function(groups, help = NULL) {
  n <- sum(vapply(groups, function(x) nrow(x$tokens), integer(1)))
  tags$summary(icon("database"), " Available metadata",
               if (n) span(class = "mp-meta-count", sprintf(" (%d fields)", n)),
               if (!is.null(help)) mp_help_tip(help, label = "Available metadata"))
}

#' Link beside a header box label that opens the Available metadata panel
#' @noRd
insert_field_link <- function(box_id) {
  tags$a(href = "#", class = "mp-insert-field", `data-box` = box_id, "Insert field")
}

#' Clickable token chips for the Export Data modal
#'
#' @param groups `export_token_groups()` output
#' @param target_id DOM id of the header box chips insert into by default
#' @param ns module namespace function (for the picker links)
#' @noRd
export_token_ui <- function(groups, target_id, ns, totals = "{}") {
  div(
    class = "mp-token-list", `data-target` = target_id, `data-totals` = totals,
    tags$input(type = "search", class = "form-control input-sm mp-token-filter",
               placeholder = "Filter columns", `aria-label` = "Filter columns"),
    p(class = "mp-token-lead", "Columns by type:"),
    lapply(names(groups), function(g) {
      x <- groups[[g]]
      link <- if (g %in% names(META_SOURCES)) {
        tagList(" ", actionLink(ns(paste0("token_fields_", tolower(g))), "Choose fields"))
      }
      tags$details(
        open = if (isTRUE(x$open)) NA else NULL,
        class = paste0("mp-token-group mp-src-", token_src_slug(g)),
        tags$summary(span(class = "mp-src-dot"), g, link),
        if (!is.null(x$hint)) p(class = "text-muted mp-token-hint", x$hint),
        div(class = "mp-token-chips", lapply(seq_len(nrow(x$tokens)), function(i) {
          tip <- paste0("Inserts ", x$tokens$insert[i],
                        if (nzchar(x$tokens$example[i])) paste0("\nFirst row: ", x$tokens$example[i]))
          tags$button(
            type = "button", class = "mp-token-chip", `data-insert` = x$tokens$insert[i],
            `data-missing` = x$tokens$missing[i], `data-title` = tip, title = tip,
            x$tokens$token[i]
          )
        }))
      )
    })
  )
}

#' Colour key for a token group in the Available metadata panel
#' @noRd
token_src_slug <- function(group) {
  switch(group, Basics = "basic", GEOME = "geome", GBIF = "gbif", NCBI = "ncbi",
         if (grepl("map", group, ignore.case = TRUE)) "mapfile" else "pipeline")
}

#' Live preview of a FASTA header template for the first record of `data`
#' @noRd
hdr_preview_ui <- function(template, data) {
  if (is.null(template) || !nzchar(trimws(template))) {
    return(span(class = "text-muted", "Empty header."))
  }
  if (is.null(data) || !nrow(data)) return(span(class = "text-muted", "No records in this group."))
  row <- data[1, , drop = FALSE]
  parts <- regmatches(template, gregexpr("\\{[^{}]*\\}", template), invert = NA)[[1]]
  # one string, since the box keeps whitespace and tag indentation would show
  body <- vapply(parts[nzchar(parts)], function(p) {
    if (!grepl("^\\{[^{}]*\\}$", p)) return(htmltools::htmlEscape(p))
    v <- tryCatch(as.character(stringr::str_glue_data(row, p)), error = function(e) NULL)
    tok <- if (is.null(v)) {
      span(class = "mp-hl-tok mp-hl-bad", `data-tok` = p, title = "Unknown field", p)
    } else if (!length(v) || is.na(v) || !nzchar(v) || v == "NA") {
      span(class = "mp-hl-tok mp-hl-empty", `data-tok` = p, title = p, "(empty)")
    } else {
      span(class = "mp-hl-tok", `data-tok` = p, title = p, v)
    }
    as.character(tok)
  }, character(1))
  div(class = "mp-hdr-preview-body", title = paste("First record:", row$ID[1]),
      HTML(paste(body, collapse = "")))
}

#' Plain-language line saying where NMNH values came from
#' @noRd
nmnh_source_text <- function(src, col) {
  lab <- c(mapfile = if (is.na(col)) "your mapping file" else sprintf("mapping file column \"%s\"", col),
           entered = "values typed in the report", GBIF = "GBIF", NCBI = "NCBI", GEOME = "GEOME",
           `derived from voucher` = "GBIF via catalog number",
           `derived from URI` = "GBIF via specimen link")
  tab <- table(factor(src[!is.na(src)], levels = names(lab)))
  tab <- tab[tab > 0]
  miss <- sum(is.na(src))
  if (!length(tab)) return("not found yet. Add GBIF, NCBI, or GEOME IDs to your samples, or pick a mapping file column below.")
  paste0("from ", paste(sprintf("%s (%d)", lab[names(tab)], tab), collapse = ", "),
         if (miss) sprintf("; missing for %d", miss), ".")
}
