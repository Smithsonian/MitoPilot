EXPORT_TOKEN_BASICS <- c("seqid", "ID", "Taxon", "genetic_code", "topology", "completeness",
                         "path", "scaffold")
EXPORT_TOKEN_REFERENCE <- c("blast_accession", "blast_ref_status", "blast_species", "blast_lineage")
EXPORT_TOKEN_ASSEMBLY <- c("length", "structure", "PCGCount", "tRNACount", "rRNACount", "ORFCount",
                           "missing", "extra", "warnings", "partial", "curate_opts")

#' Group the columns usable in a header template for the Export Data modal
#'
#' @param data rows about to be exported (first row supplies the hover example)
#' @param sample_cols column names of the `samples` table
#' @param ticked_keys keys from `meta_export_fields`
#' @return named list of groups, each `list(open, tokens, hint)`
#' @noRd
export_token_groups <- function(data, sample_cols, ticked_keys) {
  ex <- function(col) {
    if (!nrow(data) || !col %in% names(data)) return("")
    v <- data[[col]][1]
    if (is.na(v)) "" else as.character(v)
  }
  tok <- function(cols, insert) {
    data.frame(token = cols, insert = insert,
               example = vapply(cols, ex, character(1), USE.NAMES = FALSE))
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
      combo <- grepl(":combo:", keys, fixed = TRUE)
      label <- sub("^[^:]+:combo:", "", keys)
      tok(cols, ifelse(combo, paste0("[", label, "={", cols, "}]"), paste0("{", cols, "}")))
    } else {
      tok(character(), character())
    }
    list(tokens = rbind(plain(id_col), ticked), n = nrow(ticked))
  }
  pick_hint <- "Nothing ticked yet. Use Metadata Export in the Export toolbar to add fields."
  csv <- setdiff(export_metadata_cols(sample_cols, character()), EXPORT_TOKEN_BASICS)
  geome <- meta("geome", "GEOME_BCID")
  gbif <- meta("gbif", "GBIF_ID")
  csv_tokens <- plain(csv)
  list(
    Basics = list(open = TRUE, tokens = plain(EXPORT_TOKEN_BASICS), hint = NULL),
    `Your mapfile columns` = list(
      open = TRUE, tokens = csv_tokens,
      hint = if (!nrow(csv_tokens)) "Your mapping file has no extra columns." else NULL),
    GEOME = list(open = geome$n > 0, tokens = geome$tokens,
                 hint = if (!geome$n) pick_hint else NULL),
    GBIF = list(open = gbif$n > 0, tokens = gbif$tokens,
                hint = if (!gbif$n) pick_hint else NULL),
    `Reference (BLAST)` = list(open = FALSE, tokens = plain(EXPORT_TOKEN_REFERENCE), hint = NULL),
    `Assembly and annotation` = list(open = FALSE, tokens = plain(EXPORT_TOKEN_ASSEMBLY), hint = NULL)
  )
}

#' Clickable token chips for the Export Data modal
#'
#' @param groups `export_token_groups()` output
#' @param target_id DOM id of the header box chips insert into by default
#' @param ns module namespace function (for the picker links)
#' @noRd
export_token_ui <- function(groups, target_id, ns) {
  div(
    class = "mp-token-list", `data-target` = target_id,
    tags$input(type = "search", class = "form-control input-sm mp-token-filter",
               placeholder = "Filter columns", `aria-label` = "Filter columns"),
    lapply(names(groups), function(g) {
      x <- groups[[g]]
      link <- if (g %in% c("GEOME", "GBIF")) {
        tagList(" ", actionLink(ns(paste0("token_fields_", tolower(g))), "Choose fields"))
      }
      tags$details(
        open = if (isTRUE(x$open)) NA else NULL, class = "mp-token-group",
        tags$summary(g, link),
        if (!is.null(x$hint)) p(class = "text-muted mp-token-hint", x$hint),
        div(class = "mp-token-chips", lapply(seq_len(nrow(x$tokens)), function(i) {
          tags$button(
            type = "button", class = "mp-token-chip", `data-insert` = x$tokens$insert[i],
            title = paste0("Inserts ", x$tokens$insert[i],
                           if (nzchar(x$tokens$example[i])) paste0("\nFirst row: ", x$tokens$example[i])),
            x$tokens$token[i]
          )
        }))
      )
    })
  )
}
