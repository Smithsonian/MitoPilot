# Following links between GEOME, GBIF, and NCBI records

.link_first <- function(recs, fields, pattern, level = NULL) {
  keep <- recs$field %in% fields & (if (is.null(level)) TRUE else recs$level %in% level)
  r <- recs[keep, , drop = FALSE]
  r <- r[order(match(r$field, fields), r$depth), , drop = FALSE]
  for (i in seq_len(nrow(r))) {
    m <- regmatches(r$value[i], regexpr(pattern, r$value[i]))
    if (length(m)) return(list(value = m, field = r$field[i], level = r$level[i]))
  }
  NULL
}

.link_species <- function(x) {
  w <- strsplit(tolower(trimws(x %|NA|% "")), "\\s+")[[1]]
  paste(utils::head(w, 2), collapse = " ")
}

.gbif_find_voucher <- function(voucher, taxon) {
  p <- trimws(strsplit(voucher, ":", fixed = TRUE)[[1]])
  if (length(p) < 2 || !nzchar(p[1]) || !nzchar(p[length(p)])) return(NULL)
  inst <- p[1]
  cat <- p[length(p)]
  coll <- if (length(p) >= 3) p[2] else NA_character_
  want <- .link_species(taxon)
  genus_only <- !grepl(" ", want, fixed = TRUE)
  enc <- function(x) utils::URLencode(x, reserved = TRUE)
  queries <- c(paste0("occurrenceID=", enc(cat)),
               paste0("catalogNumber=", enc(paste(inst, cat))),
               paste0("catalogNumber=", enc(cat), "&institutionCode=", enc(inst)))
  for (q in queries) {
    res <- tryCatch(.gbif_get(paste0("occurrence/search?limit=50&", q))$results, error = function(e) NULL)
    if (!length(res)) next
    sp <- vapply(res, function(r) .link_species(r$scientificName %||% ""), "")
    keep <- if (genus_only) sub(" .*", "", sp) == want else sp == want
    if (!is.na(coll)) {
      keep <- keep & tolower(vapply(res, function(r) r$collectionCode %||% "", "")) == tolower(coll)
    }
    res <- res[keep]
    if (!length(res)) next
    spec <- vapply(res, function(r) identical(r$basisOfRecord, "PRESERVED_SPECIMEN"), logical(1))
    if (any(spec)) res <- res[spec]
    if (length(res) == 1L) return(list(ref = .meta_chr(res[[1]]$key), via = voucher))
    return(list(note = paste0(length(res), " possible GBIF matches for ", voucher, "; not linked")))
  }
  NULL
}

.geome_fastq_biosample <- function(bcid) {
  kids <- .geome_get(paste0("records/", bcid), list(includeChildren = "true"))$children
  for (k in kids) {
    acc <- k$bioSample$accession
    if (!is.null(acc) && nzchar(acc)) return(acc)
  }
  NA_character_
}

.meta_link_candidates <- function(source, recs, taxon, cache = new.env()) {
  out <- list()
  lab <- function(hit) paste(source, hit$level, hit$field)
  ezid <- "ark:/65665/3[0-9a-fA-F-]+"
  geome <- "ark:/21547/[A-Za-z0-9._~:-]+"
  voucher_link <- function(fields, level = NULL) {
    h <- .link_first(recs, fields, "^[^:]+:.+$", level)
    if (is.null(h)) return(NULL)
    v <- tryCatch(.gbif_find_voucher(h$value, taxon), error = function(e) NULL)
    if (!is.null(v$ref)) v$via <- paste(lab(h), v$via)
    v
  }
  if (source == "NCBI") {
    h <- .link_first(recs, "bcid", geome, "BioSample")
    if (!is.null(h)) out$GEOME <- list(ref = h$value, via = lab(h))
    h <- .link_first(recs, c("voucherURI", "catalogNumber"), ezid, "BioSample")
    out$GBIF <- if (!is.null(h)) list(ref = h$value, via = lab(h)) else
      voucher_link(c("specimen_voucher", "genbankSpecimenVoucher"), "BioSample")
  }
  if (source == "GEOME" && nrow(recs)) {
    t <- recs[recs$depth == min(recs$depth), , drop = FALSE]
    bs <- tryCatch(.geome_fastq_biosample(t$ref[1]), error = function(e) NA_character_)
    if (!is.na(bs)) out$NCBI <- list(ref = bs, via = paste("GEOME", t$level[1], "sequencing record"))
    h <- .link_first(recs, c("voucherURI", "catalogNumber"), ezid)
    out$GBIF <- if (!is.null(h)) list(ref = h$value, via = lab(h)) else
      voucher_link(c("genbankSpecimenVoucher", "materialSampleID"))
  }
  if (source == "GBIF") {
    h <- .link_first(recs, "associatedSequences", "SAM(N|EA|D)[0-9]+", "Occurrence") %||%
      .link_first(recs, "associatedSequences", "[SED]RR[0-9]+", "Occurrence")
    if (!is.null(h)) out$NCBI <- list(ref = h$value, via = lab(h))
    h <- .link_first(recs, c("occurrenceID", "materialSampleID", "catalogNumber"), geome, "Occurrence")
    if (!is.null(h)) out$GEOME <- list(ref = h$value, via = lab(h))
  }
  Filter(Negate(is.null), out)
}

.meta_link_sample <- function(con, id, caches = lapply(META_SOURCES, function(x) new.env())) {
  done <- character()
  taxon <- DBI::dbGetQuery(con, "SELECT Taxon FROM samples WHERE ID = ?", params = list(id))$Taxon[1]
  for (pass in 1:2) {
    ok <- DBI::dbGetQuery(con, "SELECT source FROM meta_status WHERE ID = ? AND status = 'ok'",
                          params = list(id))$source
    todo <- setdiff(ok, done)
    if (!length(todo)) break
    for (src in todo) {
      done <- c(done, src)
      recs <- DBI::dbGetQuery(con, "SELECT level, depth, ref, field, value FROM meta_records
                                    WHERE ID = ? AND source = ?", params = list(id, src))
      cands <- tryCatch(.meta_link_candidates(src, recs, taxon, caches[[src]]), error = function(e) list())
      for (tgt in names(cands)) {
        cand <- cands[[tgt]]
        col <- META_SOURCES[[tgt]]$col
        cur <- DBI::dbGetQuery(con, paste0("SELECT ", col, " AS v FROM samples WHERE ID = ?"),
                               params = list(id))$v[1]
        linked <- DBI::dbGetQuery(con, "SELECT ref FROM meta_links WHERE ID = ? AND source = ?",
                                  params = list(id, tgt))$ref
        linked <- length(linked) && !is.na(linked[1])
        if (!is.null(cand$note)) {
          if (is.na(cur)) {
            DBI::dbExecute(con, "INSERT OR REPLACE INTO meta_links VALUES (?, ?, NULL, NULL, ?)",
                           params = list(id, tgt, cand$note))
          }
          next
        }
        ref <- META_SOURCES[[tgt]]$normalize(cand$ref)
        if (is.na(ref)) next
        if (is.na(cur)) {
          DBI::dbExecute(con, paste0("UPDATE samples SET ", col, " = ? WHERE ID = ?"), params = list(ref, id))
          DBI::dbExecute(con, "INSERT OR REPLACE INTO meta_links VALUES (?, ?, ?, ?, NULL)",
                         params = list(id, tgt, ref, cand$via))
          if (!tgt %in% done) suppressWarnings(.meta_fetch_into(con, tgt, id, ref, caches[[tgt]]))
        } else if (cur != ref && !linked) {
          DBI::dbExecute(con, "INSERT OR REPLACE INTO meta_links VALUES (?, ?, NULL, NULL, ?)", params = list(
            id, tgt, paste0(src, " record links to ", ref, " (", cand$via, "); kept your ID ", cur)))
        }
      }
    }
  }
  invisible(NULL)
}
