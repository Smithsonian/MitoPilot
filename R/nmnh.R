# NMNH GenBank source modifiers: [specimen_voucher=] and [voucherURI=]
# Shared with RiboPilot: keep the two copies identical.

nmnh_tokens <- function() " [specimen_voucher={nmnh_specimen_voucher}] [voucherURI={nmnh_voucherURI}]"

nmnh_template_on <- function(template) {
  isTRUE(grepl("{nmnh_specimen_voucher}", template, fixed = TRUE) &&
           grepl("{nmnh_voucherURI}", template, fixed = TRUE))
}

.nmnh_mod_re <- "\\[\\s*(specimen[-_ ]?voucher|voucher[-_ ]?uri)\\s*=[^]]*\\]"

nmnh_existing_mods <- function(template) {
  regmatches(template, gregexpr(.nmnh_mod_re, template, ignore.case = TRUE))[[1]]
}

# Tokens go after the last [modifier] so the title text stays last
nmnh_template_add <- function(template) {
  t <- gsub(paste0("\\s*", .nmnh_mod_re), "", template, ignore.case = TRUE)
  p <- max(gregexpr("]", t, fixed = TRUE)[[1]])
  if (p < 0) return(paste0(t, nmnh_tokens()))
  paste0(substr(t, 1, p), nmnh_tokens(), substr(t, p + 1L, nchar(t)))
}

nmnh_template_remove <- function(template) sub(nmnh_tokens(), "", template, fixed = TRUE)

nmnh_strip_empty <- function(x) gsub("\\s*\\[(specimen_voucher|voucherURI)=\\s*\\]", "", x)

# GBIF collectionCode (upper case) -> NCBI BioCollections code
.NMNH_CODES <- c(FISH = "FISH", BIRDS = "Birds", MAMM = "MAMM", HERP = "Herp", IZ = "IZ",
                 ENT = "ENT", BOTANY = "Botany")

nmnh_normalize_voucher <- function(x, coll = NA_character_) {
  bad <- function(note) list(value = NA_character_, fixed = FALSE, note = note)
  x <- trimws(x %|NA|% "")
  if (!nzchar(x)) return(bad("missing"))
  p <- .parse_voucher(x)
  if (is.null(p)) return(bad(paste0("'", x, "' is not a voucher (expected USNM:<collection>:<catalog>)")))
  inst <- toupper(p$inst)
  cat <- sub("^(USNM|US)[ :_-]+", "", p$cat, ignore.case = TRUE)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9.-]*$", cat)) return(bad(paste0("'", x, "' has a bad catalog number")))
  if (inst == "US") {
    out <- paste0("US:", cat)
  } else if (inst == "USNM") {
    k <- toupper(p$coll %|NA|% coll %|NA|% "")
    if (k == "LAB") return(bad(paste0("'", x, "' is a USNM:LAB tissue (bio_material), not a specimen voucher")))
    if (!nzchar(k)) return(bad(paste0("'", x, "' has no USNM collection code")))
    if (!k %in% names(.NMNH_CODES)) return(bad(paste0("'", x, "' has an unknown USNM collection code")))
    out <- paste("USNM", .NMNH_CODES[[k]], cat, sep = ":")
  } else {
    return(bad(paste0("'", x, "' is not an NMNH voucher")))
  }
  list(value = out, fixed = out != x, note = if (out != x) paste0("fixed from '", x, "'"))
}

nmnh_normalize_uri <- function(x) {
  x <- trimws(x %|NA|% "")
  if (!nzchar(x)) return(list(value = NA_character_, fixed = FALSE, note = "missing"))
  ark <- .nmnh_normalize_ark(utils::URLdecode(x))
  if (is.na(ark)) {
    note <- if (grepl("65665/m3", x, fixed = TRUE)) "is a media ARK, not a specimen" else "is not an NMNH EZID"
    return(list(value = NA_character_, fixed = FALSE, note = paste0("'", x, "' ", note)))
  }
  out <- paste0("http://n2t.net/", ark)
  list(value = out, fixed = out != x, note = if (out != x) paste0("fixed from '", x, "'"))
}

.nmnh_occ_voucher <- function(inst, coll, cat) {
  if (is.null(cat) || is.null(inst)) return(NA_character_)
  nmnh_normalize_voucher(paste(inst, coll %||% "", cat, sep = ":"))$value
}

# GBIF facts for an EZID: list(basis, parent, voucher), or NULL when GBIF is unreachable
nmnh_gbif_check <- function(uri, con) {
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS nmnh_ark_cache (ark TEXT PRIMARY KEY,
    basis TEXT, parent_ark TEXT, voucher TEXT, checked_at INTEGER)")
  hit <- DBI::dbGetQuery(con, "SELECT * FROM nmnh_ark_cache WHERE ark = ?", params = list(uri))
  if (nrow(hit)) return(list(basis = hit$basis, parent = hit$parent_ark, voucher = hit$voucher))
  got <- tryCatch(.gbif_get(paste0("occurrence/search?limit=2&occurrenceID=",
                                   utils::URLencode(uri, reserved = TRUE))),
                  error = function(e) NULL)
  if (is.null(got)) return(NULL)
  r <- if (length(got$results) == 1L) got$results[[1]] else list()
  parent <- NA_character_
  for (rel in r$extensions[["http://rs.tdwg.org/dwc/terms/ResourceRelationship"]]) {
    g <- regmatches(rel[["http://rs.tdwg.org/dwc/terms/relatedResourceID"]] %||% "",
                    regexec("guid=([^&]+)", rel[["http://rs.tdwg.org/dwc/terms/relatedResourceID"]] %||% ""))[[1]]
    if (length(g)) parent <- nmnh_normalize_uri(g[2])$value
    if (!is.na(parent)) break
  }
  out <- list(basis = r$basisOfRecord %||% NA_character_, parent = parent,
              voucher = .nmnh_occ_voucher(r$institutionCode, r$collectionCode, r$catalogNumber))
  DBI::dbExecute(con, "INSERT OR REPLACE INTO nmnh_ark_cache VALUES (?, ?, ?, ?, ?)",
                 params = list(uri, out$basis, out$parent, out$voucher, as.integer(Sys.time())))
  out
}

# Mapfile columns holding the voucher and the URI (NA = none)
nmnh_columns <- function(con) {
  .meta_ensure_tables(con)
  have <- DBI::dbListFields(con, "samples")
  m <- DBI::dbGetQuery(con, "SELECT concept, column FROM meta_csv_map")
  pick <- function(k, names) {
    if (k %in% m$concept) {
      v <- m$column[m$concept == k]
      return(if (!is.na(v) && v %in% have) v else NA_character_)
    }
    hit <- have[match(tolower(names), tolower(have))]
    hit <- hit[!is.na(hit)]
    if (length(hit)) hit[1] else NA_character_
  }
  list(voucher = pick("nmnh_voucher", .SPEC_CSV_NAMES$voucher),
       uri = pick("nmnh_uri", c("voucherURI", "voucher_uri", "occurrenceID", "ezid", "ark")))
}

# "" stores "none"; NA returns the concept to detection by name
nmnh_set_columns <- function(con, voucher = NA, uri = NA) {
  .meta_ensure_tables(con)
  vals <- list(nmnh_voucher = voucher, nmnh_uri = uri)
  for (k in names(vals)) {
    if (is.na(vals[[k]])) {
      DBI::dbExecute(con, "DELETE FROM meta_csv_map WHERE concept = ?", params = list(k))
    } else {
      DBI::dbExecute(con, "INSERT OR REPLACE INTO meta_csv_map VALUES (?, ?)", params = list(k, vals[[k]]))
    }
  }
  invisible(NULL)
}

.nmnh_any_ark <- function(recs) {
  a <- .nmnh_normalize_ark(recs$value)
  a <- a[!is.na(a)]
  if (length(a)) paste0("http://n2t.net/", a[1]) else NA_character_
}

# Per-sample NMNH values. The only place NMNH edits land: mapping-file
# columns are read, never written.
.nmnh_ensure_table <- function(con) {
  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS nmnh_vouchers (ID TEXT PRIMARY KEY,",
    "specimen_voucher TEXT, specimen_voucher_source TEXT,",
    "voucherURI TEXT, voucherURI_source TEXT, updated INTEGER)"))
  invisible(NULL)
}

# Sources set by the user; automatic lookups never overwrite these
NMNH_USER_SOURCES <- c("entered", "upload")

.nmnh_field_col <- function(field) if (field == "voucher") "specimen_voucher" else "voucherURI"

# Store a user value for one sample; NA clears it back to automatic
nmnh_set_user <- function(con, id, field, value, source) {
  .nmnh_ensure_table(con)
  col <- .nmnh_field_col(field)
  DBI::dbExecute(con, "INSERT OR IGNORE INTO nmnh_vouchers (ID) VALUES (?)", params = list(id))
  DBI::dbExecute(con, sprintf("UPDATE nmnh_vouchers SET %s = ?, %s_source = ?, updated = ? WHERE ID = ?",
                              col, col),
                 params = list(value, if (is.na(value)) NA_character_ else source,
                               as.integer(Sys.time()), id))
  invisible(NULL)
}

# Check and store one value typed or uploaded by the user. Blank clears it back
# to automatic. Returns NULL when stored, else the reason it was refused.
nmnh_edit_value <- function(con, id, field, value, source = "entered") {
  value <- trimws(value %|NA|% "")
  if (!nzchar(value)) {
    nmnh_set_user(con, id, field, NA_character_, NA_character_)
    return(NULL)
  }
  n <- if (field == "voucher") nmnh_normalize_voucher(value) else nmnh_normalize_uri(value)
  if (is.na(n$value)) return(n$note)
  nmnh_set_user(con, id, field, n$value, source)
  NULL
}

# The voucher report as a CSV for bulk editing (fixed column names)
nmnh_download_df <- function(con, r) {
  s <- DBI::dbGetQuery(con, "SELECT ID, Taxon FROM samples")
  data.frame(ID = r$ID, Taxon = s$Taxon[match(r$ID, s$ID)],
             specimen_voucher = r$nmnh_specimen_voucher, voucherURI = r$nmnh_voucherURI)
}

# Read an edited voucher CSV into nmnh_vouchers. Blank cells leave a value as
# it is. Returns the IDs updated and one line per refused value.
nmnh_upload_csv <- function(con, file) {
  up <- utils::read.csv(file, check.names = FALSE, colClasses = "character")
  if (!"ID" %in% names(up)) stop("The CSV needs an ID column.", call. = FALSE)
  fields <- c(voucher = "specimen_voucher", uri = "voucherURI")
  fields <- fields[fields %in% names(up)]
  if (!length(fields)) {
    stop("The CSV needs a specimen_voucher or voucherURI column.", call. = FALSE)
  }
  known <- DBI::dbGetQuery(con, "SELECT ID FROM samples")$ID
  ids <- bad <- character()
  for (i in seq_len(nrow(up))) {
    id <- up$ID[i]
    if (!id %in% known) {
      bad <- c(bad, paste0(id, ": not a sample in this project"))
      next
    }
    for (f in names(fields)) {
      v <- trimws(up[[fields[[f]]]][i] %|NA|% "")
      if (!nzchar(v)) next
      note <- nmnh_edit_value(con, id, f, v, "upload")
      if (is.null(note)) ids <- c(ids, id) else bad <- c(bad, paste0(id, ": ", note))
    }
  }
  list(ids = unique(ids), bad = bad)
}

# Plain-language name for a stored source code
nmnh_source_label <- function(src) {
  out <- src
  m <- !is.na(src) & startsWith(src, "mapfile:")
  out[m] <- sprintf("mapping file column \"%s\"", substring(src[m], 9))
  out[src %in% "entered"] <- "typed in the report"
  out[src %in% "upload"] <- "uploaded CSV"
  out
}

nmnh_resolve <- function(con, ids, cols = nmnh_columns(con), online = TRUE, save = TRUE) {
  .meta_ensure_tables(con)
  .nmnh_ensure_table(con)
  stored <- DBI::dbReadTable(con, "nmnh_vouchers")
  s <- DBI::dbReadTable(con, "samples")
  recs <- DBI::dbGetQuery(con, "SELECT ID, source, level, depth, field, value FROM meta_records")
  rows <- lapply(ids, function(id) {
    row <- s[s$ID == id, , drop = FALSE]
    r <- recs[recs$ID == id, , drop = FALSE]
    src <- function(x) r[r$source == x, , drop = FALSE]
    col <- function(cc) if (is.na(cc) || !cc %in% names(row)) NA_character_ else .meta_chr(row[[cc]])[1]
    gb <- src("GBIF")
    st <- stored[stored$ID == id, , drop = FALSE]
    user <- function(col) {
      if (nrow(st) && isTRUE(st[[paste0(col, "_source")]] %in% NMNH_USER_SOURCES)) {
        stats::setNames(st[[col]], st[[paste0(col, "_source")]])
      }
    }
    mf <- function(cc) stats::setNames(col(cc), if (is.na(cc)) "mapfile" else paste0("mapfile:", cc))
    vc <- c(user("specimen_voucher"), mf(cols$voucher), GBIF = GBIF_COMBOS$specimen_voucher$fn(gb),
            NCBI = NCBI_COMBOS$specimen_voucher$fn(src("NCBI")),
            GEOME = GEOME_COMBOS$specimen_voucher$fn(src("GEOME")))
    uc <- c(user("voucherURI"), mf(cols$uri), GBIF = .gbif_occ(gb, "occurrenceID"),
            NCBI = .nmnh_any_ark(src("NCBI")), GEOME = .nmnh_any_ark(src("GEOME")))
    first <- function(cand, norm) {
      notes <- character()
      for (k in names(cand)) {
        if (is.na(cand[[k]]) || !nzchar(trimws(cand[[k]]))) next
        n <- norm(cand[[k]])
        if (!is.na(n$value)) {
          return(list(value = n$value, source = k, fixed = n$fixed,
                      notes = c(notes, if (n$fixed) n$note)))
        }
        notes <- c(notes, paste(k, n$note, "(skipped)"))
      }
      list(value = NA_character_, source = NA_character_, fixed = FALSE, notes = notes)
    }
    v <- first(vc, nmnh_normalize_voucher)
    u <- first(uc, nmnh_normalize_uri)
    v_bad <- u_bad <- character()
    checked <- FALSE
    if (online && is.na(u$value) && !is.na(v$value)) {
      f <- tryCatch(.gbif_find_voucher(v$value, row$Taxon[1]), error = function(e) NULL)
      occ <- if (!is.null(f$ref)) tryCatch(.gbif_get(paste0("occurrence/", f$ref))$occurrenceID,
                                           error = function(e) NULL)
      n <- nmnh_normalize_uri(occ %||% NA_character_)
      if (!is.na(n$value)) u[c("value", "source")] <- list(n$value, "GBIF via catalog number")
    }
    if (online && !is.na(u$value)) {
      chk <- nmnh_gbif_check(u$value, con)
      if (!is.null(chk) && identical(chk$basis, "MATERIAL_SAMPLE") && !is.na(chk$parent)) {
        u$value <- chk$parent
        u$fixed <- TRUE
        u$notes <- c(u$notes, "tissue ARK replaced with parent specimen")
        chk <- nmnh_gbif_check(u$value, con)
      }
      if (!is.null(chk)) {
        checked <- TRUE
        if (is.na(chk$basis)) {
          u_bad <- "GBIF has no record for this URI"
        } else if (chk$basis == "MATERIAL_SAMPLE") {
          u_bad <- "URI is a tissue record with no parent specimen"
        } else if (chk$basis != "PRESERVED_SPECIMEN") {
          u_bad <- paste0("URI is a ", chk$basis, " record, not a specimen")
        }
        if (!length(u_bad) && !is.na(chk$voucher)) {
          if (is.na(v$value)) {
            v[c("value", "source")] <- list(chk$voucher, "GBIF via specimen link")
          } else if (!identical(toupper(chk$voucher), toupper(v$value))) {
            v_bad <- paste0("does not match the GBIF record of the URI (", chk$voucher, ")")
          }
        }
      }
    }
    if (is.na(v$value)) v_bad <- c(v_bad, "missing")
    if (is.na(u$value)) u_bad <- c(u_bad, "missing")
    data.frame(ID = id, nmnh_specimen_voucher = v$value, nmnh_voucherURI = u$value,
               voucher_source = v$source, uri_source = u$source,
               voucher_note = paste(c(v_bad, v$notes), collapse = "; "),
               uri_note = paste(c(u_bad, u$notes), collapse = "; "),
               ok = !length(c(v_bad, u_bad)), voucher_ok = !length(v_bad), uri_ok = !length(u_bad),
               fixed = v$fixed || u$fixed || length(c(v$notes, u$notes)) > 0,
               checked = checked)
  })
  res <- dplyr::as_tibble(do.call(rbind, rows))
  if (save) .nmnh_save(con, res, stored)
  res
}

# Store resolved values, leaving user-set fields alone
.nmnh_save <- function(con, res, stored) {
  now <- as.integer(Sys.time())
  DBI::dbWithTransaction(con, for (i in seq_len(nrow(res))) {
    id <- res$ID[i]
    DBI::dbExecute(con, "INSERT OR IGNORE INTO nmnh_vouchers (ID) VALUES (?)", params = list(id))
    st <- stored[stored$ID == id, , drop = FALSE]
    for (f in list(c("specimen_voucher", "nmnh_specimen_voucher", "voucher_source"),
                   c("voucherURI", "nmnh_voucherURI", "uri_source"))) {
      if (nrow(st) && isTRUE(st[[paste0(f[1], "_source")]] %in% NMNH_USER_SOURCES)) next
      DBI::dbExecute(con, sprintf("UPDATE nmnh_vouchers SET %s = ?, %s_source = ?, updated = ? WHERE ID = ?",
                                  f[1], f[1]),
                     params = list(res[[f[2]]][i], res[[f[3]]][i], now, id))
    }
  })
  invisible(NULL)
}
