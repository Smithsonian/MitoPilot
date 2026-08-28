#' Refuse to export a sample contributing units from more than one assembly path.
#'
#' GetOrganelle paths are competing resolutions of one tangled graph, i.e.
#' alternative hypotheses about the SAME genome, so exporting them all would submit
#' several near-identical mitogenomes for a single specimen. Separate scaffolds
#' within one path are a different matter (they may be genuinely separate genomes),
#' so they are allowed here and only warned about in the app.
#'
#' @param units A frame of export units with ID and path columns.
#' @return Invisibly TRUE; errors listing the offending samples otherwise.
#' @noRd
check_single_path <- function(units) {
  multi_path <- units |>
    dplyr::distinct(ID, path) |>
    dplyr::count(ID, name = "n_paths") |>
    dplyr::filter(n_paths > 1)
  if (nrow(multi_path) > 0) {
    stop(
      "Cannot export ", nrow(multi_path), " sample(s) with more than one assembly path: ",
      paste(multi_path$ID, collapse = ", "),
      ".\nAssembly paths are alternative resolutions of the same genome, so exporting ",
      "each one would submit duplicate records for a single specimen. Open the ",
      "assembly details and 'ignore' all but the correct path, then export again.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Curation-options profile for one export unit
#'
#' A user-supplied assembly is annotated as one genome, so a single `annotate`
#' row covers every contig of the sample and a sibling contig has no row of its
#' own. Falling back to the sample's row keeps the caller from querying
#' `curate_opts` with no value, which builds `WHERE curate_opts IN ()`.
#'
#' @param con An open SQLite connection to the project database.
#' @param ID,path,scaffold The export unit.
#'
#' @return A single profile name; "default" when the sample has no annotate row.
#'
#' @noRd
# Append src onto dest, creating dest if needed. Replaces shell `cat >>`, which
# does not exist on Windows and broke on paths with spaces.
append_file <- function(dest, src) {
  if (!file.exists(dest)) file.create(dest)
  file.append(dest, src)
}

unit_curate_opts <- function(con, ID, path, scaffold) {
  opts <- dplyr::tbl(con, "annotate") |>
    dplyr::filter(ID == !!ID & path == !!path & scaffold == !!scaffold) |>
    dplyr::pull("curate_opts")
  if (length(opts) == 0) {
    opts <- dplyr::tbl(con, "annotate") |>
      dplyr::filter(ID == !!ID) |>
      dplyr::arrange(path, scaffold) |>
      dplyr::pull("curate_opts")
  }
  opts[1] %|NA|% "default"
}

# Map internal rRNA gene names to their export convention (rrnS -> rrn12,
# rrnL -> rrn16). Operates on the prefix so make.unique suffixes are preserved
# (rrnS.1 -> rrn12.1); non-rRNA names pass through unchanged.
.export_rrna_name <- function(x) {
  x <- sub("^rrnS", "rrn12", x)
  x <- sub("^rrnL", "rrn16", x)
  x
}

#' 5'->3' location intervals for a GenBank 5-column feature table
#'
#' Returns a list of `c(start, end)` character pairs, one per interval. A feature
#' spanning the origin of a circular contig (stored `pos1 > pos2`) needs two of
#' them: written on one line, `10623   468` means "minus strand, 468..10623" to
#' table2asn, not "wraps the origin", which would submit a ~10 kb reverse-strand
#' feature in place of the real gene.
#'
#' @param pos1,pos2 feature coordinates (`pos1 > pos2` indicates wrap-around)
#' @param direction "+" or "-"
#' @param wraps TRUE when the feature spans the origin of a circular contig
#' @param asmb_len contig length
#'
#' @noRd
tbl_locations <- function(pos1, pos2, direction, wraps, asmb_len) {
  ivs <- if (!wraps) {
    if (direction == "-") list(c(pos2, pos1)) else list(c(pos1, pos2))
  } else if (direction == "-") {
    # minus strand reads the circle backwards: pos2 -> 1, then asmb_len -> pos1
    list(c(pos2, 1L), c(asmb_len, pos1))
  } else {
    list(c(pos1, asmb_len), c(1L, pos2))
  }
  lapply(ivs, as.character)
}

#' Runs of unknown bases in a sequence
#'
#' Any run of N long enough to count as a gap under INSDC rules, whatever put it
#' there: a reference-guided join, an assembler, or a sequence the user supplied
#' that way.
#'
#' @param seq the assembly (XStringSet or character)
#' @param min_len shortest run reported as a gap
#'
#' @return data.frame(start, end, length), empty when there is nothing to report
#'
#' @noRd
find_sequence_gaps <- function(seq, min_len = 10L) {
  empty <- data.frame(start = integer(0), end = integer(0), length = integer(0))
  s <- toupper(as.character(seq)[1])
  if (is.na(s) || !nzchar(s)) {
    return(empty)
  }
  m <- gregexpr("N+", s)[[1]]
  if (length(m) == 1L && m[1] == -1L) {
    return(empty)
  }
  out <- data.frame(
    start = as.integer(m),
    length = as.integer(attr(m, "match.length"))
  )
  out$end <- out$start + out$length - 1L
  out <- out[out$length >= min_len, c("start", "end", "length"), drop = FALSE]
  out[order(out$start), , drop = FALSE]
}

#' Unknown bases inside one feature
#'
#' Counts across both intervals of a feature that spans the origin.
#'
#' @noRd
count_unknown_bases <- function(seq_chr, pos1, pos2, wraps, asmb_len) {
  ivs <- if (wraps) {
    list(c(pos1, asmb_len), c(1L, pos2))
  } else {
    list(sort(c(pos1, pos2)))
  }
  sum(vapply(ivs, function(iv) {
    sub <- substr(seq_chr, iv[1], iv[2])
    nchar(gsub("[^N]", "", toupper(sub)))
  }, integer(1)))
}

#' How a run of unknown bases should be described on export
#'
#' Three things decide it:
#'
#' * Does this run overlap a spacer the scaffold join inserted? Only then can we
#'   claim the alignment that ordered the pieces as linkage evidence. Ns a
#'   sequence arrived with are not ours to vouch for, and a run that merely
#'   happens to be the same length as a spacer is not a spacer.
#' * Could the reference size that junction? An unmapped junction gets a fixed
#'   placeholder, and submitting a placeholder as a measurement would be a false
#'   claim, so it goes out as `estimated_length unknown`.
#' * Does the ordering reference share the sample's genus? Only the user can say
#'   (see the export gap-evidence modal), so with no answer on file we claim
#'   nothing.
#'
#' @param run one row of [find_sequence_gaps()] (start, end, length)
#' @param spacers gap intervals for this unit in the SAME coordinates as `run`,
#'   a data.frame of start/end/size_known, or NULL/empty for a unit we did not
#'   join
#' @param genus_match "same", "different", or NA when the user has not answered
#'
#' @return list(estimated_length, linkage_evidence, gap_type, ours)
#'
#' @noRd
gap_qualifiers <- function(run, spacers = NULL, genus_match = NA_character_) {
  hit <- if (is.null(spacers) || nrow(spacers) == 0L) {
    spacers[0, , drop = FALSE]
  } else {
    spacers[spacers$start <= run$end & spacers$end >= run$start, , drop = FALSE]
  }
  ours <- !is.null(hit) && nrow(hit) > 0

  if (!ours) {
    # Not ours to explain. `unknown` drops the claim that this record is a
    # scaffold in an assembly, and INSDC then forbids linkage_evidence entirely.
    return(list(estimated_length = as.character(run$length),
                linkage_evidence = NA_character_, gap_type = "unknown",
                ours = FALSE))
  }

  # A run overlapping any unsized spacer cannot be reported as a measurement,
  # even if the rest of the run is real sequence.
  est <- if (any(hit$size_known == 0L)) "unknown" else as.character(run$length)
  evidence <- if (identical(genus_match, "same")) {
    "align-genus"
  } else if (identical(genus_match, "different")) {
    "align-xgenus"
  } else {
    NA_character_
  }
  # No answer on file: claim nothing rather than assert evidence we lack, which
  # means dropping the scaffold claim too (linkage_evidence is mandatory for
  # gap_type "within scaffold").
  if (is.na(evidence)) {
    return(list(estimated_length = est, linkage_evidence = NA_character_,
                gap_type = "unknown", ours = TRUE))
  }
  list(estimated_length = est, linkage_evidence = evidence,
       gap_type = "within scaffold", ours = TRUE)
}

#' Write one assembly_gap feature block
#'
#' The bases are present in the sequence; it is their identity that is unknown,
#' so a measured run reports its own length. `linkage_evidence` is emitted only
#' for a gap_type that permits it: INSDC makes it mandatory for "within
#' scaffold" and invalid for everything else.
#'
#' @param gap one row of [find_sequence_gaps()]
#' @param fn feature table to append to
#' @param qual list from [gap_qualifiers()]
#'
#' @noRd
write_tbl_gap <- function(gap, fn, qual = NULL) {
  qual <- qual %||% gap_qualifiers(gap, NULL)
  paste(c(gap$start, gap$end, "assembly_gap"), collapse = "\t") |>
    cat(file = fn, sep = "\n", append = TRUE)
  paste0("\t\t\testimated_length\t", qual$estimated_length) |>
    cat(file = fn, sep = "\n", append = TRUE)
  paste0("\t\t\tgap_type\t", qual$gap_type) |>
    cat(file = fn, sep = "\n", append = TRUE)
  if (!is.na(qual$linkage_evidence)) {
    paste0("\t\t\tlinkage_evidence\t", qual$linkage_evidence) |>
      cat(file = fn, sep = "\n", append = TRUE)
  }
}

#' Write one .tbl feature location block; only the first interval carries the key
#'
#' @noRd
write_tbl_loc <- function(pos, key, fn) {
  for (i in seq_along(pos)) {
    paste(c(pos[[i]], if (i == 1L) key), collapse = "\t") |>
      cat(file = fn, sep = "\n", append = TRUE)
  }
}

#' `transl_except` qualifier for a partial (poly-A completed) stop codon
#'
#' The codon sits at the 3' end of the CDS, which is `pos2` on the plus strand
#' and `pos1` on the minus strand - not `max()`/`min()`, which pick the wrong end
#' once the feature spans the origin.
#'
#' @noRd
.transl_except_pos <- function(pos1, pos2, direction, n_stop, wraps, asmb_len) {
  on_circle <- function(p) if (wraps) wrap_pos(p, asmb_len) else p
  if (direction == "+") {
    te_end <- pos2
    te_start <- on_circle(te_end - n_stop + 1L)
  } else {
    te_end <- pos1
    te_start <- on_circle(te_end + n_stop - 1L)
  }
  if (n_stop == 1) {
    paste0("(pos:", te_end, ",aa:TERM)")
  } else {
    paste0("(pos:", te_start, "..", te_end, ",aa:TERM)")
  }
}

#' GFF3 end coordinate for a feature that may span the origin
#'
#' GFF3 has no join() syntax, so an origin-spanning feature is written as
#' `pos1 .. (asmb_len + pos2)`, i.e. running past the end of the sequence.
#'
#' @noRd
gff_end <- function(pos2, wraps, asmb_len) {
  if (wraps) asmb_len + pos2 else pos2
}

#' Mark the 3' end of a .tbl location block as partial (">")
#'
#' The 3' coordinate is the second element of the LAST interval, which is not
#' `pos[[1]][2]` once a feature spans the origin.
#'
#' @noRd
mark_tbl_3p <- function(pos) {
  last <- length(pos)
  pos[[last]][2] <- paste0(">", pos[[last]][2])
  pos
}

#' Break a CDS at every gap of unknown size it crosses
#'
#' NCBI, https://www.ncbi.nlm.nih.gov/genbank/wgs_gapped/:
#'
#' "The exon(s) of a CDS may not cross the gap if the gap size is unknown.
#' Instead, you could have two partial CDS features (and mRNAs in eukaryoties)
#' that abut the gap, with a single gene over the whole locus."
#'
#' Works on the feature's own 5'->3' line rather than on assembly coordinates,
#' so a feature spanning the origin and a minus-strand feature are the same
#' problem. `n` gaps inside the CDS give `n + 1` pieces.
#'
#' @param pos1,pos2 feature coordinates (`pos1 > pos2` indicates wrap-around)
#' @param direction "+" or "-"
#' @param wraps TRUE when the feature spans the origin of a circular contig
#' @param asmb_len contig length
#' @param gaps data.frame(start, end) of the gaps that must not be crossed,
#'   in assembly coordinates; NULL or empty leaves the feature whole
#'
#' @return list of pieces in 5'->3' order, each `list(loc, cut5, cut3)`. `loc`
#'   is a location block in the shape [tbl_locations()] returns; `cut5`/`cut3`
#'   say whether that end of the piece was made by a gap (and so is partial)
#'   rather than being a real end of the CDS. A single piece with both flags
#'   FALSE means nothing was split. An empty list means the gaps swallowed the
#'   whole feature.
#'
#' @noRd
split_cds_at_gaps <- function(pos1, pos2, direction, wraps, asmb_len,
                              gaps = NULL) {
  pos1 <- as.integer(pos1)
  pos2 <- as.integer(pos2)
  asmb_len <- as.integer(asmb_len)
  # the feature as ascending assembly intervals, in plus-strand reading order
  ivs <- if (wraps) {
    list(c(pos1, asmb_len), c(1L, pos2))
  } else {
    list(sort(c(pos1, pos2)))
  }
  lens <- vapply(ivs, function(iv) iv[2] - iv[1] + 1L, integer(1))
  off <- cumsum(c(0L, lens))
  total <- sum(lens)

  whole <- list(list(
    loc = tbl_locations(pos1, pos2, direction, wraps, asmb_len),
    cut5 = FALSE, cut3 = FALSE
  ))
  if (is.null(gaps) || nrow(gaps) == 0L) {
    return(whole)
  }

  # each gap, projected onto that reading order
  cut <- do.call(rbind, lapply(seq_len(nrow(gaps)), function(g) {
    do.call(rbind, lapply(seq_along(ivs), function(k) {
      lo <- max(ivs[[k]][1], as.integer(gaps$start[g]))
      hi <- min(ivs[[k]][2], as.integer(gaps$end[g]))
      if (lo > hi) {
        return(NULL)
      }
      data.frame(start = off[k] + lo - ivs[[k]][1] + 1L,
                 end = off[k] + hi - ivs[[k]][1] + 1L)
    }))
  }))
  if (is.null(cut) || nrow(cut) == 0L) {
    return(whole)
  }
  cut <- cut[order(cut$start), , drop = FALSE]

  # what is left of the CDS once the gaps are removed
  keep <- list()
  p <- 1L
  for (i in seq_len(nrow(cut))) {
    if (cut$start[i] > p) {
      keep[[length(keep) + 1L]] <- c(p, cut$start[i] - 1L)
    }
    p <- max(p, cut$end[i] + 1L)
  }
  if (p <= total) {
    keep[[length(keep) + 1L]] <- c(p, total)
  }
  if (length(keep) == 0L) {
    return(list())
  }

  to_genomic <- function(lo, hi) {
    out <- list()
    for (k in seq_along(ivs)) {
      a <- max(lo, off[k] + 1L)
      b <- min(hi, off[k] + lens[k])
      if (a <= b) {
        out[[length(out) + 1L]] <-
          c(ivs[[k]][1] + a - off[k] - 1L, ivs[[k]][1] + b - off[k] - 1L)
      }
    }
    out
  }
  pieces <- lapply(keep, function(kp) {
    g <- to_genomic(kp[1], kp[2])
    cut5 <- kp[1] > 1L
    cut3 <- kp[2] < total
    if (direction == "-") {
      # minus strand reads the same bases backwards: the intervals reverse, and
      # so do the roles of the two ends
      g <- lapply(rev(g), function(iv) c(iv[2], iv[1]))
      swap <- cut5
      cut5 <- cut3
      cut3 <- swap
    }
    list(loc = lapply(g, as.character), cut5 = cut5, cut3 = cut3)
  })
  if (direction == "-") pieces <- rev(pieces)
  pieces
}

#' Write the CDS features for a gene broken by one or more unknown-size gaps
#'
#' One partial CDS per piece, each abutting the gap. Every piece carries the
#' qualifiers the single feature would have carried, plus the two notes NCBI
#' asks for: a pointer to the other piece(s) and "gap found within coding
#' sequence".
#'
#' `codon_start` is deliberately NOT emitted for anything but the first piece.
#' The gap length is unknown, so the reading frame on the far side of it cannot
#' be worked out; writing a frame there would be a guess presented as a fact.
#'
#' @param pieces list from [split_cds_at_gaps()]
#' @param partial5,partial3 partiality the whole CDS already had, applied to the
#'   first and last piece respectively
#'
#' @noRd
write_tbl_cds_pieces <- function(pieces, gene, product, genetic_code, fn,
                                 note = NULL, transl_except = NULL,
                                 codon_start = FALSE,
                                 partial5 = FALSE, partial3 = FALSE) {
  n <- length(pieces)
  coords <- vapply(pieces, function(p) {
    paste(vapply(p$loc, function(iv) paste0(iv[1], "..", iv[2]), character(1)),
          collapse = ",")
  }, character(1))

  for (i in seq_len(n)) {
    loc <- pieces[[i]]$loc
    if (isTRUE(pieces[[i]]$cut5) || (i == 1L && partial5)) {
      loc[[1]][1] <- paste0("<", loc[[1]][1])
    }
    if (isTRUE(pieces[[i]]$cut3) || (i == n && partial3)) {
      loc <- mark_tbl_3p(loc)
    }
    write_tbl_loc(loc, "CDS", fn)
    paste("\t\t\tproduct\t", product) |>
      cat(file = fn, sep = "\n", append = TRUE)
    paste("\t\t\ttransl_table\t", genetic_code) |>
      cat(file = fn, sep = "\n", append = TRUE)
    # the 3' end of the gene, and so its poly-A stop, lives on the last piece
    if (i == n && length(transl_except) > 0) {
      paste0("\t\t\ttransl_except\t", transl_except) |>
        cat(file = fn, sep = "\n", append = TRUE)
    }
    if (i == 1L && isTRUE(codon_start)) {
      paste("\t\t\tcodon_start\t", 1) |>
        cat(file = fn, sep = "\n", append = TRUE)
    }
    if (length(note) > 0) {
      paste0("\t\t\tnote\t", note) |>
        cat(file = fn, sep = "\n", append = TRUE)
    }
    paste0("\t\t\tnote\tthis is part ", i, " of ", n, " of the coding sequence ",
           "of ", gene, "; the rest is annotated at ",
           paste(coords[-i], collapse = ", ")) |>
      cat(file = fn, sep = "\n", append = TRUE)
    cat("\t\t\tnote\tgap found within coding sequence",
        file = fn, sep = "\n", append = TRUE)
  }
}

#' Generate export NCBI files
#'
#' @param group (optional) export group names
#' @param IDs One or more sample IDs to export. If not provided all samples in
#'   the export group will be exported
#' @param fasta_header Template for mitogenome fasta headers. Uses glue syntax (i.e.
#'   `{...}`) to insert values from the samples table
#' @param fasta_header_gene Template for gene fasta headers. Uses glue syntax (i.e.
#'   `{...}`) to insert values from the samples table
#' @param out_dir directory to save the exported files
#' @param generateAAalignments Generate group-level amino acid alignments
#'   (default: TRUE)
#' @param gap_min Shortest run of unknown bases (N) reported as an `assembly_gap`
#'   feature, bp (default = 10, the INSDC convention). Shorter runs are treated
#'   as ambiguous bases, and still counted in the note on a coding feature that
#'   contains them.
#' @param gene_export Export FASTAs and feature tables for individual genes?
#'   (default: FALSE)
#' @param review Run the PCG annotation outlier review after writing files and
#'   return the flagged results? (default: TRUE)
#' @param start_aa Start-offset threshold (amino acids) passed to
#'   [flag_PCG_outliers()]. Default 10.
#' @param stop_aa Stop-offset threshold (amino acids) passed to
#'   [flag_PCG_outliers()]. Default 10.
#' @param ident_pct Identity threshold (percent) passed to
#'   [flag_PCG_outliers()]. Default 60.
#' @param summary_csv Write a per-sample summary CSV (organism, topology,
#'   completeness, gene counts, reference, etc.) into the export directory?
#'   (default: TRUE)
#'
#' @return Invisibly, the list returned by [flag_PCG_outliers()] when `review`
#'   is TRUE (and a group of >1 sample is exported), otherwise `NULL`.
#'
#' @export
#'
export_files <- function(
    group = NULL,
    IDs = NULL,
    fasta_header = paste(
      "{seqid} [organism={Taxon}] [topology={topology}] [mgcode={genetic_code}]",
      "[location=mitochondrion] {Taxon} mitochondrion, {completeness}"
    ),
    fasta_header_gene = paste(
      "{seqid} [organism={Taxon}] [mgcode={genetic_code}]",
      "[location=mitochondrion] {Taxon}"
    ),
    out_dir = NULL,
    generateAAalignments = T,
    gap_min = 10,
    gene_export = F,
    review = TRUE,
    start_aa = 10,
    stop_aa = 10,
    ident_pct = 60,
    summary_csv = TRUE) {


  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(dirname(out_dir), ".sqlite"))
  on.exit(DBI::dbDisconnect(con))

  # Export runs per assembly unit (ID, path, scaffold): a fragmented sample
  # contributes one GenBank record per non-ignored scaffold. assemblies is the
  # authoritative unit list (the pipeline purges stale rows there); export_group
  # lives on the per-unit `export` table.
  unit_src <- dplyr::tbl(con, "assemblies") |>
    dplyr::filter(ignore == 0) |>
    dplyr::select(ID, path, scaffold)

  if (length(group) == 1) {
    units <- unit_src |>
      dplyr::inner_join(
        dplyr::tbl(con, "export") |>
          dplyr::filter(export_group == !!group) |>
          dplyr::select(ID, path, scaffold),
        by = c("ID", "path", "scaffold")
      ) |>
      dplyr::collect()
    group_pth <- file.path(out_dir, "export", group)
    unlink(group_pth, recursive = TRUE)
    group_gff_pth <- file.path(group_pth, "GFFs")
    dir.create(group_pth, recursive = TRUE, showWarnings = FALSE)
    dir.create(group_gff_pth, recursive = TRUE, showWarnings = FALSE)
    group_fasta <- file.path(group_pth, paste0(group, ".fasta"))
    group_tbl   <- file.path(group_pth, paste0(group, ".tbl"))
    if (gene_export) {
      group_genes_pth <- file.path(group_pth, "genes")
      dir.create(group_genes_pth, recursive = TRUE, showWarnings = FALSE)
    }
  } else {
    units <- unit_src |>
      dplyr::filter(ID %in% !!IDs) |>
      dplyr::collect()
  }

  # SeqID: plain ID unless the sample contributes >1 record to this export.
  units <- units |>
    dplyr::arrange(ID, path, scaffold) |>
    dplyr::group_by(ID) |>
    dplyr::mutate(n_units = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(seqid = export_seqid(ID, path, scaffold, n_units))
  IDs <- unique(units$ID)

  if (nrow(units) == 0) {
    stop("No samples selected")
  }

  check_single_path(units)

  if (gene_export) {
    group_allgene_tbl_fn <- file.path(group_pth, "genes", paste0(group, "_PCGs.tbl"))
    group_allgene_fasta  <- file.path(group_pth, "genes", paste0(group, "_PCGs.fasta"))
  }

  purrr::walk(seq_len(nrow(units)), function(.i) {
    .x        <- units$ID[.i]
    .path     <- units$path[.i]
    .scaffold <- units$scaffold[.i]
    .seqid    <- units$seqid[.i]

    export_path <- file.path(
      out_dir,
      .x,
      "export"
    )

    dir.create(export_path, recursive = TRUE, showWarnings = F)

    # debugging help
    message(paste0(.seqid, ":"))

    # Per unit, not per ID: ignoring a scaffold deletes its annotate row but leaves
    # its annotations rows, so an ID-only filter pulls in an ignored sibling's genes.
    annotations <- dplyr::tbl(con, "annotations") |>
      dplyr::filter(ID == !!.x & path == !!.path & scaffold == !!.scaffold) |>
      dplyr::arrange(path, pos1) |>
      dplyr::filter(pos1 > 0) |>
      dplyr::collect()

    curation_opts <- unit_curate_opts(con, .x, .path, .scaffold)

    curate_rules <- dplyr::tbl(con, "curate_opts") |>
      dplyr::filter(curate_opts == !!curation_opts) |>
      dplyr::pull("params") |>
      jsonlite::fromJSON()

    # Project-level setting (per curation-options profile): treat linear
    # assemblies as complete genomes? Absent on un-migrated DBs -> FALSE.
    linear_complete <- tryCatch(
      dplyr::tbl(con, "curate_opts") |>
        dplyr::filter(curate_opts == !!curation_opts) |>
        dplyr::pull(dplyr::any_of("linear_complete")),
      error = function(e) integer(0)
    )
    linear_complete <- isTRUE(as.integer(linear_complete[1]) == 1L)

    # check for duplicate gene names in annotations and rename
    annotations$gene_uniq <- make.unique(annotations$gene)

    # One row: the annotate join is keyed on the unit, so topology/partial come from
    # this scaffold rather than an arbitrary sibling. blast_accession/poor_blast_ref
    # are genuinely sample-level (assemble is PK ID).
    dat <- dplyr::tbl(con, "samples") |>
      dplyr::select(-dplyr::any_of("topology")) |>
      dplyr::filter(ID == !!.x) |>
      dplyr::left_join(
        dplyr::tbl(con, "annotate") |>
          dplyr::filter(path == !!.path & scaffold == !!.scaffold) |>
          dplyr::select(ID, topology, path, scaffold, dplyr::any_of("partial")),
        by = "ID"
      ) |>
      dplyr::left_join(
        dplyr::tbl(con, "assemble") |>
          dplyr::select(ID, blast_accession,
                        dplyr::any_of(c("blast_accession_auto", "poor_blast_ref"))),
        by = "ID"
      ) |>
      dplyr::collect()
    # SeqID for glue templates; the FASTA defline and the .tbl >Feature line must
    # agree exactly or table2asn rejects the submission.
    dat$seqid <- .seqid

    kept <- dplyr::tbl(con, "assemblies") |>
      dplyr::filter(ID == !!.x & path == !!.path & scaffold == !!.scaffold) |>
      dplyr::select(scaffold, topology) |>
      dplyr::collect()
    if (nrow(kept) == 0) {
      warning(.seqid, ": scaffold not found in assemblies. Skipping.")
      return()
    }
    seq <- MitoPilot::get_assembly(
      ID = .x,
      path = .path,
      scaffold = .scaffold,
      con = con
    )
    # Topology is per record. Each export unit is one scaffold and assemblies
    # carries that scaffold's own value, while annotate.topology summarizes the
    # whole unit ("fragmented") and, for user assemblies, covers every contig of
    # the sample from a single row. Only a per-scaffold value may reach a defline.
    if (!is.na(kept$topology[1]) && nzchar(kept$topology[1])) {
      dat$topology <- kept$topology[1]
    }
    unit_topology <- as.character(dat$topology)[1]
    if (!isTRUE(unit_topology %in% c("circular", "linear"))) {
      warning(
        .seqid, ": no per-scaffold topology found (found '",
        unit_topology %|NA|% "NA", "'). Exporting as linear."
      )
      dat$topology <- "linear"
    }
    # Reference for the note, resolved per unit via the same helper the synteny view
    # and both tables use, so the note always names the reference the user was shown.
    blast_acc <- resolve_unit_blast_ref(con, .x, .path, .scaffold)
    blast_note <- if (!is.null(blast_acc) && !is.na(blast_acc) && nzchar(blast_acc) &&
                      blast_acc != "NO HIT" &&
                      !isTRUE(dat$poor_blast_ref[1] %in% c("poor", "failed"))) {
      paste0(" [note=annotation compared to GenBank accession ", blast_acc, "]")
    } else {
      ""
    }
    # Genome-level completeness for the {completeness} header field.
    # Auto-derived from topology: circular -> complete, linear -> partial.
    # The per-sample "partial" flag forces partial; the project-level
    # linear_complete setting forces linear assemblies to complete.
    forced_partial <- "partial" %in% names(dat) && isTRUE(dat$partial[1] == "yes")
    is_circular <- isTRUE(dat$topology[1] == "circular")
    is_partial <- if (forced_partial) {
      TRUE
    } else if (is_circular || linear_complete) {
      FALSE
    } else {
      TRUE
    }
    dat$completeness <- if (is_partial) "partial genome" else "complete genome"
    header <- stringr::str_glue_data(dat, fasta_header)
    # Safety net for saved templates that hardcode "complete genome"
    if (is_partial) {
      header <- stringr::str_replace(header, "complete genome$", "partial genome")
    }
    names(seq) <- paste0(header, blast_note)

    # sequence name, to be used as first column in GFF
    seq_name <- sapply(strsplit(names(seq)," "), `[`, 1)

    trim_offset <- 0L
    # Trim un-annotated ends of linear assemblies. A pos1 > pos2 row is a
    # wrap-around, which MIN/MAX cannot describe and the constant shift below
    # would drive negative, so leave the assembly untrimmed instead.
    if (dat$topology == "linear" && any(annotations$pos1 > annotations$pos2)) {
      message(
        .seqid, ": a feature spans the start of this linear assembly, ",
        "so its un-annotated ends were not trimmed."
      )
    } else if (dat$topology == "linear") {
      start <- min(annotations$pos1)
      if (start > 1) {
        # Every coordinate downstream shifts by this much, spacers included.
        trim_offset <- as.integer(start - 1L)
        seq <- Biostrings::subseq(seq, start, seq@ranges@width)
        annotations <- annotations |>
          dplyr::mutate(
            pos1 = pos1 - start + 1,
            pos2 = pos2 - start + 1
          )
      }
      stop <- max(annotations$pos2)
      if (stop < seq@ranges@width) {
        seq <- Biostrings::subseq(seq, 1, stop)
      }
    }

    # Write FASTA
    fasta_fn <- file.path(export_path, paste0(.seqid, ".fasta"))
    Biostrings::writeXStringSet(seq, filepath = fasta_fn)

    # MAKE 4 column tab file
    tbl_fn <- file.path(export_path, paste0(.seqid, ".tbl"))
    if (file.exists(tbl_fn)) {
      file.remove(tbl_fn)
    }
    if ("GenBankAccession" %in% names(dat) && length(dat$GenBankAccession) > 0 && nchar(dat$GenBankAccession) > 4) {
      cat(paste(">Feature", paste0("gb|", dat$GenBankAccession, "|")), file = tbl_fn, sep = "\n")
    } else {
      cat(paste(">Feature", .seqid), file = tbl_fn, sep = "\n")
    }

    # MAKE GFF
    gff_fn <- file.path(export_path, paste0(.seqid, ".gff"))
    if (file.exists(gff_fn)) {
      file.remove(gff_fn)
    }
    # add GFF header
    "##gff-version 3" |>
      cat(file = gff_fn, sep = "\n", append = TRUE)
    # add GFF region
    # circ = tolower((dat$topology == "circular"))
    #f9 = paste0("ID=",seq_name,":1..",seq@ranges@width,";Is_circular=",circ,";Name=MT;mol_type=genomic DNA") # Is_circular currently bugged in Geneious
    asmb_len = seq@ranges@width
    f9 = paste0("ID=",seq_name,":1..",asmb_len,";Name=MT;mol_type=genomic DNA")
    paste(c(seq_name, "MitoPilot", "region", 1, asmb_len, ".", "+", ".", f9), collapse = "\t") |>
      cat(file = gff_fn, sep = "\n", append = TRUE)

    # Runs of unknown bases, whatever put them there. Declared as assembly_gap
    # features so a submission carries no undeclared gaps, and used to note any
    # coding feature sitting across one.
    seq_chr <- toupper(as.character(seq)[1])
    gaps <- find_sequence_gaps(seq_chr, min_len = gap_min)

    # Provenance for this unit's runs: was it joined, which junction lengths were
    # placeholders rather than measurements, and has the user said whether the
    # ordering reference shares the sample's genus.
    # Tolerant of a project that predates these tables: no provenance on file
    # means no claim is made, which is the same as a sequence we did not join.
    empty_spacers <- data.frame(start = integer(0), end = integer(0),
                                size_known = integer(0))
    spacers <- tryCatch(
      DBI::dbGetQuery(
        con, "SELECT start, end, size_known FROM scaffold_junctions WHERE ID = ?",
        params = list(.x)
      ),
      error = function(e) empty_spacers
    )
    # Spacers are recorded against the joined Path 0, and only that unit.
    if (!isTRUE(as.integer(.path) == 0L)) spacers <- empty_spacers
    # Trimming the un-annotated ends of a linear assembly shifts every
    # coordinate; move the spacers by the same offset so they still line up.
    if (nrow(spacers) > 0 && trim_offset > 0L) {
      spacers$start <- spacers$start - trim_offset
      spacers$end <- spacers$end - trim_offset
    }
    genus_match <- tryCatch(
      DBI::dbGetQuery(con, "SELECT genus_match FROM gap_evidence WHERE ID = ?",
                      params = list(.x))$genus_match,
      error = function(e) character(0)
    )
    genus_match <- if (length(genus_match)) genus_match[1] else NA_character_

    # Gaps a coding sequence may not cross: the ones whose length we cannot
    # report, which is exactly what gap_qualifiers() calls "unknown".
    unknown_gaps <- gaps[
      vapply(seq_len(nrow(gaps)), function(i) {
        identical(gap_qualifiers(gaps[i, ], spacers, genus_match)$estimated_length,
                  "unknown")
      }, logical(1)), , drop = FALSE
    ]

    gap_state <- new.env(parent = emptyenv())
    gap_state$i <- 1L
    # Features are written in ascending order, so emit every gap that starts
    # before the feature about to be written and keep the table sorted.
    flush_gaps <- function(before = NULL) {
      while (gap_state$i <= nrow(gaps) &&
             (is.null(before) || gaps$start[gap_state$i] < before)) {
        write_tbl_gap(
          gaps[gap_state$i, ], tbl_fn,
          gap_qualifiers(gaps[gap_state$i, ], spacers, genus_match)
        )
        gap_state$i <- gap_state$i + 1L
      }
    }
    if (nrow(gaps) > 0) {
      message(
        .seqid, ": ", nrow(gaps), " run(s) of unknown bases (",
        sum(gaps$length), " bp total) declared as assembly_gap features."
      )
      # INSDC caps a declared gap span at 1000 bp. Nothing can be done about it
      # here, so say so plainly rather than emit a feature that will bounce.
      too_long <- gaps[gaps$length > 1000L, , drop = FALSE]
      if (nrow(too_long) > 0) {
        message(
          .seqid, ": WARNING ", nrow(too_long),
          " gap(s) exceed the 1000 bp INSDC limit for a declared assembly_gap (",
          paste(sprintf("%d-%d", too_long$start, too_long$end), collapse = ", "),
          "). GenBank will query these."
        )
      }
      # A feature may not begin or end inside a gap; it has to abut the gap and
      # be partial (SEQ_FEAT.FeatureBeginsOrEndsInGap).
      in_gap <- function(p) any(gaps$start <= p & gaps$end >= p)
      bad <- vapply(seq_len(nrow(annotations)), function(i) {
        in_gap(annotations$pos1[i]) || in_gap(annotations$pos2[i])
      }, logical(1))
      if (any(bad)) {
        message(
          .seqid, ": WARNING ", sum(bad),
          " feature(s) begin or end inside a gap (",
          paste(unique(annotations$gene[bad]), collapse = ", "),
          "). GenBank rejects these; adjust the boundary to abut the gap."
        )
      }
    }

    purrr::pwalk(annotations, function(...) {
      cur <- list(...)
      note <- NULL
      transl_except <- NULL
      # pos1 > pos2 on a circular contig is the wrap-around convention, not an
      # error: the feature spans the origin and needs a two-interval location.
      wraps <- isTRUE(dat$topology == "circular") && isTRUE(cur$pos1 > cur$pos2)
      pos <- tbl_locations(cur$pos1, cur$pos2, cur$direction, wraps, asmb_len)
      flush_gaps(before = min(cur$pos1, cur$pos2))

      # Says what is true of the sequence without guessing why it is unknown.
      n_unknown <- count_unknown_bases(seq_chr, cur$pos1, cur$pos2, wraps, asmb_len)
      add_gap_note <- function(note) {
        if (n_unknown == 0L) {
          return(note)
        }
        paste(c(note, paste0("contains ", n_unknown, " bases of unknown sequence")),
              collapse = "; ")
      }

      if (cur$pos1 >= cur$pos2 && !wraps) {
        message(paste0("Warning: pos1 >= pos2 for ", dat$ID,": ", cur$gene, ", may be an annotation error"))
      }

      if (cur$type == "PCG") {
        # default product for custom-named ORFs / CDS without a standard product
        if (length(cur$product) == 0 || is.na(cur$product) || !nzchar(trimws(cur$product))) {
          cur$product <- "hypothetical protein"
        }
        # get start and stop codons from the ruleset
        cur_rules <- curate_rules$rules[[cur$gene]]
        if("stop_codons" %in% names(cur_rules)){
          stop_codons <- cur_rules$stop_codons
        } else {
          stop_codons <- curate_rules$default_rules$PCG$stop_codons
        }
        if("start_codons" %in% names(cur_rules)){
          start_codons <- cur_rules$start_codons
        } else {
          start_codons <- curate_rules$default_rules$PCG$start_codons
        }

        if ("intron" %in% names(cur_rules)){
          intron <- cur_rules$intron
        } else {
          if("intron" %in% curate_rules$default_rules$PCG){
            intron <- curate_rules$default_rules$PCG$intron
          } else {
            intron <- FALSE
          }
        }

        # User-defined join group (set in the annotate modal). Members share a
        # "JOIN: mode=<exon|frameshift> group=<id>" marker in their notes. This
        # exports a subset of same-gene rows as one joined feature, independent of
        # the curation intron rule, and supports the frameshift/slippage exception.
        join_match <- stringr::str_match(
          cur$notes %|NA|% "", "^JOIN: mode=(\\w+) group=(\\d+)( note=([^;]*))?"
        )
        join_mode <- join_match[, 2]
        join_grp <- join_match[, 3]
        join_note <- join_match[, 5]
        do_join <- !is.na(join_grp) ||
          (intron & length(which(annotations$gene == cur$gene)) > 1)
        exon_mode <- if (!is.na(join_grp)) join_mode else "exon"

        if (do_join) {  # logic to merge exons (intron rule or user join group)
          if (!is.na(join_grp)) {
            member_idx <- which(stringr::str_detect(
              dplyr::coalesce(annotations$notes, ""),
              paste0("^JOIN: mode=\\w+ group=", join_grp, "\\b")
            ))
          } else {
            member_idx <- which(annotations$gene == cur$gene)
          }
          exons <- annotations[member_idx, ]
          exons <- exons[order(exons$pos1), ]
          # skip if not the first exon
          if (cur$pos1 != exons[1,]$pos1) return()
          if (length(unique(exons$direction)) > 1) {
            message(crayon::red(
              paste0("Warning: exons on opposite strands for gene ", cur$gene)
            ))
          } else {
            # splice segments into one CDS (shared with the annotate editor)
            spliced <- splice_join_cds(
              exons, seq,
              Biostrings::getGeneticCode(as.character(dat$genetic_code))
            )
            # spliced CDS sequence (5'->3', revcomp applied for - strand); used
            # below for the per-gene FASTA export.
            merged_sequence <- Biostrings::DNAStringSet(spliced$dna)
            cur$translation <- spliced$translation
            cur$start_codon <- spliced$start_codon
            cur$stop_codon <- spliced$stop_codon
            cur$partial_start <- spliced$partial_start
            cur$partial_stop <- spliced$partial_stop
            cur$length <- spliced$length
            # Gene span across all exons, from the splice helper: sorting by
            # pos1 and taking the outer bounds reports a near-whole-circle span
            # once an exon crosses the origin. The .tbl orientation is applied
            # by tbl_locations().
            cur$pos1 <- spliced$pos1
            cur$pos2 <- spliced$pos2
            wraps <- isTRUE(dat$topology == "circular") && isTRUE(cur$pos1 > cur$pos2)
            pos <- tbl_locations(cur$pos1, cur$pos2, cur$direction, wraps, asmb_len)
          }

          if (stringr::str_detect(cur$translation, "\\*")) {
            message(crayon::red(paste("##### Internal stop codon", cur$gene, crayon::bgBlue(cur$stop_codon), "#####")))
          }
          if (cur$stop_codon %nin% stop_codons) {
            message(crayon::red(paste("Non-standard stop codon:", cur$gene, crayon::bgBlue(cur$stop_codon))))
          }
          if (cur$start_codon %nin% start_codons || isTRUE(as.integer(cur$partial_start) == 1L)) {
            message(crayon::red(paste("Non-standard start codon:", cur$gene, crayon::bgBlue(cur$start_codon))))
            # 5' partial: '<' prepends the start coordinate (first column, both strands)
            pos[[1]][1] <- paste0("<", pos[[1]][1])
            note <- "start codon not determined"
          }
          if (nchar(cur$stop_codon) < 3) {
            note <- paste(c(note, "TAA stop codon is completed by the addition of 3' A residues to the mRNA"), collapse = "; ")
            # transl_except marks the partial stop codon position(s) completed by poly-A
            n_stop <- nchar(cur$stop_codon)
            transl_except <- .transl_except_pos(
              cur$pos1, cur$pos2, cur$direction, n_stop, wraps, asmb_len
            )
          } else if (isTRUE(as.integer(cur$partial_stop) == 1L)) {
            # 3' partial (undetermined), not poly-A: '>' prepends the stop
            # coordinate (last interval, both strands)
            pos <- mark_tbl_3p(pos)
            note <- paste(c(note, "stop codon not determined"), collapse = "; ")
          }

          # A spliced CDS is not split here: the pieces would have to be
          # regrouped exon by exon and the joined translation redone. Say so
          # rather than write a feature GenBank will reject.
          if (nrow(unknown_gaps) > 0 && any(vapply(seq_len(nrow(exons)), function(i) {
            e_wraps <- isTRUE(dat$topology == "circular") &&
              isTRUE(exons[i, ]$pos1 > exons[i, ]$pos2)
            length(split_cds_at_gaps(
              exons[i, ]$pos1, exons[i, ]$pos2, exons[i, ]$direction,
              e_wraps, asmb_len, unknown_gaps
            )) != 1L
          }, logical(1)))) {
            message(
              .seqid, ": WARNING an exon of ", cur$gene, " crosses a gap of ",
              "unknown size. GenBank does not allow this; the CDS was written ",
              "whole and will need splitting by hand."
            )
          }

          # write to .tbl
          write_tbl_loc(pos, "gene", tbl_fn)
          paste0("\t\t\tgene\t", cur$gene) |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
          # CDS location lines: one interval per exon in 5'->3' order; an exon
          # spanning the origin contributes two.
          exon_order <- if (all(exons$direction == "-")) rev(seq_len(nrow(exons))) else seq_len(nrow(exons))
          cds_pos <- unlist(
            lapply(exon_order, function(i) {
              e_wraps <- isTRUE(dat$topology == "circular") &&
                isTRUE(exons[i, ]$pos1 > exons[i, ]$pos2)
              tbl_locations(
                exons[i, ]$pos1, exons[i, ]$pos2, exons[i, ]$direction,
                e_wraps, asmb_len
              )
            }),
            recursive = FALSE
          )
          write_tbl_loc(cds_pos, "CDS", tbl_fn)
          paste("\t\t\tproduct\t", cur$product) |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
          paste("\t\t\ttransl_table\t", dat$genetic_code) |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
          if (length(transl_except) > 0) {
            paste0("\t\t\ttransl_except\t", transl_except) |>
              cat(file = tbl_fn, sep = "\n", append = TRUE)
          }
          if (!cur$start_codon %in% start_codons) {
            paste("\t\t\tcodon_start\t", 1) |>
              cat(file = tbl_fn, sep = "\n", append = TRUE)
          }
          # frameshift/RNA-editing join: INSDC requires the ribosomal_slippage
          # qualifier + exception; note carries no nucleotide locations per
          # GenBank guidance
          if (identical(exon_mode, "frameshift")) {
            cat("\t\t\tribosomal_slippage", file = tbl_fn, sep = "\n", append = TRUE)
            paste0("\t\t\texception\tribosomal slippage") |>
              cat(file = tbl_fn, sep = "\n", append = TRUE)
            fs_note <- if (!is.na(join_note) && nzchar(trimws(join_note))) {
              trimws(join_note)
            } else {
              "frameshift mechanism unknown"
            }
            note <- paste(c(note, fs_note), collapse = "; ")
          }
          note_out <- add_gap_note(note)
          if (length(note_out) > 0) {
            paste0("\t\t\tnote\t", note_out) |>
              cat(file = tbl_fn, sep = "\n", append = TRUE)
          }

          # write to GFF
          # gene feature
          f9 = paste0("ID=gene-",cur$gene,";Name=",cur$gene,";gbkey=Gene;gene=",cur$gene,";gene_biotype=protein_coding")
          # a feature spanning the origin is written start..(asmb_len + end)
          paste(c(seq_name, "MitoPilot", "gene", cur$pos1, gff_end(cur$pos2, wraps, asmb_len), ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)

          # write CDS feature for each exon. GFF3 coordinates are always
          # ascending; the strand lives in column 7, so both strands share this.
          for (i in seq_len(nrow(exons))) {
            f9 = paste0("ID=cds-",cur$gene,";Parent=gene-",cur$gene,";Name=",cur$gene,";gbkey=CDS;gene=",cur$gene,";product=",cur$product,";transl_table=",dat$genetic_code)
            if (length(note_out) > 0){
              f9 = paste0(f9, ";Note=", note_out)
            }
            e_wraps <- isTRUE(dat$topology == "circular") &&
              isTRUE(exons[i, ]$pos1 > exons[i, ]$pos2)
            paste(c(seq_name, "MitoPilot", "CDS", exons[i,]$pos1, gff_end(exons[i,]$pos2, e_wraps, asmb_len), ".", cur$direction, "0", f9), collapse = "\t") |>
              cat(file = gff_fn, sep = "\n", append = TRUE)
          }

          if(gene_export){
            # EXTRACT GENE FROM ASSEMBLY
            # make directory for gene if it doesn't exist
            group_geneName_pth <- file.path(group_pth, "genes", cur$gene)
            dir.create(group_geneName_pth, recursive = T, showWarnings = F)

            # get gene region from assembly
            gene = merged_sequence

            # update FASTA header with gene name
            head_split <- strsplit(fasta_header_gene, "\\s+")
            head_split[[1]][1] <- paste0(head_split[[1]][1], "_", cur$gene)
            head_split[[1]][length(head_split[[1]])] <- paste0(head_split[[1]][length(head_split[[1]])], ", ", cur$product)
            head <- paste(c(head_split[[1]]), sep=" ", collapse=" ")
            names(gene) <- stringr::str_glue_data(dat, head)

            # write FASTA
            gene_fn <- file.path(export_path, paste0(.seqid, "_", cur$gene, ".fasta"))
            Biostrings::writeXStringSet(gene, filepath = gene_fn)

            # fix the start and stop position
            pos1_new = 1
            pos2_new = length(gene[[1]])
            # extracted gene is 5'->3': mark partial ends on the start/stop coords
            gene_p1 <- as.character(pos1_new)
            gene_p2 <- as.character(pos2_new)
            if (cur$start_codon %nin% start_codons || isTRUE(as.integer(cur$partial_start) == 1L)) {
              gene_p1 <- paste0("<", gene_p1)
            }
            if (nchar(cur$stop_codon) >= 3 && isTRUE(as.integer(cur$partial_stop) == 1L)) {
              gene_p2 <- paste0(">", gene_p2)
            }

            # write gene feature table
            gene_tbl_fn <- file.path(export_path, paste0(.seqid, "_", cur$gene, ".tbl"))
            if (file.exists(gene_tbl_fn)) {
              file.remove(gene_tbl_fn)
            }
            cat(paste0(">Feature ", .seqid, "_", cur$gene), file = gene_tbl_fn, sep = "\n")
            paste(c(gene_p1, gene_p2, "gene"), collapse = "\t") |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            paste0("\t\t\tgene\t", cur$gene) |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            paste(c(gene_p1, gene_p2, "CDS"), collapse = "\t") |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            paste("\t\t\tproduct\t", cur$product) |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            paste("\t\t\ttransl_table\t", dat$genetic_code) |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            if (nchar(cur$stop_codon) < 3) {
              # extracted gene is oriented 5'->3', partial stop is the last base(s)
              n_stop <- nchar(cur$stop_codon)
              if (n_stop == 1) {
                gene_transl_except <- paste0("(pos:", pos2_new, ",aa:TERM)")
              } else {
                gene_transl_except <- paste0("(pos:", pos2_new - n_stop + 1, "..", pos2_new, ",aa:TERM)")
              }
              paste0("\t\t\ttransl_except\t", gene_transl_except) |>
                cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            }
            if (!cur$start_codon %in% start_codons || isTRUE(as.integer(cur$partial_start) == 1L)) {
              paste("\t\t\tcodon_start\t", 1) |>
                cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            }
            note_out <- add_gap_note(note)
            if (length(note_out) > 0) {
              paste0("\t\t\tnote\t", note_out) |>
                cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            }

            # concatenate sequences and tables by gene
            if (length(group) == 1) {
              group_gene_tbl <- file.path(group_geneName_pth, paste0(group, "_", cur$gene, ".tbl"))
              append_file(group_gene_tbl, gene_tbl_fn)
              group_gene_fasta <- file.path(group_geneName_pth, paste0(group, "_", cur$gene, ".fasta"))
              append_file(group_gene_fasta, gene_fn)
            }

            # concatenate all sequences and tables
            if (length(group) == 1) {
              append_file(group_allgene_tbl_fn, gene_tbl_fn)
              append_file(group_allgene_fasta, gene_fn)
            }
          }

        } else { # normal processing, no introns
          partial5 <- FALSE
          partial3 <- FALSE
          if (stringr::str_detect(cur$translation, "\\*")) {
            message(crayon::red(paste("##### Internal stop codon", cur$gene, crayon::bgBlue(cur$stop_codon), "#####")))
          }
          if (cur$stop_codon %nin% stop_codons) {
            message(crayon::red(paste("Non-standard stop codon:", cur$gene, crayon::bgBlue(cur$stop_codon))))
          }
          if (cur$start_codon %nin% start_codons || isTRUE(as.integer(cur$partial_start) == 1L)) {
            message(crayon::red(paste("Non-standard start codon:", cur$gene, crayon::bgBlue(cur$start_codon))))
            # 5' partial: '<' prepends the start coordinate (first column, both strands)
            pos[[1]][1] <- paste0("<", pos[[1]][1])
            partial5 <- TRUE
            note <- "start codon not determined"
          }
          if (nchar(cur$stop_codon) < 3) {
            note <- paste(c(note, "TAA stop codon is completed by the addition of 3' A residues to the mRNA"), collapse = "; ")
            # transl_except marks the partial stop codon position(s) completed by poly-A
            n_stop <- nchar(cur$stop_codon)
            transl_except <- .transl_except_pos(
              cur$pos1, cur$pos2, cur$direction, n_stop, wraps, asmb_len
            )
          } else if (isTRUE(as.integer(cur$partial_stop) == 1L)) {
            # 3' partial (undetermined), not poly-A: '>' prepends the stop
            # coordinate (last interval, both strands)
            pos <- mark_tbl_3p(pos)
            partial3 <- TRUE
            note <- paste(c(note, "stop codon not determined"), collapse = "; ")
          }

          # https://www.ncbi.nlm.nih.gov/genbank/wgs_gapped/ : "The exon(s) of a
          # CDS may not cross the gap if the gap size is unknown. Instead, you
          # could have two partial CDS features (and mRNAs in eukaryoties) that
          # abut the gap, with a single gene over the whole locus."
          cds_pieces <- split_cds_at_gaps(
            cur$pos1, cur$pos2, cur$direction, wraps, asmb_len, unknown_gaps
          )
          split_cds <- length(cds_pieces) > 1
          note_out <- add_gap_note(note)

          # write to .tbl
          write_tbl_loc(pos, "gene", tbl_fn)
          paste0("\t\t\tgene\t", cur$gene) |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
          if (split_cds) {
            cat("\t\t\tnote\tgap found within coding sequence",
                file = tbl_fn, sep = "\n", append = TRUE)
            write_tbl_cds_pieces(
              cds_pieces, cur$gene, cur$product, dat$genetic_code, tbl_fn,
              note = note_out, transl_except = transl_except,
              codon_start = !cur$start_codon %in% start_codons,
              partial5 = partial5, partial3 = partial3
            )
          } else {
            write_tbl_loc(pos, "CDS", tbl_fn)
            paste("\t\t\tproduct\t", cur$product) |>
              cat(file = tbl_fn, sep = "\n", append = TRUE)
            paste("\t\t\ttransl_table\t", dat$genetic_code) |>
              cat(file = tbl_fn, sep = "\n", append = TRUE)
            if (length(transl_except) > 0) {
              paste0("\t\t\ttransl_except\t", transl_except) |>
                cat(file = tbl_fn, sep = "\n", append = TRUE)
            }
            if (!cur$start_codon %in% start_codons) {
              paste("\t\t\tcodon_start\t", 1) |>
                cat(file = tbl_fn, sep = "\n", append = TRUE)
            }
            if (length(note_out) > 0) {
              paste0("\t\t\tnote\t", note_out) |>
                cat(file = tbl_fn, sep = "\n", append = TRUE)
            }
          }

          # write to GFF. Left as one CDS line on purpose: GFF3 has no partial
          # coordinate convention, and two lines sharing a CDS ID would say the
          # two pieces are joined, which is what the split exists to avoid.
          # gene feature
          f9 = paste0("ID=gene-",cur$gene,";Name=",cur$gene,";gbkey=Gene;gene=",cur$gene,";gene_biotype=protein_coding")
          # a feature spanning the origin is written start..(asmb_len + end)
          paste(c(seq_name, "MitoPilot", "gene", cur$pos1, gff_end(cur$pos2, wraps, asmb_len), ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)

          # CDS feature
          f9 = paste0("ID=cds-",cur$gene,";Parent=gene-",cur$gene,";Name=",cur$gene,";gbkey=CDS;gene=",cur$gene,";product=",cur$product,";transl_table=",dat$genetic_code)
          if (length(note_out) > 0){
            f9 = paste0(f9, ";Note=", note_out)
          }
          # a feature spanning the origin is written start..(asmb_len + end)
          paste(c(seq_name, "MitoPilot", "CDS", cur$pos1, gff_end(cur$pos2, wraps, asmb_len), ".", cur$direction, "0", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)

          if(gene_export){
            # EXTRACT GENE FROM ASSEMBLY
            # make directory for gene if it doesn't exist
            group_geneName_pth <- file.path(group_pth, "genes", cur$gene)
            dir.create(group_geneName_pth, recursive = T, showWarnings = F)

            # get gene region from assembly
            gene = extract_circ_region(seq, cur$pos1, cur$pos2)

            # update FASTA header with gene name
            head_split <- strsplit(fasta_header_gene, "\\s+")
            head_split[[1]][1] <- paste0(head_split[[1]][1], "_", cur$gene_uniq)
            head_split[[1]][length(head_split[[1]])] <- paste0(head_split[[1]][length(head_split[[1]])], ", ", cur$product)
            head <- paste(c(head_split[[1]]), sep=" ", collapse=" ")
            names(gene) <- stringr::str_glue_data(dat, head)

            # reverse complement if needed
            if (cur$direction == "-") {
              gene = Biostrings::reverseComplement(gene)
            }

            # write FASTA
            gene_fn <- file.path(export_path, paste0(.seqid, "_", cur$gene_uniq, ".fasta"))
            Biostrings::writeXStringSet(gene, filepath = gene_fn)

            # fix the start and stop position
            pos1_new = 1
            pos2_new = length(gene[[1]])
            # extracted gene is 5'->3': mark partial ends on the start/stop coords
            gene_p1 <- as.character(pos1_new)
            gene_p2 <- as.character(pos2_new)
            if (cur$start_codon %nin% start_codons || isTRUE(as.integer(cur$partial_start) == 1L)) {
              gene_p1 <- paste0("<", gene_p1)
            }
            if (nchar(cur$stop_codon) >= 3 && isTRUE(as.integer(cur$partial_stop) == 1L)) {
              gene_p2 <- paste0(">", gene_p2)
            }

            # write gene feature table
            gene_tbl_fn <- file.path(export_path, paste0(.seqid, "_", cur$gene_uniq, ".tbl"))
            if (file.exists(gene_tbl_fn)) {
              file.remove(gene_tbl_fn)
            }
            cat(paste0(">Feature ", .seqid, "_", cur$gene_uniq), file = gene_tbl_fn, sep = "\n")
            paste(c(gene_p1, gene_p2, "gene"), collapse = "\t") |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            paste0("\t\t\tgene\t", cur$gene_uniq) |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            paste(c(gene_p1, gene_p2, "CDS"), collapse = "\t") |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            paste("\t\t\tproduct\t", cur$product) |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            paste("\t\t\ttransl_table\t", dat$genetic_code) |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            if (nchar(cur$stop_codon) < 3) {
              # extracted gene is oriented 5'->3', partial stop is the last base(s)
              n_stop <- nchar(cur$stop_codon)
              if (n_stop == 1) {
                gene_transl_except <- paste0("(pos:", pos2_new, ",aa:TERM)")
              } else {
                gene_transl_except <- paste0("(pos:", pos2_new - n_stop + 1, "..", pos2_new, ",aa:TERM)")
              }
              paste0("\t\t\ttransl_except\t", gene_transl_except) |>
                cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            }
            if (!cur$start_codon %in% start_codons || isTRUE(as.integer(cur$partial_start) == 1L)) {
              paste("\t\t\tcodon_start\t", 1) |>
                cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            }
            note_out <- add_gap_note(note)
            if (length(note_out) > 0) {
              paste0("\t\t\tnote\t", note_out) |>
                cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            }

            # concatenate sequences and tables by gene
            if (length(group) == 1) {
              group_gene_tbl <- file.path(group_geneName_pth, paste0(group, "_", cur$gene, ".tbl"))
              append_file(group_gene_tbl, gene_tbl_fn)
              group_gene_fasta <- file.path(group_geneName_pth, paste0(group, "_", cur$gene, ".fasta"))
              append_file(group_gene_fasta, gene_fn)
            }

            # concatenate all sequences and tables
            if (length(group) == 1) {
              append_file(group_allgene_tbl_fn, gene_tbl_fn)
              append_file(group_allgene_fasta, gene_fn)
            }
          }
        }
        return()
      }

      if (cur$type == "tRNA") {
        # write to .tbl
        write_tbl_loc(pos, "gene", tbl_fn)
        paste0("\t\t\tgene\t", cur$gene) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        write_tbl_loc(pos, "tRNA", tbl_fn)
        paste("\t\t\tproduct\t", cur$product) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        if (!is.na(cur$anticodon) && cur$anticodon != "NNN") {
          paste("\t\t\tnote\t", paste0("anticodon:", tolower(cur$anticodon))) |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
        }

        # write to GFF
        # tRNA feature
        f9 = paste0("ID=rna-",seq_name,":",cur$pos1,"..",cur$pos2,";Name=",cur$gene,";gbkey=tRNA;product=",cur$product)
        # a feature spanning the origin is written start..(asmb_len + end)
        paste(c(seq_name, "MitoPilot", "tRNA", cur$pos1, gff_end(cur$pos2, wraps, asmb_len), ".", cur$direction, ".", f9), collapse = "\t") |>
          cat(file = gff_fn, sep = "\n", append = TRUE)

        # exon feature
        f9 = paste0("ID=exon-",seq_name,":",cur$pos1,"..",cur$pos2,";Parent=rna-",seq_name,":",cur$pos1,"..",cur$pos2,";gbkey=tRNA;product=",cur$product)
        # a feature spanning the origin is written start..(asmb_len + end)
        paste(c(seq_name, "MitoPilot", "exon", cur$pos1, gff_end(cur$pos2, wraps, asmb_len), ".", cur$direction, ".", f9), collapse = "\t") |>
          cat(file = gff_fn, sep = "\n", append = TRUE)

        return()
      }

      if (cur$type == "rRNA") {
        # Export naming: rrnS -> rrn12, rrnL -> rrn16
        rrna_gene      <- .export_rrna_name(cur$gene)
        rrna_gene_uniq <- .export_rrna_name(cur$gene_uniq)
        # Manual 5'/3' partial flags: '<' prepends the start coord, '>' the stop
        # coord (pos is already strand-oriented, first interval first).
        rrna_note <- NULL
        if (isTRUE(as.integer(cur$partial_start) == 1L)) {
          pos[[1]][1] <- paste0("<", pos[[1]][1])
          rrna_note <- "5' end not determined"
        }
        if (isTRUE(as.integer(cur$partial_stop) == 1L)) {
          pos <- mark_tbl_3p(pos)
          rrna_note <- paste(c(rrna_note, "3' end not determined"), collapse = "; ")
        }
        # write to .tbl
        write_tbl_loc(pos, "gene", tbl_fn)
        paste0("\t\t\tgene\t", rrna_gene) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        write_tbl_loc(pos, "rRNA", tbl_fn)
        paste("\t\t\tproduct\t", cur$product) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        if (length(rrna_note) > 0) {
          paste0("\t\t\tnote\t", rrna_note) |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
        }

        # write to GFF
        # rRNA feature
        f9 = paste0("ID=rna-",seq_name,":",cur$pos1,"..",cur$pos2,";Name=",rrna_gene,";gbkey=rRNA;product=",cur$product)
        # a feature spanning the origin is written start..(asmb_len + end)
        paste(c(seq_name, "MitoPilot", "rRNA", cur$pos1, gff_end(cur$pos2, wraps, asmb_len), ".", cur$direction, ".", f9), collapse = "\t") |>
          cat(file = gff_fn, sep = "\n", append = TRUE)
        # exon feature
        f9 = paste0("ID=exon-",seq_name,":",cur$pos1,"..",cur$pos2,";Parent=rna-",seq_name,":",cur$pos1,"..",cur$pos2,";gbkey=rRNA;product=",cur$product)
        # a feature spanning the origin is written start..(asmb_len + end)
        paste(c(seq_name, "MitoPilot", "exon", cur$pos1, gff_end(cur$pos2, wraps, asmb_len), ".", cur$direction, ".", f9), collapse = "\t") |>
          cat(file = gff_fn, sep = "\n", append = TRUE)

        # export rRNAs individually
        if(gene_export){
          # EXTRACT GENE FROM ASSEMBLY
          # make directory for gene if it doesn't exist
          group_geneName_pth <- file.path(group_pth, "genes", rrna_gene)
          dir.create(group_geneName_pth, recursive = T, showWarnings = F)

          # get gene region from assembly
          gene = extract_circ_region(seq, cur$pos1, cur$pos2)

          # update FASTA header with gene name
          head_split <- strsplit(fasta_header_gene, "\\s+")
          head_split[[1]][1] <- paste0(head_split[[1]][1], "_", rrna_gene_uniq)
          head_split[[1]][length(head_split[[1]])] <- paste0(head_split[[1]][length(head_split[[1]])], ", ", cur$product)
          head <- paste(c(head_split[[1]]), sep=" ", collapse=" ")
          names(gene) <- stringr::str_glue_data(dat, head)

          # reverse complement if needed
          if (cur$direction == "-") {
            gene = Biostrings::reverseComplement(gene)
          }

          # write FASTA
          gene_fn <- file.path(export_path, paste0(.seqid, "_", rrna_gene_uniq, ".fasta"))
          Biostrings::writeXStringSet(gene, filepath = gene_fn)

          # fix the start and stop position; carry the 5'/3' partial markers
          # (extracted gene is 5'->3', so pos1_new = 5', pos2_new = 3').
          pos1_new = 1
          pos2_new = length(gene[[1]])
          gene_p1 <- as.character(pos1_new)
          gene_p2 <- as.character(pos2_new)
          if (isTRUE(as.integer(cur$partial_start) == 1L)) gene_p1 <- paste0("<", gene_p1)
          if (isTRUE(as.integer(cur$partial_stop) == 1L)) gene_p2 <- paste0(">", gene_p2)

          # write gene feature table
          gene_tbl_fn <- file.path(export_path, paste0(.seqid, "_", rrna_gene_uniq, ".tbl"))
          if (file.exists(gene_tbl_fn)) {
            file.remove(gene_tbl_fn)
          }
          cat(paste0(">Feature ", .seqid, "_", rrna_gene_uniq), file = gene_tbl_fn, sep = "\n")
          paste(c(gene_p1, gene_p2, "gene"), collapse = "\t") |>
            cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
          paste0("\t\t\tgene\t", rrna_gene_uniq) |>
            cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
          paste(c(gene_p1, gene_p2, "rRNA"), collapse = "\t") |>
            cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
          paste("\t\t\tproduct\t", cur$product) |>
            cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
          if (length(rrna_note) > 0) {
            paste0("\t\t\tnote\t", rrna_note) |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
          }

          # concatenate sequences and tables by gene
          if (length(group) == 1) {
            group_gene_tbl <- file.path(group_geneName_pth, paste0(group, "_", rrna_gene, ".tbl"))
            append_file(group_gene_tbl, gene_tbl_fn)
            group_gene_fasta <- file.path(group_geneName_pth, paste0(group, "_", rrna_gene, ".fasta"))
            append_file(group_gene_fasta, gene_fn)
          }

          # concatenate all sequences and tables
          if (length(group) == 1) {
            append_file(group_allgene_tbl_fn, gene_tbl_fn)
            append_file(group_allgene_fasta, gene_fn)
          }
        }

        return()
      }

      if (cur$type == "ctrl") {
        # write to .tbl
        write_tbl_loc(pos, "D-loop", tbl_fn)
        paste0("\t\t\tnote\tcontrol region") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)

        # write to GFF
        f9 = paste0("ID=ctrl-",seq_name,":",cur$pos1,"..",cur$pos2,";gbkey=D-loop;Note=control region")
        # a feature spanning the origin is written start..(asmb_len + end); on a
        # linear contig pos1 > pos2 is a coordinate mixup, so flip it instead
        if (!wraps && cur$pos1 > cur$pos2) {
          paste(c(seq_name, "MitoPilot", "D_loop", cur$pos2, cur$pos1, ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        } else {
          paste(c(seq_name, "MitoPilot", "D_loop", cur$pos1, gff_end(cur$pos2, wraps, asmb_len), ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        }

        return()
      }

      if (cur$type == "ORF") {
        # Unassigned ORFs: export as a hypothetical-protein CDS. ORFs have NA
        # start/stop codons, so none of the PCG codon logic (partial markers,
        # transl_except, codon membership checks) applies.
        product <- "hypothetical protein"
        note <- "open reading frame predicted by ORFfinder"

        # write to .tbl
        write_tbl_loc(pos, "gene", tbl_fn)
        paste0("\t\t\tgene\t", cur$gene) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        write_tbl_loc(pos, "CDS", tbl_fn)
        paste("\t\t\tproduct\t", product) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste("\t\t\ttransl_table\t", dat$genetic_code) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste("\t\t\tcodon_start\t", 1) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste0("\t\t\tnote\t", note) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)

        # write to GFF
        # gene feature
        f9 = paste0("ID=gene-",cur$gene,";Name=",cur$gene,";gbkey=Gene;gene=",cur$gene,";gene_biotype=protein_coding")
        # a feature spanning the origin is written start..(asmb_len + end)
        paste(c(seq_name, "MitoPilot", "gene", cur$pos1, gff_end(cur$pos2, wraps, asmb_len), ".", cur$direction, ".", f9), collapse = "\t") |>
          cat(file = gff_fn, sep = "\n", append = TRUE)

        # CDS feature
        f9 = paste0("ID=cds-",cur$gene,";Parent=gene-",cur$gene,";Name=",cur$gene,";gbkey=CDS;gene=",cur$gene,";product=",product,";transl_table=",dat$genetic_code,";Note=",note)
        # a feature spanning the origin is written start..(asmb_len + end)
        paste(c(seq_name, "MitoPilot", "CDS", cur$pos1, gff_end(cur$pos2, wraps, asmb_len), ".", cur$direction, "0", f9), collapse = "\t") |>
          cat(file = gff_fn, sep = "\n", append = TRUE)

        # Per-gene FASTA/tbl export is intentionally skipped for ORFs: ORF.N
        # numbering is per-sample and not orthologous across samples, so the
        # gene-grouped files must not mix ORFs from different samples.

        return()
      }
    })

    # Any gap past the last annotated feature.
    flush_gaps()

    if (length(group) == 1) {
      append_file(group_tbl, tbl_fn)
      file.copy(gff_fn, group_gff_pth, overwrite = TRUE)
      append_file(group_fasta, fasta_fn)
    }
  })

  # Per-sample summary CSV, dropped into the export directory
  if (isTRUE(summary_csv)) {
    drop <- c("poor_blast_ref", "blast_ref_status", "curate_opts")
    # seqid/path/scaffold lead: a sample can contribute several records, so the row
    # identity is the unit, not the ID.
    core <- c("ID", "seqid", "path", "scaffold",
              "Taxon", "topology", "completeness", "partial", "length",
              "structure", "PCGCount", "tRNACount", "rRNACount", "ORFCount",
              "missing", "extra", "warnings", "blast_accession", "blast_species",
              "blast_lineage", "export_group")
    summary_df <- fetch_export_data(con = con) |>
      dplyr::filter(ID %in% !!IDs) |>
      dplyr::select(-dplyr::any_of(drop)) |>
      dplyr::relocate(dplyr::any_of(core))
    summary_fn <- if (length(group) == 1) {
      file.path(group_pth, paste0(group, "_sample_info.csv"))
    } else {
      file.path(out_dir, paste0("sample_info_", Sys.Date(), ".csv"))
    }
    utils::write.csv(summary_df, summary_fn, row.names = FALSE)
  }

  # Mark the exported units with the export time so the Annotate tab can flag /
  # highlight what has already been exported (NULL = never exported).
  if (length(group) == 1) {
    DBI::dbExecute(
      con,
      "UPDATE export SET export_time_stamp = ? WHERE export_group = ?",
      params = list(as.integer(Sys.time()), group)
    )
  }

  db_path <- file.path(dirname(out_dir), ".sqlite")

  # Units, not IDs: the AA alignment / outlier review compare records, and one
  # sample can now contribute several.
  if (length(group) == 1 && nrow(units) > 1 && generateAAalignments) {
    make_PCG_alignments(
      export_group = group,
      db = db_path,
      out_path = group_pth,
      start_aa = start_aa,
      stop_aa = stop_aa,
      ident_pct = ident_pct
    )
  }

  if (review && length(group) == 1 && nrow(units) > 1) {
    return(invisible(flag_PCG_outliers(
      group = group,
      db = db_path,
      start_aa = start_aa,
      stop_aa = stop_aa,
      ident_pct = ident_pct
    )))
  }

  invisible(NULL)
}


#' Generate HTML report woth PCG alignments
#'
#' @param db path for sqlite database
#' @param out_path path for output files
#' @param export_group Name of the submission group
#' @param start_aa Start-offset threshold (amino acids) for the flagged-outlier
#'   section of the report. Default 10.
#' @param stop_aa Stop-offset threshold (amino acids) for the flagged-outlier
#'   section of the report. Default 10.
#' @param ident_pct Identity threshold (percent) for the flagged-outlier section
#'   of the report. Default 60.
#'
#' @export
#'
make_PCG_alignments <- function(
    export_group = NULL,
    db = NULL,
    out_path = NULL,
    start_aa = 10,
    stop_aa = 10,
    ident_pct = 60) {
  rmarkdown::render(
    input = system.file("AA_alignment_report.Rmd", package = "MitoPilot"),
    output_file = stringr::str_glue("AA_alignments_{export_group}.html"),
    output_dir = out_path,
    intermediates_dir = getwd(),
    knit_root_dir = getwd(),
    params = list(
      group = export_group,
      db_path = db,
      start_aa = start_aa,
      stop_aa = stop_aa,
      ident_pct = ident_pct
    )
  )
}


#' Collect PCG annotations for an export group (with exons merged)
#'
#' Pulls all protein-coding gene (PCG) annotations for the samples in an export
#' group and, for genes flagged as intron-containing in the curation rules,
#' merges multi-exon entries into a single translated record. The merged record
#' keeps the first exon's row, has its `translation` recomputed from the spliced
#' sequence, and its `ID` prefixed with `*` to mark the merge. Shared by the
#' AA alignment report (`inst/AA_alignment_report.Rmd`) and
#' [flag_PCG_outliers()] so both use identical sequences.
#'
#' @param con An open SQLite connection to the project database.
#' @param group Name of the export group.
#'
#' @return A data frame of PCG annotations (one row per sample/gene after
#'   merging), including `ID`, `path`, `scaffold`, `gene`, `pos1`, `pos2`,
#'   `direction`, and `translation`.
#'
#' @noRd
get_export_PCG_annotations <- function(con, group) {
  # Units in the group, with the SeqID that labels each record downstream.
  units <- dplyr::tbl(con, "assemblies") |>
    dplyr::filter(ignore == 0) |>
    dplyr::select(ID, path, scaffold) |>
    dplyr::inner_join(
      dplyr::tbl(con, "export") |>
        dplyr::filter(export_group == !!group) |>
        dplyr::select(ID, path, scaffold),
      by = c("ID", "path", "scaffold")
    ) |>
    dplyr::collect() |>
    dplyr::arrange(ID, path, scaffold) |>
    dplyr::group_by(ID) |>
    dplyr::mutate(n_units = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(seqid = export_seqid(ID, path, scaffold, n_units))

  # PCGs per unit. Keyed on (ID, path, scaffold), not ID: two scaffolds of one
  # sample can each carry a gene of the same name, and an ID-only join would make
  # them look like two exons of one CDS - which the intron rule below would then
  # splice together into a single translation.
  annotations <- units |>
    dplyr::select(ID, path, scaffold, seqid) |>
    dplyr::left_join(
      dplyr::tbl(con, "annotations") |>
        dplyr::filter(pos1 > 0 & type == "PCG") |>
        dplyr::collect(),
      by = c("ID", "path", "scaffold")
    ) |>
    # A fragmented sample can have scaffolds carrying no PCG at all (e.g. an
    # rRNA-only fragment); those join to NA and would otherwise surface as all-NA
    # rows wherever `gene == <g>` is subset.
    dplyr::filter(!is.na(gene))

  # Row indices of exons removed after being merged into their first exon
  rows_to_remove <- integer(0)

  # The per-unit loop below only does anything for units carrying a multi-exon
  # (intron) gene: the curate/sample reads and the full-scaffold get_assembly read
  # feed only the intron-merge branch, and every single-exon gene hits the
  # length<=1 skip. Reading each unit's whole assembly regardless is the dominant
  # cost of a "Back to Review" recompute, so visit only units that can merge.
  # Output is unchanged: skipped units overwrite no translation and remove no rows.
  merge_units <- annotations |>
    dplyr::count(ID, path, scaffold, gene) |>
    dplyr::filter(n > 1) |>
    dplyr::distinct(ID, path, scaffold)
  loop_units <- dplyr::semi_join(units, merge_units, by = c("ID", "path", "scaffold"))

  for (.i in seq_len(nrow(loop_units))) {
    u <- loop_units[.i, ]
    # Curation rules for the current unit
    curation_opts <- unit_curate_opts(con, u$ID, u$path, u$scaffold)

    curate_rules <- dplyr::tbl(con, "curate_opts") |>
      dplyr::filter(curate_opts == !!curation_opts) |>
      dplyr::pull("params") |>
      jsonlite::fromJSON()

    # Sample data (genetic_code); topology/path come from the unit itself
    dat <- dplyr::tbl(con, "samples") |>
      dplyr::select(-dplyr::any_of("topology")) |>
      dplyr::filter(ID == !!u$ID) |>
      dplyr::collect()

    seq <- get_assembly(
      ID = u$ID,
      path = u$path,
      scaffold = u$scaffold,
      con = con
    )

    unit_rows <- which(annotations$ID == u$ID &
                         annotations$path == u$path &
                         annotations$scaffold == u$scaffold)
    genes_in_unit <- unique(annotations$gene[unit_rows])

    for (current_gene in genes_in_unit) {
      exons_idx <- unit_rows[which(annotations$gene[unit_rows] == current_gene)]
      if (any(exons_idx %in% rows_to_remove)) next
      if (length(exons_idx) <= 1) next

      cur <- annotations[exons_idx[1], ] # Use the first exon as the reference
      cur_rules <- curate_rules$rules[[cur$gene]]
      intron <- cur_rules$intron %||% curate_rules$default_rules$PCG$intron %||% FALSE

      if (intron) {
        exons <- annotations[exons_idx, ]
        exon_seqs <- character(nrow(exons))

        message(paste("Merged ", length(exon_seqs), " exons for gene ", cur$gene, " (", u$seqid, ")", sep = ""))

        if (all(exons$direction %in% c("+", "-")) && length(unique(exons$direction)) == 1L) {
          # extract_circ_region, not subseq: an exon spanning the origin is
          # stored pos1 > pos2 and subseq() aborts on it
          for (i in 1:nrow(exons)) {
            exon_seqs[i] <- as.character(
              extract_circ_region(seq, exons$pos1[i], exons$pos2[i])
            )
          }
          merged_sequence <- Biostrings::DNAString(paste(exon_seqs, collapse = ""))
          if (exons$direction[1] == "-") {
            merged_sequence <- Biostrings::reverseComplement(merged_sequence)
          }
        } else {
          message(crayon::red(paste("Warning: exons on opposite strands for gene", cur$gene)))
          next
        }

        translation <- Biostrings::translate(
          merged_sequence,
          genetic.code = Biostrings::getGeneticCode(as.character(dat$genetic_code)),
          if.fuzzy.codon = "solve"
        ) |> as.character()
        translation <- sub("\\*$", "", translation) # remove terminal stop codon

        annotations[exons_idx[1], "translation"] <- translation
        # Mark the merge on the label the report keys by (seqid), not the raw ID.
        annotations[exons_idx[1], "seqid"] <- paste0("*", annotations[exons_idx[1], "seqid"])
        rows_to_remove <- c(rows_to_remove, exons_idx[-1])
      }
    }
  }

  if (length(rows_to_remove) > 0) {
    annotations <- annotations[-rows_to_remove, ]
  }

  annotations
}


#' Flag outlier PCG annotations in an export group
#'
#' For each protein-coding gene in an export group, aligns the amino-acid
#' translations across samples and flags annotations that are likely
#' mis-positioned: those whose start or stop extends past, or falls short of,
#' the alignment's well-occupied core by more than a set number of residues
#' (pointing at a start/stop codon placed too long or too short) and those that
#' align poorly to the rest of the group (a low sequence-identity catch-all for
#' badly annotated regions).
#'
#' @param group Name of the export group.
#' @param db Path to the project SQLite database.
#' @param start_aa Start-offset threshold (amino acids). A sample is flagged
#'   when its start extends past, or falls short of, the alignment core by more
#'   than this many residues. Default 10.
#' @param stop_aa Stop-offset threshold (amino acids). As `start_aa`, but for
#'   the stop end. Default 10.
#' @param ident_pct Identity threshold (percent). A sample is flagged when its
#'   mean pairwise identity to the rest of the group is below this. Default 60.
#' @param genes Optional character vector of gene names. When supplied, only
#'   these genes are aligned and flagged (the rest are skipped), e.g. to
#'   recompute a single gene edited via "Back to Review". Default `NULL` (all
#'   genes).
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{flags}{A tibble with one row per flagged (sample, gene):
#'       `ID`, `label`, `path`, `scaffold`, `gene`, `pct_identity`,
#'       `start_offset`, `stop_offset`, `start_flag`, `stop_flag`,
#'       `identity_flag`, `issue`.}
#'     \item{alignments}{A named list (by gene) of clustering-ordered aligned
#'       `AAStringSet` objects, for every gene that has a flagged sample (plus any
#'       explicitly requested via `genes`, even if their last flag was cleared).}
#'     \item{samples}{A named list (by gene) of tibbles listing every unit in the
#'       gene's alignment (`ID`, `label`, `path`, `scaffold`), flagged or not, so
#'       the review UI can edit any sample of the gene.}
#'   }
#'
#' @export
flag_PCG_outliers <- function(group, db, start_aa = 10, stop_aa = 10, ident_pct = 60, genes = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)
  on.exit(DBI::dbDisconnect(con))

  annotations <- get_export_PCG_annotations(con, group)
  annotations <- annotations[!is.na(annotations$translation) & nzchar(annotations$translation), , drop = FALSE]
  if (nrow(annotations) == 0) {
    return(list(flags = .empty_outlier_flags(), alignments = list()))
  }

  all_genes <- sort(unique(annotations$gene))
  # Optionally restrict the (expensive) per-gene alignment loop to a subset, e.g.
  # recomputing only the single gene edited via "Back to Review".
  loop_genes <- if (!is.null(genes)) intersect(all_genes, genes) else all_genes
  flag_rows <- list()
  alignments <- list()
  # Per-gene roster of every unit in the alignment (flagged or not), so the review
  # UI can offer to edit any sample of the gene, not only the flagged ones.
  samples <- list()

  for (g in loop_genes) {
    sub <- annotations[annotations$gene == g, , drop = FALSE]
    if (nrow(sub) < 2) next

    # Label by SeqID, not ID: a fragmented sample contributes one sequence per unit,
    # and duplicate names here would make the row lookups below silently resolve to
    # the first match, misattributing or dropping the siblings.
    seqs <- Biostrings::AAStringSet(stats::setNames(sub$translation, sub$seqid))

    aln <- DECIPHER::AlignSeqs(seqs, processors = NULL, verbose = FALSE)
    dst <- DECIPHER::DistanceMatrix(
      aln,
      includeTerminalGaps = TRUE, processors = NULL, type = "dist", verbose = FALSE
    )
    clust <- stats::hclust(dst, "complete")
    aln <- aln[clust$order]
    alignments[[g]] <- aln
    samples[[g]] <- dplyr::tibble(
      ID = sub$ID, label = sub$seqid, path = sub$path, scaffold = sub$scaffold
    )

    # Mean pairwise identity of each sample to the rest of the group
    dmat <- as.matrix(DECIPHER::DistanceMatrix(
      aln,
      includeTerminalGaps = TRUE, processors = NULL, type = "matrix", verbose = FALSE
    ))
    diag(dmat) <- NA_real_
    pct_identity <- 100 * (1 - rowMeans(dmat, na.rm = TRUE))

    # Character matrix of the alignment (rows = sequences, cols = columns)
    cmat <- do.call(rbind, strsplit(as.character(aln), "", fixed = TRUE))
    rownames(cmat) <- names(aln)
    occupancy <- colMeans(cmat != "-")
    core <- which(occupancy > 0.5)
    if (length(core) == 0) next
    first_core <- min(core)
    last_core <- max(core)

    for (label in names(aln)) {
      row_chars <- cmat[label, ]
      res_cols <- which(row_chars != "-")
      if (length(res_cols) == 0) next

      # Start (N-terminus): missing core columns before this seq starts vs
      # residues extending before the core
      start_short <- sum(core < min(res_cols)) # too short: annotation starts too late
      start_long <- sum(res_cols < first_core) # too long: annotation starts too early
      # Stop (C-terminus)
      stop_short <- sum(core > max(res_cols)) # too short: annotation stops too early
      stop_long <- sum(res_cols > last_core) # too long: annotation stops too late

      start_flag <- max(start_short, start_long) > start_aa
      stop_flag <- max(stop_short, stop_long) > stop_aa

      idx <- match(label, names(aln))
      ident <- unname(pct_identity[idx])
      identity_flag <- isTRUE(ident < ident_pct)

      if (!start_flag && !stop_flag && !identity_flag) next

      # Signed per-end offset (aa) relative to the alignment core:
      # negative = end placed too short, positive = extends too long.
      start_offset <- start_long - start_short
      stop_offset <- stop_long - stop_short

      issues <- character(0)
      if (start_flag) issues <- c(issues, if (start_offset < 0) "start too short" else "start too long")
      if (stop_flag) issues <- c(issues, if (stop_offset < 0) "stop too short" else "stop too long")
      if (identity_flag) issues <- c(issues, "low identity")

      srow <- sub[match(label, sub$seqid), ]
      flag_rows[[length(flag_rows) + 1L]] <- dplyr::tibble(
        ID = srow$ID,
        label = label,
        path = srow$path,
        scaffold = srow$scaffold,
        gene = g,
        pct_identity = round(ident, 1),
        start_offset = start_offset,
        stop_offset = stop_offset,
        start_flag = start_flag,
        stop_flag = stop_flag,
        identity_flag = identity_flag,
        issue = paste(issues, collapse = ", ")
      )
    }
  }

  flags <- if (length(flag_rows) > 0) dplyr::bind_rows(flag_rows) else .empty_outlier_flags()
  # Keep alignments/rosters for genes that have a flagged sample. When a specific
  # gene set was requested (a "Back to Review" recompute of an edited gene), also
  # keep those genes even if the edit cleared their last flag, so the review can
  # still show the corrected alignment instead of a stale one.
  keep_genes <- unique(flags$gene)
  if (!is.null(genes)) keep_genes <- union(keep_genes, intersect(names(alignments), genes))
  alignments <- alignments[names(alignments) %in% keep_genes]
  samples <- samples[names(samples) %in% keep_genes]

  list(flags = flags, alignments = alignments, samples = samples)
}

# Empty flags tibble with the canonical column types
.empty_outlier_flags <- function() {
  dplyr::tibble(
    ID = character(0),
    label = character(0),
    path = integer(0),
    scaffold = integer(0),
    gene = character(0),
    pct_identity = numeric(0),
    start_offset = integer(0),
    stop_offset = integer(0),
    start_flag = logical(0),
    stop_flag = logical(0),
    identity_flag = logical(0),
    issue = character(0)
  )
}
