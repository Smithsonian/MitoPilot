#' Validate a MITOS2 reference database directory
#'
#' MITOS2 needs a real reference directory: `auxinfo.json` (per-feature length
#' and start/stop statistics) plus the `ncRNA/` covariance models. The bundled
#' Metazoa_RefSeq231 / Metazoa_RefSeq235 tarballs are curation BLAST databases
#' (featureProt/featureNuc only) and have neither, so MITOS2 aborts on them.
#'
#' @param ref_db name of the reference database directory.
#' @param refdir directory containing `ref_db` (MITOS2 is always run with
#'   `--refdir .`, so this is the working directory).
#'
#' @noRd
check_mitos_ref_db <- function(ref_db, refdir = ".") {
  db_path <- file.path(refdir, ref_db)
  if (!dir.exists(db_path)) {
    stop(
      "MITOS2 reference database '", ref_db, "' not found in '",
      normalizePath(refdir, mustWork = FALSE), "'.",
      call. = FALSE
    )
  }
  missing <- c(
    if (!file.exists(file.path(db_path, "auxinfo.json"))) "auxinfo.json",
    if (!dir.exists(file.path(db_path, "ncRNA"))) "ncRNA/"
  )
  if (length(missing) > 0) {
    stop(
      "'", ref_db, "' is not a MITOS2 reference database (missing ",
      paste(missing, collapse = ", "), "). Curation-only databases such as ",
      "Metazoa_RefSeq231 and Metazoa_RefSeq235 (and custom databases built ",
      "from them) cannot be used for annotation. Set the annotate ref_db to ",
      "Metazoa_RefSeq89 or Chordata.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Parse the result.fas files MITOS2 wrote into `dir`. Pulled out of
# annotate_mitos2() so the empty case (a contig MITOS2 found no genes on) can be
# tested without running MITOS2.
parse_mitos_dir <- function(dir, assembly, contig_lens, genetic_code) {
    list.files(dir,
               recursive = T,
               full.names = T,
               pattern = "result.fas") |>
    purrr::map_dfr( ~ {
      annotations <- Biostrings::readDNAStringSet(.x) |>
        {
          \(x)
          data.frame(
            contig = stringr::str_extract(names(x), "^(.*?)(?=;)"),
            gene = stringr::str_extract(stringr::str_extract(names(x), "\\S+$"), "^[a-zA-Z0-9]+"),
            geneId = stringr::str_extract(names(x), "\\S+$"),
            # str_split_fixed, not str_split(simplify): MITOS2 writes an empty
            # result.fas for a contig it finds no genes on, and indexing a
            # column of a zero-row split is an error.
            pos1 = stringr::str_split_fixed(names(x), " *; *", 4)[, 2] |>
              stringr::str_extract("^[0-9]+") |> as.numeric(),
            pos2 = stringr::str_split_fixed(names(x), " *; *", 4)[, 2] |>
              stringr::str_extract("[0-9]+$") |> as.numeric(),
            direction = stringr::str_split_fixed(names(x), " *; *", 4)[, 3],
            row.names = NULL
          ) |>
            dplyr::mutate(
              type = dplyr::case_when(
                stringr::str_detect(gene, "^rrn[LS]") ~ "rRNA",
                stringr::str_detect(gene, "^trn") ~ "tRNA",
                stringr::str_detect(gene, "^Intron") ~ "intron",
                stringr::str_detect(gene, "^OL") ~ "OL",
                stringr::str_detect(gene, "^OH") ~ "ctrl",
                .default = "PCG"
              ),
              .before = "gene"
            ) |>
            dplyr::mutate(
              product = dplyr::case_when(
                stringr::str_detect(gene, "rrnL") ~ "16S ribosomal RNA",
                stringr::str_detect(gene, "rrnS") ~ "12S ribosomal RNA",
                stringr::str_detect(gene, "OH") ~ "d-loop",
                type == "tRNA" ~ paste0("tRNA-", trnA_key_MITOS[gene]),
                type == "PCG" ~ CDS_key[gene],
                .default = NA_character_
              ),
              .after = "gene"
            ) |>
            dplyr::mutate(
              anticodon = dplyr::case_when(
                type == "tRNA" ~ stringr::str_replace(
                  toupper(
                    stringr::str_extract(
                      stringr::str_extract(names(x), "\\S+$"),
                      "(?<=\\()[^\\^\\)]+"
                    )
                  ),
                  "^---$", "NNN"  # replace "---" with "NNN", to make mitos results compatible with tRNAscan results
                                  # this is caused by failure to detect the anticodon region
                                  # e.g. https://github.com/UCSC-LoweLab/tRNAscan-SE/issues/33
                ),
                .default = NA_character_
              ),
              .after = "direction"
            ) |>
            dplyr::mutate(
              tRNA_ID = dplyr::case_when(
                type == "tRNA" ~ paste0(product, "-", anticodon), # create temporary ID to compare with tRNAscan-SE results
                .default = NA_character_
              ),
              .after = "direction"
            ) |>
            # pos1 > pos2 means the feature spans the origin of a circular
            # contig, so its length is the arc around the origin (circ_len),
            # measured against THIS contig's length.
            dplyr::mutate(
              length = as.numeric(dplyr::case_when(
                pos1 < pos2 ~ pos2 - pos1 + 1,
                pos1 > pos2 ~ unname(contig_lens[contig]) - pos1 + pos2 + 1,
                .default = NA_real_
              )),
              .before = "direction"
            ) |>
            dplyr::filter(
              gene != "OH" |
                stringr::str_detect(geneId, "OH_0") |
                stringr::str_detect(geneId, "OH$")
            ) |>
            # dplyr evaluates a rowwise expression once on an empty frame to
            # infer its type, and `if (type == "PCG")` on a zero-length value is
            # an error. A contig MITOS2 found nothing on gets there.
            (\(d) if (nrow(d) == 0L) {
              dplyr::mutate(d, start_codon = NA_character_,
                            stop_codon = NA_character_, translation = NA_character_)
            } else {
              d |>
            dplyr::rowwise() |>
            dplyr::mutate(
              # Coding-strand sequence for this feature. extract_circ_region()
              # reads across the origin, so a feature stored pos1 > pos2 (it wraps
              # the origin of a circular contig) needs no special case, and the
              # contig length is taken from the contig itself rather than from a
              # shared scalar that only ever described the first one.
              .cds = if (type == "PCG") {
                suppressWarnings({
                  s <- extract_circ_region(assembly[contig], pos1, pos2)
                  if (direction == "-") s <- Biostrings::reverseComplement(s)
                  unname(as.character(s))
                })
              } else NA_character_,
              start_codon = if (type == "PCG" && !is.na(.cds)) {
                substr(.cds, 1L, 3L)
              } else NA_character_,
              stop_codon = if (type == "PCG" && !is.na(.cds)) {
                n <- nchar(.cds)
                len <- if (n %% 3L == 0L) 3L else n %% 3L
                substr(.cds, n - len + 1L, n)
              } else NA_character_,
              translation = if (type == "PCG" && !is.na(.cds)) {
                suppressWarnings(
                  Biostrings::DNAString(substr(.cds, 1L, nchar(.cds) - nchar(stop_codon))) |>
                    Biostrings::translate(
                      genetic.code = Biostrings::getGeneticCode(genetic_code),
                      if.fuzzy.codon = "solve"
                    ) |>
                    as.character()
                )
              } else NA_character_
            ) |>
            dplyr::ungroup() |>
            dplyr::select(-.cds)
            })()
        }()

      annotations <- annotations |>
        dplyr::select(-dplyr::any_of('geneId'))

      ###################
      return(annotations)
      ###################

    })
}

#' Annotate mitochondrial genomes using MITOS2
#'
#' @param assembly a DNAString object
#' @param topology "circular" or "linear"
#' @param genetic_code NCBI genetic code number (default: 2)
#' @param ref_db Mitos2 reference database (default: "Chordata")
#' @param mitos_opts Additional command line options for MITOS2
#' @param out output directory
#' @param condaenv Conda environment to run MITOS2 (default: "mitos")
#' @param rescue_no_trna Run MITOS2 a second time with tRNA prediction disabled
#'   (`--trna 0`) and add any PCGs/rRNAs it uniquely recovers to the full-run
#'   annotations (default: TRUE). MITOS2 discards rRNAs/PCGs whose locus overlaps
#'   a predicted tRNA; the tRNA-free run recovers them (e.g. scyphozoan rrnS).
#'
#' @export
#'
annotate_mitos2 <- function(
    assembly = NULL,
    topology = "circular",
    genetic_code = "2",
    ref_db = "Chordata",
    mitos_opts = "--best --intron 0 --oril 0",
    out = NULL,
    condaenv = "mitos",
    rescue_no_trna = TRUE) {
  check_mitos_ref_db(ref_db)
  genetic_code <- as.character(genetic_code)
  # write MITOS2 output into the local work dir so it is retained for inspection
  out <- out %||% "MITOS2_temp"
  dir.create(out, showWarnings = FALSE, recursive = TRUE)

  fasta <- tempfile(fileext = ".fa")

  names(assembly) <- stringr::str_extract(names(assembly), "^\\S+")
  Biostrings::writeXStringSet(assembly, fasta)

  # Run MITOS2 into `out_dir`, appending `extra_opts` (e.g. "--trna 0") to the
  # configured mitos_opts.
  run_mitos <- function(out_dir, extra_opts = "") {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    process_args <- list(
      cmd = "runmitos",
      args = stringr::str_glue(
        "--input {fasta}",
        "--outdir {out_dir}",
        "--code {genetic_code}",
        "--refseqver {ref_db}",
        "--refdir .",
        "{ifelse(topology != 'circular', '--linear', '')}",
        "{mitos_opts}",
        "{extra_opts}",
        "--noplots",
        .sep = " "
      ) |>
        stringr::str_squish()
    )
    if (!is.null(condaenv)) {
      process <- reticulate::conda_run2
      process_args$envname <- condaenv
      process_args$echo <- FALSE
    } else {
      process <- "system2"
    }
    do.call(process, process_args)
  }

  message("starting MITOS2")
  message(paste("MITOS2 out dir:", out))
  run_mitos(out)
  message("finished MITOS2")

  # Per-contig lengths, keyed by contig name (a single scalar taken from the
  # first sequence is wrong the moment an assembly has more than one contig).
  contig_lens <- setNames(Biostrings::width(assembly), names(assembly))

  # Format Mitos Output ----
  annotations_mitos <- parse_mitos_dir(out, assembly, contig_lens, genetic_code)

  # Optional rescue run: MITOS2 discards rRNAs/PCGs whose locus overlaps a
  # predicted tRNA (e.g. a scyphozoan rrnS wedged against nad5 with a tRNA in its
  # span). A second MITOS2 run with tRNA prediction disabled recovers them. Keep
  # every feature from the full run and add only the PCGs/rRNAs the tRNA-free run
  # uniquely found; tRNAs still come from the full run (and tRNAscan-SE).
  if (isTRUE(rescue_no_trna)) {
    out_nt <- paste0(out, "_noTRNA")
    message("starting MITOS2 rescue run (--trna 0)")
    run_mitos(out_nt, extra_opts = "--trna 0")
    message("finished MITOS2 rescue run")
    rescue_extra <- parse_mitos_dir(out_nt, assembly, contig_lens, genetic_code) |>
      dplyr::filter(type %in% c("PCG", "rRNA") & !(gene %in% annotations_mitos$gene))
    if (nrow(rescue_extra) > 0) {
      message(sprintf("MITOS2 rescue run recovered: %s",
                      paste(rescue_extra$gene, collapse = ", ")))
      annotations_mitos <- dplyr::bind_rows(annotations_mitos, rescue_extra)
    }
  }

  annotations_mitos
}

# PCG key for full metazoan dataset ----
CDS_key <- c(
  nad1 = "NADH dehydrogenase subunit 1",
  nad2 = "NADH dehydrogenase subunit 2",
  cox1 = "cytochrome c oxidase subunit 1",
  cox2 = "cytochrome c oxidase subunit 2",
  cox3 = "cytochrome c oxidase subunit 3",
  atp8 = "ATP synthase F0 subunit 8",
  atp6 = "ATP synthase F0 subunit 6",
  atp9 = "ATP synthase F0 subunit 9",
  nad3 = "NADH dehydrogenase subunit 3",
  nad4l = "NADH dehydrogenase subunit 4L",
  nad4 = "NADH dehydrogenase subunit 4",
  nad5 = "NADH dehydrogenase subunit 5",
  nad6 = "NADH dehydrogenase subunit 6",
  cob = "cytochrome b",
  dpo = "DNA-polymerase",
  lagli = "homing endonuclease",
  msh1 = "MutS mismatch DNA repair protein",
  mttb = "trimethylamine methyltransferase"
)

# Key for translating tRNA codes to Amino Acid codes
trnA_key_MITOS <- c(
  trnF = "Phe",
  trnV = "Val",
  trnL1 = "Leu",
  trnL2 = "Leu",
  trnI = "Ile",
  trnQ = "Gln",
  trnM = "Met",
  trnW = "Trp",
  trnA = "Ala",
  trnN = "Asn",
  trnC = "Cys",
  trnY = "Tyr",
  trnS1 = "Ser",
  trnS2 = "Ser",
  trnD = "Asp",
  trnK = "Lys",
  trnG = "Gly",
  trnR = "Arg",
  trnH = "His",
  trnE = "Glu",
  trnT = "Thr",
  trnP = "Pro"
)

