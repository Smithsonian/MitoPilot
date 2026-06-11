# Generate params_<clade>_mito.R for the new clades from the maintainer
# spreadsheet (~/Desktop/mitogenome_curation_rules.xlsx). Reproducible.
#
#   Rscript data-raw/generate_clade_params.R
#
# Outputs:
#   R/params_<clade>_mito.R                          (one per new clade)
#   data-raw/count_reconstruction_verification.csv   (REVIEW: corrupted Count cells)
#   data-raw/params_generation_log.txt               (codon sanitizations, choices)

suppressMessages(library(readxl))

XLSX <- path.expand("~/Desktop/mitogenome_curation_rules.xlsx")

# Spreadsheet sheet name -> MitoPilot target key (lowercase, matches wrappers)
sheet_to_key <- c(
  Ascidiacea = "ascidiacea", Bivalvia = "bivalvia", Bryozoa = "bryozoa",
  Crinoidea = "crinoidea", Demospongiae = "demospongiae", Echinoidea = "echinoidea",
  Gastropoda = "gastropoda", Holothuroidea = "holothuroidea",
  Homoscleromorpha = "homoscleromorpha", Malacostraca = "malacostraca",
  Hydrozoa = "hydrozoa", Nemertea = "nemertea", Ophiuroidea = "ophiuroidea",
  Platyhelminthes = "platyhelminthes", Polychaeta = "polychaeta",
  Pycnogonida = "pycnogonida", Sipuncula = "sipuncula", Thaliacea = "thaliacea",
  Thecostraca = "thecostraca"
)

# Gene-name aliases: spreadsheet name -> MitoPilot/ref-DB name
GENE_ALIAS <- c(heg = "lagli", mutS = "msh1")

# Default BLAST hit_threshold for these (invertebrate) clades.
HIT_THRESHOLD <- 60

# Per-PCG overlap template (reused from hexacoral/octocoral; the sheet has no
# overlap info). Genes not listed fall back to the PCG default (start=2,stop=F).
OVERLAP_STOP_TRUE  <- c("cox1", "atp8", "nad4l", "nad5", "nad6")
OVERLAP_START_WIDE <- c("atp6", "nad4")

log_lines <- character(0)
logit <- function(...) log_lines[[length(log_lines) + 1L]] <<- paste0(...)
verif <- list()  # rows for count reconstruction verification

# ---- parsers ----------------------------------------------------------------

# Parse a Count cell -> integer scalar (when min==max) or c(min,max).
# Reconstructs Excel date-coerced ranges (serial > 1000) back to "month-day".
parse_count <- function(raw, clade, gene) {
  raw0 <- raw
  was_corrupt <- FALSE
  if (is.na(raw) || raw %in% c("NA", "")) {
    val <- 1L
  } else if (grepl("^[0-9]+(\\.0)?$", raw) && as.numeric(raw) > 1000) {
    # Excel date-coercion corruption: "1-2" etc became a date serial
    d <- as.Date(as.numeric(raw), origin = "1899-12-30")
    val <- c(as.integer(format(d, "%m")), as.integer(format(d, "%d")))
    was_corrupt <- TRUE
  } else if (grepl("^[0-9]+\\s*-\\s*[0-9]+$", raw)) {
    val <- as.integer(strsplit(raw, "-")[[1]])
  } else if (grepl("^[0-9]+(\\.0+)?$", raw)) {
    val <- as.integer(as.numeric(raw))
  } else {
    stop(sprintf("Unparseable Count '%s' (%s/%s)", raw, clade, gene))
  }
  verif[[length(verif) + 1L]] <<- data.frame(
    clade = clade, gene = gene, raw_cell = as.character(raw0),
    parsed_min = min(val), parsed_max = max(val), was_corrupted = was_corrupt
  )
  val
}

# Intron cell -> TRUE if any nonzero copies possible, else FALSE.
parse_intron <- function(raw) {
  if (is.na(raw) || raw %in% c("NA", "")) return(FALSE)
  if (grepl("^0(\\.0+)?$", raw)) return(FALSE)
  nums <- as.integer(unlist(regmatches(raw, gregexpr("[0-9]+", raw))))
  length(nums) > 0 && max(nums) > 0
}

# Codon list cell -> uppercase character vector of valid 1-3mers ([ACGT]).
parse_codons <- function(raw, clade, what) {
  if (is.na(raw) || raw %in% c("NA", "none", "")) return(character(0))
  toks <- trimws(strsplit(raw, ";")[[1]])
  toks <- toupper(toks[nzchar(toks)])
  good <- grepl("^[ACGT]{1,3}$", toks)
  if (any(!good)) {
    logit(sprintf("[%s] dropped uncertain %s codon(s): %s", clade, what,
                  paste(toks[!good], collapse = ", ")))
  }
  unique(toks[good])
}

alias <- function(g) if (!is.na(GENE_ALIAS[g])) unname(GENE_ALIAS[g]) else g

# ---- R-code emitters --------------------------------------------------------

vec_lit <- function(x) {
  if (length(x) == 1L) return(as.character(x))
  paste0("c(", paste(x, collapse = ", "), ")")
}
chr_vec_lit <- function(x) paste0("c(", paste0('"', x, '"', collapse = ", "), ")")

# Build the per-gene rule list literal (multi-line, hexacoral style).
emit_rule <- function(gene, fields) {
  inner <- vapply(names(fields), function(k) sprintf("        %s = %s", k, fields[[k]]),
                  character(1))
  sprintf("      %s = list(\n%s\n      )", gene, paste(inner, collapse = ",\n"))
}

emit_params <- function(key, sheet, tt, std_start, std_stop, gene_rows) {
  rule_blocks <- character(0)
  for (r in seq_len(nrow(gene_rows))) {
    g0 <- gene_rows$gene[r]
    g  <- alias(g0)
    if (g != g0) logit(sprintf("[%s] aliased gene %s -> %s", sheet, g0, g))
    type <- gene_rows$type[r]
    fields <- list()

    if (type == "ctrl") {
      cnt <- parse_count(gene_rows$count[r], sheet, g0)
      fields[["count"]] <- vec_lit(cnt)
      fields[["type"]]  <- '"ctrl"'
      rule_blocks <- c(rule_blocks, emit_rule(g, fields))
      next
    }

    fields[["type"]] <- sprintf('"%s"', type)
    cnt <- parse_count(gene_rows$count[r], sheet, g0)
    if (!(length(cnt) == 1L && cnt == 1L)) fields[["count"]] <- vec_lit(cnt)
    if (parse_intron(gene_rows$intron[r])) fields[["intron"]] <- "TRUE"

    if (type == "rRNA") {
      if (g == "rrnL") fields[["max_len"]] <- "1850"
      if (g == "rrnS") fields[["max_len"]] <- "1000"
    }
    if (type == "PCG") {
      alt <- parse_codons(gene_rows$altstart[r], sheet, "start")
      if (length(alt)) {
        fields[["start_codons"]] <- chr_vec_lit(unique(c(std_start, alt)))
      }
      if (g %in% OVERLAP_STOP_TRUE) {
        fields[["overlap"]] <- "list(start = 2, stop = T)"
      } else if (g %in% OVERLAP_START_WIDE) {
        fields[["overlap"]] <- "list(start = 20, stop = F)"
      }
    }
    rule_blocks <- c(rule_blocks, emit_rule(g, fields))
  }

  rules_txt <- paste(rule_blocks, collapse = ",\n")

  sprintf('#\' Default curation and validation parameters for %s mitochondria
#\'
#\' Generated from the curation-rules spreadsheet (translation table %s).
#\'
#\' @param alt (optional) named list of default values to modify
#\'
#\' @export
#\'
params_%s_mito <- function(alt = list()) {
  params <- list(
    hit_threshold = %d,
    max_overlap = 0.25,
    default_rules = list(
      rRNA = list(
        count = 1,
        max_len = NA,
        min_len = NA,
        overlap = list(start = 0, stop = F)
      ),
      PCG = list(
        count = 1,
        max_len = NA,
        min_len = NA,
        overlap = list(start = 2, stop = F),
        stop_codons = %s,
        start_codons = %s,
        intron = FALSE
      ),
      tRNA = list(
        count = 1,
        max_len = NA,
        min_len = NA
      )
    ),
    rules = list(
%s
    )
  )
  params <- modify_list_recursive(params, alt)
}
', sheet, tt, key, HIT_THRESHOLD, chr_vec_lit(std_stop), chr_vec_lit(std_start), rules_txt)
}

# ---- main loop --------------------------------------------------------------

get_meta <- function(d, label) {
  row <- which(d[[1]] == label)
  if (length(row) == 0) return(NA_character_)
  as.character(d[row[1], 2])
}

for (sheet in names(sheet_to_key)) {
  key <- sheet_to_key[[sheet]]
  d <- suppressMessages(as.data.frame(
    read_excel(XLSX, sheet = sheet, col_names = FALSE)
  ))
  tt <- sub("\\.0+$", "", get_meta(d, "NCBI Translation Table:"))
  std_start <- parse_codons(get_meta(d, "Standard Start Codons:"), sheet, "standard start")
  std_stop  <- parse_codons(get_meta(d, "Standard Stop Codons:"), sheet, "standard stop")

  hr <- which(d[[1]] == "Gene")[1]
  g <- d[(hr + 1):nrow(d), 1:7, drop = FALSE]
  names(g) <- c("gene", "type", "count", "anticodon", "altstart", "altstop", "intron")
  g <- g[!is.na(g$gene) & nzchar(g$gene), , drop = FALSE]

  txt <- emit_params(key, sheet, tt, std_start, std_stop, g)
  writeLines(txt, file.path("R", sprintf("params_%s_mito.R", key)))
  logit(sprintf("[%s] -> R/params_%s_mito.R (tt=%s, %d genes)", sheet, key, tt, nrow(g)))
}

# ---- write review artifacts -------------------------------------------------
verif_df <- do.call(rbind, verif)
write.csv(verif_df, "data-raw/count_reconstruction_verification.csv", row.names = FALSE)
writeLines(log_lines, "data-raw/params_generation_log.txt")

cat(sprintf("Generated %d params files.\n", length(sheet_to_key)))
cat(sprintf("Count cells reconstructed (corrupted): %d of %d total.\n",
            sum(verif_df$was_corrupted), nrow(verif_df)))
