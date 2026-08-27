#' Validate a mapping file's Topology column
#'
#' The column is optional. When present it may only declare a topology the user
#' can actually vouch for; "multi" is derived from the assembly, never typed.
#'
#' @param mapping mapping data frame
#' @param mapping_id name of the ID column
#'
#' @return (invisibly) NULL; stops on an invalid value
#'
#' @noRd
validate_declared_topology <- function(mapping, mapping_id = "ID") {
  if ("Topology" %nin% colnames(mapping)) {
    return(invisible(NULL))
  }
  declared <- as.character(mapping[["Topology"]])
  bad <- !is.na(declared) & nzchar(declared) & declared %nin% c("circular", "linear")
  if (any(bad)) {
    message("problematic samples:")
    message(paste(mapping[[mapping_id]][bad], collapse = ", "))
    stop("Values in the Topology column must be either lowercase \"circular\" or \"linear\"")
  }
  invisible(NULL)
}

#' Count the contigs in each user-supplied assembly
#'
#' Headers only, so a whole-genome FASTA is never read into memory. Anything
#' that cannot be opened or parsed counts as unknown (`NA`) rather than an
#' error: the assembly directory is not always reachable from wherever the
#' project is being set up.
#'
#' @param files assembly file names, one per sample
#' @param assembly_path directory holding them, or NULL / "NA"
#'
#' @return integer vector of contig counts, `NA` where unknown
#'
#' @noRd
count_assembly_contigs <- function(files, assembly_path = NULL) {
  dir_ok <- !is.null(assembly_path) && length(assembly_path) == 1L &&
    !is.na(assembly_path) && nzchar(assembly_path) && assembly_path %nin% "NA"

  vapply(as.character(files), function(f) {
    if (is.na(f) || !nzchar(f)) {
      return(NA_integer_)
    }
    fn <- if (dir_ok) file.path(assembly_path, f) else f
    if (!file.exists(fn)) {
      return(NA_integer_)
    }
    tryCatch(
      length(Biostrings::fasta.seqlengths(fn)),
      error = function(e) NA_integer_
    )
  }, integer(1), USE.NAMES = FALSE)
}

#' Resolve each sample's topology from its assembly
#'
#' A user assembly holding more than one contig has no single topology, so the
#' sample is recorded as "multi" and any declaration is ignored (the pipeline
#' works out each contig's own topology instead). A single-contig assembly keeps
#' whatever the user declared, defaulting to "linear".
#'
#' @param mapping mapping data frame, with an `Assembly` column
#' @param assembly_path directory holding the assembly files, or NULL / "NA"
#' @param mapping_id name of the ID column
#'
#' @return character vector, one topology per row of `mapping`
#'
#' @noRd
resolve_sample_topology <- function(mapping, assembly_path = NULL, mapping_id = "ID") {
  declared <- if ("Topology" %in% colnames(mapping)) {
    as.character(mapping[["Topology"]])
  } else {
    rep(NA_character_, nrow(mapping))
  }
  stated <- !is.na(declared) & nzchar(declared)
  declared[!stated] <- "linear"

  n_contigs <- count_assembly_contigs(mapping[["Assembly"]], assembly_path)
  multi <- !is.na(n_contigs) & n_contigs > 1L

  if (any(multi & stated)) {
    warning(
      "Ignoring the declared topology for multi-contig assemblies: ",
      paste(mapping[[mapping_id]][multi & stated], collapse = ", "),
      call. = FALSE
    )
  }
  declared[multi] <- "multi"
  declared
}

#' Assembly directory recorded in a project's config
#'
#' @param path project directory
#'
#' @return the directory, or NULL when the project has none
#'
#' @noRd
project_asmb_dir <- function(path) {
  cfg <- file.path(path, ".config")
  if (!file.exists(cfg)) {
    return(NULL)
  }
  line <- grep("^\\s*asmbDir\\s*=", readLines(cfg), value = TRUE)
  if (length(line) == 0L) {
    return(NULL)
  }
  dir <- sub(".*asmbDir\\s*=\\s*'([^']*)'.*", "\\1", line[1])
  if (!nzchar(dir) || identical(dir, "NA")) NULL else dir
}
