#' Interactive browser for MitoPilot curation rulesets
#'
#' Generates a self-contained, interactive HTML visualization of the taxon-specific
#' curation rulesets bundled with MitoPilot. The left side of the page is a
#' collapsible taxonomy tree (you can scroll, expand, and collapse clades); the
#' right side displays the curation rules for the currently selected clade.
#'
#' The taxonomy backbone is built by querying NCBI's Taxonomy database
#' (\url{https://www.ncbi.nlm.nih.gov/taxonomy}) for the full lineage of each
#' ruleset's anchor clade and merging those lineages into a single tree. Each
#' leaf in the tree corresponds to one MitoPilot ruleset (e.g. \code{fish_mito}).
#' NCBI lineages are cached locally so repeat calls work offline.
#'
#' @param output_file Path for the generated HTML file. Default is a temporary
#'   file. The file is fully self-contained (no external dependencies) and can be
#'   shared or embedded in documentation.
#' @param open Logical, open the result in a browser when done? Default is
#'   \code{interactive()}.
#' @param refresh_cache Logical, ignore any cached NCBI lineages and re-fetch?
#'   Default \code{FALSE}.
#' @param targets Optional character vector to limit the browser to a subset of
#'   rulesets (e.g. \code{c("fish_mito", "bird_mito")}). Default is all rulesets.
#'
#' @return (Invisibly) the path to the generated HTML file.
#'
#' @examples
#' \dontrun{
#' # Build and open the ruleset browser
#' ruleset_browser()
#'
#' # Save to a specific location without opening
#' ruleset_browser("MitoPilot_rulesets.html", open = FALSE)
#' }
#'
#' @export
ruleset_browser <- function(output_file = tempfile(fileext = ".html"),
                            open = interactive(),
                            refresh_cache = FALSE,
                            targets = NULL) {

  # Mapping: ruleset target -> human label + NCBI anchor clade ----
  # NCBI taxids are used (rather than names) to avoid taxonomic name collisions,
  # e.g. "Ctenophora" is also a genus of diatoms and a genus of crane flies.
  ruleset_map <- list(
    fish_mito       = list(label = "Fishes",       ncbi = "Actinopterygii", taxid = "7898"),
    bird_mito       = list(label = "Birds",        ncbi = "Aves",           taxid = "8782"),
    turtle_mito     = list(label = "Turtles",      ncbi = "Testudines",     taxid = "8459"),
    mammal_mito     = list(label = "Mammals",      ncbi = "Mammalia",       taxid = "40674"),
    lepidosaur_mito = list(label = "Lepidosaurs",  ncbi = "Lepidosauria",   taxid = "8504"),
    starfish_mito   = list(label = "Sea stars",    ncbi = "Asteroidea",     taxid = "7588"),
    diptera_mito    = list(label = "True flies",   ncbi = "Diptera",        taxid = "7147"),
    copepod_mito    = list(label = "Copepods",     ncbi = "Copepoda",       taxid = "6830"),
    octocoral_mito  = list(label = "Octocorals",   ncbi = "Octocorallia",   taxid = "6132"),
    hexacoral_mito  = list(label = "Hexacorals",   ncbi = "Hexacorallia",   taxid = "6102"),
    ctenophore_mito = list(label = "Ctenophores",  ncbi = "Ctenophora",     taxid = "10197"),
    annelid_mito    = list(label = "Annelids",     ncbi = "Annelida",       taxid = "6340"),
    ascidiacea_mito      = list(label = "Sea squirts",     ncbi = "Ascidiacea",       taxid = "7713"),
    bivalvia_mito        = list(label = "Bivalves",        ncbi = "Bivalvia",         taxid = "6544"),
    bryozoa_mito         = list(label = "Bryozoans",       ncbi = "Bryozoa",          taxid = "10205"),
    crinoidea_mito       = list(label = "Crinoids",        ncbi = "Crinoidea",        taxid = "7589"),
    demospongiae_mito    = list(label = "Demosponges",     ncbi = "Demospongiae",     taxid = "6042"),
    echinoidea_mito      = list(label = "Sea urchins",     ncbi = "Echinoidea",       taxid = "7625"),
    gastropoda_mito      = list(label = "Gastropods",      ncbi = "Gastropoda",       taxid = "6448"),
    holothuroidea_mito   = list(label = "Sea cucumbers",   ncbi = "Holothuroidea",    taxid = "7705"),
    homoscleromorpha_mito = list(label = "Homoscleromorph sponges", ncbi = "Homoscleromorpha", taxid = "1417783"),
    malacostraca_mito    = list(label = "Malacostracans",  ncbi = "Malacostraca",     taxid = "6681"),
    hydrozoa_mito        = list(label = "Hydrozoans",      ncbi = "Hydrozoa",         taxid = "6074"),
    nemertea_mito        = list(label = "Ribbon worms",    ncbi = "Nemertea",         taxid = "6217"),
    ophiuroidea_mito     = list(label = "Brittle stars",   ncbi = "Ophiuroidea",      taxid = "7618"),
    platyhelminthes_mito = list(label = "Flatworms",       ncbi = "Platyhelminthes",  taxid = "6157"),
    polychaeta_mito      = list(label = "Polychaetes",     ncbi = "Polychaeta",       taxid = "6341"),
    pycnogonida_mito     = list(label = "Sea spiders",     ncbi = "Pycnogonida",      taxid = "57294"),
    sipuncula_mito       = list(label = "Peanut worms",    ncbi = "Sipuncula",        taxid = "6519"),
    thaliacea_mito       = list(label = "Salps",           ncbi = "Thaliacea",        taxid = "7166"),
    thecostraca_mito     = list(label = "Barnacles",       ncbi = "Thecostraca",      taxid = "72809")
  )

  if (!is.null(targets)) {
    bad <- setdiff(targets, names(ruleset_map))
    if (length(bad) > 0) {
      stop("Unknown target(s): ", paste(bad, collapse = ", "),
           "\nValid targets: ", paste(names(ruleset_map), collapse = ", "))
    }
    ruleset_map <- ruleset_map[targets]
  }

  # Collect rules for each ruleset ----
  rules_data <- stats::setNames(
    lapply(names(ruleset_map), function(tgt) {
      params <- get(paste0("params_", tgt), mode = "function")()
      # Genetic code is the default arg of the matching curate_* function
      gcode <- tryCatch(
        as.integer(eval(formals(get(paste0("curate_", tgt), mode = "function"))$genetic_code)),
        error = function(e) NA_integer_
      )
      build_ruleset_display(params, ruleset_map[[tgt]]$label, ruleset_map[[tgt]]$ncbi, gcode)
    }),
    names(ruleset_map)
  )

  # Fetch NCBI lineages (cached) ----
  message("Fetching NCBI taxonomy lineages...")
  lineages <- lapply(names(ruleset_map), function(tgt) {
    info <- ruleset_map[[tgt]]
    path <- fetch_ncbi_lineage(info$taxid, refresh_cache = refresh_cache)
    list(target = tgt, ncbi = info$ncbi, path = path)
  })

  # Merge lineages into a single tree, rooted at Metazoa ----
  tree <- merge_lineages(lineages, root_taxid = "33208")
  # Collapse redundant single-child chains for a compact tree
  tree <- collapse_chains(tree)

  # Build the self-contained HTML ----
  html <- build_ruleset_html(tree, rules_data)
  writeLines(html, output_file)
  message("Ruleset browser written to: ", normalizePath(output_file))

  if (isTRUE(open)) {
    utils::browseURL(normalizePath(output_file))
  }

  invisible(normalizePath(output_file))
}


# ---- NCBI taxonomy fetching ------------------------------------------------

#' Fetch and cache an NCBI lineage path for a taxid
#'
#' Returns an ordered list (root -> ... -> queried taxon), each element a list
#' with `taxid`, `name`, `rank`.
#' @noRd
fetch_ncbi_lineage <- function(taxid, refresh_cache = FALSE) {
  cache_dir <- tools::R_user_dir("MitoPilot", "cache")
  cache_file <- file.path(cache_dir, "ncbi_lineages.rds")

  cache <- list()
  if (file.exists(cache_file)) {
    cache <- tryCatch(readRDS(cache_file), error = function(e) list())
  }
  key <- paste0("taxid:", taxid)
  if (!refresh_cache && !is.null(cache[[key]])) {
    return(cache[[key]])
  }

  # efetch: taxid -> lineage XML
  efetch <- httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi") |>
    httr2::req_url_query(db = "taxonomy", id = taxid, retmode = "xml") |>
    httr2::req_user_agent("MitoPilot ruleset_browser (R package)") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
  xml <- httr2::resp_body_string(efetch)
  Sys.sleep(0.34) # be polite to NCBI (max 3 req/s without API key)

  path <- parse_lineage_xml(xml)
  if (length(path) == 0) {
    stop("Could not parse NCBI lineage for taxid '", taxid, "'.")
  }

  # update cache
  cache[[key]] <- path
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(cache, cache_file)

  path
}


#' Parse an NCBI efetch taxonomy XML string into an ordered lineage path
#'
#' The queried taxon's own TaxId/ScientificName/Rank appear before <LineageEx>
#' (interspersed with other tags such as <OtherNames> and <ParentTaxId>), while
#' its ancestors appear as adjacent triples inside <LineageEx>, ordered root ->
#' parent. The returned path is ordered root -> ... -> queried taxon.
#' @noRd
parse_lineage_xml <- function(xml) {
  grab <- function(tag, s) {
    g <- regmatches(s, regexec(paste0("<", tag, ">([^<]+)</", tag, ">"), s, perl = TRUE))[[1]]
    if (length(g) < 2) NA_character_ else g[2]
  }

  # Split the self block (before <LineageEx>) from the lineage block
  parts <- strsplit(xml, "<LineageEx>", fixed = TRUE)[[1]]
  self_block <- parts[1]
  lineage_block <- if (length(parts) > 1) {
    strsplit(parts[2], "</LineageEx>", fixed = TRUE)[[1]][1]
  } else {
    ""
  }

  self <- list(
    taxid = grab("TaxId", self_block),
    name = grab("ScientificName", self_block),
    rank = grab("Rank", self_block)
  )
  if (is.na(self$taxid) || is.na(self$name)) return(list())

  # Ancestors: adjacent TaxId/ScientificName/Rank triples inside LineageEx
  pattern <- "<TaxId>(\\d+)</TaxId>\\s*<ScientificName>([^<]+)</ScientificName>\\s*<Rank>([^<]+)</Rank>"
  matches <- regmatches(lineage_block, gregexpr(pattern, lineage_block, perl = TRUE))[[1]]
  lineage <- lapply(matches, function(s) {
    g <- regmatches(s, regexec(pattern, s, perl = TRUE))[[1]]
    list(taxid = g[2], name = g[3], rank = g[4])
  })

  c(lineage, list(self))
}


# ---- Tree construction -----------------------------------------------------

#' Merge a list of lineage paths into a nested tree
#'
#' @param lineages list of `list(target, ncbi, path)`
#' @param root_taxid optional taxid at which to root the tree; ancestors above
#'   it are trimmed from every path.
#' @noRd
merge_lineages <- function(lineages, root_taxid = NULL) {
  # Trim each path so it begins at root_taxid (if present in that path)
  if (!is.null(root_taxid)) {
    lineages <- lapply(lineages, function(lin) {
      ids <- vapply(lin$path, function(x) x$taxid, character(1))
      hit <- which(ids == root_taxid)
      if (length(hit) == 1) lin$path <- lin$path[hit:length(lin$path)]
      lin
    })
  }

  # nodes keyed by taxid; each node tracks children order
  nodes <- new.env(parent = emptyenv())

  get_node <- function(taxid, name, rank) {
    key <- taxid
    if (is.null(nodes[[key]])) {
      nodes[[key]] <- list(
        taxid = taxid, name = name, rank = rank,
        children = character(0), target = NULL
      )
    }
    nodes[[key]]
  }

  roots <- character(0)

  for (lin in lineages) {
    path <- lin$path
    prev_key <- NULL
    for (i in seq_along(path)) {
      node <- path[[i]]
      get_node(node$taxid, node$name, node$rank)
      if (is.null(prev_key)) {
        if (!(node$taxid %in% roots)) roots <- c(roots, node$taxid)
      } else {
        parent <- nodes[[prev_key]]
        if (!(node$taxid %in% parent$children)) {
          parent$children <- c(parent$children, node$taxid)
          nodes[[prev_key]] <- parent
        }
      }
      prev_key <- node$taxid
    }
    # tag the leaf with its ruleset target
    leaf <- nodes[[prev_key]]
    leaf$target <- lin$target
    nodes[[prev_key]] <- leaf
  }

  # recursively materialize into nested lists
  build <- function(taxid) {
    n <- nodes[[taxid]]
    list(
      taxid = n$taxid,
      name = n$name,
      rank = n$rank,
      target = n$target,
      children = lapply(n$children, build)
    )
  }

  # If rooted, return just the root_taxid subtree; otherwise the (single) root
  if (!is.null(root_taxid) && !is.null(nodes[[root_taxid]])) {
    build(root_taxid)
  } else if (length(roots) == 1) {
    build(roots[1])
  } else {
    list(taxid = "root", name = "Life", rank = "no rank", target = NULL,
         children = lapply(roots, build))
  }
}


#' Collapse runs of single-child, non-ruleset nodes into one combined node
#'
#' Long linear chains (e.g. Chordata > Craniata > Vertebrata > ...) are merged
#' into a single node whose `name` joins the collapsed taxa with " > ".
#' @noRd
collapse_chains <- function(node) {
  node$children <- lapply(node$children, collapse_chains)
  while (length(node$children) == 1 && is.null(node$target)) {
    child <- node$children[[1]]
    node$name <- paste(node$name, child$name, sep = " > ")
    node$rank <- child$rank
    node$taxid <- child$taxid
    node$target <- child$target
    node$children <- child$children
  }
  node
}


# ---- Ruleset display extraction --------------------------------------------

#' NCBI translation table (genetic code) names
#' @noRd
genetic_code_name <- function(code) {
  names_map <- c(
    "1" = "Standard",
    "2" = "Vertebrate Mitochondrial",
    "3" = "Yeast Mitochondrial",
    "4" = "Mold, Protozoan, and Coelenterate Mitochondrial",
    "5" = "Invertebrate Mitochondrial",
    "9" = "Echinoderm and Flatworm Mitochondrial",
    "13" = "Ascidian Mitochondrial",
    "14" = "Alternative Flatworm Mitochondrial",
    "21" = "Trematode Mitochondrial",
    "24" = "Rhabdopleuridae Mitochondrial"
  )
  nm <- names_map[[as.character(code)]]
  nm %||% "Unknown"
}

#' Format a MitoPilot params list into a display-ready structure
#' @noRd
build_ruleset_display <- function(params, label, ncbi, genetic_code = NA_integer_) {
  field_order <- c("count", "min_len", "max_len", "overlap",
                   "start_codons", "stop_codons", "intron")

  fmt_val <- function(key, val) {
    if (key == "overlap" && is.list(val)) {
      start <- val$start
      stop  <- val$stop
      fmt_one <- function(x) {
        if (is.null(x) || (length(x) == 1 && is.na(x))) return(intToUtf8(0x2014))
        if (is.logical(x)) return(if (isTRUE(x)) "yes" else "no")
        as.character(x)
      }
      return(paste0("start: ", fmt_one(start), ", stop: ", fmt_one(stop)))
    }
    if (is.null(val) || (length(val) == 1 && is.na(val))) return(NA_character_)
    if (is.logical(val) && length(val) == 1) return(if (isTRUE(val)) "yes" else "no")
    paste(as.character(val), collapse = ", ")
  }

  # Effective rule for a gene = default for its type, overlaid with overrides
  effective_row <- function(gene, rule, defaults) {
    type <- rule$type
    eff <- modifyList(defaults[[type]] %||% list(), rule)
    row <- list(gene = gene)
    for (k in field_order) {
      if (!is.null(eff[[k]])) {
        v <- fmt_val(k, eff[[k]])
        row[[k]] <- if (is.na(v)) NA_character_ else v
      } else {
        row[[k]] <- NA_character_
      }
    }
    list(type = type, row = row)
  }

  defaults <- params$default_rules
  genes <- params$rules

  rows <- lapply(names(genes), function(g) effective_row(g, genes[[g]], defaults))
  types_present <- unique(vapply(rows, function(x) x$type, character(1)))

  # Order types in a sensible way
  type_order <- c("PCG", "rRNA", "tRNA", "ctrl")
  types_present <- c(intersect(type_order, types_present),
                     setdiff(types_present, type_order))

  groups <- lapply(types_present, function(tp) {
    grp_rows <- lapply(rows[vapply(rows, function(x) x$type == tp, logical(1))],
                       function(x) x$row)
    # Columns = gene + any field with at least one non-NA value in this group
    cols <- "gene"
    for (k in field_order) {
      any_val <- any(vapply(grp_rows, function(r) !is.na(r[[k]]), logical(1)))
      if (any_val) cols <- c(cols, k)
    }
    list(type = tp, columns = cols, rows = grp_rows)
  })

  # Default rules per type (for a reference table)
  default_groups <- lapply(names(defaults), function(tp) {
    d <- defaults[[tp]]
    row <- list(gene = paste0("<default ", tp, ">"))
    for (k in field_order) {
      if (!is.null(d[[k]])) {
        v <- fmt_val(k, d[[k]])
        row[[k]] <- if (is.na(v)) NA_character_ else v
      } else {
        row[[k]] <- NA_character_
      }
    }
    row
  })

  list(
    label = label,
    ncbi = ncbi,
    genetic_code = list(
      code = genetic_code,
      name = genetic_code_name(genetic_code),
      url = paste0(
        "https://www.ncbi.nlm.nih.gov/Taxonomy/taxonomyhome.html/index.cgi?chapter=cgencodes#SG",
        genetic_code
      )
    ),
    global = list(
      hit_threshold = params$hit_threshold,
      max_overlap = params$max_overlap
    ),
    groups = groups,
    defaults = default_groups
  )
}


# ---- HTML generation -------------------------------------------------------

#' Assemble the self-contained HTML document
#' @noRd
build_ruleset_html <- function(tree, rules_data) {
  tree_json <- jsonlite::toJSON(tree, auto_unbox = TRUE, null = "null", na = "null")
  rules_json <- jsonlite::toJSON(rules_data, auto_unbox = TRUE, null = "null", na = "null")

  field_labels_json <- jsonlite::toJSON(list(
    gene = "Gene", count = "Count", min_len = "Min len", max_len = "Max len",
    overlap = "Overlap", start_codons = "Start codons",
    stop_codons = "Stop codons", intron = "Intron"
  ), auto_unbox = TRUE)

  tooltips_json <- jsonlite::toJSON(ruleset_tooltips(), auto_unbox = TRUE)

  template <- ruleset_html_template()
  template <- sub("/*__TREE_DATA__*/", paste0("var TREE_DATA = ", tree_json, ";"),
                  template, fixed = TRUE)
  template <- sub("/*__RULES_DATA__*/", paste0("var RULES_DATA = ", rules_json, ";"),
                  template, fixed = TRUE)
  template <- sub("/*__FIELD_LABELS__*/", paste0("var FIELD_LABELS = ", field_labels_json, ";"),
                  template, fixed = TRUE)
  template <- sub("/*__TOOLTIPS__*/", paste0("var TOOLTIPS = ", tooltips_json, ";"),
                  template, fixed = TRUE)

  # Substitute Unicode glyphs at runtime so the R source stays ASCII-only
  glyphs <- c(
    "@TRIDOWN@"  = intToUtf8(0x25be),  # down-pointing triangle (expanded)
    "@TRIRIGHT@" = intToUtf8(0x25b8),  # right-pointing triangle (collapsed)
    "@ELLIPSIS@" = intToUtf8(0x2026),  # horizontal ellipsis
    "@EMDASH@"   = intToUtf8(0x2014),  # em dash
    "@MIDDOT@"   = intToUtf8(0x00b7)   # middle dot
  )
  for (tok in names(glyphs)) {
    template <- gsub(tok, glyphs[[tok]], template, fixed = TRUE)
  }
  template
}


#' Short explanations for each curation/validation parameter
#'
#' Derived from how each parameter is used in the curate_* and validate_*
#' functions, and from the MitoPilot documentation.
#' @noRd
ruleset_tooltips <- function() {
  list(
    genetic_code = "NCBI translation table used to translate coding sequences and to define which start and stop codons are valid for this clade.",
    hit_threshold = "Minimum percent amino-acid similarity to a reference sequence for a BLAST hit to be used. Protein-coding genes with no hit at or above this value are skipped during curation and flagged 'low reference similarity'.",
    max_overlap = "Global cap on how much an annotation may overlap a neighboring gene on the same strand, as a fraction of its length. Annotations exceeding it are flagged 'exceeds max overlap'.",
    count = "Expected number of copies of this feature. Finding fewer reports the gene as 'missing'; finding more triggers a 'possible duplicate' warning.",
    min_len = "Minimum expected feature length in base pairs. Shorter annotations are flagged 'below min length'; NA means no minimum is enforced.",
    max_len = "Maximum expected feature length in base pairs. Longer annotations are flagged 'exceeds max length'; NA means no maximum is enforced.",
    overlap = "Permitted overlap with adjacent genes. 'start' is the maximum base pairs the feature's start may overlap a neighbor before a warning; 'stop: yes' lets the stop codon overlap the next gene, so it is neither trimmed nor flagged.",
    start_codons = "Codons accepted as valid start codons for this gene. Curation snaps the start to the nearest of these; any other start is flagged 'non-standard start codon'.",
    stop_codons = "Codons accepted as valid stops, including truncated T or TA stops that are completed by polyadenylation. Curation adjusts the stop to one of these; others are flagged 'non-standard stop codon'.",
    intron = "Whether this gene may contain introns. If yes, multiple fragments are labeled as exons rather than flagged as duplicates."
  )
}


#' The static HTML/CSS/JS shell (data injected at markers)
#' @noRd
ruleset_html_template <- function() {
'<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>MitoPilot Curation Rulesets</title>
<style>
  :root {
    --accent: #1f6f8b;
    --accent-soft: #e4f0f4;
    --border: #d9dee2;
    --text: #243137;
    --muted: #6b7b85;
  }
  * { box-sizing: border-box; }
  body {
    margin: 0; font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
    color: var(--text); background: #f6f8f9;
  }
  header {
    padding: 14px 20px; background: var(--accent); color: #fff;
    display: flex; align-items: baseline; gap: 14px;
  }
  header h1 { font-size: 18px; margin: 0; font-weight: 600; }
  header .sub { font-size: 13px; opacity: 0.85; }
  .layout { display: flex; height: calc(100vh - 52px); }
  .pane-tree {
    width: 420px; min-width: 220px; max-width: 70%; height: 100%;
    overflow: auto;
    border-right: 1px solid var(--border); background: #fff; padding: 10px 6px;
    resize: horizontal;
  }
  ul.tree { min-width: max-content; }
  .pane-detail { flex: 1; overflow: auto; padding: 22px 28px; }
  .toolbar { display: flex; gap: 6px; padding: 4px 8px 10px; align-items: center; flex-wrap: wrap; }
  .toolbar input {
    flex: 1; min-width: 120px; padding: 5px 8px; border: 1px solid var(--border);
    border-radius: 5px; font-size: 13px;
  }
  .toolbar button {
    padding: 5px 9px; font-size: 12px; border: 1px solid var(--border);
    background: #fff; border-radius: 5px; cursor: pointer; color: var(--text);
  }
  .toolbar button:hover { background: var(--accent-soft); }
  ul.tree, ul.tree ul { list-style: none; margin: 0; padding-left: 16px; }
  ul.tree { padding-left: 4px; }
  li.node { margin: 1px 0; }
  .row { display: flex; align-items: center; gap: 4px; border-radius: 4px; padding: 1px 4px; white-space: nowrap; }
  .row:hover { background: #f0f3f4; }
  .toggle {
    width: 14px; text-align: center; cursor: pointer; color: var(--muted);
    user-select: none; font-size: 10px; flex: none;
  }
  .toggle.leaf { visibility: hidden; }
  .name { font-size: 13.5px; cursor: default; }
  .rank { font-size: 11px; color: var(--muted); margin-left: 4px; }
  li.ruleset > .row .name { cursor: pointer; font-weight: 600; color: var(--accent); }
  .badge {
    font-size: 10.5px; background: var(--accent-soft); color: var(--accent);
    border: 1px solid #bcd9e2; border-radius: 10px; padding: 0 7px; margin-left: 6px;
    cursor: pointer; white-space: nowrap;
  }
  li.collapsed > ul { display: none; }
  .row.selected { background: var(--accent-soft); box-shadow: inset 3px 0 0 var(--accent); }
  .row.match > .name { background: #fff7cc; }
  .placeholder { color: var(--muted); max-width: 560px; line-height: 1.5; }
  h2.detail-title { margin: 0 0 2px; font-size: 22px; }
  .detail-sub { color: var(--muted); margin: 0 0 18px; font-size: 13px; }
  .gcode {
    display: inline-flex; align-items: baseline; gap: 8px; margin: 0 0 18px;
    background: var(--accent-soft); border: 1px solid #bcd9e2; border-radius: 8px;
    padding: 8px 14px; font-size: 14px;
  }
  .gcode-label { font-size: 11px; color: var(--muted); text-transform: uppercase; letter-spacing: .04em; }
  .gcode a { color: var(--accent); font-weight: 600; text-decoration: none; }
  .gcode a:hover { text-decoration: underline; }
  .help {
    display: inline-flex; align-items: center; justify-content: center;
    width: 14px; height: 14px; margin-left: 5px; border-radius: 50%;
    background: #b6c3ca; color: #fff; font-size: 10px; font-weight: 700;
    cursor: help; position: relative; vertical-align: middle; flex: none;
  }
  .help:hover { background: var(--accent); }
  .help:hover::after {
    content: attr(data-tip); position: absolute; top: 150%; left: 0;
    z-index: 20; width: 260px; padding: 8px 10px; border-radius: 6px;
    background: #243137; color: #fff; font-size: 12px; font-weight: 400;
    line-height: 1.4; text-align: left; text-transform: none; letter-spacing: 0;
    box-shadow: 0 4px 14px rgba(0,0,0,0.22); pointer-events: none; white-space: normal;
  }
  th .help:hover::after { left: auto; right: 0; }
  .globals { display: flex; gap: 26px; margin: 0 0 22px; }
  .globals .g { background: #fff; border: 1px solid var(--border); border-radius: 8px; padding: 10px 16px; }
  .globals .g .k { font-size: 11px; color: var(--muted); text-transform: uppercase; letter-spacing: .04em; }
  .globals .g .v { font-size: 20px; font-weight: 600; }
  details.group { margin-top: 22px; }
  summary.group-title {
    margin: 0 0 8px; font-size: 15px; color: var(--accent); font-weight: 600;
    cursor: pointer;
  }
  summary.group-title:hover { text-decoration: underline; }
  table.rules { border-collapse: collapse; width: 100%; background: #fff; font-size: 13px; }
  table.rules th, table.rules td {
    border: 1px solid var(--border); padding: 6px 9px; text-align: left; vertical-align: top;
  }
  table.rules th { background: var(--accent-soft); font-weight: 600; white-space: nowrap; }
  table.rules td.gene { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-weight: 600; }
  table.rules td.na { color: #c2cace; }
  details.defaults { margin-top: 4px; }
  details.defaults summary { cursor: pointer; color: var(--muted); font-size: 13px; margin-bottom: 8px; }
  code { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }
</style>
</head>
<body>
<header>
  <h1>MitoPilot Curation Rulesets</h1>
  <span class="sub">Taxonomy backbone from NCBI. Select a highlighted clade to view its rules.</span>
</header>
<div class="layout">
  <div class="pane-tree">
    <div class="toolbar">
      <input id="search" type="text" placeholder="Filter taxa...">
      <button id="expand-all">Expand all</button>
      <button id="collapse-all">Collapse all</button>
    </div>
    <ul class="tree" id="tree"></ul>
  </div>
  <div class="pane-detail" id="detail">
    <div class="placeholder">
      <h2 class="detail-title">Welcome</h2>
      <p>This tree shows the NCBI taxonomic lineages of every clade that has a
      dedicated MitoPilot curation ruleset. Clades shown in
      <strong style="color:var(--accent)">blue</strong> with a badge are clickable;
      selecting one displays its curation rules here.</p>
      <p>Use the toggles (@TRIRIGHT@/@TRIDOWN@) to expand and collapse clades, or the filter
      box to find a taxon.</p>
    </div>
  </div>
</div>
<script>
/*__TREE_DATA__*/
/*__RULES_DATA__*/
/*__FIELD_LABELS__*/
/*__TOOLTIPS__*/

var selectedRow = null;

function helpIcon(key) {
  var tip = TOOLTIPS[key];
  if (!tip) return null;
  var h = el("span", "help", "?");
  h.setAttribute("data-tip", tip);
  return h;
}

function el(tag, cls, txt) {
  var e = document.createElement(tag);
  if (cls) e.className = cls;
  if (txt != null) e.textContent = txt;
  return e;
}

function renderNode(node) {
  var li = el("li", "node");
  var hasChildren = node.children && node.children.length > 0;
  if (node.target) li.classList.add("ruleset");

  var row = el("div", "row");
  var toggle = el("span", "toggle" + (hasChildren ? "" : " leaf"), hasChildren ? "@TRIDOWN@" : "@TRIRIGHT@");
  row.appendChild(toggle);

  var parts = node.name.split(" > ");
  var display = node.name;
  if (parts.length > 3) {
    display = parts[0] + " > @ELLIPSIS@ > " + parts[parts.length - 1];
  }
  var name = el("span", "name", display);
  if (display !== node.name) name.title = node.name;
  row.appendChild(name);
  if (node.rank && node.rank !== "no rank") {
    row.appendChild(el("span", "rank", node.rank));
  }
  if (node.target) {
    var badge = el("span", "badge", node.target);
    row.appendChild(badge);
    var sel = function (ev) { ev.stopPropagation(); selectRuleset(node.target, row); };
    name.addEventListener("click", sel);
    badge.addEventListener("click", sel);
  }
  li.appendChild(row);

  if (hasChildren) {
    var ul = el("ul");
    node.children.forEach(function (c) { ul.appendChild(renderNode(c)); });
    li.appendChild(ul);
    toggle.addEventListener("click", function () {
      li.classList.toggle("collapsed");
      toggle.textContent = li.classList.contains("collapsed") ? "@TRIRIGHT@" : "@TRIDOWN@";
    });
  }
  return li;
}

function buildTree() {
  var root = document.getElementById("tree");
  root.innerHTML = "";
  root.appendChild(renderNode(TREE_DATA));
}

function cell(val) {
  var td = document.createElement("td");
  if (val == null) { td.textContent = "@EMDASH@"; td.className = "na"; }
  else { td.textContent = val; }
  return td;
}

function renderTable(group) {
  var table = el("table", "rules");
  var thead = el("thead");
  var htr = el("tr");
  group.columns.forEach(function (c) {
    var th = el("th", null, FIELD_LABELS[c] || c);
    var icon = helpIcon(c);
    if (icon) th.appendChild(icon);
    htr.appendChild(th);
  });
  thead.appendChild(htr);
  table.appendChild(thead);
  var tbody = el("tbody");
  group.rows.forEach(function (r) {
    var tr = el("tr");
    group.columns.forEach(function (c) {
      var td = cell(r[c]);
      if (c === "gene") td.className = "gene";
      tr.appendChild(td);
    });
    tbody.appendChild(tr);
  });
  table.appendChild(tbody);
  return table;
}

function selectRuleset(target, row) {
  if (selectedRow) selectedRow.classList.remove("selected");
  if (row) { row.classList.add("selected"); selectedRow = row; }

  var data = RULES_DATA[target];
  var d = document.getElementById("detail");
  d.innerHTML = "";

  d.appendChild(el("h2", "detail-title", data.label));
  d.appendChild(el("p", "detail-sub", "Ruleset: " + target + "  @MIDDOT@  NCBI anchor: " + data.ncbi));

  if (data.genetic_code && data.genetic_code.code != null) {
    var gc = el("div", "gcode");
    var gcLabel = el("span", "gcode-label", "Genetic code");
    var gcIcon = helpIcon("genetic_code");
    if (gcIcon) gcLabel.appendChild(gcIcon);
    gc.appendChild(gcLabel);
    var link = document.createElement("a");
    link.href = data.genetic_code.url;
    link.target = "_blank";
    link.rel = "noopener";
    link.textContent = data.genetic_code.code + " @EMDASH@ " + data.genetic_code.name;
    gc.appendChild(link);
    d.appendChild(gc);
  }

  var globals = el("div", "globals");
  [["hit_threshold", data.global.hit_threshold], ["max_overlap", data.global.max_overlap]].forEach(function (kv) {
    var g = el("div", "g");
    var k = el("div", "k", kv[0]);
    var icon = helpIcon(kv[0]);
    if (icon) k.appendChild(icon);
    g.appendChild(k);
    g.appendChild(el("div", "v", kv[1] == null ? "@EMDASH@" : kv[1]));
    globals.appendChild(g);
  });
  d.appendChild(globals);

  // Type defaults: at the top, expanded by default
  if (data.defaults && data.defaults.length) {
    var det = el("details", "defaults");
    det.open = true;
    det.appendChild(el("summary", null, "Type defaults (applied unless overridden per gene)"));
    var cols = ["gene", "count", "min_len", "max_len", "overlap", "start_codons", "stop_codons", "intron"];
    det.appendChild(renderTable({ columns: cols, rows: data.defaults }));
    d.appendChild(det);
  }

  // Per-type feature tables, each collapsible (expanded by default)
  data.groups.forEach(function (grp) {
    var gdet = el("details", "group");
    gdet.open = true;
    gdet.appendChild(el("summary", "group-title", grp.type + " features"));
    gdet.appendChild(renderTable(grp));
    d.appendChild(gdet);
  });
}

function setCollapsedAll(collapsed) {
  document.querySelectorAll("#tree li.node").forEach(function (li) {
    var hasChildren = li.querySelector(":scope > ul");
    if (!hasChildren) return;
    var toggle = li.querySelector(":scope > .row > .toggle");
    if (collapsed) { li.classList.add("collapsed"); if (toggle) toggle.textContent = "@TRIRIGHT@"; }
    else { li.classList.remove("collapsed"); if (toggle) toggle.textContent = "@TRIDOWN@"; }
  });
}

function runSearch(q) {
  q = q.trim().toLowerCase();
  var rows = document.querySelectorAll("#tree .row");
  rows.forEach(function (r) { r.classList.remove("match"); });
  if (!q) return;
  document.querySelectorAll("#tree li.node").forEach(function (li) {
    var name = li.querySelector(":scope > .row > .name");
    var hay = name ? (name.textContent + " " + (name.title || "")).toLowerCase() : "";
    if (name && hay.indexOf(q) !== -1) {
      li.querySelector(":scope > .row").classList.add("match");
      // expand ancestors
      var p = li.parentElement;
      while (p && p.id !== "tree") {
        if (p.tagName === "LI") {
          p.classList.remove("collapsed");
          var t = p.querySelector(":scope > .row > .toggle");
          if (t && !t.classList.contains("leaf")) t.textContent = "@TRIDOWN@";
        }
        p = p.parentElement;
      }
    }
  });
}

buildTree();
document.getElementById("expand-all").addEventListener("click", function () { setCollapsedAll(false); });
document.getElementById("collapse-all").addEventListener("click", function () { setCollapsedAll(true); });
document.getElementById("search").addEventListener("input", function (e) { runSearch(e.target.value); });
</script>
</body>
</html>
'
}
