#' Default curation and validation parameters for Hydrozoa mitochondria
#'
#' Generated from the curation-rules spreadsheet (translation table 4).
#'
#' @param alt (optional) named list of default values to modify
#'
#' @export
#'
params_hydrozoa_mito <- function(alt = list()) {
  params <- list(
    hit_threshold = 60,
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
        stop_codons = c("TAA", "TAG", "TA", "T"),
        start_codons = c("ATG", "GTG", "ATT", "ATA", "ATC", "TTG", "TTA", "CTG"),
        intron = FALSE
      ),
      tRNA = list(
        count = 1,
        max_len = 150,
        min_len = 50
      )
    ),
    rules = list(
      ctrl = list(
        count = 1,
        type = "ctrl"
      ),
      rrnL = list(
        type = "rRNA",
        max_len = 1850
      ),
      rrnS = list(
        type = "rRNA",
        max_len = 1000
      ),
      nad1 = list(
        type = "PCG"
      ),
      nad2 = list(
        type = "PCG"
      ),
      cox1 = list(
        type = "PCG",
        overlap = list(start = 2, stop = T)
      ),
      cox2 = list(
        type = "PCG"
      ),
      atp8 = list(
        type = "PCG",
        overlap = list(start = 2, stop = T)
      ),
      atp6 = list(
        type = "PCG",
        overlap = list(start = 20, stop = F)
      ),
      cox3 = list(
        type = "PCG"
      ),
      nad3 = list(
        type = "PCG"
      ),
      nad4l = list(
        type = "PCG",
        overlap = list(start = 2, stop = T)
      ),
      nad4 = list(
        type = "PCG",
        overlap = list(start = 20, stop = F)
      ),
      nad5 = list(
        type = "PCG",
        overlap = list(start = 2, stop = T)
      ),
      nad6 = list(
        type = "PCG",
        overlap = list(start = 2, stop = T)
      ),
      cob = list(
        type = "PCG"
      ),
      # dpo = medusozoan terminal DNA polymerase B ORF. GenBank synonyms polB /
      # dnaB (previously this rule) are normalized to dpo.
      dpo = list(
        type = "PCG"
      ),
      orf314 = list(
        type = "PCG"
      ),
      trnA = list(
        type = "tRNA",
        count = 0
      ),
      trnC = list(
        type = "tRNA",
        count = 0
      ),
      trnD = list(
        type = "tRNA",
        count = 0
      ),
      trnE = list(
        type = "tRNA",
        count = 0
      ),
      trnF = list(
        type = "tRNA",
        count = 0
      ),
      trnG = list(
        type = "tRNA",
        count = 0
      ),
      trnH = list(
        type = "tRNA",
        count = 0
      ),
      trnI = list(
        type = "tRNA",
        count = 0
      ),
      trnK = list(
        type = "tRNA",
        count = 0
      ),
      trnL = list(
        type = "tRNA",
        count = 0
      ),
      trnM = list(
        type = "tRNA"
      ),
      trnN = list(
        type = "tRNA",
        count = 0
      ),
      trnP = list(
        type = "tRNA",
        count = 0
      ),
      trnQ = list(
        type = "tRNA",
        count = 0
      ),
      trnR = list(
        type = "tRNA",
        count = 0
      ),
      trnS = list(
        type = "tRNA",
        count = 0
      ),
      trnT = list(
        type = "tRNA",
        count = 0
      ),
      trnV = list(
        type = "tRNA",
        count = 0
      ),
      trnW = list(
        type = "tRNA"
      ),
      trnY = list(
        type = "tRNA",
        count = 0
      )
    )
  )
  params <- modify_list_recursive(params, alt)
}

