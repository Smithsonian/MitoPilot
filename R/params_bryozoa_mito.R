#' Default curation and validation parameters for Bryozoa mitochondria
#'
#' Generated from the curation-rules spreadsheet (translation table 5).
#'
#' @param alt (optional) named list of default values to modify
#'
#' @export
#'
params_bryozoa_mito <- function(alt = list()) {
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
        start_codons = c("ATG", "GTG", "ATA", "ATT", "ATC", "TTG"),
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
        type = "PCG",
        intron = TRUE
      ),
      nad2 = list(
        type = "PCG",
        intron = TRUE
      ),
      cox1 = list(
        type = "PCG",
        intron = TRUE,
        overlap = list(start = 2, stop = T)
      ),
      cox2 = list(
        type = "PCG",
        intron = TRUE
      ),
      atp8 = list(
        type = "PCG",
        overlap = list(start = 2, stop = T)
      ),
      atp6 = list(
        type = "PCG",
        intron = TRUE,
        overlap = list(start = 20, stop = F)
      ),
      cox3 = list(
        type = "PCG",
        intron = TRUE
      ),
      nad3 = list(
        type = "PCG",
        intron = TRUE
      ),
      nad4l = list(
        type = "PCG",
        intron = TRUE,
        overlap = list(start = 2, stop = T)
      ),
      nad4 = list(
        type = "PCG",
        intron = TRUE,
        overlap = list(start = 20, stop = F)
      ),
      nad5 = list(
        type = "PCG",
        intron = TRUE,
        overlap = list(start = 2, stop = T)
      ),
      nad6 = list(
        type = "PCG",
        intron = TRUE,
        overlap = list(start = 2, stop = T)
      ),
      cob = list(
        type = "PCG",
        intron = TRUE
      ),
      rvt = list(
        type = "PCG",
        count = c(0, 1)
      ),
      im = list(
        type = "PCG",
        count = c(0, 1)
      ),
      trnA = list(
        type = "tRNA"
      ),
      trnC = list(
        type = "tRNA"
      ),
      trnD = list(
        type = "tRNA"
      ),
      trnE = list(
        type = "tRNA"
      ),
      trnF = list(
        type = "tRNA"
      ),
      trnG = list(
        type = "tRNA"
      ),
      trnH = list(
        type = "tRNA"
      ),
      trnI = list(
        type = "tRNA"
      ),
      trnK = list(
        type = "tRNA"
      ),
      trnL = list(
        type = "tRNA",
        count = 2
      ),
      trnM = list(
        type = "tRNA"
      ),
      trnN = list(
        type = "tRNA",
        count = c(0, 1)
      ),
      trnP = list(
        type = "tRNA"
      ),
      trnQ = list(
        type = "tRNA"
      ),
      trnR = list(
        type = "tRNA"
      ),
      trnS = list(
        type = "tRNA",
        count = c(1, 2)
      ),
      trnT = list(
        type = "tRNA"
      ),
      trnV = list(
        type = "tRNA"
      ),
      trnW = list(
        type = "tRNA",
        count = c(1, 2)
      ),
      trnY = list(
        type = "tRNA",
        count = c(0, 1)
      )
    )
  )
  params <- modify_list_recursive(params, alt)
}

