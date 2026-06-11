#' Default curation and validation parameters for Platyhelminthes mitochondria
#'
#' Generated from the curation-rules spreadsheet (translation table 9).
#'
#' @param alt (optional) named list of default values to modify
#'
#' @export
#'
params_platyhelminthes_mito <- function(alt = list()) {
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
        start_codons = c("ATG", "GTG"),
        intron = FALSE
      ),
      tRNA = list(
        count = 1,
        max_len = NA,
        min_len = NA
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
        type = "PCG",
        start_codons = c("ATG", "GTG", "ATT", "TTG")
      ),
      cox1 = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "ATT", "ATA", "TTG"),
        overlap = list(start = 2, stop = T)
      ),
      cox2 = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "ATA", "TTG")
      ),
      atp8 = list(
        type = "PCG",
        overlap = list(start = 2, stop = T)
      ),
      atp6 = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "ATA", "TTG"),
        overlap = list(start = 20, stop = F)
      ),
      cox3 = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "ATA", "TTA", "TTG")
      ),
      nad3 = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "TTG")
      ),
      nad4l = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "TTG", "CTA", "ATA"),
        overlap = list(start = 2, stop = T)
      ),
      nad4 = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "TTG", "CTA", "ATA"),
        overlap = list(start = 20, stop = F)
      ),
      nad5 = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "TTG"),
        overlap = list(start = 2, stop = T)
      ),
      nad6 = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "TTG"),
        overlap = list(start = 2, stop = T)
      ),
      cob = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "ATT", "ATC", "TTG")
      ),
      trnA = list(
        type = "tRNA",
        count = c(1, 2)
      ),
      trnC = list(
        type = "tRNA",
        count = c(1, 2)
      ),
      trnD = list(
        type = "tRNA"
      ),
      trnE = list(
        type = "tRNA",
        count = c(0, 2)
      ),
      trnF = list(
        type = "tRNA"
      ),
      trnG = list(
        type = "tRNA",
        count = c(1, 2)
      ),
      trnH = list(
        type = "tRNA",
        count = c(1, 2)
      ),
      trnI = list(
        type = "tRNA",
        count = c(1, 2)
      ),
      trnK = list(
        type = "tRNA",
        count = c(1, 2)
      ),
      trnL = list(
        type = "tRNA",
        count = c(1, 4)
      ),
      trnM = list(
        type = "tRNA",
        count = c(1, 2)
      ),
      trnN = list(
        type = "tRNA",
        count = c(1, 3)
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
        count = 2
      ),
      trnT = list(
        type = "tRNA",
        count = c(0, 1)
      ),
      trnV = list(
        type = "tRNA"
      ),
      trnW = list(
        type = "tRNA"
      ),
      trnY = list(
        type = "tRNA",
        count = c(1, 2)
      )
    )
  )
  params <- modify_list_recursive(params, alt)
}

