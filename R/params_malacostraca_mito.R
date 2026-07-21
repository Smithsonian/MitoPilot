#' Default curation and validation parameters for Malacostraca mitochondria
#'
#' Generated from the curation-rules spreadsheet (translation table 5).
#'
#' @param alt (optional) named list of default values to modify
#'
#' @export
#'
params_malacostraca_mito <- function(alt = list()) {
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
        type = "PCG"
      ),
      nad2 = list(
        type = "PCG"
      ),
      cox1 = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "ATA", "ATT", "ATC", "TTG", "ACG", "TCG", "CTG", "AAG"),
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
      trnA = list(
        type = "tRNA",
        count = c(1, 3)
      ),
      trnC = list(
        type = "tRNA",
        count = c(0, 2)
      ),
      trnD = list(
        type = "tRNA",
        count = c(1, 2)
      ),
      trnE = list(
        type = "tRNA"
      ),
      trnF = list(
        type = "tRNA",
        count = c(1, 3)
      ),
      trnG = list(
        type = "tRNA"
      ),
      trnH = list(
        type = "tRNA"
      ),
      trnI = list(
        type = "tRNA",
        count = c(0, 2)
      ),
      trnK = list(
        type = "tRNA",
        count = c(0, 1)
      ),
      trnL = list(
        type = "tRNA",
        count = c(1, 4)
      ),
      trnM = list(
        type = "tRNA",
        count = c(0, 2)
      ),
      trnN = list(
        type = "tRNA",
        count = c(1, 3)
      ),
      trnP = list(
        type = "tRNA"
      ),
      trnQ = list(
        type = "tRNA",
        count = c(0, 2)
      ),
      trnR = list(
        type = "tRNA",
        count = c(1, 3)
      ),
      trnS = list(
        type = "tRNA",
        count = c(2, 3)
      ),
      trnT = list(
        type = "tRNA"
      ),
      trnV = list(
        type = "tRNA",
        count = c(1, 2)
      ),
      trnW = list(
        type = "tRNA",
        count = c(0, 2)
      ),
      trnY = list(
        type = "tRNA",
        count = c(0, 2)
      )
    )
  )
  params <- modify_list_recursive(params, alt)
}

