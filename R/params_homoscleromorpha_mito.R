#' Default curation and validation parameters for Homoscleromorpha mitochondria
#'
#' Generated from the curation-rules spreadsheet (translation table 4).
#'
#' @param alt (optional) named list of default values to modify
#'
#' @export
#'
params_homoscleromorpha_mito <- function(alt = list()) {
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
        intron = TRUE,
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
      atp9 = list(
        type = "PCG"
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
      lagli = list(
        type = "PCG",
        count = c(0, 1)
      ),
      mttb = list(
        type = "PCG",
        count = c(0, 1)
      ),
      trnA = list(
        type = "tRNA",
        count = c(0, 1)
      ),
      trnC = list(
        type = "tRNA",
        count = c(0, 1)
      ),
      trnD = list(
        type = "tRNA",
        count = c(0, 1)
      ),
      trnE = list(
        type = "tRNA",
        count = c(0, 1)
      ),
      trnF = list(
        type = "tRNA",
        count = c(0, 1)
      ),
      trnG = list(
        type = "tRNA",
        count = c(0, 1)
      ),
      trnH = list(
        type = "tRNA",
        count = c(0, 1)
      ),
      trnI = list(
        type = "tRNA",
        count = c(1, 2)
      ),
      trnK = list(
        type = "tRNA",
        count = c(0, 1)
      ),
      trnL = list(
        type = "tRNA",
        count = c(0, 2)
      ),
      trnM = list(
        type = "tRNA",
        count = c(1, 2)
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
        type = "tRNA",
        count = c(0, 2)
      ),
      trnS = list(
        type = "tRNA",
        count = c(0, 2)
      ),
      trnT = list(
        type = "tRNA",
        count = c(0, 2)
      ),
      trnV = list(
        type = "tRNA",
        count = c(0, 2)
      ),
      trnW = list(
        type = "tRNA"
      ),
      trnY = list(
        type = "tRNA",
        count = c(0, 2)
      )
    )
  )
  params <- modify_list_recursive(params, alt)
}

