#' Default curation and validation parameters for Fish Mitochondria
#'
#' @param alt (optional) named list of default values to modify
#'
#' @export
#'
params_fish_mito <- function(alt = list()) {
  params <- list(
    #ref_dbs = list(
    #  default = "/ref_dbs/Mitos2/Chordata/featureProt/{gene}.fas"
    #),
    hit_threshold = 90,
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
        stop_codons = c("TAA", "TAG", "AGA", "AGG", "AG", "TA", "T"),
        start_codons = c("ATG", "GTG", "ATA", "ATT", "TTA", "ATC")
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
        type = "ctrl",
        min_len = 350
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
        start_codons = c("ATG", "GTG", "ATA", "ATT", "TTA", "ATC", "TTG")
      ),
      nad2 = list(
        type = "PCG"
      ),
      cox1 = list(
        type = "PCG",
        overlap = list(start = 2, stop = T)
      ),
      cox2 = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "ATA", "ATT", "TTA", "ATC", "TTG")
      ),
      atp8 = list(
        type = "PCG",
        overlap = list(start = 2, stop = T)
      ),
      atp6 = list(
        type = "PCG",
        overlap = list(start = 20, stop = F),
        start_codons = c("ATG", "GTG", "ATA", "ATT", "TTA", "ATC", "CTG")
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
      trnA = list(type = "tRNA"),
      trnC = list(type = "tRNA"),
      trnD = list(type = "tRNA"),
      trnE = list(type = "tRNA"),
      trnF = list(type = "tRNA"),
      trnG = list(type = "tRNA"),
      trnH = list(type = "tRNA"),
      trnI = list(type = "tRNA"),
      trnK = list(type = "tRNA"),
      trnL = list(
        type = "tRNA",
        count = 2
      ),
      trnM = list(type = "tRNA"),
      trnN = list(type = "tRNA"),
      trnP = list(type = "tRNA"),
      trnQ = list(type = "tRNA"),
      trnR = list(type = "tRNA"),
      trnS = list(
        type = "tRNA",
        count = 2
      ),
      trnT = list(type = "tRNA"),
      trnV = list(type = "tRNA"),
      trnW = list(type = "tRNA"),
      trnY = list(type = "tRNA")
    )
  )
  params <- modify_list_recursive(params, alt)
}
