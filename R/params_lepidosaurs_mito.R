#' Default curation and validation parameters for lepidosaur Mitochondria
#'
#' @param alt (optional) named list of default values to modify
#'
#' @export
#'
params_lepidosaur_mito <- function(alt = list()) {
  params <- list(
    hit_threshold = 80,
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
        start_codons = c("ATG", "GTG", "ATA", "ATT", "TTA", "ATC"),
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
        type = "ctrl",
        min_len = 350
      ),
      # Non-INSDC organelle-triage heuristic: vertebrate rRNAs exceed ~800 (12S) / ~1000 (16S) bp
      rrnL = list(
        type = "rRNA",
        max_len = 1850,
        min_len = 1000
      ),
      rrnS = list(
        type = "rRNA",
        max_len = 1000,
        min_len = 800
      ),
      nad1 = list(
        type = "PCG"
      ),
      nad2 = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "ATA", "ATT", "TTA", "ATC", "TTG")
      ),
      cox1 = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "ATA", "ATT", "TTA", "ATC", "TTG"),
        overlap = list(start = 2, stop = T)
      ),
      cox2 = list(
        type = "PCG"
      ),
      atp8 = list(
        type = "PCG",
        start_codons = c("ATG", "GTG", "ATA", "ATT", "TTA", "ATC", "TTG"),
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
