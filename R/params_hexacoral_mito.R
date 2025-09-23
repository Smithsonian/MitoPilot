#' Default curation and validation parameters for hexacorals Mitochondria
#'
#' @param alt (optional) named list of default values to modify
#'
#' @export
#'
params_hexacoral_mito <- function(alt = list()) {
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
        start_codons = c("TTA", "TTG", "CTG", "ATT", "ATC", "ATA", "ATG", "GTG")
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
      trnM = list(type = "tRNA"),
      trnW = list(type = "tRNA")
    )
  )
  params <- modify_list_recursive(params, alt)
}
