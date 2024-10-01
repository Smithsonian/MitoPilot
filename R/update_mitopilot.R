update_mitopilot <- function(
  path = here::here(),
  workflow = c("both", "assemble", "annotate"),
  source = "https://github.com/JonahVentures/MitoPilot"

  ){

  if(any(c("assemble", "both") %in% workflow)){
    message("Run in terminal:")
    glue::glue(
      "nextflow",
      "-log .logs/nextflow.log",
      "run {source}",
      "-r main -main-script nextflow/main.nf",
      "-c {file.path(path, 'nextflow.config')}",
      "-entry WF1",
      "-resume",
      .sep = " "
    ) |>
      print()
  }

}
