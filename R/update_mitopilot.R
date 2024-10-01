update_mitopilot <- function(
  path = here::here(),
  workflow = c("both", "assemble", "annotate"),
  source = app_sys("nextflow")

  ){

  if(any(c("assemble", "both") %in% workflow)){
    message("Run in terminal:")
    glue::glue(
      "nextflow",
      "-log .logs/nextflow.log",
      "run {source}",
      "-c {file.path(path, '.config')}",
      "-entry WF1",
      "-resume",
      .sep = " "
    ) |>
      print()
  }

}
