.onAttach <- function(libname, pkgname) {
  nf_check <- system2("nextflow", args = "-version", stdout = TRUE)
  nf_version <- nf_check[stringr::str_detect(nf_check, "version")] |> stringr::str_squish()
  if(length(nf_version) == 0){
    packageStartupMessage("A Nextflow installation is needed to run the MitoPilot pipeline.")
    glue::glue("Please install Nextflow from {crayon::underline('https://www.nextflow.io/')}") |>
      packageStartupMessage()
    return(invisible())
  }
  glue::glue("Welcome to {crayon::bold(crayon::white('{MitoPilot}'))}!") |> packageStartupMessage()
  glue::glue("Using Nextflow {nf_version}") |> packageStartupMessage()
  return(invisible())
}
