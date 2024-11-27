#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
export_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

#' export Server Functions
#'
#' @noRd
export_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

