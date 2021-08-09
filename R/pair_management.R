
pair_management_ui <- function(id){
  ns <- shiny::NS(id)
  sidebarLayout(
    sidebarPanel(
      
    ),
    mainPanel(
      
    ))
}

pair_management <- function(id, data_sheet){
  moduleServer(
    id,
    function(input, output, session){
      #data loading
      #Graph
      #Table
    })
}
