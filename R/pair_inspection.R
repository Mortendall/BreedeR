pair_inspection_ui <- function(id, data_sheet){
  ns <- shiny::NS(id)
  
  sidebarLayout(
    sidebarPanel(
      shiny::uiOutput(ns("BreedingPairSelector"))
    ),
    mainPanel(
      
    ))
}

pair_inspection <- function(id, data_sheet){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
        output$BreedingPairSelector <- renderUI({shiny::selectInput(inputId = ns("BreedingPair"),
                                                                    label = "Breeding Pair",
                                              choices = unique(data_sheet$data$Breeding_pair))
                                              })
      
      
      
      #data loading
      #Graph
      #Table
    })
}
