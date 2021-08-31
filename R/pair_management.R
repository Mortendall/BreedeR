
pair_management_ui <- function(id){
  ns <- shiny::NS(id)
  fluidPage(
    shiny::fluidRow(shiny::column(3,
      shiny::uiOutput(ns("BreedingPairSelector"))
    ),
      shiny::column(3,
      shiny::dateInput(ns("LitterDate"),label = "Litter Birth Date",  width = "50%"),
      shiny::textInput(ns("Males"),label = "No. of males", value = 0, width = "20%"),
      shiny::textInput(ns("Females"),label = "No. of females", value = 0, width = "20%"),
      shiny::textInput(ns("Wildtype"),label = "No. of Wildtypes", value = 0, width = "20%"),
      shiny::textInput(ns("Knockouts"),label = "No of Knockouts", value = 0, width = "20%"),
      shiny::actionButton(ns("Submit"), label = "Add Breeding Pair")
      ))
    )
}

pair_management <- function(id, data_sheet){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      output$BreedingPairSelector <- renderUI({shiny::selectInput(inputId = ns("BreedingPair"),
                                                                  label = "Breeding Pair",
                                                                  choices = unique(data_sheet$data$Breeding_pair))
       
      })
       shiny::observeEvent(input$Submit,{
      #   add a series of quality control steps to ensure there's an input. Add the row to 
       })   
      #data loading
      #Graph
      #Table
    })
}
