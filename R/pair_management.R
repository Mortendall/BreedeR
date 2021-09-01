
pair_management_ui <- function(id){
  ns <- shiny::NS(id)
  fluidPage(
    shiny::fluidRow(shiny::column(3,
      shiny::uiOutput(ns("BreedingPairSelector")),
      shiny::textInput(ns("Filename"), label = "Database Name", value = "MyDatabase"),
      shiny::downloadButton(outputId = ns("DatabaseDownload"),
                            label = "Download Database Sheet")
    ),
      shiny::column(3,
      shiny::h3("Add Litter"),
      shiny::dateInput(ns("LitterDate"),label = "Litter Birth Date",  width = "50%"),
      shiny::textInput(ns("Males"),label = "No. of males", value = 0, width = "20%"),
      shiny::textInput(ns("Females"),label = "No. of females", value = 0, width = "20%"),
      shiny::textInput(ns("Wildtype"),label = "No. of Wildtypes", value = 0, width = "20%"),
      shiny::textInput(ns("Knockouts"),label = "No of Knockouts", value = 0, width = "20%"),
      shiny::checkboxInput(ns("HetBox"), label = "Allow heterozygotes?", value = F),
      shiny::uiOutput(outputId = ns("HetPups")),
      shiny::actionButton(ns("Submit"), label = "Add Breeding Pair")
      
      #consider adding check box for enabling heterozygotes?
      ))
    )
}

pair_management <- function(id, data_sheet){
  moduleServer(
    id,
    
    function(input, output, session){
      ns <- session$ns
      new_litter <- reactiveValues()
      output$BreedingPairSelector <- renderUI({shiny::selectInput(inputId = ns("BreedingPair"),
                                                                  label = "Breeding Pair",
                                                                  choices = unique(data_sheet$data$Breeding_pair))
       
      })
      output$HetPups <- shiny::renderUI({
        if (isTRUE(input$HetBox)){
          shiny::textInput(ns("Heterozygotes"),label = "No of Heterozygotes", value = 0, width = "20%")
        }
          })
        
      
       shiny::observeEvent(input$Submit,{
         if (isFALSE(input$HetBox)){
           new_litter$data <- data.frame("Breeding_pair"= input$BreedingPair,
                                       "Litter_date"= input$LitterDate, 
                                       "Male_pups"= as.integer(input$Males), 
                                       "Female_pups"= as.integer(input$Females), 
                                       "WT_pups"= as.integer(input$Wildtype), 
                                       "KO_pups"= as.integer(input$Knockouts))
         }
         else{
           new_litter$data <- data.frame("Breeding_pair"= input$BreedingPair,
                                         "Litter_date"= input$LitterDate, 
                                         "Male_pups"= as.integer(input$Males), 
                                         "Female_pups"= as.integer(input$Females), 
                                         "WT_pups"= as.integer(input$Wildtype), 
                                         "KO_pups"= as.integer(input$Knockouts),
                                         "Het_pups"=as.integer(input$Heterozygotes))
         }
      #   add a series of quality control steps to ensure there's an input. Add the row to 
         
         if (length(new_litter$data) != length(data_sheet$data)){
           shinyWidgets::sendSweetAlert(session,
                                        title = "Warning",
                                        type = "Error",
                                        text = "Your database has a different column length from your input. Did you forget to allow/remove heterozygotes?")
         }
         else{
           data_sheet$data <- dplyr::bind_rows(data_sheet$data, new_litter$data)
         }
       })
       output$DatabaseDownload <- shiny::downloadHandler(
         filename = function(){
           paste(input$Filename,".xlsx",sep = "")
         },
         content = function(file){
           database <- data_sheet$data
           openxlsx::write.xlsx(database, file = file)
         }
       )
       
      #data loading
      #Graph
      #Table
    })
}
