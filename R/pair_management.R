library(shiny)
library(tidyverse)
library(here)
library(openxlsx)

pair_management_ui <- function(id){
  ns <- shiny::NS(id)
  fluidPage(
    shiny::fluidRow(shiny::column(3,
      shiny::textInput(ns("Filename"), label = "Database Name", value = "MyDatabase"),
      shiny::downloadButton(outputId = ns("DatabaseDownload"),
                            label = "Download Database Sheet")
    ),
      shiny::column(3,
      shiny::h3("Add Litter"),
      shiny::uiOutput(ns("BreedingPairSelector")),
      shiny::dateInput(ns("LitterDate"),label = "Litter Birth Date",  width = "50%"),
      shiny::textInput(ns("Males"),label = "No. of males", value = 0, width = "20%"),
      shiny::textInput(ns("Females"),label = "No. of females", value = 0, width = "20%"),
      shiny::textInput(ns("Wildtype"),label = "No. of Wildtypes", value = 0, width = "20%"),
      shiny::textInput(ns("Knockouts"),label = "No of Knockouts", value = 0, width = "20%"),
      shiny::checkboxInput(ns("HetBox"), label = "Allow heterozygotes?", value = F),
      shiny::uiOutput(outputId = ns("HetPups")),
      shiny::actionButton(ns("Submit"), label = "Add Litter")
      ),
    shiny::column(3,
                  shiny::h3("Add Breeding Pair"),
                  shiny::textInput(ns("PairID"),label = "Breeding Pair ID", value = 0, width = "50%"),
                  shiny::dateInput(ns("SetupDate"), label = "Add Setup date for new pair", width = "50%"),
                  shiny::actionButton(ns("SubmitBP"), label = "Add Breeding Pair")
                  )
    )
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
         
         if (length(new_litter$data) != length(data_sheet$data)){
           shinyWidgets::sendSweetAlert(session,
                                        title = "Warning",
                                        type = "error",
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
       shiny::observeEvent(input$SubmitBP,{
         if(is.null(data_sheet$data)){
           if (isTRUE(input$HetButton)){
             data_sheet$data<- data.frame("Breeding_pair"= input$PairID,
                                      "Litter_date" = input$SetupDate,
                                      "Male_pups"=0,
                                      "Female_pups"=0,
                                      "WT_pups"=0,
                                      "KO_pups"=0,
                                      "Het_pups"=0)
           }
           else {
             data_sheet$data <- data.frame("Breeding_pair"= input$PairID,
                                      "Litter_date" = input$SetupDate,
                                      "Male_pups"=0,
                                      "Female_pups"=0,
                                      "WT_pups"=0,
                                      "KO_pups"=0)
             }}
           else{
           if (isTRUE(input$HetButton)){
           SetupSheet <- data.frame("Breeding_pair"= input$PairID,
                                      "Litter_date" = input$SetupDate,
                                      "Male_pups"=0,
                                      "Female_pups"=0,
                                      "WT_pups"=0,
                                      "KO_pups"=0,
                                      "Het_pups"=0)
         }
         else {
           SetupSheet <- data.frame("Breeding_pair"= input$PairID,
                                      "Litter_date" = input$SetupDate,
                                      "Male_pups"=0,
                                      "Female_pups"=0,
                                      "WT_pups"=0,
                                      "KO_pups"=0)
           
         }
         if (length(SetupSheet) != length(data_sheet$data)){
           shinyWidgets::sendSweetAlert(session,
                                        title = "Warning",
                                        type = "error",
                                        text = "Your database has a different column length from your input. Did you forget to allow/remove heterozygotes?")
         }
         else{
           data_sheet$data <- dplyr::bind_rows(data_sheet$data, SetupSheet)
         }
         }
       })

    })
}
