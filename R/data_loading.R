library(shiny)
library(tidyverse)
library(here)
library(openxlsx)
data_loading_ui <- function(id){
  ns <- shiny::NS(id)
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = ns("Database"),
                label = "Input",
                buttonLabel = "Enter a database file",
                multiple = F,
                accept = c(".xlsx"))
    ),
    mainPanel(
      plotOutput(outputId = ns("SummaryStats")),
      dataTableOutput(outputId = ns("Dataoverview"))
      #Main panel should have two outputs, showing active breeding pairs, and a table with summary stats
    ))
}

data_loading <- function(id, data_sheet){
  moduleServer(
    id,
    
    function(input, output, session){
      output$Dataoverview <- renderDataTable({
       file <- input$Database 
        ext <- tools::file_ext(file$datapath)
        req(file)
        validate(need(ext == "xlsx", "Please upload an xlsx file"))
        data_displayed <- openxlsx::read.xlsx(file$datapath)
        
        for (i in 1:length(data_displayed)){
          if (endsWith(colnames(data_displayed)[i], "_date"))
            data_displayed[,i] <- convertToDate(data_displayed[,i])
        }
        data_sheet$data <- data_displayed
        })
      output$SummaryStats <- renderPlot({
          plot_data <- data_sheet$data
          if(!is.null(plot_data)){
            plot_data <- plot_data %>% dplyr::mutate(Litter_size = Male_pups + Female_pups)
            Breeding_chart <- ggplot2::ggplot(plot_data, aes(x = Litter_date, y = Litter_size, color = Breeding_pair))+geom_line()+geom_point(size = 8)
            Breeding_chart
          }
          
      })
      })
      
      #Graph
      #Table
    
  
}

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

pair_inspection_ui <- function(id){
  ns <- shiny::NS(id)
  sidebarLayout(
    sidebarPanel(
    ),
    mainPanel(
      
    ))
}

pair_inspection <- function(id, data_sheet){
  moduleServer(
    id,
    function(input, output, session){
      #data loading
      #Graph
      #Table
    })
}
