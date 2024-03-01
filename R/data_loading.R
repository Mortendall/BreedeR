# library(shiny)
# library(magrittr)
 library(dplyr)
# library(here)
# library(openxlsx)
# library(shinyTime)

#tilføj next expected litter
#kaplna-meyer for POMC + pup + youngster track
data_loading_ui <- function(id){
  ns <- shiny::NS(id)
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = ns("Database"),
                label = "Input",
                buttonLabel = "Enter a database file",
                multiple = F,
                accept = c(".xlsx")),
      shiny::checkboxInput(inputId = ns("HetButton"), label = "Add heterozygotes to template?", value = F),
      shiny::downloadButton(outputId = ns("TemplateDownload"),
                            label = "Download Template Sheet"),
      shiny::h5("New to breedeR? try loading some test data with the button below"),
      shiny::actionButton(inputId = ns("dummy"),
                          label = "Load test data")
    ),
    mainPanel(
      plotOutput(outputId = ns("SummaryStats"),
                      hover = hoverOpts(ns("netHover"), delay = 100, delayType ="debounce", nullOutside = F)),
      shiny::uiOutput(ns("ButtonPlaceholder")),
      shiny::uiOutput(ns("TimeSlider")),
      dataTableOutput(outputId = ns("Dataoverview")
      ),
      shiny::uiOutput(ns("hover"))
      #Main panel should have two outputs, showing active breeding pairs, and a table with summary stats
    ))
}

data_loading <- function(id, data_sheet){
  moduleServer(
    id,
    
    function(input, output, session){
      ns <- session$ns
      output$TimeSlider <- shiny::renderUI({
        req(data_sheet$data)
        shiny::sliderInput(ns("YearSlider"), label = "Adjust display", 
                           min = min(data_sheet$data$Litter_date), 
                           max = max(data_sheet$data$Litter_date), 
                           value = c(min(data_sheet$data$Litter_date), max(data_sheet$data$Litter_date)))
      })
      shiny::observeEvent(input$Database,{
        file <- input$Database 
        ext <- tools::file_ext(file$datapath)
        req(file)
        validate(need(ext == "xlsx", "Please upload an xlsx file"))
        data_displayed <- openxlsx::read.xlsx(file$datapath)
        
        for (i in 1:length(data_displayed)){
          if (endsWith(colnames(data_displayed)[i], "_date"))
            data_displayed[,i] <- openxlsx::convertToDate(data_displayed[,i])
        }
        data_sheet$data <- data_displayed
        })
        
      output$Dataoverview <- renderDataTable({
        req(data_sheet$data)
        data_sheet$data
        })
      
      output$SummaryStats <- renderPlot({
        req(data_sheet$data)
        req(input$YearSlider)
        plot_data <- data_sheet$data
          plot_data <-
            plot_data |> dplyr::mutate(Litter_size = Male_pups + Female_pups)
          Breeding_chart <-
            ggplot2::ggplot(plot_data,
                            ggplot2::aes(x = Litter_date, 
                                y = Litter_size, 
                                color = Breeding_pair)) + 
            ggplot2::geom_line() +
            ggplot2::geom_point(size = 8)+ 
            ggplot2::coord_cartesian(xlim = c(input$YearSlider[1], input$YearSlider[2]))
            
          data_sheet$graph <- Breeding_chart
          Breeding_chart
      })
      
      output$ButtonPlaceholder <- shiny::renderUI({
          req(data_sheet$data)
          shiny::downloadButton(ns("PlotDownload"), label = "Download Plot")
        })
      output$TemplateDownload <- shiny::downloadHandler(
        filename = function(){
          "templatesheet.xlsx"
        },
        content = function(file){
          if (isTRUE(input$HetButton)){
            templateSheet <- data.frame(matrix(ncol = 8, nrow = 1))  
            colnames(templateSheet)<-c("Breeding_pair", "Litter_date", "Male_pups", "Female_pups", "WT_pups", "KO_pups", "Het_pups", "NA_pups")
          }
          else {
            templateSheet <- data.frame(matrix(ncol = 7, nrow = 1))  
            colnames(templateSheet)<-c("Breeding_pair", "Litter_date", "Male_pups", "Female_pups", "WT_pups", "KO_pups", "NA_pups")
          }
          openxlsx::write.xlsx(templateSheet, file = file)
        }
      )
      output$PlotDownload <- shiny::downloadHandler(
        filename = function(){
          paste("breedinggraph.tif",sep = "")
        },
        content = function(file){
          device <- function(..., width, height) grDevices::tiff(..., width = 25, height = 20, res = 200, units = "cm")
          ggplot2::ggsave(file, plot = data_sheet$graph, device = device)
        }
      )
     
      output$hover <- renderUI({
        req(data_sheet$data)
        pair_data <- data_sheet$data
          pair_data <-
            pair_data |> dplyr::mutate(Litter_size = Male_pups + Female_pups)
                
        res <- nearPoints(pair_data, input$netHover, xvar = "Litter_date", yvar = "Litter_size", maxpoints = 1)
        if (nrow(res)>0) {
          hover <- input$netHover
          left_px <- hover$coords_css$x
          top_px <- hover$coords_css$y
          style <- paste0(
            "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px, "px; top:", top_px, "px;"
          )
          if(length(data_sheet$data==6)){
            wellPanel(style = style,
                      p(
                        HTML(
                          as.character(res$Litter_date),
                          "</br>",
                          res$Breeding_pair,
                          "</br>",
                          "No. KO: ",
                          res$KO_pups,
                          "</br>",
                          "No WT: ",
                          res$WT_pups,
                          "</br>",
                          "No Males: " ,
                          res$Male_pups,
                          "</br>",
                          "No. Females:",
                          res$Female_pups
                        )
                      )
            )
          }
          else{
            wellPanel(style = style,
                      p(
                        HTML(
                          "No. KO: ",
                          res$KO_pups,
                          "</br>",
                          "No WT: ",
                          res$WT_pups,
                          "</br>",
                          "No. of Hets:",
                          res$Het_pups,
                          "</br>",
                          "No Males: " ,
                          res$Male_pups,
                          "</br>",
                          "No. Females:",
                          res$Female_pups
                        )
                      )
            )
          }
        }
      })
      
      shiny::observeEvent(input$dummy,{
        data_sheet$data <- readRDS(here::here("R/testdata.rds"))
        shinyWidgets::sendSweetAlert(title = "test data loaded",
                                     type = "success",
                                     text = "test data has succesfully been loaded. You can now play around with the app!")
      })
      
      })
      
      #Graph
      #Table
    
  
}

