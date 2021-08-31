pair_inspection_ui <- function(id, data_sheet){
  ns <- shiny::NS(id)
  
  sidebarLayout(
    sidebarPanel(
      shiny::uiOutput(ns("BreedingPairSelector"))
    ),
    mainPanel(
      shiny::plotOutput(ns("BreedingPairGraph"),
                        )
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
        output$BreedingPairGraph <- renderPlot({
          pair_data <- data_sheet$data
           
          if (!is.null(pair_data)) {
            pair_data <-
              pair_data %>% dplyr::mutate(Litter_size = Male_pups + Female_pups)
            pair_data <- pair_data %>% 
              dplyr::filter(Breeding_pair %in% input$BreedingPair)
            Breeding_chart <-
              ggplot2::ggplot(pair_data,
                              aes(x = Litter_date, 
                                  y = Litter_size, 
                                  color = Breeding_pair)) + 
              geom_line() +
              geom_point(size = 8)
            Breeding_chart
          }
          
        })
      
      
      
      #data loading
      #Graph
      #Table
    })
}
