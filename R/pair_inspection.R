pair_inspection_ui <- function(id, data_sheet){
  ns <- shiny::NS(id)
  
  sidebarLayout(
    sidebarPanel(
      shiny::uiOutput(ns("BreedingPairSelector"))
    ),
    mainPanel(
      shiny::plotOutput(ns("BreedingPairGraph"),
                        hover = hoverOpts(ns("netHover"), delay = 100, delayType ="debounce", nullOutside = F)
                        ),
      shiny::uiOutput(ns("hover")),
      shiny::tableOutput(ns("BreedingStats_sex")),
      shiny::tableOutput(ns("BreedingStats_geno"))
    ))
}

pair_inspection <- function(id, data_sheet, summary_sheet){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
        output$BreedingPairSelector <- renderUI({shiny::selectInput(inputId = ns("BreedingPair"),
                                                                    label = "Breeding Pair",
                                              choices = unique(data_sheet$data$Breeding_pair))
                                              })
        output$BreedingPairGraph <- renderPlot({
          req(data_sheet$data)
          pair_data <- data_sheet$data
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
          
        )
        output$BreedingStats_sex <- shiny::renderTable({
          req(data_sheet$data)
          summary_sheet$pairs <- data_sheet$data %>% 
            dplyr::filter(Male_pups+Female_pups!=0) %>% 
            dplyr::filter(Breeding_pair %in% input$BreedingPair)%>% 
            rstatix::get_summary_stats(c(Male_pups, Female_pups), type = "mean") %>% 
            dplyr::mutate(n = as.integer(n)) %>% 
            dplyr::rename("ID" = variable, "Number of Litters" = n, "Avr. pr. litter" = mean) 
          })
        output$BreedingStats_geno <- shiny::renderTable({
          req(data_sheet$data)
          if(length(data_sheet$data ==6)){
            summary_sheet$total <- data_sheet$data %>% 
            dplyr::filter(Male_pups+Female_pups+WT_pups+KO_pups!=0) %>% 
            dplyr::filter(Breeding_pair %in% input$BreedingPair)%>%
            rstatix::get_summary_stats(c(WT_pups, KO_pups), type = "mean") %>% 
              dplyr::mutate(n = as.integer(n)) %>% 
            dplyr::rename("ID" = variable, "Number of Litters" = n, "Avr. pr. litter" = mean) 
          }
          else{
            summary_sheet$total <- data_sheet$data %>% 
              dplyr::filter(Male_pups+Female_pups+WT_pups+KO_pups!=0) %>% 
              dplyr::filter(Breeding_pair %in% input$BreedingPair)%>%
              rstatix::get_summary_stats(c(WT_pups, KO_pups, Het_pups), type = "mean") %>% 
              dplyr::mutate(n = as.integer(n)) %>% 
              dplyr::rename("ID" = variable, "Number of Litters" = n, "Avr. pr. litter" = mean) 
          }
        })
      
        output$hover <- renderUI({
          req(data_sheet$data)
          pair_data <- data_sheet$data
            pair_data <-
              pair_data %>% dplyr::mutate(Litter_size = Male_pups + Female_pups)
            pair_data <- pair_data %>% 
              dplyr::filter(Breeding_pair %in% input$BreedingPair)
          
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
      
      #data loading
      #Graph
      #Table
    })
}
