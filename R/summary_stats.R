summary_stats_ui <- function(id, data_sheet){
  ns <- shiny::NS(id)
  sidebarLayout(
    sidebarPanel(
      shiny::tableOutput(outputId = ns("summary_table_sex")),
      shiny::tableOutput(outputId = ns("summary_table_geno")),
      shiny::tableOutput(outputId = ns("TimeBetween"))),
    mainPanel(
      shiny::h3("Breeding Calculator"),
      shiny::selectInput(ns("Genotype"), label = "Select the group you are interested in", choices = c("WT", "KO", "Het", "Male", "Female")),
      shiny::textInput(ns("AnimalNum"), label = "Select how many animals you want to breed from your group of interest"),
      shiny::sliderInput(ns("AgeRange"), label = "Select Acceptable Age Range", value = c(8,12),min = 4, max = 52,step = 1,),
      shiny::dateInput(ns("StartDate"),label = "Your expected starting date"),
      shiny::actionButton(ns("Submit"), label = "Get no. of required breeding pairs"),
      shiny::textOutput(ns("RequiredPairs"))
    )
    )
}

summary_stats <- function(id, data_sheet, summary_sheet){
  moduleServer(
    id,
    function(input, output, session){
      output$summary_table_sex <- shiny::renderTable({
        req(data_sheet$data)
      summary_sheet$total_sex <- data_sheet$data %>% 
        dplyr::filter(Male_pups+Female_pups+WT_pups+KO_pups!=0) %>% 
        rstatix::get_summary_stats(c(Male_pups, Female_pups), type = "mean") %>% 
        dplyr::mutate(n = as.integer(n)) %>% 
        dplyr::rename("ID" = variable, "Number of Litters" = n, "Avr. pr. litter" = mean)
      })
      output$summary_table_geno <- shiny::renderTable({
        req(data_sheet$data)
        if(length(data_sheet$data)==7){
        summary_sheet$total_geno <- data_sheet$data %>% 
          dplyr::filter(Male_pups+Female_pups+WT_pups+KO_pups!=0) %>% 
          rstatix::get_summary_stats(c(WT_pups, KO_pups), type = "mean") %>%
          dplyr::mutate(n = as.integer(n)) %>% 
          dplyr::rename("ID" = variable, "Number of Litters" = n, "Avr. pr. litter" = mean) 
        }
        else{
          summary_sheet$total_geno <- data_sheet$data %>% 
            dplyr::filter(Male_pups+Female_pups+WT_pups+KO_pups!=0) %>% 
            rstatix::get_summary_stats(c(WT_pups, KO_pups, Het_pups), type = "mean") %>% 
            dplyr::mutate(n = as.integer(n)) %>% 
            dplyr::rename("ID" = variable, "Number of Litters" = n, "Avr. pr. litter" = mean) 
        }
        })
      output$TimeBetween <-  shiny::renderTable({
        req(data_sheet$data)
         breedingPairs <- unique(data_sheet$data$Breeding_pair)
         dateResults <- data.frame()
         for (i in 1:length(breedingPairs)){
           calculate_time <- data_sheet$data %>% 
             dplyr::filter(Breeding_pair==breedingPairs[i]) %>% 
             dplyr::select(Breeding_pair, Litter_date) %>%
             dplyr::arrange(Litter_date) 
           pairresult <- data.frame(days = NA)
           for (j in 1:(length(calculate_time$Litter_date))-1){
             pairresult[j,1]<-calculate_time$Litter_date[j+1]-calculate_time$Litter_date[j]
           }
           dateResults <- dplyr::bind_rows(dateResults,pairresult)
         }
         dateResults <- stats::na.omit(dateResults)
         BetweenLitters <- mean(dateResults$days)
         summary_sheet$AvrTime <- as.data.frame(BetweenLitters) %>% 
           dplyr::rename("Avr days between litters" = BetweenLitters)
      })
      shiny::observeEvent(input$Submit,{
        if(is.null(data_sheet$data)){
          shinyWidgets::sendSweetAlert(session,
                                       title = "Warning",
                                       type = "error",
                                       text = "You have not entered any data")
        }
        else{
          #the idea is to calculate how many mice you need based on available data
          #first, calculate how long your start date is from 
          TimeDif <- input$StartDate - Sys.Date()
          if(TimeDif<summary_sheet$AvrTime$'Avr days between litters'){
            shinyWidgets::sendSweetAlert(session,
                                         title = "Warning",
                                         type = "warning",
                                         text = "You will not be able to generate sufficient litters in such a short time frame. Increase the starting date.")
          }
           if(TimeDif<input$AgeRange[1]*7){
             shinyWidgets::sendSweetAlert(session,
                                          title = "Warning",
                                          type = "warning",
                                          text = "You cannot generate mice of the desired age range in this time frame. Increase the minimum range of the age slider")
           }
          else{SetupDate <- input$StartDate - input$AgeRange[1]*7
          InterestGroup <- switch(
            input$Genotype,
            "WT" = "WT_pups",
            "KO" = "KO_pups",
            "Het" = "Het_pups",
            "Male" = "Male_pups",
            "Female" = "Female_pups"
          ) 
          if(InterestGroup=="Male_pups"|InterestGroup=="Female_pups"){
            AnimalsPrLitter <- subset(summary_sheet$total_sex, ID == InterestGroup) %>% dplyr::select('Avr. pr. litter')
          }
          else{
            AnimalsPrLitter <- subset(summary_sheet$total_geno, ID == InterestGroup) %>% dplyr::select('Avr. pr. litter')
          }
          
          LitterNum <- round((as.integer(input$AnimalNum)/AnimalsPrLitter),0)
          LittersPrPair <- (input$AgeRange[2]*7-input$AgeRange[1]*7)/summary_sheet$AvrTime
          TotalPairs <- round(LitterNum/LittersPrPair,0)
          output$RequiredPairs <- shiny::renderText({
            paste("You need ",LitterNum, " litters to generate ", input$AnimalNum, " ", input$Genotype, " mice. This requires ",TotalPairs, " breeding pairs. You should setup your breeding pairs by ", SetupDate, sep = "")
          })
          } 
        }
      })
    })
}
