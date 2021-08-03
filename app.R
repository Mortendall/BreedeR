

library(shiny)


ui <- fluidPage(

    
    titlePanel("BreedeR: A colony management tool"),
    tabsetPanel(type = "tabs",
                tabPanel("Data Loader", data_loading_ui("Data_Loader")),
                tabPanel("Pair Inspection", pair_inspection_ui("Pair_inspection")),
                tabPanel("Pair Management", pair_management_ui("Pair_management"))
                    
                ))



server <- function(input, output, session) {
  data_sheet <- reactiveValues()
  data_loading("Data_Loader", data_sheet)
  pair_inspection("Pair_inspection", data_sheet)
  pair_management("Pair_management", data_sheet)

        
}


shinyApp(ui = ui, server = server)
