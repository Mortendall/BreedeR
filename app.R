

library(shiny)

data_sheet <- reactiveValues()
summary_sheet <- reactiveValues()

ui <- fluidPage(

    
    titlePanel("BreedeR: A colony management tool"),
    tabsetPanel(type = "tabs",
                tabPanel("Data Loader", data_loading_ui("Data_Loader")),
                tabPanel("Pair Inspection", pair_inspection_ui("Pair_inspection", data_sheet)),
                tabPanel("Pair Management", pair_management_ui("Pair_management")),
                tabPanel("Summary Stats", summary_stats_ui("Summary_stats")),
                tabPanel("Lazy Pair Adder",pair_adder_ui("Lazy_adder"))
                ))



server <- function(input, output, session) {
  data_loading("Data_Loader", data_sheet)
  pair_inspection("Pair_inspection", data_sheet, summary_sheet)
  pair_management("Pair_management", data_sheet)
  summary_stats("Summary_stats", data_sheet, summary_sheet)
  pair_adder("Lazy_adder", data_sheet)
}


shinyApp(ui = ui, server = server)
