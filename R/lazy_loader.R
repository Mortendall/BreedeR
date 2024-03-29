pair_adder_ui <-  function(id, data_sheet){
  ns <- shiny::NS(id)
  
  sidebarLayout(
    sidebarPanel(
      shiny::h4("This function is the lazy litter adder"),
      shiny::h6("It allows you to paste in data and add it to your existing sheet. This is nice if you have many pairs you need to add at once. But beware! it can be fickle and cause crashes.")
    ),
    mainPanel(
      shiny::textAreaInput(ns("dataEntry"), label = "Enter litter info with columns in the following order: Sex (M/F), Genotype (WT/KO/Het/NA), Birth date, Breeding Pair info", height = 100),
      shiny::checkboxInput(ns("HetBox"), label = "Allow heterozygotes?", value = F),
      shiny::actionButton(ns("Submit"), label = "Submit and pray"),
      shiny::tableOutput(ns("UploadedData")),
      shiny::uiOutput(ns("SubmitButton")),
      shiny::textOutput(ns("Success"))
    ))
}
#Lazy Loader need sweetalerts if you upload something wrong or upload columns without all categories. Fix in next version
pair_adder <- pair_inspection <- function(id, data_sheet, summary_sheet){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      observeEvent(input$Submit,{
        if(input$dataEntry==""){
          shinyWidgets::sendSweetAlert(title = "No data detected!",
                                       type = "error",
                                       text = "Please paste in data before pressing submit")
        }
        else{
        LoadedData <- read.table(text = input$dataEntry, sep = "\t")
        if(ncol(LoadedData)!=4){
          shinyWidgets::sendSweetAlert(title = "Incorrect number of cols!",
                                       type = "error",
                                       text = "Please paste in data as sex, genotype, birthdate and breedingpair")
        }
        else{
        colnames(LoadedData) <- c("Sex", "Genotype", "Birthdate", "BreedingPair")
        if(isFALSE(input$HetBox)){
          test_sheet_geno <- LoadedData |> 
          dplyr::group_by(Genotype, Birthdate, BreedingPair) |> 
          dplyr::tally() |> 
          tidyr::pivot_wider(names_from = Genotype, values_from = n) 
          
          permitted_col_names <- c("Birthdate", 
                                   "BreedingPair", 
                                   "WT", 
                                   "KO", 
                                   "N/A")
           if (all(colnames(test_sheet_geno) %in% permitted_col_names)==F){
             faulty_names <- setdiff(colnames(test_sheet_geno), 
                                     permitted_col_names)
             error_message <- paste(faulty_names, collapse = ", ")
             shinyWidgets::sendSweetAlert(title = "Incorrect genotype entry detected!",
                                          type = "error",
                                          text = paste("Incorrect genotype entries detected: ", 
                                                       error_message,
                                                       ". Please ensure genotype entries are correct (WT, KO, N/A)", sep = ""))
             
           }
          else{
          if (!"WT" %in% colnames(test_sheet_geno)){
            test_sheet_geno <- test_sheet_geno |> 
              dplyr::mutate(WT=0)
          }
          if (!"KO" %in% colnames(test_sheet_geno)){
            test_sheet_geno <- test_sheet_geno |> 
              dplyr::mutate(KO=0)
          }
          if (!"N/A" %in% colnames(test_sheet_geno)){
            test_sheet_geno <- test_sheet_geno |> 
              dplyr::mutate("N/A"=0)
          }
          test_sheet_geno <- test_sheet_geno |> 
          dplyr::mutate(ID = paste(Birthdate,BreedingPair, sep = "_")) |> 
          tidyr::replace_na(list(WT = 0, KO = 0, "N/A"=0)) |> 
          dplyr::select(Birthdate, BreedingPair, WT, KO, "N/A", ID)
          }
          
        test_sheet_sex <- LoadedData |> 
          dplyr::group_by(Sex, Birthdate, BreedingPair) |> 
          dplyr::tally() |> 
          tidyr::pivot_wider(names_from = Sex, 
                             values_from = n)
        
        permitted_col_names2 <- c("Birthdate", 
                                  "BreedingPair", 
                                  "M", 
                                  "F")
        if(all(colnames(test_sheet_sex) %in% permitted_col_names2)==F){
          faulty_names <- setdiff(colnames(test_sheet_sex), 
                                  permitted_col_names2)
          error_message <- paste(faulty_names, collapse = ", ")
          shinyWidgets::sendSweetAlert(title = "Incorrect sex entry detected!",
                                       type = "error",
                                       text = paste("Incorrect sex entries detected: ", 
                                                    error_message,
                                                    ". Please ensure genotype entries are correct (M, F).", sep = ""))
        }
        
        else{
        if (!"F" %in% colnames(test_sheet_sex)){
          test_sheet_sex <- test_sheet_sex |> 
            dplyr::mutate("F"=0)
        }
        if (!"M" %in% colnames(test_sheet_sex)){
          test_sheet_sex <- test_sheet_sex|> 
            dplyr::mutate("M"=0)
        }
        test_sheet_sex <- test_sheet_sex |> 
          dplyr::mutate(ID = paste(Birthdate,BreedingPair, sep = "_")) |> 
          tidyr::replace_na(list("F"=0, M = 0))
        }
 
        if("ID"%in%colnames(test_sheet_geno)==T&"ID"%in%colnames(test_sheet_sex)==T){
                  test_sheet_joined <-
          dplyr::left_join(test_sheet_geno,
                    test_sheet_sex,
                    by = "ID",
                    suffix = c("", ".y")) |> 
          dplyr::select_at(dplyr::vars(-ends_with(".y"))) |>  
          dplyr::select(-ID) |> 
          dplyr::rename(Breeding_pair = "BreedingPair",
                        Litter_date = "Birthdate",
                        Male_pups = "M",
                        Female_pups = "F",
                        WT_pups = "WT",
                        KO_pups = "KO",
                        NA_pups = "N/A") |> 
          dplyr::mutate(Litter_date = as.Date(Litter_date, format = "%m/%d/%Y"),
                        Male_pups = as.integer(Male_pups),
                        Female_pups = as.integer(Female_pups),
                        WT_pups = as.integer(WT_pups),
                        KO_pups = as.integer(KO_pups),
                        NA_pups = as.integer(NA_pups)) |> 
          dplyr::select("Breeding_pair", "Litter_date", "Male_pups", "Female_pups", "WT_pups", "KO_pups", "NA_pups")
        }
        else{
          test_sheet_joined <- NULL
        }
        }
        else{
          test_sheet_geno <- LoadedData |> 
            dplyr::group_by(Genotype, Birthdate, BreedingPair) |> 
            dplyr::tally() |> 
            tidyr::pivot_wider(names_from = Genotype, values_from = n)
          
          permitted_col_names <- c("Birthdate", 
                                   "BreedingPair", 
                                   "WT", 
                                   "KO", 
                                   "N/A",
                                   "Het")
          if (all(colnames(test_sheet_geno) %in% permitted_col_names)==F){
            faulty_names <- setdiff(colnames(test_sheet_geno), 
                                    permitted_col_names)
            error_message <- paste(faulty_names, collapse = ", ")
            shinyWidgets::sendSweetAlert(title = "Incorrect genotype entry detected!",
                                         type = "error",
                                         text = paste("Incorrect genotype entries detected: ", 
                                                      error_message,
                                                      ". Please ensure genotype entries are correct (WT, KO, N/A)", sep = ""))
          }
          else{
            
          if (!"WT" %in% colnames(test_sheet_geno)){
            test_sheet_geno <- test_sheet_geno |> 
              dplyr::mutate(WT=0)
          }
          if (!"KO" %in% colnames(test_sheet_geno)){
            test_sheet_geno <- test_sheet_geno |> 
              dplyr::mutate(KO=0)
          }
          if (!"N/A" %in% colnames(test_sheet_geno)){
            test_sheet_geno <- test_sheet_geno |> 
              dplyr::mutate("N/A"=0)
          }
          if (!"Het" %in% colnames(test_sheet_geno)){
            test_sheet_geno <- test_sheet_geno |> 
              dplyr::mutate("Het"=0)
          }
          test_sheet_geno <- test_sheet_geno |> 
            dplyr::mutate(ID = paste(Birthdate,BreedingPair, sep = "_")) |> 
            tidyr::replace_na(list(WT = 0, KO = 0, Het = 0, "N/A"=0)) |> 
            dplyr::select(Birthdate, BreedingPair, WT, KO, Het, "N/A", ID)
          }
          
          test_sheet_sex <- LoadedData |> 
            dplyr::group_by(Sex, Birthdate, BreedingPair) |> 
            dplyr::tally() |> 
            tidyr::pivot_wider(names_from = Sex, values_from = n)
          
          permitted_col_names2 <- c("Birthdate", 
                                    "BreedingPair", 
                                    "M", 
                                    "F")
          if(all(colnames(test_sheet_sex) %in% permitted_col_names2)==F){
            faulty_names <- setdiff(colnames(test_sheet_sex), 
                                    permitted_col_names2)
            error_message <- paste(faulty_names, collapse = ", ")
            shinyWidgets::sendSweetAlert(title = "Incorrect sex entry detected!",
                                         type = "error",
                                         text = paste("Incorrect sex entries detected: ", 
                                                      error_message,
                                                      ". Please ensure genotype entries are correct (M, F).", sep = ""))
          }
          else{
          if (!"F" %in% colnames(test_sheet_sex)){
            test_sheet_sex <- test_sheet_sex |> 
              dplyr::mutate("F"=0)
          }
          if (!"M" %in% colnames(test_sheet_sex)){
            test_sheet_sex <- test_sheet_sex|> 
              dplyr::mutate("M"=0)
          }
          test_sheet_sex <- test_sheet_sex |> 
            dplyr::mutate(ID = paste(Birthdate,BreedingPair, sep = "_")) |> 
            tidyr::replace_na(list("F"=0, M = 0))
          }
          if ("ID"%in%colnames(test_sheet_geno)==T&"ID"%in%colnames(test_sheet_sex)==T){
          test_sheet_joined <-
            left_join(test_sheet_geno,
                      test_sheet_sex,
                      by = "ID",
                      suffix = c("", ".y")) |> 
            dplyr::select_at(dplyr::vars(-ends_with(".y"))) |>  
            dplyr::select(-ID) |> 
            dplyr::rename(Breeding_pair = "BreedingPair",
                          Litter_date = "Birthdate",
                          Male_pups = "M",
                          Female_pups = "F",
                          WT_pups = "WT",
                          KO_pups = "KO",
                          Het_pups = "Het", 
                          NA_pups = "N/A") |> 
            dplyr::mutate(Male_pups = as.integer(Male_pups),
                          Female_pups = as.integer(Female_pups),
                          WT_pups = as.integer(WT_pups),
                          KO_pups = as.integer(KO_pups),
                          Het_pups = as.integer(Het_pups),
                          NA_pups = as.integer(NA_pups)) |> 
            dplyr::select("Breeding_pair", "Litter_date", "Male_pups", "Female_pups", "WT_pups", "KO_pups", "Het_pups") 
          }
          else{
            test_sheet_joined <- NULL
          }
        }
        if(!is.null(test_sheet_joined)){
          data_sheet$lazy <- test_sheet_joined
          output$UploadedData <- shiny::renderTable({test_sheet_joined})
          output$SubmitButton<- shiny::renderUI({shiny::actionButton(ns("AddData"), label = "Add Litters to main analysis")})
        }
         
        }
        }
      })
      
        trigger = 0
        shiny::observeEvent(input$AddData,{
          trigger = trigger+1
          if(trigger>0){
            req(data_sheet$data)
          data_sheet$lazy$Litter_date <- as.Date(data_sheet$lazy$Litter_date, "%d-%m-%Y")
          data_sheet$data <- dplyr::bind_rows(data_sheet$data, data_sheet$lazy)
          output$Success <- shiny::renderText({"Data successfully joined"})
          trigger=0
          }
          
        })
    })
    }