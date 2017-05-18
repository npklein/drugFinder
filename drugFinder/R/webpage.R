
preprocess_data <- function(drug_results){
  combined_positive <- drug_results$combined_results_positive
  combined_negative <- drug_results$combined_results_negative
  atc_positive <- drug_results$atc_results_positive
  atc_negative <- drug_results$atc_results_negative
  name_positive <- drug_results$name_results_positive
  name_negative <- drug_results$name_results_negative
  if(!file.exists('drug_names.txt')){
    writeLines(rownames(combined_positive), con='drug_names.txt')
  }
  drugcodes <- get_drugbank_info('drug_names.txt', 'drugcodes.txt')
  rownames(drugcodes) <- drugcodes$drug_name
  combined_positive$drug_name <- character(length=dim(combined_positive)[1])
  combined_negative$drug_name <- character(length=dim(combined_negative)[1])
  name_positive$drug_name <- character(length=dim(name_positive)[1])
  name_negative$drug_name <- character(length=dim(name_negative)[1])
  for (drug_name in rownames(combined_positive)){
    if (drug_name %in% drugcodes[rownames(combined_positive),]$drug_name){
      combined_positive[drug_name,]$drug_name <- paste('<A HREF ="http://www.drugbank.ca/drugs/',drugcodes[drug_name,]$drugbank_code,
                                           '">',drug_name,'</A>',sep='')
      combined_negative[drug_name,]$drug_name <- paste('<A HREF ="http://www.drugbank.ca/drugs/',drugcodes[drug_name,]$drugbank_code,
                                                       '">',drug_name,'</A>',sep='')
      name_positive[drug_name,]$drug_name <- paste('<A HREF ="http://www.drugbank.ca/drugs/',drugcodes[drug_name,]$drugbank_code,
                                                       '">',drug_name,'</A>',sep='')
      name_negative[drug_name,]$drug_name <- paste('<A HREF ="http://www.drugbank.ca/drugs/',drugcodes[drug_name,]$drugbank_code,
                                                   '">',drug_name,'</A>',sep='')
    }
    else{
      combined_positive[drug_name,]$drug_name <- drug_name
      combined_negative[drug_name,]$drug_name <- drug_name
      name_positive[drug_name,]$drug_name <- drug_name
      name_negative[drug_name,]$drug_name <- drug_name
    }
  }
  

  n_columns <- dim(combined_positive)[2]
  combined_positive <-  combined_positive[,c(n_columns,1:(n_columns-1))]
  n_columns <- dim(combined_negative)[2]
  combined_negative <-  combined_negative[,c(n_columns,1:(n_columns-1))]
    
  atc_positive$atc_code <- paste('<A HREF ="http://www.whocc.no/atc_ddd_index/?code=',rownames(atc_positive),
                                 '">',rownames(atc_positive),'</A>',sep='')
  n_columns <- dim(atc_positive)[2]
  atc_positive <-  atc_positive[,c(n_columns,1:(n_columns-1))]
  n_columns <- dim(atc_negative)[2]
  atc_negative <-  atc_negative[,c(n_columns,1:(n_columns-1))]
  atc_negative$atc_code <- paste('<A HREF ="http://www.whocc.no/atc_ddd_index/?code=',rownames(atc_negative),
                                 '">',rownames(atc_negative),'</A>',sep='')
  
  n_columns <- dim(name_positive)[2]
  name_positive <-  name_positive[,c(n_columns,1:(n_columns-1))]
  n_columns <- dim(name_negative)[2]
  name_negative <-  name_negative[,c(n_columns,1:(n_columns-1))]
  return(list('combined_positive'=combined_positive, 'combined_negative'=combined_negative,
              'atc_positive'=atc_positive, 'atc_negative'=atc_negative,
              'name_positive'=name_positive, 'name_negative'=name_negative))
}

webpage_output <- function(drug_results, outfolder_location_root, input_dataset_name){
  outfolder_location <- paste(outfolder_location_root,input_dataset_name,'/',sep='')
  drug_results_processed <- preprocess_data(drug_results)
  dir.create(paste(outfolder_location,sep=""), showWarnings = FALSE)
  drug_results_out <- paste(outfolder_location,'drug_results',sep='')
  if(!file.exists(drug_results_out)){
    save(drug_results_processed,file=drug_results_out)
  }
  
  ui_file <- c(
    'load("drug_results")',
    'require(shiny)',
    'shinyUI(navbarPage("Result types",',
    '  tabPanel("Summary drug results",',
    '    titlePanel("Summary drug results"),',
    '    mainPanel(',
    '      tabsetPanel(',
    '        tabPanel("Positive enrichment",',
    '          fluidRow(column(6,dataTableOutput("drug_combined_positive")))),',
    '      tabPanel("Negative enrichment",',
    '          fluidRow(column(6,dataTableOutput("drug_combined_negative")))))',
    '    )',
    '  ),',
    '  tabPanel("ATC results",',
    '           titlePanel("ATC drug results"),',
    '           sidebarPanel(',
    '           checkboxGroupInput("show_vars_atc", "Columns to show:",', 
    '                               names(drug_results_processed$atc_positive),',
    '                               selected=names(drug_results_processed$atc_positive))),',
    '           mainPanel(',
    '             tabsetPanel(',
    '               tabPanel("Positive enrichment",',
    '                        fluidRow(column(6,dataTableOutput("atc_combined_positive")))),',
    '               tabPanel("Negative enrichment",',
    '                         fluidRow(column(6,dataTableOutput("atc_combined_negative")))))',
    '    )',
    '  ),',
    '  tabPanel("Drug name results",',
    '           titlePanel("Drug name results"),',
    '           sidebarPanel(',
    '             checkboxGroupInput("show_vars_name", "Columns to show:",', 
    '                                names(drug_results_processed$name_positive),',
    '                                selected=names(drug_results_processed$name_positive))),',
    '         mainPanel(',
    '           tabsetPanel(',
    '             tabPanel("Positive enrichment",',
    '                      fluidRow(column(6,dataTableOutput("name_combined_positive")))),',
    '             tabPanel("Negative enrichment",', 
    '                      fluidRow(column(6,dataTableOutput("name_combined_negative")))))',
    '         )',
    '  )',
    '))')
  writeLines(ui_file, con=paste(outfolder_location,'ui.R',sep=''))
  server_file <- c(
    'load("drug_results")',
    'shinyServer(function(input,output){',
    '  output$drug_combined_positive <- renderDataTable({drug_results_processed$combined_positive} )',
    '  output$drug_combined_negative <- renderDataTable({drug_results_processed$combined_negative} )',
    '  output$atc_combined_positive <- renderDataTable({drug_results_processed$atc_positive[, input$show_vars_atc, drop=FALSE]} )',
    '  output$atc_combined_negative <- renderDataTable({drug_results_processed$atc_negative[, input$show_vars_atc, drop=FALSE]} )',
    '  output$name_combined_positive <- renderDataTable({drug_results_processed$name_positive[, input$show_vars_name, drop=FALSE]} )',
    '  output$name_combined_negative <- renderDataTable({drug_results_processed$name_negative[, input$show_vars_name, drop=FALSE]} )',
    '})')
    
  writeLines(server_file, con=paste(outfolder_location,'server.R',sep=''))
  run_webpages <- c(
    paste('setwd("',outfolder_location,'"")',sep=''),
    'list.of.packages <- c("shiny")',
    'new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]',
    'if(length(new.packages)) install.packages(new.packages)',
    'require(shiny)',
    paste('runApp("',input_dataset_name,'")',sep=''))
  writeLines(run_webpages, con=paste(outfolder_location_root,input_dataset_name,'_results.R',sep=''))
  #runApp(list(ui=ui,server=server))
}

#webpage_output(drug_results)
#l <- list(c(1,2),c(0.005,0.003), c(2.17, 5.71), c('NaN','NaN'), c('',''),c('',''))
#l <- data.frame(l)
#rownames(l) <- c('fisetin', 'DL.PPMP')
#colnames(l) <- c('rank','p_value', 'specificity_mean', 'stability', 'atc_codes', 'atc_rankings')






