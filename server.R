library(shiny)
library(shinydashboard)
library(data.table)
library(sqldf)
library(plotly)
library(xlsx)
library(C50)
options(scipen=999)
setwd("D:\\D\\Rscript\\Reporting_GUI\\R\\")
source("D:\\D\\Rscript\\Reporting_GUI\\R\\TrainRelatedDetails.R")
source("D:\\D\\Rscript\\Reporting_GUI\\R\\testRelatedDetails.R")
source("D:\\D\\Rscript\\Reporting_GUI\\R\\ReportingExcel.R")
source("D:\\D\\Rscript\\Reporting_GUI\\R\\PkgLoader.R")
source("D:\\D\\Rscript\\Reporting_GUI\\R\\GetDatasetList.R")
source("D:\\D\\Rscript\\Reporting_GUI\\R\\GetColumnsType.R")
source("D:\\D\\Rscript\\Reporting_GUI\\R\\DataSummary.R")
source("D:\\D\\Rscript\\Reporting_GUI\\R\\Preprocess.R")
source("D:\\D\\Rscript\\Reporting_GUI\\R\\StatisticsTest.R")
source("D:\\D\\Rscript\\Reporting_GUI\\R\\global.R")
source("D:\\D\\Rscript\\Reporting_GUI\\R\\Clustering.R")
source("D:\\D\\Rscript\\Reporting_GUI\\R\\Classification.R")
source("D:\\D\\Rscript\\Reporting_GUI\\R\\Visualize.R")
shinyServer(function(input, output){
  fileinput <- fread("D:\\D\\Rscript\\Reporting_GUI\\Data\\FULL_REQ.txt")
  treeinput <- fread("D:\\D\\Rscript\\Reporting_GUI\\Data\\Train_Sample.csv")
  treeinput$Churn_Status<-as.factor(treeinput$Churn_Status)
  
  
  #TrainDataCall
  output$table_trainps<-renderTable({
    dataset<-TrainPredictionStatusCall(fileinput)
    dataset
  })
  
  output$trainSummary<-renderTable({
    segment<-input$segmentsV
    if(segment == "OverAll"){
        dataset<-TrainModelPredictionSummaryOverall(fileinput)
        dataset
    }
    else if(segment == "High"){
      dataset<-TrainModelPredictionSummaryHigh(fileinput)
      dataset
    }
    else if(segment == "Medium"){
      dataset<-TrainModelPredictionSummaryMedium(fileinput)
      dataset
    }else if(segment == "Low"){
      dataset<-TrainModelPredictionSummaryLow(fileinput)
      dataset
    }
  })
  
  output$table_trainCPI<-renderTable({
    dataset<-TrainCPIScore(fileinput)
    dataset
  })
  
  output$table_trainTrf<-renderTable({
    dataset<-TrainTariffPlan(fileinput)
    dataset
  })
  
  output$table_trainRgn<-renderTable({
    dataset<-TrainRegion(fileinput)
    dataset
  })
  
  output$table_trainValue<-renderTable({
    dataset<-TrainValueSegmentation(fileinput)
    dataset
  })
  
  output$text_trainps<-renderPrint({
    dataset<-TrainPredictionStatusCall(fileinput)
    summary(dataset)
  })
  
  output$trainSummarytext<-renderPrint({
    segment<-input$segmentsV
    if(segment == "OverAll"){
      dataset<-TrainModelPredictionSummaryOverall(fileinput)
      summary(dataset)
    }
    else if(segment == "High"){
      dataset<-TrainModelPredictionSummaryHigh(fileinput)
      summary(dataset)
    }
    else if(segment == "Medium"){
      dataset<-TrainModelPredictionSummaryMedium(fileinput)
      summary(dataset)
    }else if(segment == "Low"){
      dataset<-TrainModelPredictionSummaryLow(fileinput)
      summary(dataset)
    }
  })
  
  output$text_trainCPI<-renderPrint({
    dataset<-TrainCPIScore(fileinput)
    summary(dataset)
  })
  
  output$text_trainTrf<-renderPrint({
    dataset<-TrainTariffPlan(fileinput)
    summary(dataset)
  })
  
  output$text_trainRgn<-renderPrint({
    dataset<-TrainRegion(fileinput)
    summary(dataset)
  })
  
  output$text_trainValue<-renderPrint({
    dataset<-TrainValueSegmentation(fileinput)
    summary(dataset)
  })
  
  
  #treeinputDataCall
  output$table_treeinputStatus<-renderTable({
    dataset<-treeinputPredictionStatusCall(fileinput)
    dataset
  })
  
  output$table_treeinputCPI<-renderTable({
    dataset<-treeinputCPIScore(fileinput)
    dataset
  })
  output$table_treeinputTrf<-renderTable({
    dataset<-treeinputTariffPlan(fileinput)
    dataset
  })
  output$table_treeinputRgn<-renderTable({
    dataset<-treeinputRegion(fileinput)
    dataset
  })
  output$table_treeinputValue<-renderTable({
    dataset<-treeinputValueSegmentation(fileinput)
    dataset
  })
  
  output$text_treeinputStatus<-renderPrint({
    dataset<-treeinputPredictionStatusCall(fileinput)
    dataset
  })
  
  output$text_treeinputCPI<-renderPrint({
    dataset<-treeinputCPIScore(fileinput)
    summary(dataset)
  })
  output$text_treeinputTrf<-renderPrint({
    dataset<-treeinputTariffPlan(fileinput)
    summary(dataset)
  })
  output$text_treeinputRgn<-renderPrint({
    dataset<-treeinputRegion(fileinput)
    summary(dataset)
  })
  output$text_treeinputValue<-renderPrint({
    dataset<-treeinputValueSegmentation(fileinput)
    summary(dataset)
  })
  
  output$table_testov<-renderTable({
    dataset<-TestPredictionStatusCall(fileinput)
    dataset
  })
  
  output$table_testAON<-renderTable({
    dataset<-TestAON(fileinput)
    dataset
  })
  
  output$table_testCPI<-renderTable({
    dataset<-TestCPIScore(fileinput)
    dataset
  })
  
  output$table_testTrf<-renderTable({
    dataset<-TestTariffPlan(fileinput)
    dataset
  })
  
  output$table_testRgn<-renderTable({
    dataset<-TestRegion(fileinput)
    dataset
  })
  
  output$table_testValue<-renderTable({
    dataset<-TestValueSegmentation(fileinput)
    dataset
  })
  
  output$text_testov<-renderPrint({
    dataset<-TestPredictionStatusCall(fileinput)
    summary(dataset)
  })
  
  output$text_testAON<-renderPrint({
    dataset<-TestAON(fileinput)
    summary(dataset)
  }) 
  
  output$text_testCPI<-renderPrint({
    dataset<-TestCPIScore(fileinput)
    summary(dataset)
  })
  
  output$text_testTrf<-renderPrint({
    dataset<-TestTariffPlan(fileinput)
    summary(dataset)
  })
  
  output$text_testRgn<-renderPrint({
    dataset<-TestRegion(fileinput)
    summary(dataset)
  })
  
  output$text_testValue<-renderPrint({
    dataset<-TestValueSegmentation(fileinput)
    summary(dataset)
  })
  
  #Plotting Graphs
  dateRangeInput<-reactive({
    dataset = switch(input$segments,
                     "TotalTrainingSet" = TotalTrainingSet,
                     "High" = High,
                     "Medium" = Medium,
                     "Low" = Low)  
    return(dataset)
  })
  
  output$barplot <-renderPlot({
    PredictionStatus<-TrainPredictionStatusCall(fileinput)
    ggplot(data=PredictionStatus, 
    aes_string(x=PredictionStatus$PredictionStatus,y=input$segments)) + 
      stat_summary(fun.y = sum, geom ="bar") +
      geom_bar(stat="identity",fill="blue") + 
      labs(title=input$segments, y ="Count") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5))        
  })
  
  # Plotting value segment wise
  output$barplot_arpu <-renderPlot({
    segments<-input$segmentsV
    if(segments=="OverAll"){
      ValueSegment<-TrainModelPredictionSummaryOverall(fileinput)
      ggplot(data=ValueSegment, 
             aes_string(x=ValueSegment$ModelPredictionSummaryOverall,y=input$segmentsV)) + 
        stat_summary(fun.y = sum, geom ="bar") +
        geom_bar(stat="identity",fill="blue") + 
        labs(title=input$segmentsV, y ="Count") +
        theme_classic() + 
        theme(plot.title = element_text(hjust = 0.5))
    }
    else if(segments=="Low"){
      ValueSegment<-TrainModelPredictionSummaryHigh(fileinput)
      ggplot(data=ValueSegment, 
             aes_string(x=ValueSegment$ModelPredictionSummaryH,y=input$segmentsV)) + 
        stat_summary(fun.y = sum, geom ="bar") +
        geom_bar(stat="identity",fill="blue") + 
        labs(title=input$segmentsV, y ="Count") +
        theme_classic() + 
        theme(plot.title = element_text(hjust = 0.5))
    }
    else if(segments=="Medium"){
      ValueSegment<-TrainModelPredictionSummaryMedium(fileinput)
      ggplot(data=ValueSegment, 
             aes_string(x=ValueSegment$ModelPredictionSummaryM,y=input$segmentsV)) + 
        stat_summary(fun.y = sum, geom ="bar") +
        geom_bar(stat="identity",fill="blue") + 
        labs(title=input$segmentsV, y ="Count") +
        theme_classic() + 
        theme(plot.title = element_text(hjust = 0.5))
    }
    else if(segments=="High"){
      ValueSegment<-TrainModelPredictionSummaryLow(fileinput)
      ggplot(data=ValueSegment, 
             aes_string(x=ValueSegment$ModelPredictionSummaryL,y=input$segmentsV)) + 
        stat_summary(fun.y = sum, geom ="bar") +
        geom_bar(stat="identity",fill="blue") + 
        labs(title=input$segmentsV, y ="Count") +
        theme_classic() + 
        theme(plot.title = element_text(hjust = 0.5))
    }     
  })
  
  #Detailed Reports.
  output$table <- DT::renderDataTable({
    DT::datatable(fileinput,filter = 'top',extensions = c('Buttons','AutoFill','ColReorder'), 
    options = list(autoFill=TRUE,colReorder=TRUE,dom = 'Blfrtip',
    buttons = list('copy','print',list(extend = 'collection',
    buttons = c('csv', 'excel', 'pdf'),text = 'Download'))))
  })
  
  #ReportingMethods
  
  datasetInput <- reactive({
    switch(input$dreportperiod,
           "201801" = 201801,
           "201712" = 201712,
           "201711" = 201711)
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste(input$dreportperiod, ".xlsx", sep = "")
      },
    content = function(file) {
      fname<-file
      report<-SummaryReports()
      saveWorkbook(report,file=fname)
      file.rename(fname,file)
    },
    contentType="application/xlsx" 
  )
  
  #Decision Trees
  
  output$choose_y <- renderUI({
    treeinput[is.na(treeinput)]<-0
    is_factor <- sapply(treeinput, FUN = is.factor)
    y_choices <- names(treeinput)[is_factor]
    selectInput('choose_y', label = 'Choose Target Variable', choices = y_choices)
  })
  
  output$choose_x <- renderUI({
    x_choices <- names(treeinput)[!names(treeinput) %in% input$choose_y]
    checkboxGroupInput('choose_x', label = 'Choose Predictors', choices = x_choices)
  })
  
  observeEvent(input$add_button, {
    treeinput[is.na(treeinput)]<-0
    form <- paste(isolate(input$choose_y), '~', paste(isolate(input$choose_x), collapse = '+'))
    c50_fit <- eval(parse(text = sprintf("C5.0(%s, data = treeinput)", form)))
    output$tree_summary <- renderPrint(summary(c50_fit))
    output$tree_plot_c50 <- renderPlot({
      plot(c50_fit)
    })
    output$cf_plot_c50 <- renderPlot({
      b <- summary(c50_fit)
      print(b)
      pos1 <- gregexpr(pattern ='\\(a\\)', b$output)[[1]][1]
      pos2 <- gregexpr(pattern ='class Y', b$output)[[1]][1]
      text <- substr(b$output, pos1, pos2)
      slist<-strsplit(text," ")
      print(text)
      Y <- c()
      for(i in 1:length(slist[[1]])){
        digit<-grep("\\d+",slist[[1]][i])
        ifelse(digit==1,Y<-c(Y,slist[[1]][i]),print("No"))
      }
      print(Y)
      Y<-as.numeric(Y)
      Y[is.na(Y)]<-0
      TClass <- factor(c("Non-Churn", "Non-Churn", "Churn", "Churn"))
      PClass <- factor(c("Non-Churn", "Churn", "Non-Churn", "Churn"))
      df <- data.frame(TClass, PClass, Y)
      library(ggplot2)
      ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
        geom_tile(aes(fill = Y), colour = "white") +
        geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
        scale_fill_gradient(low = "green", high = "red") +
        theme_bw() + theme(legend.position = "none")+
        labs(x="True Condition",y="Predicted Condition")+ 
        ggtitle("Confusion Matrix - Training Data")+
        theme(text  = element_text(size = 25))
    })
  })
  ###################################################################################################
  #Dataset
  
  dataset_result <- reactiveValues(dataset = NULL, numeric_list = NULL, non_numeric_list = NULL, summary_numeric = NULL, summary_non_numeric = NULL, plot = NULL, plot_non_numeric = NULL, plot_height = NULL)
  
  output$dataset_parameter_panel <- renderUI({
    switch(input$dataset_type,
           "Build-in Dataset" = list(
             fluidRow(
               column(4, uiOutput("dataset_list"))
             ),
             fluidRow(
               column(11),
               column(1, actionButton("select_dataset", label = "Select"))
             )
           ),
           "Upload CSV" = list(
             fluidRow(
               column(3, fileInput("csv", label = h4("Choose CSV File"), accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
               column(3, radioButtons("header", label = h4("Header"), c(True = TRUE, False = FALSE))),
               column(3, radioButtons("sep", label = h4("Separator"), c(Comma=",", Semicolon=";", Tab="\t"), ",")),
               column(3, radioButtons("quote", label = h4("Quote"), c(None="", "Double Quote"="\"", "Single Quote"="'"), "\""))
             ),
             fluidRow(
               column(11),
               column(1, actionButton("select_dataset", label = "Select"))
             )
           )
    )
  })
  
  output$dataset_list <- renderUI({
    withProgress(min=1, max=20, expr={
      for(i in 1:20) {
        setProgress(message = "Page loading.", detail = "This may take a while...", value=i)
        print(i)
        Sys.sleep(0.05)
      }
    })
    dataset_list <- getDatasetList()
    selectInput("dataset", label = h4("Choose Dataset"), dataset_list, selected = "201801.csv")
  })
  
  observeEvent(input$select_dataset, {
    withProgress(min=1, max=20, expr={
      for(i in 1:20) {
        setProgress(message = "Processing.", detail = "This may take a while...", value=i)
        print(i)
        Sys.sleep(0.1)
      }
    })
    dataset_result$dataset <- switch(input$dataset_type,
                                     "Build-in Dataset" = readRDS(input$dataset),
                                     "Upload CSV" = read.table(input$csv$datapath, header = if(input$header == "TRUE") TRUE else FALSE, sep = input$sep , quote = input$quote)
    )
    if(!is.null(dataset_result$dataset)) {
      dataset_result$numeric_list <- unlist(getColumnsType(dataset_result$dataset)[1])
      dataset_result$non_numeric_list <- unlist(getColumnsType(dataset_result$dataset)[2])
      dataset_result$summary_numeric <- dataSummary_numreic(dataset_result$dataset)
      dataset_result$summary_non_numeric <- dataSummary_non_numeric(dataset_result$dataset)
      if(!is.null(dataset_result$summary_numeric)) {
        dataset_result$plot_non_numeric <- if(input$dataset_type == "Upload CSV") FALSE else TRUE
        dataset_result$plot <- plotPairs(dataset_result$dataset, non_numeric = dataset_result$plot_non_numeric)
        dataset_result$plot_height <- if(length(names(dataset_result$dataset)) <= 8) paste(length(names(dataset_result$dataset)) * 200, "px", sep = "") else "1600px"
      }
    }
  })
  
  output$kpi_list<-renderUI({
    observeEvent(input$dataset_first, {
    withProgress(min=1, max=20, expr={
      for(i in 1:20) {
        setProgress(message = "Processing.", detail = "This may take a while...", value=i)
        print(i)
        Sys.sleep(0.1)
      }
    })
    dataset_result$dataset_first <- readRDS(input$dataset_first)
    dataset_result$dataset_second <- readRDS(input$dataset_second)
    if(!is.null(dataset_result$dataset_first) & !is.null(dataset_result$dataset_second)) {
      first_header <- names(dataset_result$dataset_first)
      second_header <- names(dataset_result$dataset_second)
      if(all(first_header==second_header)){
        dataset_result$kpi_list<-first_header
        selectInput("impact_kpis", label = h4("Select Impacting KPIs"),dataset_result$kpi_list,
                    multiple = FALSE,selectize = TRUE,width = '200px'
        )
      }
      else{
        print("Column names are not matching...")
      }
    }
    })
  })
  
  output$attribute_list_panel <- renderUI({
    column(width=3,box(width = NULL, status = "primary", solidHeader = TRUE,
                       uiOutput("kpi_list")
    ))
  })
  
  output$dataset_result_panel <- renderUI({
    if(!is.null(dataset_result$summary_numeric)) {
      tabsetPanel(
        tabPanel("Summary",
                 fluidRow(
                   column(6,
                          fluidRow(column(12, h4("Numeric summary"))),
                          fluidRow(column(12, tableOutput("dataset_summary_numeric")))
                   ),
                   column(9,
                          fluidRow(column(12, h4("Correlation Matrix"))),
                          fluidRow(column(12, tableOutput("correlation_matrix")))
                   )
                 ),
                 fluidRow(
                   column(6,
                          fluidRow(column(12, h4("Non-Numeric Summary"))),
                          if(length(dataset_result$summary_non_numeric) == 1) {
                            fluidRow(column(12, tableOutput("dataset_summary_non_numeric")))
                          }else {
                            fluidRow(verbatimTextOutput("dataset_summary_non_numerics"))
                          }
                   )
                 )
        ),
        tabPanel("Plot",
                 uiOutput("dataset_plot_parameter_panel"),
                 fluidRow(column(12, plotOutput("dataset_plot", height = dataset_result$plot_height)))
        ),
        tabPanel("Table",
                 fluidRow(column(12, dataTableOutput("dataset_datatable")))
        )
      )
    }else if(!is.null(dataset_result$dataset)){
      tabsetPanel(
        tabPanel("Table",
                 fluidRow(column(12, h4("This dataset may not be used to clustering and classification."))),
                 fluidRow(column(12, tableOutput("dataset_table")))
        )
      )
    }
  })
  
  output$dataset_summary_numeric <- renderTable({
    if(!is.null(dataset_result$summary_numeric)) print(as.data.frame(dataset_result$summary_numeric[1]))
  })
  
  output$correlation_matrix <- renderTable({
    if(!is.null(dataset_result$summary_numeric)) print(as.data.frame(dataset_result$summary_numeric[2]))
  })
  
  output$dataset_summary_non_numeric <- renderTable({
    if(!is.null(dataset_result$summary_non_numeric)) print(as.data.frame(dataset_result$summary_non_numeric))
  })
  
  output$dataset_summary_non_numerics <- renderPrint({
    for( table in dataset_result$summary_non_numeric) {
      print(as.data.frame(table))
    }
  })
  
  output$dataset_plot_parameter_panel <- renderUI({
    if(!is.null(dataset_result$summary_non_numeric) && dataset_result$plot_non_numeric) {
      list(
        fluidRow(
          column(4, selectInput("dataset_plot_class", label = h4("Choose Class Attribute"), c("None", unlist(getColumnsType(dataset_result$dataset)[2]))))
        ),
        fluidRow(
          column(11),
          column(1, actionButton("dataset_plot", label = "Plot"))
        )
      )
    }
  })
  
  observeEvent(input$dataset_plot, {
    if(!is.null(dataset_result$summary_numeric)) {
      class <- if(input$dataset_plot_class == "None") NULL else input$dataset_plot_class
      dataset_result$plot <- plotPairs(dataset_result$dataset, non_numeric = dataset_result$plot_non_numeric)
    }
  })
  
  output$dataset_plot <- renderPlot({
    if(!is.null(dataset_result$plot)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Creating Plot.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.1)
        }
      })
      print(dataset_result$plot)
    }
  })
  
  output$dataset_datatable <- renderDataTable({
    withProgress(min=1, max=20, expr={
      for(i in 1:20) {
        setProgress(message = "Creating Table.", detail = "This may take a while...", value=i)
        print(i)
        Sys.sleep(0.1)
      }
    })
    dataset_result$dataset[, drop =FALSE]
  }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10)
  )
  
  output$dataset_table <- renderTable({
    dataset_result$dataset
  })
  
  ###################################################################################################
  #Statistics Test
  
  statistics_result <- reactiveValues(result = NULL, reg_model = NULL)
  
  output$statistics_variable_panel <- renderUI({
    if(!is.null(dataset_result$summary_numeric)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Page Loading.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.05)
        }
      })
      switch(input$statistics_method,
             "Regression" = list(
               column(4, selectInput("statistics_reg_response", label = h4("Choose Response Variable"), dataset_result$numeric_list))
             ),
             "Paired T Test" = list(
               column(4, selectInput("statistics_ttest_var1", label = h4("Choose Firset Variable"), dataset_result$numeric_list)),
               column(4, selectInput("statistics_ttest_var2", label = h4("Choose Second Variable"), dataset_result$numeric_list))
             ),
             "One-way ANOVA" = list(
               column(4, selectInput("statistics_oneway_anova_group", label = h4("Choose Group Variable"), dataset_result$non_numeric_list)),
               column(4, selectInput("statistics_oneway_anova_response", label = h4("Choose Response Variable"), dataset_result$numeric_list))
             ),
             "MANOVA" = list(
               column(4, selectInput("statistics_manova_groups", label = h4("Choose Group Variable"), dataset_result$non_numeric_list, multiple = TRUE)),
               column(4, selectInput("statistics_manova_responses", label = h4("Choose Response Variable"), dataset_result$numeric_list, multiple = TRUE))
             )
      )
    }
  })
  
  output$statistics_parameter_panel <- renderUI({
    if(!is.null(dataset_result$dataset)) {
      fluidRow(
        column(11),
        column(1, actionButton("start_statistics", label = "Start"))
      )
    }
  })
  
  observeEvent(input$start_statistics, {
    statistics_result$result <- switch(input$statistics_method,
                                       "Regression" = statisticsRegression(dataset_result$dataset, input$statistics_reg_response),
                                       "Paired T Test" = statisticsTtest(dataset_result$dataset, input$statistics_ttest_var1, input$statistics_ttest_var2),
                                       "One-way ANOVA" = statisticsOnewayANOVA(dataset_result$dataset, input$statistics_oneway_anova_group, input$statistics_oneway_anova_response),
                                       "MANOVA" = statisticsMANOVA(dataset_result$dataset, input$statistics_manova_groups, input$statistics_manova_responses)
    )
  })
  
  output$statistics_result_panel <- renderUI({
    if(!is.null(statistics_result$result)) {
      switch(input$statistics_method,
             "Regression" = {
               tabsetPanel(
                 tabPanel("Detail",
                          fluidRow(column(12, verbatimTextOutput("statistics_result_detail_1")))
                 ),
                 tabPanel("Step",
                          fluidRow(column(12, verbatimTextOutput("statistics_result_detail_2")))
                 ),
                 tabPanel("Outlier Test",
                          fluidRow(column(12, verbatimTextOutput("statistics_result_detail_3")))
                 )
               )
             },
             "Paired T Test" = {
               tabsetPanel(
                 tabPanel("Detail",
                          fluidRow(column(12, verbatimTextOutput("statistics_result_detail_1")))
                 )
               )
             },
             "One-way ANOVA" = {
               tabsetPanel(
                 tabPanel("Detail",
                          fluidRow(column(12, verbatimTextOutput("statistics_result_detail_1")))
                 ),
                 tabPanel("Tukey",
                          fluidRow(column(12, verbatimTextOutput("statistics_result_detail_2")))
                 )
               )
             },
             "MANOVA" = {
               tabsetPanel(
                 tabPanel("Detail",
                          fluidRow(column(12, verbatimTextOutput("statistics_result_detail_1")))
                 )
               )
             }
      )
    }
  })
  
  output$statistics_result_detail_1 <- renderPrint({
    if(!is.null(statistics_result$result)) {
      switch(input$statistics_method,
             "Regression" = print(summary(statistics_result$result)),
             "Paired T Test" = print(statistics_result$result),
             "One-way ANOVA" = print(summary(statistics_result$result)),
             "MANOVA" = {
               print(statistics_result$result)
               print(summary(statistics_result$result))
             }
      )
    }
  })
  
  output$statistics_result_detail_2 <- renderPrint({
    if(!is.null(statistics_result$result)) {
      switch(input$statistics_method, 
             "Regression" = print(statisticsRegression_step(statistics_result$result)),
             "One-way ANOVA" = print(statisticsOnewayANOVA_tukey(statistics_result$result))
      )
    }
  })
  
  output$statistics_result_detail_3 <- renderPrint({
    if(!is.null(statistics_result$result)) {
      switch(input$statistics_method,
             "Regression" = print(statisticsRegression_outlier(statistics_result$result))
      )
    }
  })
  
  ###################################################################################################
  #Clustering
  
  clustering_result <- reactiveValues(result = NULL, result_data_frame = NULL, plot = NULL, plot_height = NULL, manova = NULL)
  
  output$clustering_parameters_panel <- renderUI({
    if(!is.null(dataset_result$summary_numeric)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Page Loading.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.05)
        }
      })
      switch(input$clustering_method,
             "K-Means" = list(
               fluidRow(
                 column(4, sliderInput("kmeans_k", label = h4("Set K"), min = 2, max = 10, value = 2))
               ),
               fluidRow(
                 column(11),
                 column(1, actionButton("start_clustering", label = "Clustering"))
               )
             ),
             "EM" = list(
               fluidRow(
                 column(11),
                 column(1, actionButton("start_clustering", label = "Clustering"))
               )
             ),
             "DBSCAN" = list(
               fluidRow(
                 column(4, sliderInput("dbscan_eps", label = h4("Set Eps"), min = 0.1, max = 1, value = 0.5)),
                 column(4, sliderInput("dbscan_pts", label = h4("Set MinPts"), min = 2, max = 20, value = 10))
               ),
               fluidRow(
                 column(11),
                 column(1, actionButton("start_clustering", label = "Clustering"))
               )
             ),
             "Spectral" = list(
               fluidRow(
                 column(4, sliderInput("spectral_centers", label = h4("Set Centers"), min = 0, max = 10, value = 0)),
                 column(4, sliderInput("spectral_nn", label = h4("Set NN"), min = 2, max = 20, value = 7))	
               ),
               fluidRow(
                 column(11),
                 column(1, actionButton("start_clustering", label = "Clustering"))
               )
             )
      )
      
    }
  })
  
  observeEvent(input$start_clustering, {
    clustering_result$result <- switch(input$clustering_method,
                                       "K-Means" = clusteringKmeans(dataset_result$dataset, input$kmeans_k),
                                       "EM" = clusteringEM(dataset_result$dataset),
                                       "DBSCAN" = clusteringDBSCAN(dataset_result$dataset, input$dbscan_eps, input$dbscan_pts),
                                       "Spectral" = clusteringSpectral(dataset_result$dataset, input$spectral_centers, input$spectral_nn)
    )
    clustering_result$result_data_frame <- as.data.frame(clustering_result$result[1][1])
    clustering_result$manova <- statisticsMANOVA(clustering_result$result_data_frame, names(clustering_result$result_data_frame)[length(names(clustering_result$result_data_frame))], names(clustering_result$result_data_frame)[1:length(names(clustering_result$result_data_frame)) - 1])
    clustering_result$plot <- plotClusteringResult(clustering_result$result_data_frame)
    clustering_result$plot_height <- if(length(names(clustering_result$result_data_frame)) <= 8) paste(length(names(clustering_result$result_data_frame)) * 200, "px", sep = "") else "1600px"
  })
  
  output$clustering_result_panel <-renderUI({
    if(!is.null(clustering_result$result)) {
      tabsetPanel(
        tabPanel("Detail",
                 fluidRow(column(12, verbatimTextOutput("clustering_result")))
        ),
        tabPanel("Plot",
                 fluidRow(column(12, plotOutput("clustering_result_plot", height = clustering_result$plot_height)))
        ),
        tabPanel("MANOVA",
                 fluidRow(column(12, verbatimTextOutput("clustering_manova")))
        ),
        tabPanel("Table",
                 fluidRow(column(12, dataTableOutput("clustering_result_table")))
        )
      )
    }
  })
  
  output$clustering_result <- renderPrint({
    if(!is.null(clustering_result$result)) {
      print(clustering_result$result[2])
    }
  }, width = 180
  )
  
  output$clustering_result_plot <- renderPlot({
    if(!is.null(clustering_result$result)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Creating Plot.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.1)
        }
      })
      print(clustering_result$plot)
    }
  })
  
  output$clustering_manova <- renderPrint({
    if(!is.null(clustering_result$result)) {
      print(clustering_result$manova)
      print(summary(clustering_result$manova))
    }
  })
  
  output$clustering_result_table <- renderDataTable({
    if(!is.null(clustering_result$result)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Creating Table.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.1)
        }
      })
      clustering_result$result_data_frame[, drop =FALSE]
    }
  }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10)
  )
  
  ###################################################################################################
  #Classification
  
  classification_result <- reactiveValues(
    dataset = NULL,
    training_dataset = NULL,
    testing_dataset = NULL,
    plot = NULL,
    plot_height = NULL,
    model = NULL,
    confusion_matrix = NULL
  )
  
  output$class_attribute_panel <- renderUI({
    withProgress(min=1, max=20, expr={
      for(i in 1:20) {
        setProgress(message = "Page Loading.", detail = "This may take a while...", value=i)
        print(i)
        Sys.sleep(0.05)
      }
    })
    if(!is.null(dataset_result$non_numeric_list)) {
      column(4, selectInput("class_attribute", label = h4("Choose Class Attribute"), dataset_result$non_numeric_list))
    }else {
      column(4, h4("No any class attribute exist."))
    }
  })
  
  output$classification_parameters_panel <- renderUI({
    if(!is.null(dataset_result$non_numeric_list)) {
      switch(input$classification_method,
             "Decision Tree" = list(
               fluidRow(
                 column(11),
                 column(1, actionButton("start_classify", label = "Classify"))
               )
             ),
             "Random Forest" = list(
               fluidRow(
                 column(11),
                 column(1, actionButton("start_classify", label = "Classify"))
               )
             ),
             "K-Nearest Neighbors" = list(
               fluidRow(
                 column(4, sliderInput("knn_k", label = h4("Set K"), min = 2, max = 20, value = 7)),
                 column(4, sliderInput("knn_distance", label = h4("Set Distance"), min = 0, max = 5, value = 2))
               ),
               fluidRow(
                 column(11),
                 column(1, actionButton("start_classify", label = "Classify"))
               )
             ),
             "Support Vector Machine" = list(
               fluidRow(
                 column(11),
                 column(1, actionButton("start_classify", label = "Classify"))
               )
             ),
             "Naive Bayes Classifier" = list(
               fluidRow(
                 column(11),
                 column(1, actionButton("start_classify", label = "Classify"))
               )
             ),
             "Feed-Forward Neural Network" = list(
               fluidRow(
                 column(4, sliderInput("nn_size", label = h5("Set Number of Units in the Hidden Layer"), min = 0, max = 20, value = 10))
               ),
               fluidRow(
                 column(11),
                 column(1, actionButton("start_classify", label = "Classify"))
               )
             )
      )
    }
  })
  
  observeEvent(input$start_classify, {
    classification_result$dataset <- dataPreprocess_Classification(dataset_result$dataset)
    classification_result$training_dataset <- as.data.frame(classification_result$dataset[1])
    classification_result$testing_dataset <- as.data.frame(classification_result$dataset[2])
    switch(input$classification_method,
           "Decision Tree" = {
             classification_result$model <- classificationDecisionTree(classification_result$training_dataset, class = input$class_attribute)
             classification_result$testing_dataset <- as.data.frame(evaluationDecissionTree(classification_result$model, classification_result$testing_dataset))
           },
           "Random Forest" = {
             classification_result$model <- classificationRandomForest(classification_result$training_dataset, class = input$class_attribute)
             classification_result$testing_dataset <- as.data.frame(evaluationRandomForest(classification_result$model, classification_result$testing_dataset))
           },
           "K-Nearest Neighbors" = {
             classification_result$model <- classificationKNN(classification_result$training_dataset, classification_result$testing_dataset, class = input$class_attribute, k = input$knn_k, distance = input$knn_distance)
             classification_result$testing_dataset$Predict_result <- fitted(classification_result$model)
           },
           "Support Vector Machine" = {
             classification_result$model <- classificationSVM(classification_result$training_dataset, class = input$class_attribute)
             classification_result$testing_dataset <- as.data.frame(evaluationSVM(classification_result$model, classification_result$testing_dataset))
           },
           "Naive Bayes Classifier" = {
             classification_result$model <- classificationNaiveBayes(classification_result$training_dataset, class = input$class_attribute)
             classification_result$testing_dataset <- as.data.frame(evaluationNaiveBayes(classification_result$model, classification_result$testing_dataset))
           },
           "Feed-Forward Neural Network" = {
             classification_result$model <- classificationNN(classification_result$training_dataset, class = input$class_attribute, input$nn_size)
             classification_result$testing_dataset <- as.data.frame(evaluationNN(classification_result$model, classification_result$testing_dataset))
           }
    )
    classification_result$plot <- plotClassificationResult(classification_result$testing_dataset)
    classification_result$plot_height <- if(length(names(classification_result$testing_dataset)) <= 8) paste(length(names(classification_result$testing_dataset)) * 200, "px", sep = "") else "1600px"
    classification_result$confusion_matrix <- evaluationConfusionMatrix(classification_result$testing_dataset[, length(names(classification_result$testing_dataset))], classification_result$testing_dataset[, length(names(classification_result$testing_dataset)) - 1])
  })
  
  output$classification_result_panel <- renderUI({
    if(!is.null(classification_result$dataset)) {
      tabsetPanel(
        tabPanel("Model",
                 fluidRow(column(12, verbatimTextOutput("classification_model_summary"))),
                 fluidRow(column(12, plotOutput("classification_model_plot", height = "800px")))
        ),
        tabPanel("Plot",
                 fluidRow(column(12, plotOutput("classification_result_plot", height = classification_result$plot_height)))
        ),
        tabPanel("Confusion Matrix",
                 fluidRow(
                   column(12,
                          fluidRow(column(12, h4("Summary"))),
                          fluidRow(
                            column(6, verbatimTextOutput("classification_confusion_matrix")),
                            column(6, plotOutput("classification_confusion_matrix_plot", height = "170px"))
                          )
                   )
                 ),
                 fluidRow(
                   column(12,
                          fluidRow(column(12, h4("Overall"))),
                          fluidRow(column(12, verbatimTextOutput("classification_confusion_matrix_overall")))
                   )
                 ),
                 fluidRow(
                   column(12,
                          fluidRow(column(12, h4("By Class"))),
                          fluidRow(column(12, plotOutput("classification_confusion_matrix_byclass_plot", height = "100px")))
                   )
                 )
        ),
        tabPanel("Table",
                 fluidRow(column(12, dataTableOutput("classification_result_table")))
        )
      )
    }
  })
  
  output$classification_model_summary <- renderPrint({
    if(!is.null(classification_result$model)) {
      switch(input$classification_method,
             "Decision Tree" = print(summary(classification_result$model)),
             "Random Forest" = print(summary(classification_result$model)),
             "EM" = print(classification_result$model),
             "K-Nearest Neighbors" = print(classification_result$model),
             "Support Vector Machine" = print(summary(classification_result$model)),
             "Naive Bayes Classifier" = print(classification_result$model),
             "Feed-Forward Neural Network" = print(classification_result$model)
      )
    }
  })
  
  output$classification_model_plot <- renderPlot({
    if(!is.null(classification_result$model)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Creating Model.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.1)
        }
      })
      switch(input$classification_method,
             "Decision Tree" = {
               plot(classification_result$model)
               text(classification_result$model)
             }
      )
    }
  })
  
  output$classification_result_plot <- renderPlot({
    if(!is.null(classification_result$plot)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Creating Plot.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.1)
        }
      })
      print(classification_result$plot)
    }
  })
  
  output$classification_result_table <- renderDataTable({
    if(!is.null(classification_result$testing_dataset)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Creating Datatable.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.1)
        }
      })
      classification_result$testing_dataset[, drop =FALSE]
    }
  }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10)
  )
  
  output$classification_confusion_matrix <- renderPrint({
    if(!is.null(classification_result$confusion_matrix)) print(classification_result$confusion_matrix[2])
  })
  
  output$classification_confusion_matrix_overall <- renderPrint({
    if(!is.null(classification_result$confusion_matrix)) print(classification_result$confusion_matrix[3])
  }, width = 180
  )
  
  output$classification_confusion_matrix_byclass <- renderPrint({
    if(!is.null(classification_result$confusion_matrix)) print(classification_result$confusion_matrix[4])
  }, width = 180
  )
  
  output$classification_confusion_matrix_plot <- renderPlot({
    if(!is.null(classification_result$confusion_matrix)) print(classification_result$confusion_matrix[1])
  })
  
  output$classification_confusion_matrix_byclass_plot <- renderPlot({
    if(!is.null(classification_result$confusion_matrix)) print(classification_result$confusion_matrix[5])
  })
  
  dataset_result1 <- reactiveValues(dataset1 = NULL, numeric_list = NULL, non_numeric_list = NULL, summary_numeric = NULL, summary_non_numeric = NULL, plot = NULL, plot_non_numeric = NULL, plot_height = NULL)
  
  output$dataset_parameter_panel1 <- renderUI({
    switch(input$dataset_type1,
           "Build-in Dataset" = list(
             fluidRow(
               column(4, uiOutput("dataset_list1"))
             ),
             fluidRow(
               column(11),
               column(1, actionButton("select_dataset1", label = "Select"))
             )
           ),
           "Upload CSV" = list(
             fluidRow(
               column(3, fileInput("csv", label = h4("Choose CSV File"), accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
               column(3, radioButtons("header", label = h4("Header"), c(True = TRUE, False = FALSE))),
               column(3, radioButtons("sep", label = h4("Separator"), c(Comma=",", Semicolon=";", Tab="\t"), ",")),
               column(3, radioButtons("quote", label = h4("Quote"), c(None="", "Double Quote"="\"", "Single Quote"="'"), "\""))
             ),
             fluidRow(
               column(11),
               column(1, actionButton("select_dataset1", label = "Select"))
             )
           )
    )
  })
  
  output$dataset_list1 <- renderUI({
    withProgress(min=1, max=20, expr={
      for(i in 1:20) {
        setProgress(message = "Page loading.", detail = "This may take a while...", value=i)
        print(i)
        Sys.sleep(0.05)
      }
    })
    dataset_list1 <- getDatasetList()
    selectInput("dataset1", label = h4("Choose Dataset"), dataset_list1, selected = "201801.csv")
  })
  
  observeEvent(input$select_dataset1, {
    withProgress(min=1, max=20, expr={
      for(i in 1:20) {
        setProgress(message = "Processing.", detail = "This may take a while...", value=i)
        print(i)
        Sys.sleep(0.1)
      }
    })
    dataset_result1$dataset1 <- switch(input$dataset_type1,
                                       "Build-in Dataset" = readRDS(input$dataset1),
                                       "Upload CSV" = read.table(input$csv$datapath, header = if(input$header == "TRUE") TRUE else FALSE, sep = input$sep , quote = input$quote)
    )
    if(!is.null(dataset_result1$dataset1)) {
      dataset_result1$numeric_list <- unlist(getColumnsType(dataset_result1$dataset1)[1])
      dataset_result1$non_numeric_list <- unlist(getColumnsType(dataset_result1$dataset1)[2])
      dataset_result1$summary_numeric <- dataSummary_numreic(dataset_result1$dataset1)
      dataset_result1$summary_non_numeric <- dataSummary_non_numeric(dataset_result1$dataset1)
      if(!is.null(dataset_result1$summary_numeric)) {
        dataset_result1$plot_non_numeric <- if(input$dataset_type1 == "Upload CSV") FALSE else TRUE
        dataset_result1$plot <- plotPairs(dataset_result1$dataset1, non_numeric = dataset_result1$plot_non_numeric)
        dataset_result1$plot_height <- if(length(names(dataset_result1$dataset1)) <= 8) paste(length(names(dataset_result1$dataset1)) * 200, "px", sep = "") else "1600px"
      }
    }
  })
  
  output$dataset_result_panel1 <- renderUI({
    if(!is.null(dataset_result1$summary_numeric)) {
      tabsetPanel(
        tabPanel("Summary",
                 fluidRow(
                   column(6,
                          fluidRow(column(12, h4("Numeric summary"))),
                          fluidRow(column(12, tableOutput("dataset_summary_numeric1")))
                   ),
                   column(9,
                          fluidRow(column(12, h4("Correlation Matrix"))),
                          fluidRow(column(12, tableOutput("correlation_matrix1")))
                   )
                 ),
                 fluidRow(
                   column(6,
                          fluidRow(column(12, h4("Non-Numeric Summary"))),
                          if(length(dataset_result1$summary_non_numeric) == 1) {
                            fluidRow(column(12, tableOutput("dataset_summary_non_numeric1")))
                          }else {
                            fluidRow(verbatimTextOutput("dataset_summary_non_numerics1"))
                          }
                   )
                 )
        ),
        tabPanel("Plot",
                 uiOutput("dataset_plot_parameter_panel1"),
                 fluidRow(column(12, plotOutput("dataset_plot1", height = dataset_result1$plot_height)))
        ),
        tabPanel("Table",
                 fluidRow(column(12, dataTableOutput("dataset_datatable1")))
        )
      )
    }else if(!is.null(dataset_result1$dataset1)){
      tabsetPanel(
        tabPanel("Table",
                 fluidRow(column(12, h4("This dataset may not be used to clustering and classification."))),
                 fluidRow(column(12, tableOutput("dataset_table1")))
        )
      )
    }
  })
  
  output$dataset_summary_numeric1 <- renderTable({
    if(!is.null(dataset_result1$summary_numeric)) print(as.data.frame(dataset_result1$summary_numeric[1]))
  })
  
  output$correlation_matrix1 <- renderTable({
    if(!is.null(dataset_result1$summary_numeric)) print(as.data.frame(dataset_result1$summary_numeric[2]))
  })
  
  output$dataset_summary_non_numeric1 <- renderTable({
    if(!is.null(dataset_result1$summary_non_numeric)) print(as.data.frame(dataset_result1$summary_non_numeric))
  })
  
  output$dataset_summary_non_numerics1 <- renderPrint({
    for( table in dataset_result1$summary_non_numeric) {
      print(as.data.frame(table))
    }
  })
  
  output$dataset_plot_parameter_panel1 <- renderUI({
    if(!is.null(dataset_result1$summary_non_numeric) && dataset_result1$plot_non_numeric) {
      list(
        fluidRow(
          column(4, selectInput("dataset_plot_class1", label = h4("Choose Class Attribute"), c("None", unlist(getColumnsType(dataset_result1$dataset1)[2]))))
        ),
        fluidRow(
          column(11),
          column(1, actionButton("dataset_plot1", label = "Plot"))
        )
      )
    }
  })
  
  observeEvent(input$dataset_plot1, {
    if(!is.null(dataset_result1$summary_numeric)) {
      class <- if(input$dataset_plot_class1 == "None") NULL else input$dataset_plot_class1
      dataset_result1$plot <- plotPairs(dataset_result1$dataset1, non_numeric = dataset_result1$plot_non_numeric)
    }
  })
  
  output$dataset_plot1 <- renderPlot({
    if(!is.null(dataset_result1$plot)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Creating Plot.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.1)
        }
      })
      print(dataset_result1$plot)
    }
  })
  
  output$dataset_datatable1 <- renderDataTable({
    withProgress(min=1, max=20, expr={
      for(i in 1:20) {
        setProgress(message = "Creating Table.", detail = "This may take a while...", value=i)
        print(i)
        Sys.sleep(0.1)
      }
    })
    dataset_result1$dataset1[, drop =FALSE]
  }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10)
  )
  
  output$dataset_table1 <- renderTable({
    dataset_result1$dataset1
  })
  
  statistics_result1 <- reactiveValues(result1 = NULL, reg_model1 = NULL)
  
  output$statistics_variable_panel1 <- renderUI({
    if(!is.null(dataset_result1$summary_numeric)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Page Loading.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.05)
        }
      })
      switch(input$statistics_method1,
             "Regression" = list(
               column(4, selectInput("statistics_reg_response1", label = h4("Choose Response Variable"), dataset_result1$numeric_list))
             ),
             "Paired T Test" = list(
               column(4, selectInput("statistics_ttest_var11", label = h4("Choose Firset Variable"), dataset_result1$numeric_list)),
               column(4, selectInput("statistics_ttest_var21", label = h4("Choose Second Variable"), dataset_result1$numeric_list))
             ),
             "One-way ANOVA" = list(
               column(4, selectInput("statistics_oneway_anova_group1", label = h4("Choose Group Variable"), dataset_result1$non_numeric_list)),
               column(4, selectInput("statistics_oneway_anova_response1", label = h4("Choose Response Variable"), dataset_result1$numeric_list))
             ),
             "MANOVA" = list(
               column(4, selectInput("statistics_manova_groups1", label = h4("Choose Group Variable"), dataset_result1$non_numeric_list, multiple = TRUE)),
               column(4, selectInput("statistics_manova_responses1", label = h4("Choose Response Variable"), dataset_result1$numeric_list, multiple = TRUE))
             )
      )
    }
  })
  
  output$statistics_parameter_panel1 <- renderUI({
    if(!is.null(dataset_result1$dataset1)) {
      fluidRow(
        column(11),
        column(1, actionButton("start_statistics1", label = "Start"))
      )
    }
  })
  
  observeEvent(input$start_statistics1, {
    statistics_result1$result1 <- switch(input$statistics_method1,
                                         "Regression" = statisticsRegression(dataset_result1$dataset1, input$statistics_reg_response1),
                                         "Paired T Test" = statisticsTtest(dataset_result1$dataset1, input$statistics_ttest_var11, input$statistics_ttest_var21),
                                         "One-way ANOVA" = statisticsOnewayANOVA(dataset_result1$dataset1, input$statistics_oneway_anova_group1, input$statistics_oneway_anova_response1),
                                         "MANOVA" = statisticsMANOVA(dataset_result1$dataset1, input$statistics_manova_groups1, input$statistics_manova_responses1)
    )
  })
  
  output$statistics_result_panel1 <- renderUI({
    if(!is.null(statistics_result1$result1)) {
      switch(input$statistics_method1,
             "Regression" = {
               tabsetPanel(
                 tabPanel("Detail",
                          fluidRow(column(12, verbatimTextOutput("statistics_result_detail_11")))
                 ),
                 tabPanel("Step",
                          fluidRow(column(12, verbatimTextOutput("statistics_result_detail_21")))
                 ),
                 tabPanel("Outlier Test",
                          fluidRow(column(12, verbatimTextOutput("statistics_result_detail_31")))
                 )
               )
             },
             "Paired T Test" = {
               tabsetPanel(
                 tabPanel("Detail",
                          fluidRow(column(12, verbatimTextOutput("statistics_result_detail_11")))
                 )
               )
             },
             "One-way ANOVA" = {
               tabsetPanel(
                 tabPanel("Detail",
                          fluidRow(column(12, verbatimTextOutput("statistics_result_detail_11")))
                 ),
                 tabPanel("Tukey",
                          fluidRow(column(12, verbatimTextOutput("statistics_result_detail_21")))
                 )
               )
             },
             "MANOVA" = {
               tabsetPanel(
                 tabPanel("Detail",
                          fluidRow(column(12, verbatimTextOutput("statistics_result_detail_11")))
                 )
               )
             }
      )
    }
  })
  
  output$statistics_result_detail_11 <- renderPrint({
    if(!is.null(statistics_result1$result1)) {
      switch(input$statistics_method1,
             "Regression" = print(summary(statistics_result1$result1)),
             "Paired T Test" = print(statistics_result1$result1),
             "One-way ANOVA" = print(summary(statistics_result1$result1)),
             "MANOVA" = {
               print(statistics_result1$result1)
               print(summary(statistics_result1$result1))
             }
      )
    }
  })
  
  output$statistics_result_detail_21 <- renderPrint({
    if(!is.null(statistics_result1$result1)) {
      switch(input$statistics_method1, 
             "Regression" = print(statisticsRegression_step(statistics_result1$result1)),
             "One-way ANOVA" = print(statisticsOnewayANOVA_tukey(statistics_result1$result1))
      )
    }
  })
  
  output$statistics_result_detail_31 <- renderPrint({
    if(!is.null(statistics_result1$result1)) {
      switch(input$statistics_method1,
             "Regression" = print(statisticsRegression_outlier(statistics_result1$result1))
      )
    }
  })
  
  clustering_result1 <- reactiveValues(result = NULL, result_data_frame = NULL, plot = NULL, plot_height = NULL, manova = NULL)
  
  output$clustering_parameters_panel1 <- renderUI({
    if(!is.null(dataset_result1$summary_numeric)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Page Loading.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.05)
        }
      })
      switch(input$clustering_method1,
             "K-Means" = list(
               fluidRow(
                 column(4, sliderInput("kmeans_k1", label = h4("Set K"), min = 2, max = 10, value = 2))
               ),
               fluidRow(
                 column(11),
                 column(1, actionButton("start_clustering1", label = "Clustering"))
               )
             ),
             "EM" = list(
               fluidRow(
                 column(11),
                 column(1, actionButton("start_clustering1", label = "Clustering"))
               )
             ),
             "DBSCAN" = list(
               fluidRow(
                 column(4, sliderInput("dbscan_eps1", label = h4("Set Eps"), min = 0.1, max = 1, value = 0.5)),
                 column(4, sliderInput("dbscan_pts1", label = h4("Set MinPts"), min = 2, max = 20, value = 10))
               ),
               fluidRow(
                 column(11),
                 column(1, actionButton("start_clustering1", label = "Clustering"))
               )
             ),
             "Spectral" = list(
               fluidRow(
                 column(4, sliderInput("spectral_centers1", label = h4("Set Centers"), min = 0, max = 10, value = 0)),
                 column(4, sliderInput("spectral_nn1", label = h4("Set NN"), min = 2, max = 20, value = 7))	
               ),
               fluidRow(
                 column(11),
                 column(1, actionButton("start_clustering1", label = "Clustering"))
               )
             )
      )
      
    }
  })
  
  observeEvent(input$start_clustering1, {
    clustering_result1$result <- switch(input$clustering_method1,
                                        "K-Means" = clusteringKmeans(dataset_result1$dataset1, input$kmeans_k1),
                                        "EM" = clusteringEM(dataset_result1$dataset1),
                                        "DBSCAN" = clusteringDBSCAN(dataset_result1$dataset1, input$dbscan_eps1, input$dbscan_pts1),
                                        "Spectral" = clusteringSpectral(dataset_result1$dataset1, input$spectral_centers1, input$spectral_nn1)
    )
    clustering_result1$result_data_frame <- as.data.frame(clustering_result1$result[1][1])
    clustering_result1$manova <- statisticsMANOVA(clustering_result1$result_data_frame, names(clustering_result1$result_data_frame)[length(names(clustering_result1$result_data_frame))], names(clustering_result1$result_data_frame)[1:length(names(clustering_result1$result_data_frame)) - 1])
    clustering_result1$plot <- plotClusteringResult(clustering_result1$result_data_frame)
    clustering_result1$plot_height <- if(length(names(clustering_result1$result_data_frame)) <= 8) paste(length(names(clustering_result1$result_data_frame)) * 200, "px", sep = "") else "1600px"
  })
  
  output$clustering_result_panel1 <-renderUI({
    if(!is.null(clustering_result1$result)) {
      tabsetPanel(
        tabPanel("Detail",
                 fluidRow(column(12, verbatimTextOutput("clustering_result1")))
        ),
        tabPanel("Plot",
                 fluidRow(column(12, plotOutput("clustering_result_plot1", height = clustering_result1$plot_height)))
        ),
        tabPanel("MANOVA",
                 fluidRow(column(12, verbatimTextOutput("clustering_manova1")))
        ),
        tabPanel("Table",
                 fluidRow(column(12, dataTableOutput("clustering_result_table1")))
        )
      )
    }
  })
  
  output$clustering_result1 <- renderPrint({
    if(!is.null(clustering_result1$result)) {
      print(clustering_result1$result[2])
    }
  }, width = 180
  )
  
  output$clustering_result_plot1 <- renderPlot({
    if(!is.null(clustering_result1$result)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Creating Plot.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.1)
        }
      })
      print(clustering_result1$plot)
    }
  })
  
  output$clustering_manova1 <- renderPrint({
    if(!is.null(clustering_result1$result)) {
      print(clustering_result1$manova)
      print(summary(clustering_result1$manova))
    }
  })
  
  output$clustering_result_table1 <- renderDataTable({
    if(!is.null(clustering_result1$result)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Creating Table.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.1)
        }
      })
      clustering_result1$result_data_frame[, drop =FALSE]
    }
  }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10)
  )
  
  
  classification_result1 <- reactiveValues(
    dataset = NULL,
    training_dataset = NULL,
    testing_dataset = NULL,
    plot = NULL,
    plot_height = NULL,
    model = NULL,
    confusion_matrix = NULL
  )
  
  output$class_attribute_panel1 <- renderUI({
    withProgress(min=1, max=20, expr={
      for(i in 1:20) {
        setProgress(message = "Page Loading.", detail = "This may take a while...", value=i)
        print(i)
        Sys.sleep(0.05)
      }
    })
    if(!is.null(dataset_result1$non_numeric_list)) {
      column(4, selectInput("class_attribute1", label = h4("Choose Class Attribute"), dataset_result1$non_numeric_list))
    }else {
      column(4, h4("No any class attribute exist."))
    }
  })
  
  output$classification_parameters_panel1 <- renderUI({
    if(!is.null(dataset_result1$non_numeric_list)) {
      switch(input$classification_method1,
             "Decision Tree" = list(
               fluidRow(
                 column(11),
                 column(1, actionButton("start_classify1", label = "Classify"))
               )
             ),
             "Random Forest" = list(
               fluidRow(
                 column(11),
                 column(1, actionButton("start_classify1", label = "Classify"))
               )
             ),
             "K-Nearest Neighbors" = list(
               fluidRow(
                 column(4, sliderInput("knn_k1", label = h4("Set K"), min = 2, max = 20, value = 7)),
                 column(4, sliderInput("knn_distance1", label = h4("Set Distance"), min = 0, max = 5, value = 2))
               ),
               fluidRow(
                 column(11),
                 column(1, actionButton("start_classify1", label = "Classify"))
               )
             ),
             "Support Vector Machine" = list(
               fluidRow(
                 column(11),
                 column(1, actionButton("start_classify1", label = "Classify"))
               )
             ),
             "Naive Bayes Classifier" = list(
               fluidRow(
                 column(11),
                 column(1, actionButton("start_classify1", label = "Classify"))
               )
             ),
             "Feed-Forward Neural Network" = list(
               fluidRow(
                 column(4, sliderInput("nn_size1", label = h5("Set Number of Units in the Hidden Layer"), min = 0, max = 20, value = 10))
               ),
               fluidRow(
                 column(11),
                 column(1, actionButton("start_classify1", label = "Classify"))
               )
             )
      )
    }
  })
  
  observeEvent(input$start_classify1, {
    classification_result1$dataset <- dataPreprocess_Classification(dataset_result1$dataset1)
    classification_result1$training_dataset <- as.data.frame(classification_result1$dataset[1])
    classification_result1$testing_dataset <- as.data.frame(classification_result1$dataset[2])
    switch(input$classification_method1,
           "Decision Tree" = {
             classification_result1$model <- classificationDecisionTree(classification_result1$training_dataset, class = input$class_attribute1)
             classification_result1$testing_dataset <- as.data.frame(evaluationDecissionTree(classification_result1$model, classification_result1$testing_dataset))
           },
           "Random Forest" = {
             classification_result1$model <- classificationRandomForest(classification_result1$training_dataset, class = input$class_attribute1)
             classification_result1$testing_dataset <- as.data.frame(evaluationRandomForest(classification_result1$model, classification_result1$testing_dataset))
           },
           "K-Nearest Neighbors" = {
             classification_result1$model <- classificationKNN(classification_result1$training_dataset, classification_result1$testing_dataset, class = input$class_attribute1, k = input$knn_k1, distance = input$knn_distance1)
             classification_result1$testing_dataset$Predict_result <- fitted(classification_result1$model)
           },
           "Support Vector Machine" = {
             classification_result1$model <- classificationSVM(classification_result1$training_dataset, class = input$class_attribute1)
             classification_result1$testing_dataset <- as.data.frame(evaluationSVM(classification_result1$model, classification_result1$testing_dataset))
           },
           "Naive Bayes Classifier" = {
             classification_result1$model <- classificationNaiveBayes(classification_result1$training_dataset, class = input$class_attribute1)
             classification_result1$testing_dataset <- as.data.frame(evaluationNaiveBayes(classification_result1$model, classification_result1$testing_dataset))
           },
           "Feed-Forward Neural Network" = {
             classification_result1$model <- classificationNN(classification_result1$training_dataset, class = input$class_attribute1, input$nn_size1)
             classification_result1$testing_dataset <- as.data.frame(evaluationNN(classification_result1$model, classification_result1$testing_dataset))
           }
    )
    classification_result1$plot <- plotClassificationResult(classification_result1$testing_dataset)
    classification_result1$plot_height <- if(length(names(classification_result1$testing_dataset)) <= 8) paste(length(names(classification_result1$testing_dataset)) * 200, "px", sep = "") else "1600px"
    classification_result1$confusion_matrix <- evaluationConfusionMatrix(classification_result1$testing_dataset[, length(names(classification_result1$testing_dataset))], classification_result1$testing_dataset[, length(names(classification_result1$testing_dataset)) - 1])
  })
  
  output$classification_result_panel1 <- renderUI({
    if(!is.null(classification_result1$dataset)) {
      tabsetPanel(
        tabPanel("Model",
                 fluidRow(column(12, verbatimTextOutput("classification_model_summary1"))),
                 fluidRow(column(12, plotOutput("classification_model_plot1", height = "800px")))
        ),
        tabPanel("Plot",
                 fluidRow(column(12, plotOutput("classification_result_plot1", height = classification_result1$plot_height)))
        ),
        tabPanel("Confusion Matrix",
                 fluidRow(
                   column(12,
                          fluidRow(column(12, h4("Summary"))),
                          fluidRow(
                            column(6, verbatimTextOutput("classification_confusion_matrix1")),
                            column(6, plotOutput("classification_confusion_matrix_plot1", height = "170px"))
                          )
                   )
                 ),
                 fluidRow(
                   column(12,
                          fluidRow(column(12, h4("Overall"))),
                          fluidRow(column(12, verbatimTextOutput("classification_confusion_matrix_overall1")))
                   )
                 ),
                 fluidRow(
                   column(12,
                          fluidRow(column(12, h4("By Class"))),
                          fluidRow(column(12, plotOutput("classification_confusion_matrix_byclass_plot1", height = "100px")))
                   )
                 )
        ),
        tabPanel("Table",
                 fluidRow(column(12, dataTableOutput("classification_result_table1")))
        )
      )
    }
  })
  
  output$classification_model_summary1 <- renderPrint({
    if(!is.null(classification_result1$model)) {
      switch(input$classification_method1,
             "Decision Tree" = print(summary(classification_result1$model)),
             "Random Forest" = print(summary(classification_result1$model)),
             "EM" = print(classification_result1$model),
             "K-Nearest Neighbors" = print(classification_result1$model),
             "Support Vector Machine" = print(summary(classification_result1$model)),
             "Naive Bayes Classifier" = print(classification_result1$model),
             "Feed-Forward Neural Network" = print(classification_result1$model)
      )
    }
  })
  
  output$classification_model_plot1 <- renderPlot({
    if(!is.null(classification_result1$model)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Creating Model.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.1)
        }
      })
      switch(input$classification_method1,
             "Decision Tree" = {
               plot(classification_result1$model)
               text(classification_result1$model)
             }
      )
    }
  })
  
  output$classification_result_plot1 <- renderPlot({
    if(!is.null(classification_result1$plot)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Creating Plot.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.1)
        }
      })
      print(classification_result1$plot)
    }
  })
  
  output$classification_result_table1 <- renderDataTable({
    if(!is.null(classification_result1$testing_dataset)) {
      withProgress(min=1, max=20, expr={
        for(i in 1:20) {
          setProgress(message = "Creating Datatable.", detail = "This may take a while...", value=i)
          print(i)
          Sys.sleep(0.1)
        }
      })
      classification_result1$testing_dataset[, drop =FALSE]
    }
  }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10)
  )
  
  output$classification_confusion_matrix1 <- renderPrint({
    if(!is.null(classification_result1$confusion_matrix)) print(classification_result1$confusion_matrix[2])
  })
  
  output$classification_confusion_matrix_overall1 <- renderPrint({
    if(!is.null(classification_result1$confusion_matrix)) print(classification_result1$confusion_matrix[3])
  }, width = 180
  )
  
  output$classification_confusion_matrix_byclass1 <- renderPrint({
    if(!is.null(classification_result1$confusion_matrix)) print(classification_result1$confusion_matrix[4])
  }, width = 180
  )
  
  output$classification_confusion_matrix_plot1 <- renderPlot({
    if(!is.null(classification_result1$confusion_matrix)) print(classification_result1$confusion_matrix[1])
  })
  
  output$classification_confusion_matrix_byclass_plot1 <- renderPlot({
    if(!is.null(classification_result1$confusion_matrix)) print(classification_result1$confusion_matrix[5])
  })
  
})