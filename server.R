library(shiny)
library(shinydashboard)
library(data.table)
library(sqldf)
library(plotly)
library(xlsx)
library(C50)
source("D:\\D\\Rscript\\Reporting_GUI\\TrainRelatedDetails.R")
source("D:\\D\\Rscript\\Reporting_GUI\\testRelatedDetails.R")
source("D:\\D\\Rscript\\Reporting_GUI\\ReportingExcel.R")
shinyServer(function(input, output){
  fileinput <- fread("D:\\D\\Rscript\\Reporting_GUI\\FULL_REQ.txt")
  treeinput <- fread("D:\\D\\Rscript\\Reporting_GUI\\Train_Sample.csv")
  treeinput$CHURN_STATUS<-as.factor(treeinput$CHURN_STATUS)
  
  
  #TrainDataCall
  output$table_trainps<-renderTable({
    dataset<-TrainPredictionStatusCall(fileinput)
    dataset
  })
  
  output$trainSummary<-renderTable({
    segment<-input$segments
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
    str(dataset)
  })
  
  output$trainSummarytext<-renderPrint({
    segment<-input$segments
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
    str(dataset)
  })
  
  output$text_trainTrf<-renderPrint({
    dataset<-TrainTariffPlan(fileinput)
    str(dataset)
  })
  
  output$text_trainRgn<-renderPrint({
    dataset<-TrainRegion(fileinput)
    str(dataset)
  })
  
  output$text_trainValue<-renderPrint({
    dataset<-TrainValueSegmentation(fileinput)
    str(dataset)
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
    str(dataset)
  })
  output$text_treeinputTrf<-renderPrint({
    dataset<-treeinputTariffPlan(fileinput)
    str(dataset)
  })
  output$text_treeinputRgn<-renderPrint({
    dataset<-treeinputRegion(fileinput)
    str(dataset)
  })
  output$text_treeinputValue<-renderPrint({
    dataset<-treeinputValueSegmentation(fileinput)
    dataset
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
  })
  
})