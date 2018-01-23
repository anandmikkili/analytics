library(shiny)
library(shinydashboard)
library(data.table)
library(sqldf)
library(plotly)
library(xlsx)
source("D:\\D\\Rscript\\Reporting_GUI\\TrainRelatedDetails.R")
source("D:\\D\\Rscript\\Reporting_GUI\\TestRelatedDetails.R")
source("D:\\D\\Rscript\\Reporting_GUI\\ReportingExcel.R")
shinyServer(function(input, output){
  fileinput <- fread("D:\\D\\Rscript\\Reporting_GUI\\FULL_REQ.txt")
  
  #TrainDataCall
  output$table_trainps<-renderTable({
    dataset<-TrainPredictionStatusCall(fileinput)
    dataset
  })
  
  output$table_trainov<-renderTable({
    dataset<-TrainModelPredictionSummaryOverall(fileinput)
    dataset
  })
  
  output$table_trainOH<-renderTable({
   dataset<-TrainModelPredictionSummaryHigh(fileinput)
   dataset
})
  
  output$table_trainOM<-renderTable({
    dataset<-TrainModelPredictionSummaryMedium(fileinput)
    dataset
  })
  output$table_trainOL<-renderTable({
    dataset<-TrainModelPredictionSummaryLow(fileinput)
    dataset
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

  output$text_trainov<-renderPrint({
    dataset<-TrainModelPredictionSummaryOverall(fileinput)
    str(dataset)
  })
  
  output$text_trainOH<-renderPrint({
    dataset<-TrainModelPredictionSummaryHigh(fileinput)
    str(dataset)
  })
  output$text_trainOM<-renderPrint({
    dataset<-TrainModelPredictionSummaryMedium(fileinput)
    str(dataset)
  })
  output$text_trainOL<-renderPrint({
    dataset<-TrainModelPredictionSummaryLow(fileinput)
    str(dataset)
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
  
  
  #TestDataCall
  output$table_testStatus<-renderTable({
    dataset<-TestPredictionStatusCall(fileinput)
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
  
  output$text_testStatus<-renderPrint({
    dataset<-TestPredictionStatusCall(fileinput)
    dataset
  })
  
  output$text_testCPI<-renderPrint({
    dataset<-TestCPIScore(fileinput)
    str(dataset)
  })
  output$text_testTrf<-renderPrint({
    dataset<-TestTariffPlan(fileinput)
    str(dataset)
  })
  output$text_testRgn<-renderPrint({
    dataset<-TestRegion(fileinput)
    str(dataset)
  })
  output$text_testValue<-renderPrint({
    dataset<-TestValueSegmentation(fileinput)
    dataset
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
  
})