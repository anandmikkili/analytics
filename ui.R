library(shiny)
library(shinydashboard)
shinyUI(
  dashboardPage(
    dashboardHeader(title = "6D Telecom Solutions"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Churn Reporting", tabName = "churnreporting",icon = icon("database", lib="font-awesome"), startExpanded = TRUE, menuItem("Train Summary Reports", tabName = "trainsummaryReport",startExpanded = TRUE,
                                                                                                                                           menuItem("Prediction Summary",tabName = "trainOv"),menuItem("ARPU Status",tabName = "trainArpu", startExpanded = TRUE,menuSubItem("High",tabName = "trainOH"),menuSubItem("Medium",tabName = "trainOM"),menuSubItem("Low",tabName = "trainOL")) ,menuItem("CPI Score",tabName = "traincpiScore"),menuItem("Age On Network",tabName = "trainaon"),menuItem("Tariff",tabName = "traintariff"),menuItem("Region",tabName = "trainregion"),menuItem("Value Segment",tabName = "trainvaluesegment")),
                 menuItem("Test Summary Reports", tabName = "testsummaryReport", startExpanded = TRUE,
                          menuItem("Prediction Status",tabName = "testpredstatus"),menuItem("CPI Score",tabName = "testcpiScore"),menuItem("Age On Network",tabName = "testaon"),menuItem("Tariff",tabName = "testtariff"),menuItem("Region",tabName = "testregion"),menuItem("Value Segment",tabName = "testvaluesegment")),
                 menuItem("Detailed Reports", tabName = "detailedReport",startExpanded = TRUE,
                          menuItem("A",tabName = "DA"),menuItem("B",tabName = "DB")),
                 menuItem("Churn Reason Reports",tabName = "churnreasonReport",startExpanded = TRUE,
                          menuItem("A",tabName = "CA"),menuItem("B",tabName = "CB")),
                 menuItem("Churn Report Files",tabName = "churnReportFiles",startExpanded = TRUE,
                          menuItem("Down Load Reports",tabName = "downReports"),menuItem("Upload Reports",tabName = "uploadReports"))),
        menuItem("CAR",tabName = "car",icon = icon("heartbeat", lib="font-awesome"), startExpanded = TRUE,menuItem("Train Summary Reports", tabName = "a",startExpanded = TRUE,
                                                                                                                   menuItem("Prediction Summary",tabName = "b"),menuItem("ARPU Status",tabName = "c", startExpanded = TRUE,menuSubItem("High",tabName = "d"),menuSubItem("Medium",tabName = "e"),menuSubItem("Low",tabName = "f")) ,menuItem("CPI Score",tabName = "g"),menuItem("Age On Network",tabName = "h"),menuItem("Tariff",tabName = "j"),menuItem("Region",tabName = "k"),menuItem("Value Segment",tabName = "l"))),
        menuItem("Data Analytics",tabName = "dataanalytics",icon = icon("eye", lib="font-awesome"),startExpanded = TRUE,menuItem("Train Summary Reports", tabName = "m",startExpanded = TRUE,
                                                                                                                                 menuItem("Prediction Summary",tabName = "n"),menuItem("ARPU Status",tabName = "o", startExpanded = TRUE,menuSubItem("High",tabName = "p"),menuSubItem("Medium",tabName = "q"),menuSubItem("Low",tabName = "r")) ,menuItem("CPI Score",tabName = "s"),menuItem("Age On Network",tabName = "t"),menuItem("Tariff",tabName = "u"),menuItem("Region",tabName = "v"),menuItem("Value Segment",tabName = "w")))
        
      )),
    dashboardBody(
      tabItems(
        tabItem("trainOv", fluidPage(
          fluidRow(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainov")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotlyOutput("plot_trainov", height = "200px"),selectInput("select", "Select", label = h2("Selection Pane"),choices = list(TotalTrainingSet = "TotalTrainingSet", High = "High",Medium="Medium",Low="Low"))),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainov"))
          )
        ))),
        tabItem("trainOH", fluidPage(
          fluidRow(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainOH")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotlyOutput("plot_trainOH", height = "200px"),selectInput("select", "Select", label = h2("Selection Pane"),choices = list(PredictedNonUser = "PredictedNonUser", PredictedUser = "PredictedUser",ActualChurners="ActualChurners",GrandTotal="GrandTotal"))),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainOH"))
          )
        ))),
      tabItem("trainOM", fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainOM")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotlyOutput("plot_trainOM", height = "200px"),selectInput("select", "Select", label = h2("Selection Pane"),choices = list(PredictedNonUser = "PredictedNonUser", PredictedUser = "PredictedUser",ActualChurners="ActualChurners",GrandTotal="GrandTotal"))),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainOM"))
          )
        )),
      tabItem("trainOL", fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainOL")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotlyOutput("plot_trainOL", height = "200px"),selectInput("select", "Select", label = h2("Selection Pane"),choices = list(PredictedNonUser = "PredictedNonUser", PredictedUser = "PredictedUser",ActualChurners="ActualChurners",GrandTotal="GrandTotal"))),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainOL"))
          )
        )),
      tabItem("traincpiScore", fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainCPI")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotlyOutput("plot_trainCPI", height = "200px"),selectInput("select", "Select", label = h2("Selection Pane"),choices = list(ModelPredictedNonUsercount="ModelPredictedNonUsercount",ActualChurners="ActualChurners",ModelPostValidationAccuracy="ModelPostValidationAccuracy"))),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainCPI"))
          )
        )),
      tabItem("trainaon", fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainAON")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_trainAON")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainAON"))
          )
        )),
      tabItem("traintariff", fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainTrf")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_trainTrf")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainTrf"))
          )
        )),
      tabItem("trainregion", fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainRgn")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_trainRgn")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainRgn"))
          )
        )),
      tabItem("trainvaluesegment", fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainValue")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_trainValue")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainValue"))
          )
        )),
      tabItem("testpredstatus", fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_testStatus")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_testStatus")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_testStatus"))
          )
        )),
      tabItem("testcpiScore", fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_testCPI")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_testCPI")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_testCPI"))
          )
        )),
      tabItem("testaon", fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_testAON")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_testAON")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_testAON"))
          )
        )),
      tabItem("testtariff", fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_testTrf")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_testTrf")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_testTrf"))
          )
        )),
      tabItem("testregion", fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_testRgn")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_testRgn")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_testRgn"))
          )
        )),
        
      tabItem("testvaluesegment", fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"),tableOutput("table_testValue")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_testValue")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_testValue"))
          )
        )),
      tabItem("downReports", fluidRow(
        column(width = 4,
               box(title="Prediction Period",width = NULL, status = "primary", solidHeader = TRUE,
                   selectInput("reportperiod", "",
                               choices = c(
                                 "201801" = 201801,
                                 "201712" = 201712,
                                 "201711" = 201711,
                                 "201710" = 201711
                               )
                   ))
        ),
        column(width = 4,
               box(title="Report Type",width = NULL, status = "primary", solidHeader = TRUE,
                   selectInput("reporttype", "",
                               choices = c(
                                 "Prediction Summary","Churn Reason"
                               )
                   ))
        )
      ))
      )
      
    )
  )
)