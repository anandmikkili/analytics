library(shiny)
library(shinydashboard)
shinyUI(
  dashboardPage(
    dashboardHeader(title = "6D Telecom Solutions"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Churn Reporting", tabName = "churnreporting",icon = icon("database", lib="font-awesome"), startExpanded = TRUE, menuItem("Train Summary Reports", tabName = "trainsummaryReport",startExpanded = TRUE,
                                                                                                                                           menuItem("Prediction Summary",tabName = "trainPS"),
                                                                                                                                           menuItem("ARPU Status",tabName = "trainArpu"),
                                                                                                                                           menuItem("CPI Score",tabName = "traincpiScore"),
                                                                                                                                           menuItem("Age On Network",tabName = "trainaon"),
                                                                                                                                           menuItem("Tariff",tabName = "traintariff"),
                                                                                                                                           menuItem("Region",tabName = "trainregion"),
                                                                                                                                           menuItem("Value Segment",tabName = "trainvaluesegment")),
                 menuItem("Test Summary Reports", tabName = "testsummaryReport", startExpanded = TRUE,
                          menuItem("Prediction Status",tabName = "testpredstatus"),menuItem("CPI Score",tabName = "testcpiScore"),menuItem("Age On Network",tabName = "testaon"),menuItem("Tariff",tabName = "testtariff"),menuItem("Region",tabName = "testregion"),menuItem("Value Segment",tabName = "testvaluesegment")),
                  menuItem("Detailed Reports", tabName = "detailedReport",startExpanded = TRUE),
                 menuItem("Churn Reason Reports",tabName = "churnreasonReport",startExpanded = TRUE,
                          menuItem("Churn Reason Segments",tabName = "CA"),menuItem("Visualize Churn Reason",tabName = "CB")),
                 menuItem("Churn Report Files",tabName = "churnReportFiles",startExpanded = TRUE,
                          menuItem("Down Load Reports",tabName = "downReports"),menuItem("Upload Reports",tabName = "uploadReports")),
                 menuItem("Algorithm Details", tabName = "algodetails",startExpanded = TRUE,
                          menuItem("Decision Tree", tabName = "decisiontree"),menuItem("Modelling", tabName = "modelling"))),
        menuItem("CAR",tabName = "car",icon = icon("heartbeat", lib="font-awesome"), startExpanded = TRUE,
                 menuItem("Consumer Data Details",tabName = "consumer_data_details"),menuItem("Consumer Behavior Pattern")),
        menuItem("Data Analytics",tabName = "dataanalytics",icon = icon("eye", lib="font-awesome"),startExpanded = TRUE,
                 menuItem("Study of Churn Prediction ",tabName = "study_of_churn"),
                 menuItem("Data Preprocessing ",tabName = "data_preprocessing"),
                 menuItem("Statistical Analysis ",tabName = "statistical_analysis"),
                 menuItem("Clustering ",tabName = "clustering_process"),
                 menuItem("Classification ",tabName = "classification_process"),
                 menuItem("Future Estimation",tabName = "future_estimation"))
        
      )),
    dashboardBody(
      tabItems(
        tabItem("trainPS", h2("PREDICTION SUMMARY"),fluidPage(
          fluidRow(
            tabsetPanel(
              tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainps")),
              tabPanel(title="Plot",icon = icon("bar-chart-o"),column(width=9,plotOutput("barplot")),column(width = 3,
                                                                                                            box(title="Select Segment",width = NULL, status = "primary", solidHeader = TRUE,
                                                                                                                selectInput("segments", "",
                                                                                                                            choices = c(
                                                                                                                              "TotalTrainingSet"="TotalTrainingSet",
                                                                                                                              "High" = "High",
                                                                                                                              "Medium" = "Medium",
                                                                                                                              "Low" = "Low"
                                                                                                                            ))
                                                                                                            ))),
              tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainps"))
            )
            
          ))),
        
        tabItem("trainArpu", fluidPage(fluidRow(
          column(width = 9,
                 tabsetPanel(
                   tabPanel(title="Table",icon = icon("table"), tableOutput("trainSummary")),
                   tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("barplot_arpu")),
                   tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("trainSummarytext"))
                 )),
          column(width = 3,
                 box(title="Select Segment",width = NULL, status = "primary", solidHeader = TRUE,
                     selectInput("segmentsV", "",
                                 choices = c(
                                   "OverAll"="OverAll",
                                   "High" = "High",
                                   "Medium" = "Medium",
                                   "Low" = "Low"
                                 ))
                 ))
          
        ))),
        
      tabItem("traincpiScore", h2("CHURN PROBABILITY INDEX"),fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainCPI")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotlyOutput("plot_trainCPI", height = "200px"),selectInput("select", "Select", label = h2("Selection Pane"),choices = list(ModelPredictedNonUsercount="ModelPredictedNonUsercount",ActualChurners="ActualChurners",ModelPostValidationAccuracy="ModelPostValidationAccuracy"))),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainCPI"))
          )
        )),
      tabItem("trainaon", h2("AGE ON NETWORK"),fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainAON")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_trainAON")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainAON"))
          )
        )),
      tabItem("traintariff", h2("TARIFF DETAILS"),fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainTrf")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_trainTrf")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainTrf"))
          )
        )),
      tabItem("trainregion", h2("DEMOGRAPHIC DETAILS"),fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainRgn")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_trainRgn")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainRgn"))
          )
        )),
      tabItem("trainvaluesegment", h2("VALUE SEGMENTATION"),fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_trainValue")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_trainValue")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_trainValue"))
          )
        )),
      tabItem("testpredstatus", h2("PREDICTION STATUS"),fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_testov")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_testStatus")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_testov"))
          )
        )),
      tabItem("testcpiScore", h2("CHURN PROBABILITY INDEX"),fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_testCPI")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_testCPI")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_testCPI"))
          )
        )),
      tabItem("testaon", h2("AGE ON NETWORK"),fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_testAON")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_testAON")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_testAON"))
          )
        )),
      tabItem("testtariff", h2("TARIFF DETAILS"),fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_testTrf")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_testTrf")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_testTrf"))
          )
        )),
      tabItem("testregion", h2("DEMOGRAPHIC DETAILS"),fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"), tableOutput("table_testRgn")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_testRgn")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_testRgn"))
          )
        )),
        
      tabItem("testvaluesegment", h2("VALUE SEGMENTATION"),fluidPage(
          tabsetPanel(
            tabPanel(title="Table",icon = icon("table"),tableOutput("table_testValue")),
            tabPanel(title="Plot",icon = icon("bar-chart-o"),plotOutput("plot_testValue")),
            tabPanel(title="Text",icon = icon("th"),verbatimTextOutput("text_testValue"))
          )
        )),
      
      tabItem("detailedReport", fluidPage(fluidRow(
        column(width = 12,
               box(
                title = "Table",icon = icon("table"),width = NULL, status = "primary",
                div(style = 'overflow-x: scroll', DT::dataTableOutput('table'))
              ))
        
      ))),
      tabItem("downReports", h2("DOWNLOAD REPORTS"),fluidPage(
        fluidRow(
          column(width = 8),
          column(width = 4,
                 box(title="Prediction Period",width = NULL, status = "primary", solidHeader = TRUE,
                     selectInput("dreportperiod", "",
                                 choices = c(
                                   "201801" = 201801,
                                   "201712" = 201712,
                                   "201711" = 201711,
                                   "201710" = 201711
                                 )
                     ),
                     downloadButton("downloadData", "   Download Report")
                     )
          )
        )
      )),
      tabItem("uploadReports",h2("UPLOAD REPORTS"),fluidPage(
        fluidRow(
          column(width = 8),
          column(width = 4,
                 box(title="Prediction Period",width = NULL, status = "primary", solidHeader = TRUE,
                     selectInput("ureportperiod", "",
                                 choices = c(
                                   "201801" = 201801,
                                   "201712" = 201712,
                                   "201711" = 201711,
                                   "201710" = 201711
                                 )
                     ),
                     downloadButton("uploadData", "   Upload Report")
                 )
          )
        )
      )) ,
      tabItem("decisiontree", fluidPage( fluidRow(
        column(width = 8,
               tabsetPanel(
                 tabPanel(title="Tree Plot",icon = icon("bar-chart-o"),plotOutput("tree_plot_c50")),
                 tabPanel(title="Tree Summary",icon = icon("tree"),verbatimTextOutput("tree_summary")),
                 tabPanel(title="Confusion Matrix",icon = icon("tree"),plotOutput("cf_plot_c50"))
               )
        ),
        column(width = 4,
               box(title="Choose Segments",width = NULL, status = "primary", solidHeader = TRUE,
                   uiOutput('choose_y'),uiOutput('choose_x'),
                   actionButton('add_button', label = 'Generate', icon = icon("apple"), width = '160px')
               )
        )
        
      )
      
      )),
      tabItem("modelling", fluidPage(
        tabsetPanel(
        tabPanel("Dataset",
                 fluidRow(
                   column(1),
                   column(10,
                          fluidRow(
                            column(4, selectInput("dataset_type", label = h4("Chooses Type of Datset"), c("Build-in Dataset", "Upload CSV")))
                          ),
                          fluidRow(
                            column(12, uiOutput("dataset_parameter_panel"))
                          ),
                          fluidRow(column(12, uiOutput("dataset_result_panel")))
                   )
                 )
        ),
        
        ###################################################################################################
        #Statistics Test
        tabPanel("Statistics Test",
                 fluidRow(
                   column(1),
                   column(10,
                          fluidRow(
                            column(4, selectInput("statistics_method", label = h4("Choose Statistics Test"), c("Regression", "Paired T Test", "One-way ANOVA", "MANOVA"))),
                            uiOutput("statistics_variable_panel")
                          ),
                          uiOutput("statistics_parameter_panel"),
                          fluidRow(column(12, uiOutput("statistics_result_panel")))
                   )
                 )
        ),
        
        ###################################################################################################
        #Clustering
        tabPanel("Clustering",
                 fluidRow(
                   column(1),
                   column(10,
                          fluidRow(
                            column(4, selectInput("clustering_method", label = h4("Choose Clustering Method"), c("K-Means", "EM", "DBSCAN", "Spectral"), selected = "K-Means"))
                          ),
                          uiOutput("clustering_parameters_panel"),
                          fluidRow(column(12, uiOutput("clustering_result_panel")))
                   )
                 )
        ),
        
        ###################################################################################################
        #Classification
        tabPanel("Classification",
                 fluidRow(
                   column(1),
                   column(10,
                          fluidRow(
                            column(4, selectInput("classification_method", label = h4("Choose Classification Method"), c("Decision Tree", "Random Forest", "K-Nearest Neighbors", "Support Vector Machine", "Naive Bayes Classifier", "Feed-Forward Neural Network"), selected = "Decision Tree")),
                            uiOutput("class_attribute_panel")
                          ),
                          uiOutput("classification_parameters_panel"),
                          fluidRow(column(12, uiOutput("classification_result_panel")))
                   )
                 )
        ))
      )),
      tabItem("consumer_data_details", fluidPage(fluidRow(
        column(width = 12,
               box(
                 title = "Latest 3 Months Data", width = NULL, status = "primary",solidHeader = TRUE,
                 selectInput("data_period", "Range ID",multiple = FALSE,selectize = TRUE,width = '100px',
                             choices = c(
                               "201801" = 201801,
                               "201712" = 201712,
                               "201711" = 201711,
                               "201710" = 201711
                             )
                 ),
                 div(style = 'overflow-x: scroll', DT::dataTableOutput('123'))
               )
        )
        
      ))),
      tabItem("study_of_churn",fluidPage(fluidRow(
        column(width = 12,
               box(
                 title = "Churn Prediction Comparision", width = NULL, status = "primary",solidHeader = TRUE,
                 fluidRow(
                   column(width=3,box(width = NULL, status = "primary", solidHeader = TRUE,
                                      selectInput("dataset_first", label = h4("Choose First Dataset"),multiple = FALSE,selectize = TRUE,width = '180px',
                                                  choices = getDatasetList(), selected = "201801.csv"
                                      )
                   )),
                   column(width=3,box(width = NULL, status = "primary", solidHeader = TRUE,
                                      selectInput("dataset_second", label = h4("Choose Second Dataset"),multiple = FALSE,selectize = TRUE,width = '180px',
                                                  choices = getDatasetList(),selected = "201801.csv"
                                      )
                   )),
                   uiOutput("attribute_list_panel"),
                   column(width=2,box(width = NULL, status = "primary", solidHeader = TRUE,
                                      actionButton('show_button', label = h5('Show'), width = '60px')
                                      
                   ))
                 )
                 
                 
               )
        )
        
      ))),
      tabItem("data_preprocessing", fluidPage(
        fluidRow(
          column(1),
          column(10,
                 fluidRow(
                   column(4, selectInput("dataset_type1", label = h4("Chooses Type of Datset"), c("Build-in Dataset", "Upload CSV")))
                 ),
                 fluidRow(
                   column(12, uiOutput("dataset_parameter_panel1"))
                 ),
                 fluidRow(column(12, uiOutput("dataset_result_panel1")))
          )
        )
      )),
      tabItem("statistical_analysis",
              fluidRow(
                column(1),
                column(10,
                       fluidRow(
                         column(4, selectInput("statistics_method1", label = h4("Choose Statistics Test"), c("Regression", "Paired T Test", "One-way ANOVA", "MANOVA"))),
                         uiOutput("statistics_variable_panel1")
                       ),
                       uiOutput("statistics_parameter_panel1"),
                       fluidRow(column(12, uiOutput("statistics_result_panel1")))
                )
              )
      ),
      tabItem("clustering_process",
              fluidRow(
                column(1),
                column(10,
                       fluidRow(
                         column(4, selectInput("clustering_method1", label = h4("Choose Clustering Method"), c("K-Means", "EM", "DBSCAN", "Spectral"), selected = "K-Means"))
                       ),
                       uiOutput("clustering_parameters_panel1"),
                       fluidRow(column(12, uiOutput("clustering_result_panel1")))
                )
              )
      ),    
      tabItem("classification_process",
              fluidRow(
                column(1),
                column(10,
                       fluidRow(
                         column(4, selectInput("classification_method1", label = h4("Choose Classification Method"), c("Decision Tree", "Random Forest", "K-Nearest Neighbors", "Support Vector Machine", "Naive Bayes Classifier", "Feed-Forward Neural Network"), selected = "Decision Tree")),
                         uiOutput("class_attribute_panel1")
                       ),
                       uiOutput("classification_parameters_panel1"),
                       fluidRow(column(12, uiOutput("classification_result_panel1")))
                )
              )
      )
      
      
      
      
      
      )
      
    )
  )
)