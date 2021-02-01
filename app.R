library(shiny)
source("UsefulFunctions.R")


# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("Estimation of the Risk-Neutral Density of Future Inflation Rates"),
    tabsetPanel(
        tabPanel("Data Input",
                 sidebarLayout(
                     sidebarPanel(
                         withMathJax(),
                         h3("How to use this app"),
                         helpText(intro1()),
                         br(),
                         h3("Data Formatting"),
                         helpText(intro2()),
                         downloadButton("downloadOptions", "Download Option Template"),
                         downloadButton("downloadSWIL", "Download ILS Template"),
                         downloadButton("downloadOIS", "Download OIS Template"),
                         helpText(intro4())
                     ),
                     
                     # Main panel
                     mainPanel(
                         checkboxInput("example_data", label = "Upload dataset", value = FALSE),
                         conditionalPanel(condition = "input.example_data == true", 
                                          fileInput("options", label = "Upload option prices", multiple = TRUE, accept = c(".csv")),
                                          fileInput("swil", label = "Upload swap rates"),
                                          fileInput("ois", label = "Upload OIS rates"),
                                          selectizeInput(
                                              "totalMats"
                                              , "Enter the option maturities as a vector:"
                                              , choices = NULL
                                              , multiple = TRUE
                                              , options = list(create = TRUE)
                                          )
                         ),
                         actionButton("compute", label ="Update Data"),
                         br(),
                         br()
                     )
                 )
        ),
        tabPanel("Plot", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         dateInput("date_in", label = "Select date:", value = "2009-12-11", min = "2009-10-05", max = "2019-12-13", 
                                   daysofweekdisabled = c(0,6)),
                         selectizeInput(
                             "mats"
                             , "Maturity:"
                             , choices = NULL
                             , multiple = TRUE
                             , options = list(create = TRUE)
                         ),
                         helpText(intro3()),
                         br(),
                         downloadButton("downloadDensity", "Download Density")
                     ),
                     mainPanel(
                         plotOutput("densPlot", width = "75%", height = "500px")
                     )
                 )
        )
    )
     
    
)

# Define server logic
server <- function(input, output) {
    
    data1 <- eventReactive(input$compute,{
        ReadOptions(get_file_options())
    }, ignoreNULL = FALSE)
    data2 <- eventReactive(input$compute,{
        GetSWIL(get_file_SWIL())
    }, ignoreNULL = FALSE)
    data3 <- eventReactive(input$compute,{
        Get10YBonds(get_file_OIS())
    }, ignoreNULL = FALSE)
    
    getDataMats <- eventReactive(input$compute, {
        if(!input$example_data){
            c(1,3)
        } else{
            as.vector(as.numeric(input$totalMats))
        }
    }, ignoreNULL = FALSE) 
    
    getCurrentMats <- reactive({
        if(length(input$mats) == 0){
            getDataMats()
        } else {
            as.numeric(input$mats)
        }
    })
    
    
    output$densPlot <- renderPlot({
        DensityPlot(data1(), data2(), data3(), input$date_in, 10, 11, getDataMats(), getCurrentMats())
    })
    get_file_options <- eventReactive(input$compute, {
        if (!input$example_data) {
            c("data/test1Y.csv", "data/test3Y.csv")
        } else {
            input$options$datapath
        }
    }, ignoreNULL = FALSE)
    get_file_SWIL <- eventReactive(input$compute, {
        if (!input$example_data) {
            "data/testSWIL.csv"
        } else {
            input$swil$datapath
        }
    }, ignoreNULL = FALSE)
    get_file_OIS <- eventReactive(input$compute, {
        if (!input$example_data) {
            "data/testOIS.csv"
        } else {
            input$ois$datapath
        }
    }, ignoreNULL = FALSE)
    
    output$downloadOptions <- downloadHandler(
        filename = function() {
            "OptionTemplate.csv"
        },
        content = function(file) {
            file.copy("data/OptionTemplate.csv", file)
        }
    )
    output$downloadSWIL <- downloadHandler(
        filename = function() {
            "ILSTemplate.csv"
        },
        content = function(file) {
            file.copy("data/ILSTemplate.csv", file)
        }
    )
    output$downloadOIS <- downloadHandler(
        filename = function() {
            "OISTemplate.csv"
        },
        content = function(file) {
            file.copy("data/OISTemplate.csv", file)
        }
    )
    output$downloadDensity <- downloadHandler(
        filename = function() {
            "density.csv"
        },
        content = function(file) {
            r1 <- DensityWrite(data1(), data2(), data3(), input$date_in, 10, 11, getDataMats(), getCurrentMats())
            t1 <- getDataMats()
            t2 <- getCurrentMats()
            if(length(t2) == 1){
                ind <- which(t1 == t2)
                write.csv(r1[[ind]], file, row.names = FALSE)
            } else {
                write.csv(r1[[1]], file, row.names = FALSE)
            }
            
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
