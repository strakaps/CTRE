library(shiny)
library(CTRE)
library(magrittr)
library(markdown)

ui <- fluidPage(
  titlePanel("CTRM Statistics"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "dataChoice",
        label = "Which dataset should we use?",
        choiceNames = list(
          "Solar Flares",
          "Bitcoin trades",
          "Earthquakes",
          "Simulated",
          "Uploaded*"
        ),
        choiceValues = list("flares", "bitcoin", "seaquakes", "simulated", "upload")
      ),
      actionButton("simButton", label = "Re-simulate"),
      fileInput(
        'file1',
        'Upload your own CSV File',
        accept = c('text/csv',
                   'text/comma-separated-values,text/plain',
                   '.csv')
      ),
      includeText("upload-instructions.md"),
      numericInput(
        "shape",
        label = "Tail parameter:",
        value = 0.85,
        min = 0.1,
        max = 1,
        step = 0.05
      ),
      sliderInput("num_exceedances",
                  label = "Number of Exceedances:",
                  10, 600, 100)
    ),
    mainPanel(tabsetPanel(
      type = "pills",
      tabPanel(
        "Data Plot",
        includeMarkdown("dataPlot.md"),
        plotOutput("dataPlot")
      ),
      tabPanel(
        "Exceedance Times",
        includeMarkdown("stabilityPlot.md"),
        plotOutput("tailscalePlot")
      )
    ))
  ))

server <- function(input, output) {
  simData <- eventReactive(input$simButton, {
    simMLdata(n = num_sim, tail = input$shape, cutT)
  },
  ignoreNULL = FALSE)

  upData <- reactive({
    inFile <- input$file1
    df <- read.csv(inFile$datapath)
    names(df) <- c("times", "magnitudes")
    df$idxJ <- order(df$magnitudes, decreasing = TRUE)
    return(df)
  })

  useData <- reactive({
    switch(
      input$dataChoice,
      "flares" = CTRE::flares,
      "quakes" = CTRE::seaquakes,
      "bitcoin" = CTRE::bitcoin,
      "simulated" = simData(),
      "upload" = upData()
    )
  })

  ctre_model <- reactive({
    ctre(useData())
  })

  output$dataPlot <- renderPlot({
    plot(1:10, 1:10)
  })

}

shinyApp(ui, server)
