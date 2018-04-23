library(shiny)
library(CTRE)
library(MittagLeffleR)
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
          "Simulated",
          "Uploaded*"
        ),
        choiceValues = list("flares", "bitcoin", "simulated", "upload")
      ),
      radioButtons(
        "yscale",
        label = "Scale of magnitudes",
        choiceNames = list("linear", "log"),
        choiceValues = list("", "y")
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
  simData <- eventReactive(
      input$simButton, {
        data.frame(
          cumsum(MittagLeffleR::rml(n = 1000, tail = 0.8, scale = 5)),
          rexp(n = 1000)
        )
      },
      ignoreNULL = FALSE
    )

  upData <- reactive({
    inFile <- input$file1
    df <- read.csv(inFile$datapath)
    names(df) <- c("times", "magnitudes")
    df$idxJ <- order(df$magnitudes, decreasing = TRUE)
    return(df)
  })

  base_ctre <- reactive({
    switch(
      input$dataChoice,
      "flares" = CTRE::flares,
      "quakes" = CTRE::seaquakes,
      "bitcoin" = CTRE::bitcoin,
      "simulated" = simData(),
      "upload" = upData()
    ) %>% ctre()
  })

  output$dataPlot <- renderPlot({
    k <- input$num_exceedances
    n <- length(base_ctre())
    ylim <- NULL
    if (input$yscale == 'y')
      ylim <- c(0.01, max(base_ctre() %>% magnitudes()))
    base_ctre() %>% plot(p = k/n, log = input$yscale, ylim = ylim)
  })

}

shinyApp(ui, server)
