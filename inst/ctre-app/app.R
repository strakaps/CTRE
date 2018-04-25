library(shiny)
library(CTRE)
library(MittagLeffleR)
library(magrittr)
library(markdown)

ui <- fluidPage(
  titlePanel("CTRE Modelling"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "dataChoice",
        label = "Dataset",
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
        "tail",
        label = "Tail parameter",
        value = 0.85,
        min = 0.1,
        max = 1,
        step = 0.05
      ),
      sliderInput("num_exceedances",
                  label = "Number of Exceedances",
                  10, 600, 120)
    ),
    mainPanel(
      tabsetPanel(
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
        ),
        tabPanel(
          "Diagnostics",
          includeMarkdown("diagnostics.md"),
          plotOutput("mlqqcopulaPlot"),
          plotOutput("acfPlot")
        ),
        tabPanel(
          "Exceedances",
          includeMarkdown("exceedancePlot.md"),
          plotOutput("tshapeplot"),
          plotOutput("tscaleplot")
        )
      )
    )
  ))

server <- function(input, output) {
  simData <- eventReactive(
      input$simButton, {
        data.frame(
          cumsum(MittagLeffleR::rml(n = 1000, tail = input$tail, scale = 5)),
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
    if (input$yscale == 'y') {
      mags <- magnitudes(base_ctre())
      ylim <- c(max(0.01, min(mags)), max(mags))
      base_ctre() %>% plot(p = k/n, log = 'y')
    }
    else
      base_ctre() %>% plot(p = k/n)
  })

  output$tailscalePlot <- renderPlot({
    par(mfrow = c(1,2))
    base_ctre() %>%
      thin(k = input$num_exceedances) %>%
      MLestimates(tail = input$tail)
  })

  output$mlqqcopulaPlot <- renderPlot({
    thinned_ctre <- base_ctre() %>% thin(k = input$num_exceedances)
    par(mfrow = c(1,2))
    thinned_ctre %>% interarrival() %>% mlqqplot(tail = input$tail, log = 'xy')
    thinned_ctre %>% empcopula()
  })

  output$acfPlot <- renderPlot({
    base_ctre() %>% thin(k = input$num_exceedances) %>% acf()
  })

  output$tshapeplot <- renderPlot({
    base_ctre() %>% thin(k = input$num_exceedances) %>%
      magnitudes() %>% evmix::tshapeplot()
  })

  output$tscaleplot <- renderPlot({
    base_ctre() %>% thin(k = input$num_exceedances) %>%
      magnitudes() %>% evmix::tscaleplot()
  })
}

shinyApp(ui, server)
