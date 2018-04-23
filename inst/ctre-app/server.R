library(shiny)
library(CTRE)
library(magrittr)

source("helpers.R")

num_sim <- 10000
num_eff <- 500
cutT <- 1000

function(input, output) {
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
    plot(ctre_model)
  })

  output$acfPlot <- renderPlot({
    acf(ctre_model)
  })

  output$tailscalePlot <- renderPlot({
    par(mfrow = c(1, 2))
    MLestimates(ctre_model)
  })
}
