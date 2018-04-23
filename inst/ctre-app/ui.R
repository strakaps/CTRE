library(markdown)
library(shiny)

fluidPage(
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
        )
      )
    )
  ))
