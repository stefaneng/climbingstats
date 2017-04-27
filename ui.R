library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Climbing Stats"),

    # Show Routes per Grade
    mainPanel(
       plotOutput("routesPlot"),
       plotOutput("bouldersPlot"),
       plotOutput("pitchesMonthYearPlot"),
       plotOutput("pitchesYearPlot")
    )
  )
)
