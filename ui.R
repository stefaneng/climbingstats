library(shiny)
library(plotly)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Climbing Stats"),

    # Show Routes per Grade
    mainPanel(
       plotlyOutput("routesPlot"),
       plotlyOutput("bouldersPlot"),
       plotlyOutput("pitchesMonthYearPlot"),
       plotlyOutput("pitchesYearPlot"),
       fluidRow(column(4, tableOutput('firstBoulderGradeTable')))
    )
  )
)
