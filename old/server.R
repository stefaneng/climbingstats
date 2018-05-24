library(shiny)
library(plotly)
source("climbing_stats.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$routesPlot <- renderPlotly({
    ggRoutesPlot
  })
  
  output$bouldersPlot <- renderPlotly({
    ggBouldersPlot
  })
  
  output$pitchesMonthYearPlot <- renderPlotly({
    ggPitchesMonthYearPlot
  }) 
  
  output$pitchesYearPlot <- renderPlotly({
    ggPitchesYearPlot
  })
  bouldersByGrade <- group_by(boulders, grade)
  
  output$firstBoulderGradeTable <- renderTable(
    firstBoulderGradeDf
  )
})
