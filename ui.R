# ui.R
library(shiny)
library(networkD3)
library(plotly)

# UI
ui <- fluidPage(
  titlePanel("Shiny App with Stacked Visuals (pbc2)"),
  
  tags$head(tags$style(HTML("
    .section {
      margin-bottom: 40px;
      padding: 20px;
      border: 1px solid #ccc;
      border-radius: 8px;
      background-color: #f9f9f9;
    }
  "))),
  
  div(class = "section",
      h3("Histogram of Age"),
      plotOutput("plot1", height = "300px")
  ),
  
  div(class = "section",
      h3("Sankey Plot: Sex to Drug"),
      sankeyNetworkOutput("plot2", height = "300px")
  ),
  
  div(class = "section",
      h3("Kaplan-Meier Survival Curve"),
      plotlyOutput("plot3", height = "300px")
  ),
  
  div(class = "section",
      h3("Density Plot of Serum Bilirubin"),
      plotlyOutput("plot4", height = "300px")
  )
)
