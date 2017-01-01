source('config.R')


shinyUI(fluidPage(
  
  title = UI_TITLE,
  UI_HEADER,
  theme=UI_THEME,
  UI_STYLE,
  titlePanel(UI_TITLE),
  
  fluidRow(
    column(width=5,
      wellPanel(
        checkboxInput("sampleMode", tags$b("Sample mode"), T),
        uiOutput('tableRender'),
        selectInput("chart", label="Chart type", selected="scatter", choices=list(
          "Scatter plot" = "scatter",
          "Line chart" = "line",
          #"Bar chart" = "bar",
          #"Histogram" = "histogram",
          "Sample data" = "sample"
        ))
      )
    ),
    
    
    column(width=7,
           wellPanel(
             uiOutput('filtersList'),
             uiOutput('filtersChoose')
           )
    )
  ),
  fluidRow(
    columns=12,
    
    uiOutput('outView')
  )
))