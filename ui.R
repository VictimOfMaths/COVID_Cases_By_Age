library(shiny)
library(lubridate)

ui <- fluidPage(
  
  titlePanel("Visualising age patterns in English Local Authority COVID-19 case data"),
  
  sidebarPanel(
    
    selectInput('LA', 'Select Local Authority', 
                sort(as.character(unique(shortdata$areaName[shortdata$areaType=="ltla"]))), 
    multiple=FALSE, selected="Sheffield"),
  selectInput('plottype', 'Select plot', c("Heatmap of case numbers"=1,
                                           "Heatmap of case rates"=2,
                                           "Line chart of case rates (detailed ages)"=3,
                                           "Line chart of case rates (broad ages)"=4, 
                                           "Streamgraph of case numbers"=5)),
  sliderInput('StartDate', 'Select start date for plot', min=min(shortdata$date)+days(3), 
              max=max(shortdata$date)-days(4), value=as.Date("2020-08-01"))),
  
  mainPanel(
    plotOutput('plot')
  )
)