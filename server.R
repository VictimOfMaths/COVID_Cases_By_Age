library(shiny)
library(tidyverse)
library(curl)
library(readxl)
library(RcppRoll)
library(paletteer)
library(ggstream)
library(lubridate)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    LA <- input$LA
    StartDate <- input$StartDate
    
    if (input$plottype == 1){
      p <- data %>% 
        filter(areaType=="ltla" & areaName==LA & !is.na(casesroll)) %>% 
        ggplot()+
        geom_tile(aes(x=date, y=age, fill=casesroll))+
        scale_fill_paletteer_c("viridis::magma", name="Daily cases")+
        scale_x_date(name="", limits=c(StartDate, NA))+
        scale_y_discrete(name="Age")+
        theme_classic()+
        theme(plot.title=element_text(face="bold"))+
        labs(title=paste("Age patterns in COVID-19 cases in", LA),
             subtitle="Rolling 7-day average of confirmed new cases by age group",
             caption="Data from Public Health England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.13158122")
    }
    
    if (input$plottype == 2){
      p <- data %>% 
        filter(areaType=="ltla" & areaName==LA & !is.na(caserateroll)) %>% 
        ggplot()+
        geom_tile(aes(x=date, y=age, fill=caserateroll))+
        scale_fill_paletteer_c("viridis::magma", name="Daily cases\nper 100,000")+
        scale_x_date(name="", limits=c(StartDate, NA))+
        scale_y_discrete(name="Age")+
        theme_classic()+
        theme(plot.title=element_text(face="bold"))+
        labs(title=paste("Age patterns in COVID-19 cases in", LA),
             subtitle="Rolling 7-day average rate per 100,000 of confirmed new cases by age group",
             caption="Data from Public Health England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.13158122")
    }
    
    if (input$plottype == 3){
      plotlabel<- if_else(input$scale=="Log", "Daily cases per 100,000 (log scale)",
                          "Daily cases per 100,000")
      scaletype <- if_else(input$scale=="Log", "log2", "identity")
      p <- data %>% 
        filter(areaType=="ltla" & areaName==LA & !is.na(caserateroll)) %>% 
        ggplot()+
        geom_line(aes(x=date, y=caserateroll, colour=age))+
        scale_colour_paletteer_d("pals::stepped", name="Age")+
        scale_x_date(name="", limits=c(StartDate, NA))+
        scale_y_continuous(name=plotlabel, position="right", trans=scaletype)+
        theme_classic()+
        theme(plot.title=element_text(face="bold"))+
        guides(colour=guide_legend(ncol=2, byrow=TRUE))+
        labs(title=paste("Age patterns in COVID-19 cases in", LA),
             subtitle="Rolling 7-day average rate per 100,000 of confirmed new cases by age group",
             caption="Data from Public Health England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.13158122")
    }
    
    if (input$plottype == 4){
      plotlabel<- if_else(input$scale=="Log", "Daily cases per 100,000 (log scale)",
                          "Daily cases per 100,000")
      scaletype <- if_else(input$scale=="Log", "log2", "identity")
      p <-shortdata %>% 
        filter(areaType=="ltla" & areaName==LA & !is.na(caserateroll)) %>% 
        ggplot()+
        geom_line(aes(x=date, y=caserateroll, colour=ageband))+
        scale_colour_paletteer_d("awtools::a_palette", name="Age")+
        scale_x_date(name="", limits=c(StartDate, NA))+
        scale_y_continuous(name=plotlabel, position="right", trans=scaletype)+
        theme_classic()+
        theme(plot.title=element_text(face="bold"))+
        labs(title=paste("Age patterns in COVID-19 cases in", LA),
             subtitle="Rolling 7-day average rate per 100,000 of confirmed new cases by age group",
             caption="Data from Public Health England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.13158122") 
    }
    
    if (input$plottype == 5){
      p <- shortdata %>% 
        filter(areaType=="ltla" & areaName==LA & !is.na(casesroll)) %>% 
        ggplot()+
        geom_stream(aes(x=date, y=casesroll, fill=ageband), bw=0.2)+
        scale_fill_paletteer_d("awtools::a_palette", name="Age")+
        scale_x_date(name="", limits=c(StartDate, NA))+
        scale_y_continuous(name=" Daily cases per 100,000", labels=abs, position="right")+
        theme_classic()+
        theme(plot.title=element_text(face="bold"))+
        labs(title=paste("Age patterns in COVID-19 cases in", LA),
             subtitle="Rolling 7-day average of confirmed new cases by age group",
             caption="Data from Public Health England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.13158122")
    }
    p     
  })
  
}