library(shiny)
library(tidyverse)
library(curl)
library(readxl)
library(RcppRoll)
library(paletteer)
library(ggstream)
library(lubridate)

  #Grab data from https://coronavirus.data.gov.uk/about-data#cases-by-age
  temp <- tempfile()
  source <- "https://coronavirus.data.gov.uk/downloads/demographic/cases/specimen_date-latest.csv"
  temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
  
  data <- read.csv(temp) %>% 
    select(c(1:23)) %>% 
    gather(age, cases, c(5:23)) 
  
  #Tidy up
  data <- data %>% 
    mutate(age=gsub("X", "", age), age=gsub("_", "-", age), age=gsub("\\.", "\\+", age),
           date=as.Date(date), age=factor(age, levels=c("0-4", "5-9", "10-14", "15-19",
                                                        "20-24", "25-29", "30-34", "35-39", 
                                                        "40-44", "45-49", "50-54", "55-59", 
                                                        "60-64", "65-69", "70-74", "75-79", 
                                                        "80-84", "85-89", "90+"))) 
  
  #Bring in populations
  temp <- tempfile()
  source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls"
  temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
  
  pop <- read_excel(temp, sheet="MYE2 - Persons", range="A5:CQ431")
  
  #Align age bands
  pop <- pop %>% 
    gather(age.sgl, pop, c(5:95)) %>% 
    mutate(age.sgl=as.numeric(gsub("\\+", "", age.sgl)),
           age=case_when(
             age.sgl<5 ~ "0-4",
             age.sgl<10 ~ "5-9",
             age.sgl<15 ~ "10-14",
             age.sgl<20 ~ "15-19",
             age.sgl<25 ~ "20-24",
             age.sgl<30 ~ "25-29",
             age.sgl<35 ~ "30-34",
             age.sgl<40 ~ "35-39",
             age.sgl<45 ~ "40-44",
             age.sgl<50 ~ "45-49",
             age.sgl<55 ~ "50-54",
             age.sgl<60 ~ "55-59",
             age.sgl<65 ~ "60-64",
             age.sgl<70 ~ "65-69",
             age.sgl<75 ~ "70-74",
             age.sgl<80 ~ "75-79",
             age.sgl<85 ~ "80-84",
             age.sgl<90 ~ "85-89",
             TRUE ~ "90+"
           )) %>% 
    #And sort out Buckinghamshire codes
    mutate(Code=case_when(
      Code %in% c("E07000005", "E07000006", "E07000007", "E07000008") ~ "E06000060",
      TRUE ~ Code
    )) %>% 
    group_by(age, Code) %>%
    summarise(pop=sum(pop))
  
  #Merge into case data
  data <- data %>% 
    merge(pop, by.x=c("areaCode", "age"), by.y=c("Code", "age"), all.x=TRUE) %>% 
    arrange(date) 
  
  #Collapse age bands further
  shortdata <- data %>% 
    mutate(ageband=case_when(
      age %in% c("0-4", "5-9", "10-14") ~ "0-14",
      age %in% c("15-19", "20-24") ~ "15-24",
      age %in% c("25-29", "30-34", "35-39", "40-44") ~ "25-44",
      age %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64",
      age %in% c("65-69", "70-74", "75-79") ~ "65-79",
      TRUE ~ "80+"
    )) %>% 
    group_by(ageband, areaCode, areaType, areaName, date) %>% 
    summarise(cases=sum(cases), pop=sum(pop)) %>% 
    ungroup() %>% 
    mutate(caserate=cases*100000/pop) %>% 
    group_by(ageband, areaCode, areaType) %>% 
    mutate(casesroll=roll_mean(cases, n=7, align="center", fill=NA),
           caserateroll=roll_mean(caserate, n=7, align="center", fill=NA)) %>% 
    ungroup()
  
  data <- data %>% 
    mutate(caserate=cases*100000/pop) %>% 
    group_by(age, areaCode, areaType) %>% 
    mutate(casesroll=roll_mean(cases, n=7, align="center", fill=NA),
           caserateroll=roll_mean(caserate, n=7, align="center", fill=NA)) %>% 
    ungroup()
  