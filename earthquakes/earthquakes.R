rm(list = ls())
library(shiny)
library(plotly)
library(tidyverse)
library(shinyjs)
library(DT)
library(ggplot2)
library(maps)
library(shinythemes)
library(scales)
library(viridis)


### data preprocessing ###

data = read_csv("data.csv")

data = data[,c(1:6,9)]
data = filter(data, Type == "Earthquake") #Only earthquake are relevant in this app
data = data[,-5]

data$Longitude = as.numeric(data$Longitude)
data$Latitude = as.numeric(data$Latitude)

data$Year = as.numeric(format(strptime(as.character(data$Date), "%m/%d/%Y"), "%Y"))
data$ScaledMagnitude = rescale(data$Magnitude, to=c(5,100))
data = drop_na(data, c("Year"))


### launching ###


shinyApp(ui = ui, server = server, options = list(height = 300, width = 600))

runGitHub("shinyapp", "codefluence", subdir = "earthquakes")

