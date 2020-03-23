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

ui = navbarPage("EARTHQUAKES",
                
                tabPanel("Observations",
                         pageWithSidebar(
                             
                             headerPanel(""),
                             
                             sidebarPanel(
                                 
                                 sliderInput("slider", "Time Span:",
                                             min = 1965, max = 2016,
                                             value = c(2000,2016)),
                                 
                                 sliderInput("slider2", "Magnitude Range:",
                                             min = min(data$Magnitude), max = max(data$Magnitude),
                                             value = c(7,9.1)),
                                 
                                 downloadButton("report", "Generate report"), width = 3

                             ),
                             
                             mainPanel(
                                 
                                 fluidPage(theme = shinytheme("cerulean"),
                                     
                                     fluidRow(
                                         plotlyOutput("plot"),
                                         DT::dataTableOutput("plot_brushed_points")
                                     )
                                 )
                             )
                         )
                ),
                tabPanel("Density",
                         
                         pageWithSidebar(
                             
                             headerPanel(""),
                             
                             sidebarPanel(
                                 
                                 selectInput("select", label = h3("Country"), 
                                             choices = list("Chile" = 1, "Indonesia" = 2, "Japan" = 3), 
                                             selected = 1),
                                 
                                 sliderInput("aslider2", "Magnitude Range:",
                                             min = min(data$Magnitude), max = max(data$Magnitude),
                                             value = c(7,9.1)),

                             ),
                             
                             mainPanel(
                                 
                                 plotOutput('plot1')
                             )
                         )
                )
)

