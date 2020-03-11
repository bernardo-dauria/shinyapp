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



### launching ###


shinyApp(ui = ui, server = server, options = list(height = 300, width = 600))

runGitHub("shinyapp", "codefluence", subdir = "earthquakes")

