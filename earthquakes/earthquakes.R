# rm(list = ls()) # read here https://www.tidyverse.org/blog/2017/12/workflow-vs-script/

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


# Set the working directory to the forlder eartquakes

### define the app ### 1st way
app <- shinyAppDir(".", options = list(height = 300, width = 600))

### define the app ### 2nd way
#source("ui.R")
#source("server.R")
#app <- shinyApp(ui=ui, server=server, options = list(height = 300, width = 600))

### launching ###
runApp(app)

### command to launch on Github ###
# runGitHub("shinyapp", "codefluence", subdir = "earthquakes")  # keep this commented

