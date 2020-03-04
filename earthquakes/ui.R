rm(list = ls())
library(shiny)
library(plotly)
library(tidyverse)


shinyUI = shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))

ui <- fluidPage(
    plotlyOutput("plot"),
    verbatimTextOutput("event")
)



shinyServer = shinyServer(function(input, output) {
    
    output$plot <- renderPlotly({
        
        df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')
        
        df$q <- with(df, cut(pop, quantile(pop)))
        levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
        df$q <- as.ordered(df$q)
        
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showland = TRUE,
            landcolor = toRGB("gray85"),
            subunitwidth = 1,
            countrywidth = 1,
            subunitcolor = toRGB("white"),
            countrycolor = toRGB("white")
        )
        
        p <- plot_geo(df, locationmode = 'USA-states', sizes = c(1, 250)) %>%
            add_markers(
                x = ~lon, y = ~lat, size = ~pop, color = ~q, hoverinfo = "text",
                text = ~paste(df$name, "<br />", df$pop/1e6, " million")
            ) %>%
            layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)
        
        p
    })
    
    
})


server <- function(input, output) {
    
    df2 <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')
    
    df = read_csv("earthquakes/data.csv")
    df$Magnitude = df$Magnitude * 1e5
    
    df$q <- with(df, cut(Magnitude, quantile(Magnitude)))
    levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
    df$q <- as.ordered(df$q)
    
    g <- list(
        #scope = 'usa',
        #projection = list(type = 'albers usa'),
        showland = TRUE,
        landcolor = toRGB("gray85"),
        subunitwidth = 1,
        countrywidth = 1,
        list(lonaxis = list(range = c(-30, 30))),
        list(lataxis = list(range = c(-30, 30))),
        subunitcolor = toRGB("white"),
        countrycolor = toRGB("white")
    )
    
    #locationmode = 'USA-states', 
    p <- plot_geo(df, sizes = c(1, 250), width = 1200, height = 700) %>%
         add_markers(
            x = ~Longitude, y = ~Latitude, size = ~Magnitude, color = ~q, hoverinfo = "text",
            text = ~paste(df$name, "<br />", df$Magnitude/1e6, " million")
        ) %>%
        layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)
    
    
    # renderPlotly() also understands ggplot2 objects!
    output$plot <- renderPlotly({
        p$elementId <- NULL
        p
    })
    output$event <- renderPrint({
        d <- event_data("plotly_hover")
        if (is.null(d)) "Select a point for more detail." else d
    })
}


##########


data = read_csv("earthquakes/data.csv")

shinyApp(ui = ui, server = server, options = list(height = 830, width = 830))





