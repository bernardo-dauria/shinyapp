rm(list = ls())
library(shiny)
library(plotly)
library(tidyverse)
library(shinyjs)



data = read_csv("earthquakes/data.csv")

data$Magnitude = data$Magnitude * 1e5
data$q <- with(data, cut(Magnitude, quantile(Magnitude)))
levels(data) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
data$q <- as.ordered(data$q)


data$Year = as.numeric(format(strptime(as.character(data$Date), "%m/%d/%Y"), "%Y"))
data = drop_na(data, c("Year"))

table(data$Year)

plot(density(as.numeric(data$Year)))
range(data$Year)


###############################################################


ui = shinyUI(fluidPage(

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
            
        )
    ),
    
    sliderInput("slider", "Range:",
                min = min(data$Year), max = max(data$Year),
                value = c(2000,2016)),
    
    plotlyOutput("plot"),
    
    verbatimTextOutput("event"),
    
    fluidRow(
        column(4, verbatimTextOutput("aaa"))
    )
))


server <- function(input, output) {
    
    dataInput = reactive({
        
        mmin = min(as.numeric(input$slider))
        mmax = max(as.numeric(input$slider))
        copy = filter(data, Year>= mmin & Year <= mmax)
        copy
    })
    
    # renderPlotly() also understands ggplot2 objects!
    output$plot <- renderPlotly({
        
        df = dataInput()
        
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
                text = ~paste(df$name, "<br />", df$Magnitude/1e6, " year is:", nrow(df))
            ) %>%
            layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)
    })
    
    output$aaa <- renderPrint({ input$slider })
    
    output$event <- renderPrint({
        d <- event_data("plotly_hover")
        if (is.null(d)) "Select a point for more detail." else d
    })
}


##########


shinyApp(ui = ui, server = server, options = list(height = 830, width = 830))





