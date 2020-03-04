rm(list = ls())
library(shiny)
library(plotly)
library(tidyverse)
library(shinyjs)



data = read_csv("earthquakes/data.csv")

data = data[,c(1:6,9)]
data$Magnitude = data$Magnitude

#data$q <- with(data, cut(Magnitude, quantile(Magnitude)))
#levels(data) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
#data$q <- as.ordered(data$q)

data$Year = as.numeric(format(strptime(as.character(data$Date), "%m/%d/%Y"), "%Y"))
data = drop_na(data, c("Year"))

#table(data$Year)
#plot(density(as.numeric(data$Year)))


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
    
    fluidRow(
        column(width = 9,
               wellPanel(width = 9,
                         h4("Points selected by brushing, with brushedPoints():"),
                         DT::dataTableOutput("plot_brushed_points")
               )
        ),
        column(width = 3,
               verbatimTextOutput("plot_brushinfo")
        )
    ),
    
    sliderInput("slider", "Time Span:",
                min = min(data$Year), max = max(data$Year),
                value = c(2000,2016)),
    
    sliderInput("slider2", "Magnitude Range:",
                min = min(data$Magnitude), max = max(data$Magnitude),
                value = c(7,9.1)),

    plotlyOutput("plot"),
    
    verbatimTextOutput("event")
    
    ##fluidRow(
    #    column(4, verbatimTextOutput("aaa"))
    #)
))


server <- function(input, output) {
    
    dataInput = reactive({
        
        copy = filter(data, Year>= min(as.numeric(input$slider)) & Year <= max(as.numeric(input$slider)))
        copy = filter(copy, Magnitude>= min(as.numeric(input$slider2)) & Magnitude <= max(as.numeric(input$slider2)))
        
        #copy = rbind(copy,list(Magnitude=min(data$Magnitude)))
        #copy = rbind(copy,list(Magnitude=max(data$Magnitude)))

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
                x = ~Longitude, y = ~Latitude, size = ~Magnitude, color = ~Depth, hoverinfo = "text", sizes = range(exp(df$Magnitude))/40,
                text = ~paste(df$name, "<br /> Magnitude:", df$Magnitude, "<br />", df$Year, "<br />", df$Latitude, "<br />", df$Longitude),
                brush = ~brushOpts("plot_brush")
            ) %>%
            layout(title = paste(nrow(df),"earquakes"), geo = g)
    })

    output$event <- renderPrint({
        d <- event_data("plotly_hover")
        if (is.null(d)) "Select a point for more detail." else d
    })

    # listen to the brushing event and draw a
    # rect shape that mimics the brush
    observe({
        brush <- event_data("plotly_brushing")
        
        if (!is.null(brush) && !is.na(brush) && !is.na(brush$geo) && nrow(brush$geo) == 2)
        {
            print(brush$geo)
            
            copy = filter(data, Year>= min(as.numeric(input$slider)) & Year <= max(as.numeric(input$slider)))
            copy = filter(copy, Magnitude>= min(as.numeric(input$slider2)) & Magnitude <= max(as.numeric(input$slider2)))
            copy = filter(copy, Longitude>= brush$geo[1,1] & Longitude <= brush$geo[2,1])
            copy = filter(copy, Latitude>= brush$geo[2,2] & Latitude <= brush$geo[1,2])
            
            #res <- brushedPoints(dataInput(), brush)
            
            #rint("res")
            print(paste("len",nrow(copy)))
            datatable(copy)
        }
    })
    
    
    output$plot_brushed_points <- DT::renderDataTable({
        
        brush <- event_data("plotly_brushing")
        
        if (!is.null(brush) && !is.na(brush) && !is.na(brush$geo) && nrow(brush$geo) == 2)
        {
            print(brush$geo)
            
            copy = filter(data, Year>= min(as.numeric(input$slider)) & Year <= max(as.numeric(input$slider)))
            copy = filter(copy, Magnitude>= min(as.numeric(input$slider2)) & Magnitude <= max(as.numeric(input$slider2)))
            copy = filter(copy, Longitude>= brush$geo[1,1] & Longitude <= brush$geo[2,1])
            copy = filter(copy, Latitude>= brush$geo[2,2] & Latitude <= brush$geo[1,2])
            
            #res <- brushedPoints(dataInput(), brush)
            
            #rint("res")
            print(paste("len",nrow(copy)))
            datatable(copy)
        }
    })
    
}


##########


shinyApp(ui = ui, server = server, options = list(height = 830, width = 830))





