rm(list = ls())
library(shiny)
library(plotly)
library(tidyverse)
library(shinyjs)
library(DT)
library(ggplot2)
library(maps)
library(shinythemes)

data = read_csv("data.csv")

data = data[,c(1:6,9)]
data = filter(data, Type == "Earthquake")
data = data[,-5]

data$Longitude = as.numeric(data$Longitude)
data$Latitude = as.numeric(data$Latitude)

data$Year = as.numeric(format(strptime(as.character(data$Date), "%m/%d/%Y"), "%Y"))
data = drop_na(data, c("Year"))


###############################################################


ui = navbarPage("EARTHQUAKES",
                
                tabPanel("Observations",
                         pageWithSidebar(
                             
                             # Application title
                             headerPanel(""),
                             
                             # Sidebar with controls to select the dataset and forecast ahead duration
                             sidebarPanel(
                                 
                                 sliderInput("slider", "Time Span:",
                                             min = min(data$Year), max = max(data$Year),
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
                             
                             # Application title
                             headerPanel(""),
                             
                             # Sidebar with controls to select the dataset and forecast ahead duration
                             sidebarPanel(
                                 
                                 selectInput("select", label = h3("Country"), 
                                             choices = list("Chile" = 1, "Indonesia" = 2, "Italy" = 3, "Japan" = 4), 
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
    
    



server <- function(input, output) {
    
    dataInput = reactive({
        
        copy = filter(data, Year>= min(as.numeric(input$slider)) & Year <= max(as.numeric(input$slider)))
        copy = filter(copy, Magnitude>= min(as.numeric(input$slider2)) & Magnitude <= max(as.numeric(input$slider2)))
        
        copy
    })
    
    kdeInput = reactive({
        
        copy = filter(data, Magnitude>= min(as.numeric(input$aslider2)) & Magnitude <= max(as.numeric(input$aslider2)))
        
        x = copy[,c(4,3)]
        
        if (input$select == 1)#Chile
        {
            copy = filter(x, Longitude>=-75 & Longitude <=-65)
            copy = filter(copy, Latitude>=-60 & Latitude <=-10)
        }
        else if (input$select == 1)#Indonesia
        {
            copy = filter(x, Longitude>=-20 & Longitude <=20)
            copy = filter(copy, Latitude>=25 & Latitude <=70)
        }
        else if (input$select == 1)#Italy
        {
            copy = filter(x, Longitude>=-20 & Longitude <=20)
            copy = filter(copy, Latitude>=25 & Latitude <=70)
        }
        else#Japan
        {
            copy = filter(x, Longitude>=100 & Longitude <=200)
            copy = filter(copy, Latitude>=-25 & Latitude <=25)
        }
        
        bw <- diag(c(1.25, 0.75))
        kde <- ks::kde(x = copy, H = bw)
        
        kde
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
        p <- plot_geo(df, width = 900, height = 500) %>%
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
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.pdf",
        content = function(file) {
            
            brush <- event_data("plotly_brushing")
            
            if (!is.null(brush) && !is.na(brush) && !is.na(brush$geo) && nrow(brush$geo) == 2)
            {
                print(brush$geo)
                
                copy = filter(data, Year>= min(as.numeric(input$slider)) & Year <= max(as.numeric(input$slider)))
                copy = filter(copy, Magnitude>= min(as.numeric(input$slider2)) & Magnitude <= max(as.numeric(input$slider2)))
                copy = filter(copy, Longitude>= brush$geo[1,1] & Longitude <= brush$geo[2,1])
                copy = filter(copy, Latitude>= brush$geo[2,2] & Latitude <= brush$geo[1,2])
                
                # Copy the report file to a temporary directory before processing it, in
                # case we don't have write permissions to the current working dir (which
                # can happen when deployed).
                tempReport <- file.path(tempdir(), "report.Rmd")
                file.copy("C:/Users/codef/Desktop/shinyapp/earthquakes/report.Rmd", tempReport, overwrite = TRUE)
                
                # Knit the document, passing in the `params` list, and eval it in a
                # child of the global environment (this isolates the code in the document
                # from the code in this app).
                rmarkdown::render(tempReport, output_file = file,
                                  params = list(d = copy),
                                  envir = new.env(parent = globalenv())
                )
            }

        }
    )
    
    output$plot1 <- renderPlot({
        
        kde = kdeInput()
        
        plot(kde, main="Italy")
        
        #Plotting with base-graphics, then overlaying map
        #plot(Latitude~Longitude,copy,pch=16,col="red",asp=1)
        map("world",add=TRUE,col="gray",fill=FALSE)
    })
    
    output$value <- renderPrint({ input$select })
}


##########


shinyApp(ui = ui, server = server, options = list(height = 830, width = 830))





