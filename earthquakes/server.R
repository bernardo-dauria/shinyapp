
server <- function(input, output) {
    
    dataInput = reactive({
        
        copy = filter(data, Year>= min(as.numeric(input$slider)) & Year <= max(as.numeric(input$slider)))
        copy = filter(copy, Magnitude>= min(as.numeric(input$slider2)) & Magnitude <= max(as.numeric(input$slider2)))
        
        copy
    })
    
    kdeInput = reactive({
        
        copy = filter(data, Magnitude>= min(as.numeric(input$aslider2)) & Magnitude <= max(as.numeric(input$aslider2)))
        
        x = copy[,c(4,3)]
        r = list("xlim"=c(),"ylim"=c(),"kde"=c())
        
        if (input$select == 1)#Chile
        {
            copy = filter(x, Longitude>=-90 & Longitude <=-50)
            copy = filter(copy, Latitude>=-50 & Latitude <=-15)
            r[["xlim"]] = c(-90,-50)
            r[["ylim"]] = c(-50,-15)
        }
        else if (input$select == 2)#Indonesia
        {
            copy = filter(x, Longitude>=100 & Longitude <=170)
            copy = filter(copy, Latitude>=-23 & Latitude <=23)
            r[["xlim"]] = c(100,170)
            r[["ylim"]] = c(-23,23)
        }
        else#Japan
        {
            copy = filter(x, Longitude>=125 & Longitude <=150)
            copy = filter(copy, Latitude>=25 & Latitude <=50)
            r[["xlim"]] = c(125,150)
            r[["ylim"]] = c(25,50)
        }
        
        bw = diag(c(1, 1))
        
        if (nrow(copy) > 0)
        {
            kde = ks::kde(x = copy, H = bw)
            r[["kde"]] = kde
        }
        
        r
    })
    
    output$plot <- renderPlotly({
        
        df = dataInput()
        
        g <- list(
            showland = TRUE,
            landcolor = toRGB("gray85"),
            subunitwidth = 1,
            countrywidth = 1,
            list(lonaxis = list(range = c(-30, 30))),
            list(lataxis = list(range = c(-30, 30))),
            subunitcolor = toRGB("white"),
            countrycolor = toRGB("white")
        )
        
        p <- plot_geo(df, width = 900, height = 500) %>%
            add_markers(
                x = ~Longitude, y = ~Latitude, size = ~Magnitude, color = ~Depth, colors = viridis(2)[c(2,1)],hoverinfo = "text", sizes = range(df$ScaledMagnitude),
                text = ~paste(" Magnitude:", df$Magnitude, "<br /> Depth:", df$Depth, "<br /> Year:", df$Year),
                brush = ~brushOpts("plot_brush")
            ) %>%
            layout(title = paste(nrow(df),"earquakes"), geo = g)
    })
    
    observe({
        brush <- event_data("plotly_brushing")
        
        if (!is.null(brush) && !is.na(brush) && !is.na(brush$geo) && nrow(brush$geo) == 2)
        {
            print(brush$geo)
            
            copy = filter(data, Year>= min(as.numeric(input$slider)) & Year <= max(as.numeric(input$slider)))
            copy = filter(copy, Magnitude>= min(as.numeric(input$slider2)) & Magnitude <= max(as.numeric(input$slider2)))
            copy = filter(copy, Longitude>= brush$geo[1,1] & Longitude <= brush$geo[2,1])
            copy = filter(copy, Latitude>= brush$geo[2,2] & Latitude <= brush$geo[1,2])
            
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
            copy = copy[,-8]
            
            print(paste("len",nrow(copy)))
            datatable(copy)
        }
    })
    
    output$report <- downloadHandler(
        
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
                copy = copy[,-8]
                
                tempReport <- file.path(tempdir(), "report.Rmd")
                file.copy("C:/Users/codef/Desktop/shinyapp/earthquakes/report.Rmd", tempReport, overwrite = TRUE)
                
                rmarkdown::render(tempReport, output_file = file,
                                  params = list(d = copy),
                                  envir = new.env(parent = globalenv())
                )
            }
            
        }
    )
    
    output$plot1 <- renderPlot({
        
        r = kdeInput()
        
        plot(r[["kde"]], main="Density Contours", ylim=r[["ylim"]], xlim=r[["xlim"]])
        map("world",add=TRUE,col="ghostwhite",fill=TRUE)
        plot(r[["kde"]], main="Density Contours", ylim=r[["ylim"]], xlim=r[["xlim"]],add=TRUE ,col="firebrick2")
        
    }, height = 600, width = 600)
    
    
    output$value <- renderPrint({ input$select })
}

