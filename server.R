#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(data.table)
library(leaflet.extras)
library(RColorBrewer)
library(tidyverse)
library(shinyWidgets)
library(tibble)
library(openxlsx)
library(DT)
library(leaflet)
library(htmltools)
library(chipPCR)
library(readxl)

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
    
    #################################################### NEW DATA - INTEGRATION (Page 02) #################################################### 
    
    # How the data integration works? 
    
    # In qMaPCR, the data integration works by first transforming the matrix containing \n
    # all the fluorescence data into a single column of strings of values. Each \n
    # string represents the fluorecense data (numerical values with an average length \n
    # of 40 - 45) for one sample. The transformed column will then be combined \n
    # with the metadata in which each row is for one individual sample.  
    
    # The qMaPCR accepts qPCR data from multiple qPCR Machines including:
    # MIC 
    # Biomeme39
    # Biomeme23
    
    ###### Component 01 - Data Integration ######
    input_integration <- reactive({
        if (is.null(input$qpcr))
        {return(NULL)} else {
            
            
            ### Machine Option 1: MIC ###
            if (input$platform == "MIC") {
                
                # Read qPCR data
                qpcr.raw.mic <- read.csv(input$qpcr$datapath)
                
                # Read metadata data
                metadata.mic <- as.data.frame(openxlsx::read.xlsx(input$meta$datapath, rows=(1:49)))
                metadata.mic$eventDate <- as.Date(metadata.mic$eventDate, origin = "1900-01-01")
                
                # Create a dataframe for storing the transformed qPCR data and calculated Ct values. 
                qpcr.convert.mic <- tibble(flr_raw = character(), 
                                           ct_value = numeric())
                
                # Prepare parameters for Ct value calculation 
                number.sample <- length(qpcr.raw.mic)-1
                number.cycle <-  nrow(qpcr.raw.mic)
                cycle <- seq(number.cycle)
                
                # Convert qPCR data into a column of strings of values 
                for (i in 1:number.sample) {
                    qpcr.convert.mic[i,1] <- toString(qpcr.raw.mic[,i+1])
                }
                
                # Calculate Ct values  
                for (i in 1:number.sample) {
                    flr <- as.numeric(unlist(str_split(qpcr.convert.mic[i,1], pattern = ",")))
                    th.cyc <- th.cyc(cycle, flr, r = round(metadata.mic$threshold[i], 3), linear = TRUE)
                    qpcr.convert.mic[i,2] <- th.cyc[1]
                }
                
                #When there is no significant increase in fluorescence signals, 
                #the calculated Ct values may go extreme. Here I am assigning those Ct values to 100.  
               qpcr.convert.mic$ct_value[qpcr.convert.mic$ct_value<0] <- 100
               qpcr.convert.mic$ct_value[qpcr.convert.mic$ct_value > 40] <- 100

                # Combine the transformed qPCR data with the metadata 
                metadata.mic <- cbind(metadata.mic, qpcr.convert.mic)
                
                # This integrated datatable will then be displayed on Page 2 - New Data of qMaPCR. 
                metadata.mic
                
            } 
            
            ### Machine Option 2: Biomeme three9 ###
            
            else if (input$platform == "Biomeme three9") {
                
                ### Note: Biomeme39 has 45 cycles run for each sample. 
                
                # Read qPCR data
                qpcr.baseline.biomeme39 <- openxlsx::read.xlsx(input$qpcr$datapath, rows=c(15:43), cols = c(1:46))
                
                # Read metadata data
                metadata.biomeme.39 <- as.data.frame(openxlsx::read.xlsx(input$meta$datapath, rows=(1:30)))
                metadata.biomeme.39$eventDate <- as.Date(metadata.biomeme.39$eventDate, origin = "1900-01-01")
                
                # Create a dataframe for storing the transformed qPCR data and calculated Ct values. 
                qpcr.string.biomeme39 <- tibble(run_location=character(), 
                                                flr_raw=character(),
                                                ct_value= numeric())
                
                # Prepare parameters for Ct value calculation 
                number.sample <- nrow(qpcr.baseline.biomeme39)
                number.cycle <- ncol(qpcr.baseline.biomeme39)-1 
                cycle <- seq(number.cycle)
                
                # Convert qPCR data into a column of strings of values 
                for (i in 1:number.sample) {
                    qpcr.string.biomeme39[i,1] <- qpcr.baseline.biomeme39[i,1]
                    qpcr.string.biomeme39[i,2] <- toString(qpcr.baseline.biomeme39[i,-1])
                }
                
                # Calculate Ct values  
                for (i in 1:number.sample) {
                    flr <- as.numeric(unlist(str_split(qpcr.string.biomeme39[i,2], pattern = ",")))
                    th.cyc <- th.cyc(cycle, flr, r = round(metadata.biomeme.39$threshold[i], 3), linear = TRUE)
                    qpcr.string.biomeme39[i,3] <- round(as.numeric(th.cyc[1]),4)
                }
                
                #When there is no significant increase in fluorescence signals, 
                #the calculated Ct values may go extreme. Here I am assigning those Ct values to 100.  
                qpcr.string.biomeme39$ct_value[qpcr.string.biomeme39$ct_value<0] <- 100
                qpcr.string.biomeme39$ct_value[qpcr.string.biomeme39$ct_value > 40] <- 100
                qpcr.string.biomeme39 <- qpcr.string.biomeme39[,-1]
                
                # Combine the transformed qPCR data with the metadata 
                merged.39 <- cbind(metadata.biomeme.39, qpcr.string.biomeme39)
                
                # This integrated datatable will then be displayed on Page 2 - New Data of qMaPCR. 
                merged.39
                
            } 
            
            
            
            ### Machine Option 3: Biomeme Biomeme two3 ###
            
            else if (input$platform == "Biomeme two3") {
                
                # Read qPCR data
                qpcr.baseline.biomeme23 <- openxlsx::read.xlsx(input$qpcr$datapath,rows=c(10:17),cols=c(1:41))
                
                # Read metadata data
                metadata.biomeme.23 <- as.data.frame(openxlsx::read.xlsx(input$meta$datapath, rows=(1:8)))
                metadata.biomeme.23$eventDate <- as.Date(metadata.biomeme.23$eventDate, origin = "1900-01-01")
                metadata.biomeme.23$threshold <- as.numeric(metadata.biomeme.23$threshold)
                
                # Create a dataframe for storing the transformed qPCR data and calculated Ct values. 
                qpcr.string.biomeme23 <- tibble(run_location=character(), 
                                                flr_raw=character(), 
                                                ct_value=numeric())
                
                
                # Prepare parameters for Ct value calculation 
                number.sample <- nrow(qpcr.baseline.biomeme23)
                number.cycle <- ncol(qpcr.baseline.biomeme23)-1
                cycle <- seq(number.cycle)
                
                
                
                # Convert qPCR data into a column of strings of values 
                for (i in 1:number.sample) {
                    qpcr.string.biomeme23[i,1] <- qpcr.baseline.biomeme23[i,1]
                    qpcr.string.biomeme23[i,2] <- toString(qpcr.baseline.biomeme23[i,-1]) 
                    # Threshold in the summary is for the baseline substracted data. 
                }        
                
                # Calculate Ct values  
                for (i in 1:number.sample) {
                    flr <- as.numeric(unlist(str_split(qpcr.string.biomeme23[i,2], pattern = ",")))
                    th.cyc <- th.cyc(cycle, flr, r = round(metadata.biomeme.23$threshold[i], 3), linear = TRUE)
                    qpcr.string.biomeme23[i,3] <- round(as.numeric(th.cyc[1]),4)
                }
                
                #When there is no significant increase in fluorescence signals, 
                #the calculated Ct values may go extreme. Here I am assigning those Ct values to 100.  
                qpcr.string.biomeme23$ct_value[qpcr.string.biomeme23$ct_value<0] <- 100
                qpcr.string.biomeme23$ct_value[qpcr.string.biomeme23$ct_value>48] <- 100
                
                # Combine the transformed qPCR data with the metadata 
                merged.23 <- base::merge(metadata.biomeme.23, qpcr.string.biomeme23, by = "run_location")
                
                # This integrated datatable will then be displayed on Page 2 - New Data of qMaPCR. 
                merged.23
                
            }
        }
    }) 
    
    ###### Component 02 - Integrated Data Display ######
    
    # Objectives: 
    # Create functions to make datatable editable
    # Add delete button to each row of the datatable 
    # Able to download data 
    
    shinyInput <- function(FUN, len, id, ...) {
        inputs <- character(len)
        for (i in seq_len(len)) {
            inputs[i] <- as.character(FUN(paste0(id, i), ...))
        }
        inputs
    }
    
    all <- reactive({
        if (is.null(input$meta)) {return(NULL)} else {
            data = as.data.table(input_integration()) 
            data <- data %>% 
                mutate(Remove = shinyInput(actionButton, nrow(input_integration()), 'button_', label = "Delete", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ))
            data }
    })
    
    # Put the integrated datatable into a reactive environment 
    df <- reactiveValues(
        all = NULL,
        data = all
    )
    
    # set vals$df whenever df updates (Make df reactive)
    observe({
        df$data <- all()
    })
    
    # Everytime users click on the delete button, that row will be removed. 
    observeEvent(input$select_button, {
        selectedRow <- as.numeric(strsplit(as.character(input$select_button), "_")[[1]][2])
        df$data <- df$data[rownames(df$data) != selectedRow, ]
    })
    
    #Datatable Display on UI 
    output$data <- DT::renderDataTable({
        DT::datatable(df$data[colnames(df$data) != c("flr")], editable = TRUE, escape = FALSE, options = list(scrollX=TRUE))
    }) 
    
    
    ###### Component 03 - Data Download ######
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("qMaPCR", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            write.xlsx(df$data[,1:(length(df$data)-1)], file, row.names=TRUE)
        }
    )
    
    
    
    ########################################################## MAP (Page 01) ##########################################################  
    ###### Component 01 - Absolute Panel and Select Data ###### 
    #### Component 1.1 - Read Input File (all data) ####
    # Function for reading all the content from an Excel Spreadsheet 
    read_excel_allsheets <- function(filename, tibble = FALSE) {
        sheets <- readxl::excel_sheets(filename)
        x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
        if(!tibble) x <- lapply(x, as.data.frame)
        names(x) <- sheets
        x
    }
    # Read all the data from the input file (including all the worksheets)
    all.data <- reactive({
        if (is.null(input$file1)) {return(NULL)} else {
            data.all <- read_excel_allsheets(input$file1$datapath)
            data.length <- length(data.all)
            all.data <- tibble()
            for (i in 1:data.length) {
                sub_data <- as.data.frame(data.all[[i]]) 
                sub_data$eventDate <- as.Date(sub_data$eventDate, format = "%Y-%m-%d", origin = "1900-01-01")
                all.data <- rbind(all.data,sub_data)
            }
            all.data 
        }
    })
    
    #### Component 1.2 - Project Input ####
    scope_list <- reactive({
        if (!is.null(input$file1)) {
            data <- as.data.frame(all.data())
            scope <- as.character(unique(data$organismScope))
            return(scope)} else {
                return(NULL)}
    })
    
    observe({
        updatePickerInput(session, "organism_name_Page_01", 
                          choices = scope_list()) 
    })
    
    #### Component 1.3 - Species Input ####
    observe({
        species <- if (is.null(input$organism_name_Page_01)) character(0) else {
            data <- as.data.frame(all.data())
            filter(data, organismScope %in% input$organism_name_Page_01) %>%
                `$`('species') %>%
                unique() %>%
                sort()
        }
        updatePickerInput(session, "species1", choices = species)
    })
    
    #### Component 1.4 - Filtered Data ####
    filtered.data <- reactive({
        
        all.data <- as.data.frame(all.data())
        start <- as.Date(input$daterange1[1], origin = "1970-01-01")
        end <- as.Date(input$daterange1[2], origin = "1970-01-01")
        
        if (is.null(input$organism_name_Page_01)) {
            filtered.data <- data.frame()
            filtered.data   # Checked - No Issue Here. 
        } else if (is.null(input$species1)) {
            filtered.data <- data.frame()
            filtered.data   # Checked - No Issue Here. 
        } else {
            all.data %>% 
                filter(organismScope %in% input$organism_name_Page_01) %>% 
                filter(species %in% input$species1) %>% 
                filter(eventDate >= start & eventDate <= end)
        }
        
    })
    
    
    ###### Component 02 - Spacial Visualization by R Leaflet ######
    #Color Palette for Signal Intensity Legend: 
    # Previous Version:
    # Presence - 5 Levels 
    # Darkest Red - #b30000
    # Red - #ee0000
    # Lighter Red - #ff9f9f
    # Pink - #ffc7c7
    # Whitish Pink - #ffeeee
    # Absence - 1 Level
    # Green - #00b300
    
    # Updated Version (Colorblind-friendly)
    
    #### Component 2.1 - Palette/Legend Creation ####
    #Define the Palette 
    palette_map <- c("#D55E00", "#CC79A7", "#0072B2", "#F0E442", "#009E73")
    #Cut Ct Values Into Customized Intensity Categories Based On User Demand 
    ct_cut <- reactive({
        if (is.null(input$file1)) {
            ct_value <- c(10,20,30,40,50,100)
            cut <- cut(ct_value, 
                       c(0,20,30,35,40,100),
                       include.lowest = TRUE,
                       labels = c('Very Strong', 'Strong', 'Moderate', 'Weak', 'None'))
        } else {
            
            if (input$weak==0) {
                data <- as.data.frame(filtered.data())
                ct_value <- as.numeric(data$ct_value)
                cut <- cut(ct_value, 
                           c(0,20,30,35,40,100),
                           include.lowest = TRUE,
                           labels = c('Very Strong', 'Strong', 'Moderate', 'Weak', 'None'))
            } else {
                data <- as.data.frame(filtered.data())
                ct_value <- as.numeric(data$ct_value)
                cut <- cut(ct_value, 
                           c(input$verystrong,input$strong,input$moderate,input$weak,49,100),
                           include.lowest = TRUE,
                           labels = c('Very Strong', 'Strong', 'Moderate', 'Weak', 'None'))
            }
            cut
        }
    })
    
    #### Component 2.2 - Map ####
    #1. Create a basic map 
    output$map <- renderLeaflet({
        
        leaflet() %>% 
            addTiles() %>%
            addLegend(position = 'topleft', 
                      colors = palette_map, 
                      labels = c('Very Strong', 'Strong', 'Moderate', 'Weak', 'None'),
                      title = 'qPCR Signal Intensity',
                      opacity = 0.6) %>% 
            addFullscreenControl() %>% 
            setView(lng = -79.3832, lat = 43.6532, zoom = 3)
    })
    
    #2. Update the map based on user input 
    observe({
        
        df <- filtered.data()
        
        if(nrow(df) == 0) {
            leafletProxy("map") %>% 
                clearMarkers() %>% 
                clearMarkerClusters() %>% 
                clearPopups() 
            
        } else {
            
            #3. Create the content for pop-windows 
            content <- reactive({
                data <- as.data.frame(filtered.data())
                content.output <- paste("<strong><h5>Species:", data$species, "</strong>",
                                        "<strong><h5> NCBI Taxon ID:", "<a href='https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=", 
                                        data$taxonID, "'>", data$taxonID, "</a>", "</strong>",
                                        "<strong><h5>Ct Value:", data$ct_value, "</strong>",
                                        "<br><h6>qPCR Device:", data$run_platform,
                                        "<br><h6>Common Name:", data$vernacularName,
                                        "<br><h6>Event Date:", data$eventDate,
                                        "<h6>Event Coordinate(Lat, Lon):", data$decimalLatitude, data$decimalLongtitude,
                                        "<h6>Event Identifer(s):", data$recordedBy)
                return(content.output)
            })
            
            df <- filtered.data()
            
            pal <- colorFactor(palette = palette_map, 
                               ct_cut())
            
            leafletProxy("map", data = df) %>% 
                clearMarkers() %>% 
                clearMarkerClusters() %>% 
                clearPopups() %>% 
                addCircleMarkers(lng = ~decimalLongtitude, 
                                 lat = ~decimalLatitude, 
                                 popup = content(), 
                                 popupOptions = popupOptions(closeButton=FALSE), 
                                 group = filtered.data(),
                                 color = ~pal(ct_cut()), 
                                 fillOpacity = 0.6, 
                                 opacity = 0.8,
                                 clusterOptions = markerClusterOptions(showCoverageOnHover = TRUE, 
                                                                       zoomToBoundsOnClick = TRUE,
                                                                       spiderfyOnMaxZoom = TRUE, 
                                                                       spiderfied = TRUE,
                                                                       animationed = TRUE, 
                                                                       spiderfyDistanceMultiplier = 2,
                                                                       removeOutsideVisibleBounds = TRUE,
                                                                       spiderLegPolylineOptions = list(weight = 2.0, 
                                                                                                       color = "#ffffff",
                                                                                                       opacity =0.8)))
        }
    })
  
    
    
    ########################################################## View Data (Page 03) ##########################################################  
    ###### Component 01 - View data being uploaded #####
    output$data_page_03 <- DT::renderDataTable({
        DT::datatable(as.data.frame(filtered.data()), editable = FALSE, escape = FALSE, options = list(scrollX=TRUE))
    })
    
    
}
)
