#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# Define UI for application that draws a histogram
shinyUI(navbarPage( title = "qMaPCR",
                    
                    ########################### PAGE 01 MAPPING ###############################
                    
                    tabPanel("Mapping",
                             leafletOutput("map", width="100%", height="900px"), # Map display on Page 1 
                             
                             ###### Customize Design of Page 01 Using HTML ######
                             tags$head(tags$style(
                                 HTML('
                                  
                                  div.outer {
                                  position: fixed;
                                  top: 41px;
                                  left: 0;
                                  right: 0;
                                  bottom: 0;
                                  overflow: hidden;
                                  padding: 0;
                                  }
                               
                                  
                                  body, label,input,button,select {
                                  font-family: "Helvetica Neue", Helvetica;
                                  font-weight:200;
                                  font-size: 16px;
                                  }
                                  
                                  h1,h2,h3,h4,h5,h6 {font-weight: 300;}
                                  
                                  #controls {
                                  /* Appearance */
                                  background-color: white;
                                  padding: 0 20px 20px 20px;
                                  cursor: move;
                                  /* Fade out while not hovering */
                                  opacity: 0.45;
                                  zoom: 0.9;
                                  transition: opacity 500ms 1s;
                                  }
                                  #controls:hover {
                                  /* Fade in while hovering */
                                  opacity: 0.95;
                                  transition-delay: 0;
                                  }
                                  #sel_date {background-color: rgba(0,0,255,1);}
                                  
                                  .marker-custom-small {
                                  background-color: rgba(253, 156, 115, 0.6);
                                  }
                                  .marker-customr-small div {
                                  background-color: rgba(241, 128, 23, 0.6);
                                  }
                                  
                                  .marker-custom-medium {
                                  background-color: rgba(241, 211, 87, 0.6);
                                  }
                                  .marker-custom-medium div {
                                  background-color: rgba(240, 194, 12, 0.6);
                                  }
                                  
                                  .marker-custom-large {
                                  background-color: rgba(251, 170, 120, 0.6);
                                  }
                                  
                                  .marker-custom-large div {
                                  background-color: rgba(245, 180, 100, 0.6);
                                  }
                                  
                                  
                                  .selectize-input, .selectize-dropdown {
                                  padding: 2px 2px;
                                  min-height: 1px;
                                  min-width: 1px;
                                  }'
                                      
                                 ))),
                             
                             ##### Floating Panel on Page 01 #####
                             
                             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                           width = 350, height = "auto",  # Size of the Floating Panel  
                                           
                                           #tags$br(), 
                                           
                                           # Upload data source  
                                           h4("Upload Data Source for Visualization:"),
                                           
                                           a("Example File", target="_blank",href="map_example_file.xlsx"),
                                           
                                           # Upload data file for visualization
                                           fileInput('file1', 'Data Source File:', accept = c(".xlsx", ".csv")),
                                           
                                           h4("Which Species Are You Looking For?"), # Floating Panel Title
                                           
                                           
                                           # Data Filter by Project Names 
                                           
                                           pickerInput("organism_name_Page_01", "Organism Scope(s)", 
                                                       options = list(`actions-box` = TRUE), 
                                                       choices = NULL, multiple = TRUE), 
                                           
                                           # Data Filter by Species 
                                           
                                           conditionalPanel("input.organism_name_Page_01", pickerInput("species1", "Species", 
                                                                                                      options = list(`actions-box` = TRUE), 
                                                                                                      choices = NULL), multiple = TRUE), 
                                           
                                           
                                           # Data Filter by Dates 
                                           
                                           dateRangeInput("daterange1", "Date range:",
                                                          start = "2015-01-01",
                                                          end   = "2020-12-31"),
                                           
                                           # Floating Panel Subtitle 01 - Customize Legend Range 
                                           
                                           h4("Customize Legend Range:"),
                                           h5("(Starting Point)"),
                                           tags$br(),
                                           
                                           # Select Inputs for how users would like to define the signal intensity ranges. 
                                           
                                           fluidRow(column(5,selectInput(inputId = "verystrong", "Very Strong", choices = 0:40, multiple = FALSE)),
                                                    column(5,selectInput(inputId = "strong", "Strong", choices = 0:40, multiple = FALSE))),
                                           fluidRow(column(5,selectInput(inputId = "moderate", "Moderate", choices = 0:40, multiple = FALSE)),
                                                    column(5,selectInput(inputId = "weak", "Weak", choices = 0:40, multiple = FALSE))))
                             
                    ),
                    
                    
                    ########################### PAGE 02 NEW DATA ###############################
                    
                    tabPanel("New Data",
                             
                             ###### Data Submission Panel ###### 
                             
                             fluidRow(style = "background-color: #DCDCDC;",
                                      
                                      tags$div(tags$blockquote("New Data Submission"), style="padding:16px;"),
                                      
                                      # Select qPCR Machine 
                                      column(4, selectInput(inputId = "platform",  
                                                            label = "Data Source", 
                                                            choices = c("Biomeme three9", "Biomeme two3", "MIC", "Cepheid"),
                                                            multiple = FALSE)),
                                      
                                      # Upload qPCR data here 
                                      column(4, fileInput(inputId = "qpcr", label = "Upload qPCR data (CSV or Excel)",
                                                          accept = c(".csv", ".xls", ".xlsx"))),
                                      
                                      # Upload  metadata here 
                                      column(4, fileInput(inputId = "meta", label = "Upload Metadata (Excel Spreadsheet)",
                                                          multiple = TRUE, accept = c(".xls", ".xlsx"))), 
                                      
                                      tags$br(),
                                      
                                      a("Example Files (.zip)", target="_blank",href="newdata_example_files.zip"), tags$br(),tags$br()
                                      
                                      
                             ),
                             
                             
                             ###### Display of Integrated Datatable (qPCR raw data + metadata) ###### 
                             
                             # Part 1 - Datatable 
                             fluidRow(DT::dataTableOutput(outputId = "data")), 
                             
                             # Part 2 - Choose where the submitted data will be stored 
                             fluidRow(downloadLink("downloadData", "Download")),
                             
                             hr())
                    
                    
))
