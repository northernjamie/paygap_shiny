
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; library(raster) ; library(SPARQL) ; library(DT); library(ggplot2)

shinyUI(fluidPage(
  fluidRow(
    column(7, offset = 1,
           br(),
           div(h3("Gender Pay Gap")),
           div(h4(textOutput("const_name"))),
           div(h4("The gap between male and female pay by parliamentary constituency")),
           div(h4(textOutput("title"), align = "center"), style = "color:black"),
           div(h5(textOutput("period"), align = "center"), style = "color:black"),
           br())),
  fluidRow(
    column(7, offset = 1,
           tabsetPanel(
             tabPanel("Map",leafletOutput("map", height="600"),
                      br(),
                      actionButton("reset_button", "Reset view")),
             tabPanel("Table",DT::dataTableOutput("table"))
           )
    ),
    column(3,
           sliderInput("smokerange", "Choose the range of values you would like to display", min = -5000, max = 20000, value = c(-5000,20000)),
           plotOutput("plot1"),
           br()))
  
))
