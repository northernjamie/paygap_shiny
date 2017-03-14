
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; library(raster) ; library(SPARQL) ; library(DT) ; library(reshape2) ; library(ggplot2)

endpoint <- "http://ons.publishmydata.com/sparql"
query <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT ?value ?areaname ?areacode1 ?sex ?stat
WHERE 

{ 
?s qb:dataSet <http://statistics.data.gov.uk/data/ashe-earnings> ;
<http://statistics.data.gov.uk/def/dimension/earningsStatistics> ?statcode ;
<http://purl.org/linked-data/cube#measureType> <http://statistics.data.gov.uk/def/measure-properties/value> ;
<http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?areacode ;
<http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> <http://reference.data.gov.uk/id/year/2016> ;
<http://purl.org/linked-data/sdmx/2009/dimension#sex> ?sexcode ;
<http://statistics.data.gov.uk/def/dimension/workingPattern> <http://statistics.data.gov.uk/def/concept/working-pattern/full-time> ;
<http://statistics.data.gov.uk/def/measure-properties/value> ?value ;
<http://statistics.data.gov.uk/def/dimension/earnings> <http://statistics.data.gov.uk/def/concept/earnings/annual-pay-gross> .
?areacode <http://statistics.data.gov.uk/def/statistical-geography#officialname> ?areaname ;
skos:notation ?areacode1 .
?sexcode rdfs:label ?sex .
?statcode rdfs:label ?stat .
}"

# SPARQL is too big to run succesfully
#qd <- SPARQL(endpoint,query)

# Load the constituency geography file
parlconst <- readOGR("westminster_const_lat_long_condensed.geojson", "OGRGeoJSON")

# Load the csv file containing the pay data
pgdata <- read.csv2("paygapdatalong.csv",header = TRUE, sep=",")

#Turn the value column into from scientific notation to number
pgdata <- transform(pgdata, value = as.numeric(value))

pgdata <- as.data.frame(pgdata)
pgdata2 <- pgdata[ which(pgdata$year == "2016" & pgdata$stat == "Median"), ]
#pgdata <- subset(pgdata, year == "2016" & stat == "Median")
pgdata2 <- dcast(pgdata2,areaname + areacode1 + year ~ sex, value.var = 'value')
pgdata2$gap <- with(pgdata2,Male - Female)

pgdist <- pgdata[ which(pgdata$year == "2016"), ]

pgdist2 <- dcast(pgdist,areaname + areacode1 + year + stat ~ sex, value.var = 'value')

pgdist2$gap <- with(pgdist2, Male - Female)

pgdist2Nat <- pgdist2[ which(pgdist2$areacode1 == "K03000001"),]



server <- (function(input, output, session) {
  
  # This bit is the bit that responds to the slider on the UI
  #selected <- reactive({
  # subset(smokpreg,
  #        percsmoking < input$smokerange[2] & percsmoking >= input$smokerange[1])
  #})
  
  selected <- reactive({
    subset(pgdata2,
           gap < input$paygaprange[2] & gap >= input$paygaprange[1])
  })
  
  # Put the default map co-ordinates and zoom level into variables
  lat <- 53.542788
  lng <- -3.144708
  zoom <- 6
  
  # Draw the map
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      #maybe set a more colourful map background..?
      addProviderTiles("CartoDB.Positron") %>% 
      setView(lat = lat, lng = lng, zoom = zoom) %>%
      addPolygons(data = parlconst, opacity=1, color = "black", weight = 1, fillOpacity=0.5, layerId = parlconst$CODE)
    
  })
  
  # Draw the table (the columns need to match with all those in selected())
  output$table <- DT::renderDataTable({
    data.frame(x=selected())}, colnames = c('areaname','areacode1','year','All','Female','Male','gap'), options = list(order = list(7,'desc')))
  
  #
  observe({
    
    #merge the data from the csv / sparql with the geojson data for mapping
    parlconst@data <- left_join(parlconst@data, selected(), by=c("CODE"="areacode1"))
    
    #sets the colour range to be used on the choropleth
    qpal <- colorNumeric("RdYlGn", pgdata2$gap, na.color = "#bdbdbd")
    
    #the popup on the map
    popup <- paste0("<h5>",parlconst$NAME,"</h5><br /><h3>£",parlconst$gap,"</h3>")
    
    #draw the map with stuff on
    leafletProxy("map", data = parlconst) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(data = parlconst, fillColor = ~qpal(gap), fillOpacity = 0.7, 
                color = "#bdbdbd", weight = 1, popup = popup, layerId = parlconst$CODE) %>%
      addLegend(pal = qpal, values = ~gap, opacity = 0.7,
                position = 'bottomleft', 
                title = paste0("The Pay Gap"))
  })
  
  observe({
    input$reset_button
    leafletProxy("map") %>% setView(lat = lat, lng = lng, zoom = zoom)
  })
  
  observe({
    click<-input$map_shape_click
    if(is.null(click))
      return()
   
    available <- pgdata2[ which(pgdata2$areacode1 == click$id), ]
    
    text2 <- paste0("Constituency: ", available[1,1], " (", available[1,2],")")
    
    #text2<-paste("You've selected shape: ", click$id)
    output$const_name<-renderText({
      text2
    })
    
    pgdist2Const <- pgdist2[ which(pgdist2$areacode1 == available[1,2]),]
    output$plot1 <- renderPlot({
      ggplot() + geom_bar(data = pgdist2Const, aes(x=stat,y=All), stat="identity") + coord_flip() + geom_point(data = pgdist2Nat, aes(x=stat,y=All), stat="identity")
    })
    
  })
  
  
  
})
