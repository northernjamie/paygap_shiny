library(shiny) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; library(raster) ; library(SPARQL) ; library(DT)

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

qd <- SPARQL(endpoint,query)