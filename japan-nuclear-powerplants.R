# initialize
## load libraries
library(leafletR)
library(dplyr)
library(readr)
library(maptools)
library(sp)
library(stringr)
library(XML)

## initialize fuctions
### character encoding conversion SJIS -> UTF8 for data frames
dfSjis2Utf8 <- function(data){
    for (i in colnames(data)){
        data[, i] <- iconv(data[, i], "CP932", "UTF-8")
    }
    return(data)
}

### character encoding conversion SJIS -> UTF8 for spatial data frames
spSjis2Utf8 <- function(sp.data){
    for (i in colnames(sp.data@data)){
        sp.data@data[, i] <- iconv(sp.data@data[, i], "CP932", "UTF-8")
    }
    return(sp.data)
}



# load data

## power plant locations

### scrape table from Wikipedia page
wikiUrl <- "http://en.wikipedia.org/wiki/Nuclear_power_in_Japan"
tables <- readHTMLTable(wikiUrl)
powerplants <- tables[[1]] %>% 
    select(name = Station, 
           status = Status, 
           coordinates = Coordinates)

### extract coordinates
### (regex from http://download.e-bookshelf.de/download/0002/9258/04/L-G-0002925804-0005490991.pdf)
y_coords <- str_extract(powerplants$coordinates, 
                        "[/][ -]*[[:digit:]]*[.]*[[:digit:]]*[;]")
y_coords <- as.numeric(str_sub(y_coords, 3, -2))
powerplants$y <- y_coords
x_coords <- str_extract(powerplants$coordinates, 
                        "[;][ -]*[[:digit:]]*[.]*[[:digit:]]*")
x_coords <- as.numeric(str_sub(x_coords, 3, -1))
powerplants$x <- x_coords
powerplants$coordinates <- NULL
### remove missing and erroneous data
powerplants <- filter(powerplants, 
                      (!is.na(x) & !is.na(y)) & status != "Withdrawn")
### geocode
coordinates(powerplants) = ~ x + y
proj4string(powerplants) = CRS("+init=epsg:4326")

### clean status columns
powerplants$status <- gsub("[[[:digit:]]*]", "", powerplants$status)
powerplants$status <- gsub("\n", " ", powerplants$status)


## census data
## data: http://www.e-stat.go.jp/SG1/estat/CsvdlE.do?sinfid=000012460662
## type code table: http://www.stat.go.jp/data/kokusei/2010/users-g/pdf/03.pdf
### load data, convert encoding to UTF8, and filter to relevant units
census <- read_csv("./data/census.csv", 
                   skip = 10, 
                   col_types = "__cci_ci____d", 
                   col_names = c("code", "type", "year", 
                                 "name", "pop", "area")) %>% 
    filter(type %in% c("0", "2", "3")) %>% 
    as.data.frame() %>% 
    dfSjis2Utf8()


## census locations
## data: http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03.html -> SHP
### unzip data
unzip("./data/shichouson2010.zip", exdir = "./data")
### load data and convert encoding to UTF8
shichouson <- readShapePoly("./data/N03-14_140401.shp", 
                            proj4string=CRS("+init=epsg:4612")) %>% 
    spSjis2Utf8()
### remove temporary data
file.remove(paste0(rep("./data/N03-14_140401.", 2), 
                   c("dbf", "prj", "sbn", "sbx", "shp", "shx")))
### extract, dissolve and filter attribute data
shichousonData <- shichouson@data %>% 
    select(code = N03_007, 
           name = N03_004, 
           pref = N03_001) %>% 
    group_by(code) %>% 
    summarise(name = min(name), 
              pref = min(pref)) %>% 
    filter(!is.na(code)) %>%
    as.data.frame
### dissolve multi-part polygons
shichouson <- unionSpatialPolygons(shichouson, IDs = shichouson$N03_007)
### combine polygon andf attribute data
row.names(shichousonData) <- shichousonData$code
shichouson <- SpatialPolygonsDataFrame(shichouson, shichousonData)


# plot map using Leaflet
powerplantsJSON <- toGeoJSON(powerplants, 
                             dest = "./data")
style <- styleCat(prop = "status", 
                  val = unique(powerplants$status), 
                  style.val = c("black", "darkred", "red", "orange", "yellow", "grey"), 
                  leg = "Status")
leaflet(powerplantsJSON, 
        title = "Nuclear Power Plants in Japan", 
        style = style, 
        popup = c("name", "status"), 
        base.map = "tonerlite", 
        incl.data = TRUE)