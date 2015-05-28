library(dplyr)
library(leafletR)
library(readr)
library(sp)
library(stringr)
library(XML)


# load data

## power plant locations

## scrape table from Wikipedia page
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