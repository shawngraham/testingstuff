devtools::install_github("tarakc02/rmapzen")
Sys.setenv(MAPZEN_KEY = "mapzen-gcMb3SB")

install.packages('leaflet')
library('leaflet')
library(rmapzen)

mz_geocode("Santuario Vescovio, Torri in Sabina, Italia")

fn <- mz_geocode("Santuario Vescovio, Torri in Sabina, Italia")
isos <- mz_isochrone(
  fn,
  costing_model = mz_costing$pedestrian(),
  contours = mz_contours(c(5, 10, 20, 30))
)

library(leaflet)
leaflet(as_sp(isos)) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(color = ~paste0("#", color), weight = 1) %>%
  addLegend(colors = ~paste0("#", color), 
            labels = ~paste(contour, "minutes"),
            title = "Walking times from <br/> Forum Novum")



# first geocode the TransAmerica Center
paterson <- mz_geocode("Paterson Hall, Carleton University, Ottawa, Ontario")

# then identify the area that is within 15 minutes walking distance
walkable <- mz_isochrone(
  paterson,
  costing_model = mz_costing$pedestrian(),
  contours = mz_contours(15))

# now search for museums, but limit the search to areas within "walkable"
stores <- mz_search(
  "burger", 
  boundary.rect = mz_bbox(walkable), 
  layers = mz_layers$venue, 
  size = 15
)

# use the leaflet package to draw the map
leaflet(as_sp(walkable)) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(color = "#ffffff", weight = 1) %>%
  addMarkers(
    data = paterson, 
    lat = ~geocode_latitude, 
    lng = ~geocode_longitude,
    popup = "Paterson Hall") %>%
  addCircleMarkers(
    data = as_sp(stores), 
    weight = 1,
    radius = 7,
    opacity = 1,
    popup = ~name,
    color = "#ff0000")
