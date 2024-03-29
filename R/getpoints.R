#' Get Stream Stats Stream Tile
#'
#' @param state Must be a single state abbreviation or full name
#' @return A mapedit result
#' @export
#'
get_StreamStats_points <- function(state){

  if(missing(state)){stop('need a state tile map to choose from')}

  state_num <- data.frame(id = c(0,seq(3,30,3),
                                 seq(34,64,3),
                                 seq(68,143,3)),
                          abb = c('AK','AL','AR','AZ', 'CA', 'CO',
                                  'CRB', 'CT', 'DE', 'DRB', 'GA',
                                  'HI', 'IA', 'ID', 'IL', 'IN', 'KS',
                                  'KY', 'MA', 'MD', 'ME', 'MN', 'MO',
                                  'MO STL', 'MS', 'MT', 'NC', 'ND',
                                  'NH', 'NJ', 'NM', 'NY', 'OH', 'OK',
                                  'OR', 'PA', 'PR', 'RI', 'RRB', 'SC',
                                  'SD', 'TN', 'UT', 'VA', 'VT', 'WA',
                                  'WI', 'WV'))

  state_num <- state_num %>%
               dplyr::filter(abb %in% state)

  grp <- c("Esri.WorldImagery", "CartoDB.Positron",
           "OpenStreetMap", "CartoDB.DarkMatter", "OpenTopoMap",
           "Hydrography")

  att <- paste0("<a href='https://www.usgs.gov/'>", "U.S. Geological Survey</a> | ",
                "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
                "Policies</a>")
  GetURL <- function(service, host = "basemap.nationalmap.gov") {
    sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer",
            host, service)
  }

  state_sf <- USAboundaries::us_states(states = state_num$abb)
  bb <- sf::st_bbox(state_sf)

  map <- leaflet::leaflet()

  map <- leaflet::addProviderTiles(map = map,
                                   provider = grp[[1]],
                                   group = grp[[1]])
  map <- leaflet::addProviderTiles(map = map,
                                   provider = grp[[2]],
                                   group = grp[[2]])
  map <- leaflet::addProviderTiles(map = map,
                                   provider = grp[[3]],
                                   group = grp[[3]])
  map <- leaflet::addProviderTiles(map = map,
                                   provider = grp[[4]],
                                   group = grp[[4]])
  map <- leaflet::addProviderTiles(map = map,
                                   provider = grp[[5]],
                                   group = grp[[5]])
  opt <- leaflet::WMSTileOptions(format = "image/png",
                                 transparent = TRUE)

  map <- leaflet::addWMSTiles(map, GetURL("USGSHydroCached"),
                              group = grp[6], options = opt, layers = "0", attribution = att)

  opt <- leaflet::layersControlOptions(collapsed = TRUE)

  map <- leaflet::addPolygons(map,data = state_sf,
                              fillOpacity = 0,
                              color = 'black',
                              weight = 5)

  map <- leaflet::addLayersControl(map, baseGroups = grp[1:5],
                                   overlayGroups = grp[6], options = opt)

  map <- leaflet::fitBounds(map, bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])

  url <- 'https://gis.streamstats.usgs.gov/arcgis/rest/services/StreamStats/stateServices/MapServer'

  map <- map %>%
    leaflet.esri::addEsriDynamicMapLayer(url = url,
                                         options =  leaflet.esri::dynamicMapLayerOptions(minZoom = 12,
                                                                                         layers = list(state_num$id))) %>%
    mapedit::drawFeatures(map = .)
}
