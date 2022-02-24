test_that("streamstats", {

  data <- tidyr::tibble(Lat = c(48.30602, 48.62952, 48.14946),
                        Lon = c(-115.54327, -114.75546, -116.05935),
                        Site = c("Granite Creek", "Louis Creek", "WF Blue Creek"))

  data <- data %>% sf::st_as_sf(coords = c('Lon', 'Lat')) %>% sf::st_set_crs(4326)

  data <- data %>% sf::st_transform(crs = 4326)

  lon <- data.frame(lon = sf::st_coordinates(data)[,1])
  lat <- data.frame(lat = sf::st_coordinates(data)[,2])

  expect_equal(nrow(lon), nrow(lat))

  st <- vector()

  for(i in 1:nrow(data)){

    state <-   maps::map.where(database="state",
                               sf::st_geometry(data)[[i]][[1]], sf::st_geometry(data)[[i]][[2]])
    state <-  state.abb[grep(paste(stringr::str_to_title(state)), state.name)]
    st <- append(st, state)
  }

  expect_equal(nrow(data), length(st))


})
