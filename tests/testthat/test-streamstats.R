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

  group <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(group = dplyr::row_number()) %>%
    dplyr::select(group)

  testthat::skip_on_cran()

  usgs_raws <- data.frame(lat = lat, lon = lon, group = group[,1], state = st, crs = 4326)

  watersheds <-  usgs_raws %>%
    split(.$group) %>%
    purrr::map(purrr::safely(~dl_ws(.))) %>%
    purrr::keep(~length(.) != 0) %>%
    purrr::map(~.x[['result']])

  expect_equal(nrow(usgs_raws), length(watersheds))

  final_df <-
    purrr::map(watersheds, purrr::safely(~get_flow_basin(.)))%>%
    purrr::map(~.x[['result']]) %>%
    plyr::rbind.fill()

  expect_equal(nrow(data), nrow(watersheds))
})
