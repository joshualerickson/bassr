
#' @title Download Multiple Stream Stats Locations
#'
#' @description Takes sf point object and returns watershed boundary (sf). Uses state to generate basin
#' delineation(s).
#' @param data sf data.frame with POINT geometry
#' @param group A vector to group by. \code{optional}
#' @param crs A \code{numeric} crs value
#' @param parallel \code{logical} indicating whether to use future_map(). (experimental)
#' @param ... additional arguments to pass to furrr::future_map().
#'
#' @note The use of \code{parallel} is likely to not return all of the requests.
#'
#' @importFrom Rdpack reprompt
#' @importFrom sf st_as_sf
#' @importFrom purrr map safely
#' @importFrom tidyr nest pivot_wider
#' @importFrom dplyr group_by left_join select filter mutate row_number tibble
#' @importFrom plyr rbind.fill
#' @importFrom magrittr "%>%"
#' @return Returns an sf (simple feature) object with associated basin characteristics.
#' @export
#'
#' @examples \dontrun{
#' # Bring in data
#'
#' data <- tidyr::tibble(Lat = c(48.30602, 48.62952, 48.14946),
#'                  Lon = c(-115.54327, -114.75546, -116.05935),
#'                    Site = c("Granite Creek", "Louis Creek", "WF Blue Creek"))
#'
#' data <- data %>%
#'          sf::st_as_sf(coords = c('Lon', 'Lat')) %>%
#'          sf::st_set_crs(4326)
#'
#' three_sites <- batch_StreamStats(data,
#'                                  group = 'Site',
#'                                  crs = 4326,
#'                                  parallel = FALSE)
#'
#' }



batch_StreamStatsWS <- function(data, group, crs = 4326, parallel = FALSE, ...){


  if(!'POINT' %in% sf::st_geometry_type(data)){usethis::ui_stop("Need a sf data.frame with POINT geometry")}
  data <- data %>% sf::st_transform(crs = crs)
  lon <- data.frame(lon = sf::st_coordinates(data)[,1])
  lat <- data.frame(lat = sf::st_coordinates(data)[,2])

  if(missing(group)){

    group <- data %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(group = dplyr::row_number()) %>%
      dplyr::select(group)

  } else {

    group <- data  %>%
      sf::st_drop_geometry() %>%
      dplyr::select(group = {{group}})
  }

  # Create a vector of state abbreviations

  st <- vector()

  for(i in 1:nrow(data)){

    state <-   maps::map.where(database="state",
                               sf::st_geometry(data)[[i]][[1]], sf::st_geometry(data)[[i]][[2]])
    state <-  state.abb[grep(paste(stringr::str_to_title(state)), state.name)]
    st <- append(st, state)
  }


  if(!nrow(group) == nrow(lat)) {usethis::ui_stop("group is not the same length as lat and lon")}

  usgs_raws <- data.frame(lat = lat, lon = lon, group = group[,1], state = st, crs = crs)

  if(isTRUE(parallel)){

    watersheds <-  usgs_raws %>%
      split(.$group) %>%
      furrr::future_map(purrr::safely(~dl_ws(.x)),
                        .options = furrr::furrr_options(globals = FALSE),
                        ...) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']])

  } else {

    watersheds <-  usgs_raws %>%
      split(.$group) %>%
      purrr::map(purrr::safely(~dl_ws(.x))) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']])

  }

}


#' Batch Basin Characteristics
#'
#' @param watersheds A previously created batch_StreamStatsWS object.
#' @param parallel \code{logical} indicating whether to use future_map(). (experimental)
#' @param ... additional arguments to pass to furrr::future_map().
#'
#' @return A sf data.frame with watersheds and basin characteristics.
#' @export
#'
batch_StreamStatsBC <- function(watersheds, parallel = FALSE, ...){

  if(isTRUE(parallel)){

    final_df <-
      furrr::future_map(watersheds, purrr::safely(~get_flow_basin(.x)),
                        .options = furrr::furrr_options(globals = FALSE),
                        ...)%>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill()

  } else {


    final_df <-
      purrr::map(watersheds, purrr::safely(~get_flow_basin(.x))) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill()
  }


  final_df_wrangle <- final_df %>%
    dplyr::select(code, 'value', group, wkID, state, geometry) %>%
    sf::st_as_sf()%>%
    sf::st_drop_geometry() %>%
    pivot_wider(names_from = code, values_from = value)

  final_sf <- final_df %>%
    dplyr::group_by(group) %>%
    dplyr::slice(n=1) %>%
    dplyr::select(group,geometry) %>%
    dplyr::right_join(final_df_wrangle, by = 'group') %>%
    dplyr::relocate(geometry,.after = dplyr::last_col()) %>%
    sf::st_as_sf() %>%
    dplyr::ungroup()

}

#' @title Batch Regression Based Scenarios (RBS)
#' @description Provides the Regression Based Scenarios (RBS).
#' Uses methods from \insertCite{ries2017streamstats} to generate RBS.
#' @param data A previously created batch_StreamStatsBC object.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... additional arguments to pass to furrr::future_map().
#' @return Returns a data.frame with associated regional regression estimates.
#' @note The use of \code{parallel} is likely to not return all of the requests.
#' @examples \dontrun{
#' # Bring in data
#'
#' #### use batch_StreamStats object
#'
#' data <- tibble(Lat = c(48.30602, 48.62952, 48.14946),
#'                  Lon = c(-115.54327, -114.75546, -116.05935),
#'                    Site = c("Granite Creek", "Louis Creek", "WF Blue Creek"))
#'
#' data <- data %>%
#'          sf::st_as_sf(coords = c('Lon', 'Lat')) %>%
#'          sf::st_set_crs(4326)
#'
#'
#' }
#'
#' @importFrom dplyr select tibble all_of
#' @importFrom jsonlite fromJSON
#' @importFrom plyr rbind.fill
#' @references {
#' \insertAllCited{}
#' }
#' @export
#'


batch_StreamStatsRBS <- function(data, parallel = FALSE, ...) {

  if(!'sf' %in% class(data)){usethis::ui_stop('need an sf object with state, wkID and group variables.')}

  data <- data %>% sf::st_drop_geometry()


    if(isTRUE(parallel)){

      rbs <- data %>%
        split(.$group) %>%
        furrr::future_map(safely(~get_rbs(.$state, .$wkID, .$group)),
                          .options = furrr::furrr_options(globals = FALSE),
                          ...) %>%
        purrr::keep(~length(.) != 0) %>%
        purrr::map(~.x[['result']])

    } else {

      rbs <- data %>%
        split(.$group) %>%
        purrr::map(safely(~get_rbs(.$state, .$wkID, .$group)))%>%
        purrr::keep(~length(.) != 0) %>%
        purrr::map(~.x[['result']])


    }

}


# compute basin characteristics ----
get_flow_basin <- function(watersheds) {

  usgs_ws <- watersheds %>%
             dplyr::mutate(state = stringr::str_sub(wkID,end = 2))

  base_url <- paste0(
    "https://streamstats.usgs.gov/streamstatsservices/parameters.json?rcode=",
    usgs_ws$state,
    "&workspaceID=",
    usgs_ws$wkID,
    "&includeparameters=true&withCredentials=true"
  )

  req <- httr2::request(base_url) %>%
         httr2::req_headers(accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
                            authority = 'streamstats.usgs.gov',
                            scheme = 'https')

  resp <- req %>% httr2::req_retry(is_transient = ~ httr2::resp_status(.x) %in% c(400, 404, 500),
                                   max_tries = 4, max_seconds = 8) %>%
                  httr2::req_perform()

  if(httr2::resp_status(resp) == 500){

    usethis::ui_stop('Sever Error 500')

  } else if (httr2::resp_status(resp) == 400){

    usethis::ui_stop('Sever Error 400')

  } else if(httr2::resp_status(resp) == 404){

    usethis::ui_stop('Sever Error 404')

  } else {

  json_str <- resp %>% httr2::resp_body_json()

  flow_stats <- json_str$parameters %>%
                dplyr::bind_rows()

  flow_stats <- flow_stats %>%
                dplyr::mutate(wkID = usgs_ws$wkID)

  final <- dplyr::left_join(usgs_ws %>%
                              dplyr::select(wkID,state, group),
                              flow_stats,
                              by = 'wkID')
  }

}


# delineateWatershed ----

dl_ws <- function(df){

  base_url <- paste0(
    "https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?rcode=",df$state,
    "&xlocation=", df$lon,
    "&ylocation=",df$lat,
    "&crs=", df$crs,
    "&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=true&withCredentials=true"
  )

  req <- httr2::request(base_url) %>%
         httr2::req_headers(accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
                            authority = 'streamstats.usgs.gov',
                            scheme = 'https')

  resp <- req %>% httr2::req_retry(is_transient = ~ httr2::resp_status(.x) %in% c(400, 404, 500),
                                   max_tries = 4, max_seconds = 8) %>%
                  httr2::req_perform()

  if(httr2::resp_status(resp) == 500){

    usethis::ui_stop('Sever Error 500')

  } else if (httr2::resp_status(resp) == 400){

    usethis::ui_stop('Sever Error 400')

  } else if(httr2::resp_status(resp) == 404){

    usethis::ui_stop('Sever Error 404')

  } else {

    # read in the watershed data
    json <- resp %>% httr2::resp_body_json()

    dfclip <- json[["featurecollection"]][[2]][["feature"]][["features"]][[1]][["geometry"]]

    json2sf <- geojsonio::geojson_list(dfclip$coordinates[[1]], geometry = 'polygon')

    json2sf <- geojsonio::geojson_sf(json2sf)

    json2sf$wkID <- json$workspaceID

    json2sf$group <- df$group

    json2sf

  }
}


#' @title regression based scenarios
#' @param state A character abb of a state
#' @param wkID A previously created wkID
#' @param group A group in the data.frame
#'
get_rbs <- function(state, wkID, group){

  base_url <- paste0(
    "https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=",state,"&workspaceID=",
    wkID,
    "&includeflowtypes=true&withCredentials=true"
  )

  # try to download the data
  req <- httr2::request(base_url)

  resp <- req %>% httr2::req_retry(is_transient = ~ httr2::resp_status(.x) %in% c(400, 404, 500),
                                   backoff = ~ 10) %>%
                  httr2::req_perform()

  if(error$status_code == 500){

    usethis::ui_stop('Sever Error 500')

  } else if(error$status_code == 400){

    usethis::ui_stop('Sever Error 400')

  } else if(error$status_code == 404){

    usethis::ui_stop('Sever Error 404')

  } else {

    rbs <- resp %>% httr2::resp_body_json()

  }

}
