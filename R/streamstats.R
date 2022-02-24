
#' @title Download Multiple Stream Stats Locations
#'
#' @description Takes sf point object and returns catchment characteristics and watershed boundary (sf). Uses state to generate basin
#' delineation(s) and characteristics per methods from \insertCite{ries2017streamstats}{whitewater}
#' @param data sf data.frame with POINT geometry
#' @param group A vector to group by. \code{optional}
#' @param crs A \code{numeric} crs value
#' @param parallel \code{logical} indicating whether to use future_map(). (experimental)
#' @param ... additional arguments to pass to furrr::future_map().
#'
#' @note The use of \code{parallel} is likely to not return all of the requests.
#'
#' @references {
#' \insertAllCited{}
#' }
#' @importFrom Rdpack reprompt
#' @importFrom geojsonsf geojson_sf
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



batch_StreamStats <- function(data, group, crs = 4326, parallel = FALSE, ...){


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
      furrr::future_map(purrr::safely(~dl_ws(.)),...) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']])

    final_df <-
      furrr::future_map(watersheds, purrr::safely(~get_flow_basin(.)), ...)%>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill()

  } else {

    watersheds <-  usgs_raws %>%
      split(.$group) %>%
      purrr::map(purrr::safely(~dl_ws(.))) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']])

    final_df <-
      purrr::map(watersheds, purrr::safely(~get_flow_basin(.))) %>%
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

  dropped_geom <- final_sf %>% sf::st_drop_geometry()

  if(nrow(dplyr::filter(usgs_raws, !group %in% dropped_geom$group)) > 0) {

    usethis::ui_warn(paste0("Group(s)/watersheds not generated: ",
                            filter(usgs_raws, !group %in% dropped_geom$group) %>% select(group)))
    if(any(apply(dropped_geom, 2, function(x) any(is.na(x))))){
      usethis::ui_info('NA values were found in the output. You may want to check')
    }

  } else {

    usethis::ui_done("All groups/watersheds delineated")

    if(any(apply(dropped_geom, 2, function(x) any(is.na(x))))){
      usethis::ui_info('NA values were found in the output. You may want to check')
    }
  }

  return(final_sf)

}


#' @title Batch USGS Regional Regression Estimates (RRE)
#' @description Provides the USGS regressions from a \link[wildlandhydRo]{batch_StreamStats}.
#' Uses methods from \insertCite{ries2017streamstats}{wildlandhydRo} to generate RRE's.
#' @param data A previously created \link[whitewater]{batch_StreamStats} object.
#' @param parallel \code{logical} indicating whether to use future_map().
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
#' three_sites <- batch_StreamStats( data,
#'                                   group = 'Site',
#'                                   crs = 4326,
#'                                   parallel = FALSE)
#'
#' rre_peak <- batch_RRE(three_sites)
#'
#' }
#'
#' @importFrom dplyr select tibble all_of
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET write_disk
#' @importFrom plyr rbind.fill
#' @references {
#' \insertAllCited{}
#' }
#' @export
#'


batch_RRE <- function(data, parallel = FALSE) {

  if(!'sf' %in% class(data)){usethis::ui_stop('need an sf object with state, wkID and group variables.')}

  data <- data %>% sf::st_drop_geometry()


  if(length(data$group)>1){

    if(isTRUE(parallel)){

      peak_rre <- data %>%
        split(.$group) %>%
        furrr::future_map(safely(~get_peak_flow(.$state, .$wkID, .$group))) %>%
        purrr::keep(~length(.) != 0) %>%
        purrr::map(~.x[['result']])

    } else {

      peak_rre <- data %>%
        split(.$group) %>%
        purrr::map(safely(~get_peak_flow(.$state, .$wkID, .$group)))
        # purrr::keep(~length(.) != 0) %>%
        # purrr::map(~.x[['result']])
    }

    # peak_group <- peak_rre %>%
    #   group_by(group) %>%
    #   dplyr::slice(n=1)
    #
    # if(nrow(filter(data, !group %in% peak_group$group)) > 0) {
    #
    #   usethis::ui_warn(paste0("Group(s) not generated: ",
    #                filter(data, !group %in% peak_group$group) %>% dplyr::select(group)))
    # } else {
    #
    #   usethis::ui_done("All groups/watersheds delineated")
    #
    #
    # }

  } else {

    peak_rre <- get_peak_flow(data$state, data$wkID, data$group)


  }

  return(peak_rre)
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
    "&includeparameters=true"
  )

  req <- httr2::request(base_url) %>%
         httr2::req_headers(accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
                            authority = 'streamstats.usgs.gov',
                            scheme = 'https')

  resp <- req %>% httr2::req_retry(is_transient = ~ httr2::resp_status(.x) %in% c(400, 404, 500),
                            max_seconds = 5, max_tries = 2) %>%
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
    "&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=true"
  )

  req <- httr2::request(base_url) %>%
         httr2::req_headers(accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
                            authority = 'streamstats.usgs.gov',
                            scheme = 'https')

  resp <- req %>% httr2::req_retry(is_transient = ~ httr2::resp_status(.x) %in% c(400, 404, 500),
                            max_seconds = 5, max_tries = 2) %>%
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

# getting peak flow ----

get_peak_flow <- function(state, wkID, group){

  base_url <- paste0(
    "https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=",state,"&workspaceID=",
    wkID,
    "&includeflowtypes=true"
  )

  # try to download the data
  error <- httr::RETRY("GET",
                       url = base_url,
                       httr::write_disk(path = file.path(tempdir(),
                                                         "peak_tmp.json"),overwrite = TRUE),
                       times = 1)

  if(error$status_code == 500){

    usethis::ui_stop('Sever Error 500')

  } else if(error$status_code == 400){

    usethis::ui_stop('Sever Error 400')

  } else if(error$status_code == 404){

    usethis::ui_stop('Sever Error 404')

  } else {

    peak_s <- jsonlite::fromJSON(file.path(tempdir(),"peak_tmp.json"))

  }

}
