collect_peak_flows <- function(peak_s) {



  variables <- c( "Name", "code", "Description", "Value", "Equation", "IntervalBounds.Lower", "IntervalBounds.Upper")

  peak_s <- (peak_s$RegressionRegions[[1]])[[6]][[1]] %>%
    dplyr::select(dplyr::any_of(variables))

  peak_rre <- peak_rre %>%
    plyr::rbind.fill() %>%
    dplyr::mutate(return_interval = 1/(readr::parse_number(Name)*0.01))

}

