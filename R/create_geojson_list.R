#'
create_nest_geojson_list <- function(data, geodata, level1, level2, prefix,
                                     sep = "_", ...) {

  lvls <- unique(data[,level1])

  purrr::map(
    lvls,
    create_geojson_list,
    data,
    geodata,
    level1,
    level2,
    prefix,
    ...
  )

}

#'
create_geojson_list <- function(lvl, data, geodata, level1, level2, prefix,
                                sep, ...) {

  level1_ <- dplyr::enquo(level1)
  level2_ <- dplyr::enquo(level2)

  if (length(prefix) > 1) {
    prefix1 <- prefix[1]
    prefix2 <- prefix[2]
  } else {
    prefix1 <- prefix2 <- prefix[1]
  }

  data <- data %>%
    dplyr::filter(!!level1_ == lvl) %>%
    dplyr::mutate(
      drilldown = paste(prefix2, !!level2_, sep = sep)
    )

  geodata <- geodata %>%
    dplyr::filter(!!level2_ %in% data[,level2]) %>%
    sf::st_simplify(...) %>%
    sf::as_Spatial() %>%
    geojsonio::geojson_json()

  id <- paste(prefix1, lvl, sep = sep)

  list(
    id = id,
    name = lvl,
    mapData = geodata,
    joinBy = level2,
    data = highcharter::list_parse(data),
    dataLabels = list(
      enabled = TRUE,
      format = '{point.tooltip}'
    )
  )

}
