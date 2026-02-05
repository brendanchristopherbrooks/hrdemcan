make_vsicurl_url <-
  function(url){
    paste0("/vsicurl",
           "?pc_url_signing=yes",
           "&url=",
           url)
  }

#' Access and Download Digital Elevation Data
#'
#' Download digital surface or terrain model data from Canada's High Resolution
#' Digital Elevation Model (HRDEM), masked by a feature geometry.
#'
#' @param x an object of class sf, sfc, or sfg
#' @param path a character; the path to write the accessed digital elevation
#'             models to.  The default corresponds to the working directory.
#' @param model a character; the desired digital elevation model.  Valid values
#'              include "dsm" or "dtm", representing the digital surface model
#'              or digital terrain model, respectively.
#' @param pattern a character; the desired digital elevation model's file type.
#'                Valid values include "tif" or "vrt", representing GeoTIFF or
#'                virtual raster file types, respectively.
#'
#' @returns NULL
#'
#' @source Government of Canada. (2025). High Resolution Digital Elevation Model (HRDEM) - CanElevation Series.
#' \cr https://open.canada.ca/data/en/dataset/957782bf-847c-4644-a757-e383c0057995
#'
#' @export
#'
#' @examples
#' point <-
#'   sf::st_point(c(-98.35814, 49.35103)) |>
#'   sf::st_sfc(crs = 4326L) |>
#'   sf::st_transform(26914L)
#'
#' buffer <-
#'   sf::st_buffer(point, 403L)
#'
#' temp_path <-
#'   tempdir()
#'
#' hrdem_dl(buffer, temp_path, "dtm", "vrt")
#'
#' file_paths <-
#'   list.files(temp_path,
#'              pattern = "\\.vrt$",
#'              full.names = TRUE)
#'
#' rasters <-
#'   lapply(file_paths,
#'          terra::rast)
#'
#' raster <-
#'   rasters[[2]]
#'
#' terra::plot(raster)
#'
#' file.remove(file_paths)
hrdem_dl <-
  function(x,
           path = ".",
           model,
           pattern){
    stopifnot(model %in% c("dsm", "dtm"))

    stopifnot(pattern %in% c("tif", "vrt"))

    asset_regex <-
      paste0(model, "\\.", pattern, "$")

    stac_bbox <-
      x |>
      sf::st_transform(4326L) |>
      sf::st_bbox()

    cog_bbox <-
      x |>
      sf::st_transform(3979L) |>
      sf::st_bbox()

    stac_source <-
      rstac::stac("https://datacube.services.geo.ca/stac/api")

    stac_query <-
      rstac::stac_search(q = stac_source,
                         collections = "hrdem-lidar",
                         bbox = stac_bbox)

    executed_stac_query <-
      rstac::get_request(stac_query)

    stac_assets <-
      rstac::assets_url(executed_stac_query)

    hrdem_urls <-
      stac_assets[grepl(asset_regex, stac_assets) == TRUE] |>
      make_vsicurl_url()

    hrdem_names <-
      regmatches(hrdem_urls,
                 regexpr("(?<=/)(?!.*/).*",
                         hrdem_urls,
                         perl = TRUE))

    .mapply(\(x, y) sf::gdal_utils(util = "warp",
                                   source = x,
                                   destination = paste0(normalizePath(path,
                                                                      winslash = "/"),
                                                        "/",
                                                        y),
                                   options = c("-t_srs", sf::st_crs(cog_bbox)$wkt,
                                               "-te", cog_bbox,
                                               "-overwrite")),
            dots = list(hrdem_urls,
                        hrdem_names),
            MoreArgs = list())
  }
