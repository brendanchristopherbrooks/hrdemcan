#' Combine Accessed Digital Elevation Data
#'
#' Combine accessed high resolution digital elevation data into a single
#' mosaciked raster.  Rasters are resampled to the resolution of the lowest
#' resolution raster using bilinear interpolation and overlapping cells are
#' averaged, by default.
#'
#' @param x a list of SpatRasters
#' @param method a character; the method used to estimate resampled cell values.
#'               Can be "near", "bilinear", "cubic", "cublicspline", "lanczos",
#'               "sum", "min", "q1", "med", "q3", "max", "average", "mode", or
#'               "rms".
#' @param fun a character; the function used to assign values to overlapping
#'            cells.  Can be "mean", "median", "min", "max", "modal", "sum",
#'            "first", or "last".
#'
#' @returns SpatRaster
#' @export
#'
#' @examples
#' point <-
#'   sf::st_point(c(-97.79828, 49.97858)) |>
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
#'   hrdem_mosaic(rasters,
#'                methods = "bilinear",
#'                fun = "mean")
#'
#' terra::plot(raster)
#'
#' file.remove(file_paths)
hrdem_mosaic <-
  function(x,
           method = "bilinear",
           fun = "mean"){
    resolutions <-
      lapply(x,
             terra::res)

    x_resolutions <-
      sapply(resolutions,
             \(x) x[[1L]])

    y_resolutions <-
      sapply(resolutions,
             \(x) x[[2L]])

    equalities <-
      .mapply(all.equal,
              dots = list(x_resolutions,
                          y_resolutions),
              MoreArgs = list())

    equalities <-
      unlist(equalities)

    stopifnot(all(equalities) == T)

    resolutions <-
      x_resolutions

    max_resolution <-
      max(resolutions)

    lengths <-
      lapply(x,
             \(x) length(terra::cells(x)))

    target_raster_index <-
      which(lengths > 0L & resolutions == max_resolution)[[1L]]

    target_raster <-
      x[[target_raster_index]]

    hrdems <-
      lapply(x,
             \(x) terra::resample(x,
                                  target_raster,
                                  method = method))

    hrdems <-
      terra::sprc(hrdems)

    terra::mosaic(hrdems,
                  fun = fun)
  }
