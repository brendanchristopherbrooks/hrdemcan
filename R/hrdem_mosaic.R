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
