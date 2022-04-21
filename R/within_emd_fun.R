#' Calculates the Earth-Mover's Distance (EMD) within the same range
#'
#' @param t List of rasters containing the raster for each period within the
#   specified interval.
#' @param nintervals The length that the rasters are broken up into by the
#'  specified interval.
#'
#' @return The output is a list of matrices that contain the within- range EMDs

#' @export
# Calculates the geographical emd within a timeframe
within_emd <- function(t) {
  id <- lapply(list(t),function(x)split(unlist(x), names(x)));

  Map(
    function(subl1) {
      outer(unlist(subl1), unlist(subl1),
            FUN = Vectorize(function(x, y) {
              res <- NA
              if (inherits(x, "RasterStack")) {
                try({
                  res <- emd_env(x, y)
                  return(res)
                }, silent = TRUE)
              } else {
                try({
                  res <- emd_geo(x, y)
                  return(res)
                }, silent = TRUE)
              }

              if(is.na(res)){
                warning("EMDs contain NAs")
                Filter(Negate(is.null), res)
              }
            })
      )
    },
    split(id[[1]], ceiling(seq_along(id[[1]])))
  )

}
