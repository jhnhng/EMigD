#' Creates raster stacks from a environmental covariate layer and home range
#'
#' @param env A environmental raster layer
#' @param hrr A home range that the environmental covariate is resampled to.
#' @param attribute A attribute value that the environmental layers should be
#' applied accordingly.
#'
#' @return The output is a raster stack that contains the home ranges and the
#'  environmental covariate layer.

# Creating raster stacks with the home range estimate and the environmental
# variables

#' @export
env_raster_stack <- function(env, hrr, attribute = NULL) { #start
  # If the `attribute`argument is not provided run this:
  if(is.null(attribute)){

    rst <- lapply(hrr,function(x){
      # Resamples the environmental rasters to the same extend as the locohs
      rs <- raster::resample(env, x)
      # Create Raster stacks
      rst <- raster::stack(x, rs)
    })

  }else{ # If an value is provided for the `attribute` argument run this:

    multi_env <- do.call(c, Map(function(x, y)
      lapply(x, function(z){
        rs <- raster::resample(y, z)
        rst <- raster::stack(z, rs)
      }),
      split(hrr, attr(hrr, attribute)),
      env
    ))

  }

  if(is.null(attribute)){
    return(rst)
  }else{
    return(multi_env)
  }

} # end
