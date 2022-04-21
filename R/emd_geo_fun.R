#' Process UD raster to calculate EMD
#'
#' @param r1 is a RasterLayer representing a rasterized UD.
#' @param r2 is a RasterLayer representing a rasterized UD.
#'
#' @return Function calculates EMD between r1 and r2 in geographic space.
#'
#'@importFrom sp coordinates
#'@importFrom raster values
#'@importFrom transport wpp wasserstein


##################################################################### X
#----Process UD raster to calculate EMD with package 'transport'-----X
#---------------------------Brian J. Smith---------------------------X
#----------------------Code started 2021-07-01-----------------------X
##################################################################### X
#----------------------Last updated 2021-07-01-----------------------X
##################################################################### X

# Function to calculate EMD in geographic space

# r1 is a RasterLayer representing a rasterized UD.
# r2 is a RasterLayer representing a rasterized UD.

# Function calculates EMD between r1 and r2 in geographic
# space.

#' @export
emd_geo <- function(r1, r2) {

  # Check r1 and r2
  if (!inherits(r1, "RasterLayer")) {
    stop("The object passed to 'r1' must be a RasterLayer object.")
  }

  if (!inherits(r2, "RasterLayer")) {
    stop("The object passed to 'r2' must be a RasterLayer object.")
  }

  # Matrix of coordinates
  coord1 <- coordinates(r1)
  coord2 <- coordinates(r2)

  # Vector of masses (UD values)
  mass1 <- values(r1)
  mass2 <- values(r2)

  # Cannot have NAs in the raster (UD should be 0)
  mass1[is.na(mass1)] <- 0
  mass2[is.na(mass2)] <- 0

  # Renormalize masses b/c transport::compatible.wpp will
  # not accept even floating point differences
  mass1 <- mass1 / sum(mass1)
  mass2 <- mass2 / sum(mass2)

  # Create wpp objects
  wpp1 <- wpp(
    coordinates = coord1,
    mass = mass1
  )
  wpp2 <- wpp(
    coordinates = coord2,
    mass = mass2
  )

  # Calculate EMD in geographic space
  res <- wasserstein(a = wpp1, b = wpp2)

  # Return
  return(res)
}
