#' Process UD raster to calculate EMD with pacakge 'transport'
#'
#' @param rs1 A RasterStack with two layers: (1) the rasterized UD and (2) the
#'   values of the environmental covariate.
#' @param rs2 A RasterStack with two layers: (1) the rasterized UD and (2) the
#'    values of the environmental covariate.
#' @param nbins a numeric vector of length == 1 giving the number of bins to
#'    discretize environmental space into. Will create a sequence from
#'    the min -- max environmental values (from both ranges combined).
#'    Defaults to 50.
#'
#' @return EMD between rs1 and rs2 in one dimension of environmental space.
#'
#' @importFrom raster values
#' @importFrom stats aggregate
#' @importFrom transport wasserstein1d



##################################################################### X
#----Process UD raster to calculate EMD with package 'transport'-----X
#---------------------------Brian J. Smith---------------------------X
#----------------------Code started 2021-07-01-----------------------X
##################################################################### X
#----------------------Last updated 2021-07-01-----------------------X
##################################################################### X

# Function to calculate EMD in environmental space

# rs1 is a RasterStack with two layers: (1) the rasterized UD and (2) the
#     values of the environmental covariate [in that order].
# rs2 is a RasterStack with two layers: (1) the rasterized UD and (2) the
#     values of the environmental covariate [in that order].
# nbins is a numeric vector of length == 1 giving the number of bins to
#     discretize environmental space into. Will create a sequence from
#     the min -- max environmental values (from both ranges combined).
#     Defaults to 50.

# Function calculates EMD between rs1 and rs2 in one dimension of environmental
# space.

#' @export
emd_env <- function(rs1, rs2, nbins = 50) {

  # Check rs1 and rs2
  if (!inherits(rs1, "RasterStack")) {
    stop("The object passed to 'rs1' must be a RasterStack object.")
  }

  if (!inherits(rs2, "RasterStack")) {
    stop("The object passed to 'rs2' must be a RasterStack object.")
  }

  if (raster::nlayers(rs1) != 2) {
    stop("The RasterStack passed to 'rs1' must have exactly 2 layers.")
  }

  if (raster::nlayers(rs2) != 2) {
    stop("The RasterStack passed to 'rs2' must have exactly 2 layers.")
  }

  # Get UD values (NAs should be set to 0)
  ud1 <- values(rs1)[, 1]
  ud1[is.na(ud1)] <- 0
  ud2 <- values(rs2)[, 1]
  ud2[is.na(ud2)] <- 0

  # Get environmental covariate values
  env1 <- values(rs1)[, 2]
  env2 <- values(rs2)[, 2]
  env_all <- c(env1, env2)

  # Combine with UD values in data.frame
  env_df1 <- data.frame(
    env = env1,
    ud = ud1
  )
  env_df2 <- data.frame(
    env = env2,
    ud = ud2
  )

  # UD will be used to weight distances
  # Weights of 0 are not allowed
  env_df1 <- env_df1[which(env_df1$ud > 0), ]
  env_df2 <- env_df2[which(env_df2$ud > 0), ]

  # "Rasterize" by the environmental covariate by binning
  bin_start <- 0.95 * min(env_all, na.rm = TRUE) # Shouldn't be NAs, but be safe
  bin_end <- 1.05 * max(env_all, na.rm = TRUE) # Shouldn't be NAs, but be safe
  env_bins <- seq(bin_start, bin_end, length.out = nbins)

  # Assign each value to its bin
  env_df1$bin <- findInterval(env_df1$env, env_bins)
  env_df2$bin <- findInterval(env_df2$env, env_bins)

  # Summarize by bin
  agg1 <- aggregate(env_df1$ud, list(env_df1$bin), sum, na.rm = TRUE)
  names(agg1) <- c("bin", "ud")
  agg2 <- aggregate(env_df2$ud, list(env_df2$bin), sum, na.rm = TRUE)
  names(agg2) <- c("bin", "ud")

  # Create key for bins
  bin_key <- data.frame(
    bin = 1:length(env_bins),
    start = env_bins,
    end = c(env_bins[2:length(env_bins)], NA)
  )

  # Join key to data
  agg_env1 <- merge(agg1, bin_key, by = "bin")
  agg_env2 <- merge(agg2, bin_key, by = "bin")

  # Now calculate 1d EMD
  #   Shouldn't matter if you use start.
  res <- wasserstein1d(
    a = agg_env1$start,
    b = agg_env2$start,
    wa = agg_env1$ud,
    wb = agg_env2$ud
  )

  # Return
  return(res)
}
