% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/env_raster_stack_fun.R
\name{env_raster_stack}
\alias{env_raster_stack}
\title{Creates raster stacks from a environmental covariate layer and home range}
\usage{
env_raster_stack(env, hrr, attribute = NULL)
}
\arguments{
\item{env}{A environmental raster layer}

\item{hrr}{A home range that the environmental covariate is resampled to.}

\item{attribute}{A attribute value that the environmental layers should be
applied accordingly.}
}
\value{
The output is a raster stack that contains the home ranges and the
environmental covariate layer.
}
\description{
Creates raster stacks from a environmental covariate layer and home range
}
