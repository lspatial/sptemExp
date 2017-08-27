#' SpatialPointDataFrame as container of raster to geo-link with the specific date prediction of PM2.5.
#'
#' Container of raster to geo-link with the specific date prediction of PM2.5,
#' and will be used to generate the surface of PM2.5 concentration at high resolution for Shandong Province.
#'
#' @source Collected
#' @format \code{SpatialPointsDataFrame}
#' \describe{
#' \item{ogc_fid}{inner id}
#' \item{layer}{layers value}
#' \item{pre_m}{predicted value}
#' \item{pre_sd}{estimate of standard variance of the predicted value}
#' }
#' @examples
#'   spointspre
"spointspre"
