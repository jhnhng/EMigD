#' Re-formats the User's data frame for the EMigD workflow
#'
#' @param dat A data frame
#' @param id_ The individual ID column in 'dat'. This column should be a
#' 'Factor'.
#' @param dt_ The date and time column in 'dat'. Thhis column should be
#' 'POSIXct' class.
#' @param x_  The x-coordinate column in 'dat'.
#' @param y_  The y-coordinate column in 'dat'.
#' @param year_ The column containing the years in 'dat'. The column should be
#' a numeric value.
#' @param month_ The column containing the months in 'dat'. The column should
#' be a numeric value.
#' @param jdate_ The column containing the julian dates. This column should be
#' a numeric value

#' @return A data frame with the columns specified, and renames the column names
#'

#' @export
EmigD_format <- function(dat, id_, dt_, x_, y_, year_, month_, jdate_){
  EMigD_cols <- subset(dat,
                       select=c(id_,
                                dt_,
                                x_,
                                y_,
                                year_,
                                month_,
                                jdate_))

  names(EMigD_cols) <- c("id_",
                         "dt_",
                          "x_",
                          "y_",
                          "year_",
                          "month_",
                          "jdate_")
  return(EMigD_cols)
}
