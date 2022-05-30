#' Grabs the attributes that you assigned to a list
#'
#' @param attribute_name The name of the attribute item created for both
#' `list1` and `list2` The input for this argument must be a character.
#' @param list The list object that the attribute is being assigned to.
#'
#'
#' @return This grabs the attributes that you assigned to the list.
#'
#' @export
#'
# Convenience function to grab the attributes
check <- function(list, attribute_name) {
  return(attr(list, attribute_name))
}

#' Matches the number of intervals between the two seasons selected
#'
#' @param attribute_name The name of the attribute item created for both
#' `list1` and `list2` The input for this argument must be a character.
#' @param list1 A list object that is the reference for `list2`
#' @param list2 A list object that the length of `list2` is being matched to
#' @param id_col This should be the name of assigned to the id column within the
#' attribute table created.
#' @param interval_col This should be the name assigned to the interval column
#' within the attribute_table created.
#'
#'
#' @return The output is a list that contains two new list. These two new list
#' have the same individuals and intervals for those individuals.
#'
#' @export
#'
match_intervals <- function(attribute_name, list1, list2, id_col, interval_col){

  # Creates an index for the two list based on the attributes,
  dat2 <- check(list2, attribute_name)
  dat1 <- check(list1, attribute_name)

  ids_common <- intersect(dat2$id, dat1$id)
  inds1 <- dat2$id %in% ids_common
  inds2 <- dat1$id %in% ids_common
  i1 <-   paste(dat2[[id_col]], format(as.Date(dat2[[interval_col]]),
                                       "%Y-%d")) %in%
    paste(dat1[[id_col]], format(as.Date(dat1[[interval_col]]),
                                 "%Y-%d"))

  i2 <-   paste(dat1[[id_col]], format(as.Date(dat1[[interval_col]]),
                                       "%Y-%d")) %in%
    paste(dat2[[id_col]], format(as.Date(dat2[[interval_col]]),
                                 "%Y-%d"))

  comb_list <- list(list1[i2 & inds2], list2[i1 & inds1])


}
