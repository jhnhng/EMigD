#' Matches the number of intervals between the two seasons selected
#'
#' @param list1 A list object that is the reference for `list2`
#' @param list2 A list object that the length of `list1` is being matched to
#'
#' @return The output is a list that has the same intervals as that in the
#' `list2` object.

#' @export
match_intervals <- function(list1, list2){
i1 <- sub("(.*)-\\d+-(.*)", "\\1-\\2",
          names(list1)) %in% sub("(.*)-\\d+-(.*)",
                                "\\1-\\2", names(list2))
list1 <- list1[i1]

}
