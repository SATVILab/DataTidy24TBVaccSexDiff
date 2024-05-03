#' Capitalise a character if it is not in a vec1.
#'
#' Capitalises an individual character if is
#' not in an exclusion vector. Only used in \code{str_to_upper_sel},
#' and not exported.
#'
#' @param x character. Character to possibly capitalise.
#' @param vec1 character vector. Vector of characters to not
#' capitalise.
#' @return A character that is capitalised if it not in vec1,
#' and is not capitalised if it is in vec1.

str_to_upper_sel_elem <- function(x, vec1) {
  if (!(x %in% vec1)) {
    return(str_to_upper(x))
  }
  x
}

#' Capitalise all letters in a string except some.
#'
#' \code{str_to_upper_sel} capitalises all letters in \code{x},
#' except for those specified in \code{vec1}.
#'
#' @param x character. String to
#' @param vec1 character vector. Characters against which each individual
#' part of x is compared
#' @return A character string.
#' @examples
#' str_to_upper_sel("abc", NULL)
#' str_to_upper_sel("abc", c("b", "c"))
#' @export

str_to_upper_sel_string <- function(x, vec1) {
  plyr::laply(str_split(x, "", simplify = TRUE), function(y) {
    str_to_upper_sel_elem(y, vec1)
  }) %>%
    str_c(collapse = "")
}

#' Capitalise all letters in a string except some using rename.
#'
#' \code{str_to_upper_sel_rename}
#'
#' @param x character vector. Character vector of column names.
#' Provided by rename.
#' @param vec1 character vector. Character vector of characters
#' to not capitalise.
#' @export
str_to_upper_sel <- function(x, vec1) {
  plyr::laply(x, function(x) str_to_upper_sel_string(x, vec1))
}
