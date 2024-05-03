#' Calculate number of unique vector elements.
#'
#' Convenience function around \code{length()} and \code{unique()}
#' that calculates the number of unique elements in a vector.
#'
#' @param x vector.
#' @export
calcLU <- function(x) length(unique(x))

#' Calculate baseline-subtracted response.
#'
#' @param dataTbl dataframe. A dataframe with least the columns med, timePoint and resp, containing
#' the longitudinal responses for a single individual for a single response type.
#' @return A dataframe with either that individual's day zero response subtracted from all responses,
#' or the median response at day for that individual's group subtracted from all responses.
#' @export
calcSub <- function(dataTbl) {
  # dataTbl %<>% arrange( timePoint )

  if (dataTbl$timePoint[1] == 0) {
    return(dataTbl %<>% mutate(resp = resp - resp[1]))
  }

  dataTbl %>% mutate(resp = resp - med)
}
