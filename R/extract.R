#' @rdname extract_timepoint
#' @title Extract data from particular timepoints
#'
#' @description
#' These functions extract data from particular timepoints.
#' The functions `vacc_extract_baseline`, `vacc_extract_peak`, and
#' `vacc_extract_memory` extract the baseline, peak, and memory
#' response, respectively.
#' The timepoints are specified in the `peak_time` and `mem_time`
#' arguments for the `vacc_extract_peak` and `vacc_extract_memory`
#' functions, respectively.
#'
#' @param .data A data frame.
#' @param cn_time character.
#' Column name in `.data` that contains the timepoint.
#' @param peak_time named numeric vector.
#' Named vector specifying the peak timepoint for each vaccine.
#' Names are the protocol names, and values are the peak timepoints.
#' Must be supplied
#' @param mem_time named numeric vector.
#' Named vector specifying the memory timepoint for each vaccine.
#' Names are the protocol names, and values are the memory timepoints.
#' If not supplied, then the defaults are used.
#'
#' @export
vacc_extract_baseline <- function(.data, cn_time = "timepoint") {
  # extract the baseline response
  .data |>
    dplyr::filter(.data[[cn_time]] == 0)
}

#' @rdname extract_timepoint
#' @export
vacc_extract_peak <- function(.data,
                              cn_time = "timepoint",
                              peak_time = NULL) {
  # extract the peak response
  peak_time <- .vacc_extract_peak_peak_time(peak_time)
  .data |>
    dplyr::mutate(peak_time = peak_time[.data[["prid"]]]) |>
    dplyr::filter(.data[[cn_time]] == .data[["peak_time"]]) |>
    dplyr::select(-peak_time)
}

.vacc_extract_peak_peak_time <- function(peak_time) {
  if (!is.null(peak_time)) {
    return(peak_time)
  }
  c(
    "040" = 70,
    "c-035-456" = 70,
    "tb008" = 7, # check
    "tb011" = 7, # check
    "thyb04" = 70,
    "tb010" = 37
  )
}

#' @rdname extract_timepoint
#' @export
vacc_extract_memory <- function(.data,
                                cn_time = "timepoint",
                                mem_time = NULL) {
  # extract the memory response
  mem_time <- .vacc_extract_memory_mem_time(mem_time)
  .data |>
    dplyr::mutate(mem_time = mem_time[.data[["prid"]]]) |>
    dplyr::filter(.data[[cn_time]] == .data[["mem_time"]]) |>
    dplyr::select(-mem_time)
}

.vacc_extract_memory_mem_time <- function(mem_time) {
  if (!is.null(mem_time)) {
    return(mem_time)
  }
  c(
    "040" = 999,
    "c-035-456" = 292,
    "tb008" = 168,
    "tb011" = 364,
    "thyb04" = 224,
    "tb010" = 210
  )
}
