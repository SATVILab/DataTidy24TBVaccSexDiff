# =====================================
# calculate vaccine-induced response
# =====================================

#' @title Calculate the vaccine-induced response
#'
#' @description
#' To calculate the vaccine-induced response, we subtract the baseline response.
#' If the baseline response is missing for a particular individual,
#' we subtract the
#' median baseline response for that individual's vaccine and
#' infection status group.
#'
#' @export
vacc_calc_vaccine_induced_response <- function(.data,
                                               cn_resp,
                                               grp = NULL) {
  # find the value to actually subtract,
  # using the day zero value if available but the median value if not
  .data <- .calc_day_zero_to_subtract(.data, cn_resp = cn_resp, grp = grp)
  # subtract day zero
  .data[[cn_resp]] <- .data[[cn_resp]] - .data[["response_d0"]]
  # remove day 0 column
  .data |> dplyr::select(-response_d0) # nolint
}

.calc_day_zero_to_subtract <- function(.data, grp = NULL, cn_resp) {
  # get median day zero value
  med_tbl <- .calc_day_zero_med(.data, grp = grp, cn_resp = cn_resp)
  # get actual day zero value
  actual_tbl <- .get_day_zero_actual(.data, grp = grp, cn_resp = cn_resp)
  # join the median and actual day zero values onto this the original table
  data_join <- .data |>
    dplyr::left_join(med_tbl) |>
    dplyr::left_join(actual_tbl)
  # if there is no actual value, choose median. otherwise choose actual
  data_join |>
    dplyr::mutate(
      response_d0 = response_d0_actual, # nolint
      response_d0 = ifelse(
        !is.na(response_d0_actual), response_d0_actual, response_d0_median # nolint
      )
    ) |>
    dplyr::select(-c(response_d0_actual, response_d0_median))
}

.get_day_zero_actual <- function(.data, grp = NULL, cn_resp) {
  # get the actual day zero value for each participant
  grp_vec <- .get_grp_vec(.data, grp = grp)
  grp_vec <- setdiff(grp_vec, c("timepoint"))
  .data <- .data |>
    dplyr::filter(timepoint == 0) # nolint
  .data[["response_d0_actual"]] <- .data[[cn_resp]]
  .data
}

.calc_day_zero_med <- function(.data, grp = NULL, cn_resp) {
  # calculate the median day zero value
  # allow setting the grouping variables in advance
  # so that this function can be used throughout
  grp_vec <- .get_grp_vec(.data, grp = grp) |>
    setdiff(c("sex", "ptid", "timepoint"))
  .data |>
    dplyr::filter(timepoint == 0) |> # nolint
    dplyr::group_by(dplyr::across(grp_vec)) |> # nolint
    dplyr::summarise(
      response_d0_median = median(.data[[cn_resp]], na.rm = TRUE),
      .groups = "drop"
    )
}

.get_grp_vec <- function(.data, grp = NULL) {
  # get variables to group within.
  # will choose only those that are found in `.data`
  if (is.null(grp)) {
    grp <- c(
      "type", "vaccine", "prid", "timepoint", "infxn", "sex",
      "ptid", "subset", "cyt_combn"
    )
  }
  grp[grp %in% colnames(.data)]
}

#' @title Set negative responses to zero
#'
#' @param .data A data frame.
#' @cn_resp character.
#' Column name in `.data` that contains the response.
#'
#' @export
vacc_set_neg_to_zero <- function(.data, cn_resp) {
  .data[, cn_resp] <- pmax(.data[[cn_resp]], 0)
  .data
}

#' @rdname calc_response_transformation
#' @title Calculate the summed and profile response
#'
#' @description
#' The functions `calc_response_summed` and `calc_response_profile`
#' calculate the summed and profile response, respectively,
#' as defined in Rodo (2019).
#'
#' @param .data A data frame.
#' @param cn_resp character.
#' Column name in `.data` that contains the response.
#' @param grp character vector.
#' Columns in `.data` containing the grouping variable.
#' If `NULL` (the default), then these are automatically determined.
#'
#' @export
vacc_calc_response_summed <- function(.data, cn_resp, grp = NULL) {
  grp_vec <- .get_grp_vec(.data, grp = grp) |> setdiff("cyt_combn")
  .data <- .data |>
    dplyr::group_by(dplyr::across(grp_vec)) |>
    dplyr::summarise(
      response = sum(.data[[cn_resp]], na.rm = TRUE),
      .groups = "drop"
    )
  if (cn_resp == "response") {
    return(.data)
  }
  .data[, cn_resp] <- .data[["response"]]
  .data |> dplyr::select(-response) # nolint
}

#' @rdname calc_response_transformation
#' @export
vacc_calc_response_profile <- function(.data, cn_resp, grp = NULL) {
  grp_vec <- .get_grp_vec(.data, grp = grp) |> setdiff("cyt_combn")
  .data |>
    dplyr::group_by(dplyr::across(grp_vec)) |>
    dplyr::mutate(
      response_abs_sum = sum(abs(.data[[cn_resp]]), na.rm = TRUE),
      response_abs_sum = ifelse(
        response_abs_sum == 0, NA_real_, response_abs_sum # nolint
      ),
      response = .data[[cn_resp]] / response_abs_sum
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-response_abs_sum)
  if (cn_resp == "response") {
    return(.data)
  }
  .data[, cn_resp] <- .data[["response"]]
  .data |> dplyr::select(-response) # nolint
}
