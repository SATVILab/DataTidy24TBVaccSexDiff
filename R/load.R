path_get_data_tidy_vacc_freq <- function() {
  path_tmp <- projr:::.projr_path_get_cache_auto_version(
    "data", "data_tidy_vacc_freq.rda",
    profile = Sys.getenv("PROJR_PROFILE", unset = "default")
  )
  if (file.exists(path_tmp)) {
    return(path_tmp)
  }
  path_output <- projr::projr_path_get(
    "data", "data_tidy_vacc_freq.rda",
    safe = FALSE
  )
  if (!file.exists(path_output)) {
    stop('Cannot find "data_tidy_vacc_freq.rda"')
  }
  path_output
}
