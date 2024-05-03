str_rep_pos_neg_char <- function(char) {
  if (char == "+") {
    return("p")
  }
  if (char == "-") {
    return("n")
  }
  char
}

str_rep_pos_neg_string <- function(string) {
  plyr::laply(str_split(string, "", simplify = TRUE), function(char) str_rep_pos_neg_char(char)) %>%
    str_c(collapse = "")
}

#' @export
str_rep_pos_neg <- function(string) {
  plyr::laply(string, function(x) str_rep_pos_neg_string(x))
}
