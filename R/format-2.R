.cyt_combn_reorder <- function(cyt_combn,
                               cyt_order,
                               pos_ind = "+",
                               neg_ind = "-",
                               debug = FALSE) {
  purrr::map_chr(
    cyt_combn,
    function(cyt_combn_curr) {
      out <- NULL
      if (debug) {
        print(cyt_combn)
      }

      for (cyt in cyt_order) {
        if (debug) {
          print(cyt_order)
        }
        if (.cyt_combn_reorder_detect_pos(
          cyt, cyt_combn_curr, pos_ind, neg_ind
        )) {
          if (debug) {
            print("pos detected")
          }
          out <- paste0(out, cyt, "+")
        } else if (
          .cyt_combn_reorder_detect_neg(
            cyt, cyt_combn_curr, pos_ind, neg_ind
          )
        ) {
          if (debug) {
            print("neg detected")
          }
          out <- paste0(out, cyt, "-")
        } else {
          stop("Detected neither pos nor neg")
        }
      }
      out
    }
  )
}

.cyt_combn_reorder_detect_pos <- function(cyt, cyt_combn, pos_ind, neg_ind) {
  if (pos_ind == "+") {
    pos_ind == "\\+"
  }
  if (neg_ind == "-") {
    neg_ind == "\\-"
  }
  paste0(cyt, pos_ind) |>
    .cyt_combn_reorder_fix_start(pos_ind, neg_ind) |>
    grepl(cyt_combn)
}

.cyt_combn_reorder_detect_neg <- function(cyt, cyt_combn, pos_ind, neg_ind) {
  if (neg_ind == "-") {
    neg_ind == "\\-"
  }
  if (pos_ind == "+") {
    pos_ind == "\\+"
  }
  paste0(cyt, neg_ind) |>
    .cyt_combn_reorder_fix_start(pos_ind, neg_ind) |>
    grepl(cyt_combn)
}

.cyt_combn_reorder_fix_start <- function(x, pos_ind, neg_ind) {
  if (pos_ind == "+") {
    pos_ind == "\\+"
  }
  if (neg_ind == "-") {
    neg_ind == "\\-"
  }
  paste0("^", x, "|", pos_ind, x, "|", neg_ind, x)
}

.anonymise <- function(pid, vaccine, trial) {
  pid_vec_unique <- unique(pid)
  int_vec <- sample(seq_along(pid_vec_unique), length(pid_vec_unique))
  pid_to_anon_vec <- setNames(
    paste0(vaccine[1], "-", trial[1], "-", int_vec),
    as.character(pid_vec_unique)
  )
  pid_to_anon_vec[as.character(pid)]
}
