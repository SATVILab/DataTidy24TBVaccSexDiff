#' Locate all occurrences of all strings in a character vector.
#'
#' \code{str_locate_multi} provides the a sorted vector
#' of the locations of character elements (or strings) in a character
#' string.
#'
#' @param string character. String to search through.
#' @param pattern character vector. Vector of strings to search for in \code{string}.
#' @return Numeric vector of found positions for any character in \code{pattern},
#' sorted in ascending order.
#' @examples
#' str_locate_multi("abcdeabcdexyza", c("ab", "x"))
str_locate_multi <- function(string, pattern) {
  lapply(as.list(pattern), function(x) str_locate_all(string, x)) %>%
    unlist() %>%
    unique() %>%
    sort()
}

#' Find the border positions of the characters to split for.
#'
#' \code{str_split_chunk_pos} provides a list, where
#' each element is a length-two integer vector of the
#' locations at which \code{string} must be split at
#' to create sub-strings (or chunks), where the right-hand
#' endpoints of thhe chunks are at an element of
#' \code{pattern}.
#'
#' @inheritParams str_locate_multi
#' @return A list with each element an ordered integer
#' vector containing the start and end position of each
#' chunk.
#' @examples
#' str_split_chunk_pos("abcdeabcdexyza", c("a", "x"))
str_split_chunk_pos <- function(string, pattern) {
  posVec <- str_locate_multi(string, pattern)
  if (length(posVec) == 0) {
    return(list(c(1, str_length(string))))
  }
  chunkBorderPosList <- list(c(1, posVec[1]))
  lapply(1:(length(posVec) - 1), function(i) {
    chunkBorderPosList[[i + 1]] <<- c(posVec[i] + 1, posVec[i + 1])
  })
  chunkBorderPosList
}

#' Split a string into chunks.
#'
#' \code{str_split_chunk} works like str_split, except that it:
#' \itemize{
#'   \item allows detection of multiple characters, and
#'   \item includes the characters from \code{pattern} in the
#'   chunks.
#' }
#' @return A list of strings.
#' @examples
#' str_split_chunk("GpTpIL2nIL17p", c("p", "n"))
str_split_chunk <- function(string, pattern) {
  chunkBorderPosList <- str_split_chunk_pos(string, pattern)
  lapply(1:length(chunkBorderPosList), function(i) {
    str_sub(
      string,
      chunkBorderPosList[[i]][1], chunkBorderPosList[[i]][2]
    )
  })
}


#' Re-order chunks in a string.
#'
#' \code{str_chunk_reorder_string} re-orders the chunks of a
#' string.
#'
#' @inheritParams str_locate_multi
#' @param order integer vector. The chunk originally in the i-th position
#' in \code{string} is placed in the \code{order[i]}-th position.
#' @return A string.
#' @examples
#' str_chunk_reorder_string("GpTpIL2nIL17p", c("p", "n"), c(1, 3, 2, 4))
str_chunk_reorder_string <- function(string, pattern, order) {
  chunkList <- str_split_chunk(string, pattern)
  if (length(order) != length(chunkList)) stop("Number of chunks implied by order not equal to number of chunks defined by pattern.")
  outString <- chunkList[[which(order == 1)]]
  if (length(order) > 1) {
    plyr::l_ply(2:length(order), function(i) {
      outString <<- str_c(outString, chunkList[[which(order == i)]])
    })
  }
  outString
}

#' Re-order chunks in a string.
#'
#' \code{str_chunk_reorder} re-orders the chunks of a string, and is vectorised.
#'
#' @inheritParams str_locate_multi
#' @inheritParams str_chunk_reorder_string
#'
#' @return A string, or a character vector.
#' @examples
#' str_chunk_reorder("GpTpIL2nIL17p", c("p", "n"), c(1, 3, 2, 4))
#' str_chunk_reorder(c("GpTpIL2nIL17p", "GpTpIL2nIL17n"), c("p", "n"), c(1, 3, 2, 4))
#' @export
str_chunk_reorder <- function(string, pattern, order) {
  plyr::laply(string, function(x) {
    str_chunk_reorder_string(x, pattern, order)
  })
}
