#' Convert vaccine name to assigned integer.
#'
#' \code{convVaccNameToInteger} converts the names of the vaccines
#' each individual was given, as per the source file, to assigned
#' integers (stored as characters).
#'
#' The conversion table is as follows.
#' \tabular{cc}{
#' Original name \tab Converted name \cr
#' 402 \tab 1 \cr
#' H56 \tab 2 \cr
#' GSK.M72 \tab 3 \cr
#' MVA85A \tab 4 \cr
#' H1 \tab 5 \cr
#' BCG \tab 6 \cr
#' }
#'
#'
#' @param vaccVec character vector. Vector containing vaccine names.
#' @return character vector.
#' @export


convVaccNameToInteger <- function(vaccVec) {
  ### CONVERSION LOOP
  for (i in 1:length(vaccVec)) {
    currVac <- vaccVec[i]
    if (currVac == "402") {
      vaccVec[i] <- 1
    } else if (currVac == "H56") {
      vaccVec[i] <- 2
    } else if (currVac == "GSK.M72") {
      vaccVec[i] <- 3
    } else if (currVac == "MVA85A") {
      vaccVec[i] <- 4
    } else if (currVac == "H1") {
      vaccVec[i] <- 5
    } else if (currVac == "BCG") {
      vaccVec[i] <- 6
    } else {
      stop("Unknown vaccine in Main table")
    }
  }

  ### OUTPUT
  return(vaccVec)
}

#' Convert protocol name to assigned integer.
#'
#' \code{convProtNameToInteger} converts the names of the
#' trial protocols
#' each individual was given, as per the source file, to assigned
#' integers (stored as characters).
#'
#' The conversion table is as follows.
#' \tabular{cc}{
#' Original name \tab Converted name \cr
#' 402.003 \tab 1 \cr
#' 402.032 \tab 2 \cr
#' GSK.010 \tab 3 \cr
#' GSK.012 \tab 4 \cr
#' MVA.008 \tab 5 \cr
#' MVA.011 \tab 6 \cr
#' THYB-04 \tab 7 \cr
#' #' TBRU \tab 8 \cr
#' }
#'
#'
#' @param protVec character vector. Vector containing protocol names.
#' @return character vector.
#' @export

convProtNameToInteger <- function(protVec) {
  for (i in 1:length(protVec)) { # loop over observations
    currProt <- protVec[i] # current protocol
    if (currProt == "402.003") {
      protVec[i] <- 1
    } else if (currProt == "402.032") {
      protVec[i] <- 2
    } else if (currProt == "GSK.010") {
      protVec[i] <- 3
    } else if (currProt == "GSK.012") {
      protVec[i] <- 4
    } else if (currProt == "MVA.008") {
      protVec[i] <- 5
    } else if (currProt == "MVA.011") {
      protVec[i] <- 6
    } else if (currProt == "THYB-04") {
      protVec[i] <- 7
    } else if (currProt == "TBRU") {
      protVec[i] <- 8
    } else {
      stop("Unknown protocol in Main table")
    }
  }

  return(protVec)
}

#' Replace cytokine combination positivity indicator.
#'
#' \code{replaceCytPosIndicator} replaces whatever the original
#' indicators were for cytokine positivity and negativity in
#' the cytokine combination shorthand labels with selected
#' replacements (such as 'p' and 'n').
#'
#' @param dataTibble dataframe. A dataframe with the last columns
#' only being the names of cytokine combinations.
#' @param firstMultCytIndex integer. The column index of the first
#' column for frequencies for a cytokine combination.
#' @param posPattern,negPattern character. The original indicators for positivity
#' and negativity.
#' @param posRep,negRep character. The replacement indicators for
#' positivity and negativity.
#' @param alphaNumericOrigSign logical. If TRUE, then posPattern and
#' negPattern are alphanumeric.
#' @param signLocVec integer vector. Specifies the character positions of
#' posPattern and negPattern.
#'
#' @return A dataframe with cytokine combination labels formatted to
#' having the chosen cytokine positivity indicators.
#' @export

replaceCytPosIndicator <- function(
    dataTibble, firstMultCytIndex, posPattern,
    negPattern, posRep, negRep, alphaNumericOrigSign = FALSE,
    signLocVec = NULL) {
  ### PRELIMINARIES
  nCol <- ncol(dataTibble)
  origCytNameVec <- tbl_vars(dataTibble)[firstMultCytIndex:nCol]
  cytNameVec <- tbl_vars(dataTibble)[firstMultCytIndex:nCol]

  if (alphaNumericOrigSign == FALSE) {
    ### GET FORM TO REPLACE WITH
    cytNameVec <- str_replace_all(cytNameVec, pattern = posPattern, replacement = posRep)
    cytNameVec <- str_replace_all(cytNameVec, pattern = negPattern, replacement = negRep)
  } else if (alphaNumericOrigSign == TRUE) {
    for (i in 1:length(origCytNameVec)) {
      # current cytCombo
      currCyt <- origCytNameVec[i]

      ### SIGN CONVERSION

      # signs
      currSignVec <- str_sub(currCyt, signLocVec, signLocVec)

      for (j in 1:length(currSignVec)) {
        if (currSignVec[j] == posPattern) {
          str_sub(currCyt, signLocVec[j], signLocVec[j]) <- posRep
        } else if (currSignVec[j] == negPattern) {
          str_sub(currCyt, signLocVec[j], signLocVec[j]) <- negRep
        }
      }

      # save current cytName
      cytNameVec[i] <- currCyt
    }
  }

  ### CHANGE NAMES IN TABLE

  renameText <- str_sub(str_c("`", cytNameVec, "` = `", origCytNameVec, "`, ", sep = "", collapse = ""), end = -3L)
  parseText <- str_c("dataTibble %<>% rename(", renameText, ")")
  eval(parse(text = parseText))

  ### OUTPUT

  return(dataTibble)
}

#' Change order of cytokines in cytokine combination labels.
#'
#' \code{changeCytOrder} changes the order of cytokine combinations
#' in a dataframe.
#'
#' @param dataTibble dataframe. A dataframe with the cytokine
#' combination frequency response variables the last (right-most)
#' variables.
#' @param firstCytColIndex integer. The index of the first column
#' containing cytokine combination response data.
#' @param firstCytNameLoc integer. The first position in the string
#' of the cytokine combination label that specifies a cytokine.
#' This helps avoid prefixes like 'CD4' or 'CD8'.
#' @param signLocVec integer vector. The positions of the positivity
#' indicators for each cytokine.
#' @param orderLocVec integer vector. The i-th entry denotes the desired
#' order of the cytokine combination that originally appeared
#' i-th.
#' @param cdPresent logical. If TRUE, then the cytokine combination
#' originally is taken to be prefixed with CD4 or CD8, and
#' the cluster of differentiation is then stored as a
#' character vector. Defaults to TRUE.
#'
#' @return A dataframe with the order of the cytokines
#' in the cytokine combinations changed.
#' @export

changeCytOrder <- function(
    dataTibble, firstCytColIndex, firstCytNameLoc, signLocVec,
    orderLocVec, cdPresent = TRUE) {
  nCol <- ncol(dataTibble)
  cytNameVec <- tbl_vars(dataTibble)[firstCytColIndex:nCol]
  origCytNameVec <- tbl_vars(dataTibble)[firstCytColIndex:nCol]

  ### MANIPULATION

  if (cdPresent == TRUE) {
    origPreCytStoreVec <- str_sub(cytNameVec, end = firstCytNameLoc - 1)
    cdTypeVec <- ifelse(str_detect(origPreCytStoreVec, "4") == TRUE, 4, 8)
    cytNameVec <- str_sub(cytNameVec, start = firstCytNameLoc)
    signLocVec <- signLocVec - firstCytNameLoc + 1
  }

  # convert posPattern to posRep and negPattern to negRep

  for (i in 1:length(cytNameVec)) {
    # current cytCombo
    currCyt <- cytNameVec[i]

    ### ORDER ADJUSTMENT

    currCytStoreVec <- rep("a", length(signLocVec))
    currCytStoreVec[1] <- str_sub(currCyt, start = 1, end = signLocVec[1])

    for (j in 2:length(signLocVec)) {
      currCytStoreVec[j] <- str_sub(currCyt, start = signLocVec[j - 1] + 1, end = signLocVec[j])
    }

    newCytName <- ""

    for (j in 1:length(signLocVec)) {
      newAddindex <- which(orderLocVec == j)
      newCytName <- str_c(newCytName, currCytStoreVec[newAddindex])
    }

    ### FINAL EDITING

    cytNameVec[i] <- str_c("CD", cdTypeVec[i], newCytName, sep = "")
  }

  ### CHANGE NAMES IN TABLE

  midText <- str_sub(str_c("`", cytNameVec, "` = `", origCytNameVec, "`, ", sep = "", collapse = ""), end = -3L)
  parseText <- str_c("dataTibble %<>% rename(", midText, ")")
  eval(parse(text = parseText))
}

#' Remove the non-alphanumeric entries in specified columns.
#'
#' \code{removeNonANEntries} removes the non-alphanumeric entries in
#' columns specified by \code{columnNameVec} in \code{dataTibble}.
#'
#' @param dataTibble dataframe. A dataframe containing the
#' columns in columnNameVec.
#' @param columnNameVec character vector. A character vector
#' of the columns in \code{dataTibble} for which the
#' non-alphanumeric entries must be removed.
#' @export

removeNonANEntries <- function(dataTibble, columnNameVec) {
  for (i in 1:length(columnNameVec)) {
    currentColumnName <- columnNameVec[i]
    parseText <- str_c("currentString = dataTibble$", currentColumnName)
    eval(parse(text = parseText))

    upperLettersSearchVec <- str_to_upper(letters)
    lettersIndexVec <- as.character(1:26)
    numbersSearchVec <- as.character(0:9)
    numbersIndexVec <- as.character(1:10)

    upperList <- str_locate_all(currentString, upperLettersSearchVec)
    midText <- str_sub(str_c("upperList[[ ", lettersIndexVec, " ]][ , 1 ], ", collapse = ""), end = -3L)
    parseText <- str_c("lettersLocVec = c( ", midText, " )")
    eval(parse(text = parseText))

    numberList <- str_locate_all(currentString, numbersSearchVec)
    midText <- str_sub(str_c("numberList[[ ", numbersIndexVec, " ]][ , 1 ], ", collapse = ""), end = -3L)
    parseText <- str_c("numbersLocVec = c( ", midText, " )")
    eval(parse(text = parseText))

    anLocVec <- as.numeric(sort(c(lettersLocVec, numbersLocVec)))
    replacementString <- str_c(str_sub(currentString, start = anLocVec, end = anLocVec), sep = "", collapse = "")

    parseText <- str_c("dataTibble$", currentColumnName, " = replacementString")
    eval(parse(text = parseText))
  }

  # OUTPUT
  return(dataTibble)
}

#' Compare expected to actual column entries for BCG source data.
#'
#' \code{compToFullName} provides a list of rows for which the
#' expected data, as per \code{mainColumnName}, is not found in
#' \code{compColumnName}.
#'
#' @param dataTibble dataframe. A dataframe containing columns with
#' names as per \code{mainColumnName} and \code{compColumnName}.
#' @param mainColumnName,compColumnName character. Strings denoting the
#' actual (\code{mainColumnName}) and expected (\code{compColumnName})
#' entries in a row of \code{dataTibble}.
#'
#' @return A list of the differing entries in \code{mainColumnName} and
#' \code{compColumnName}.
#' @export

compToFullName <- function(dataTibble, compColumnName, mainColumnName) {
  ### PRELIMINARIES

  compVec <- eval(parse(text = str_c("dataTibble$", compColumnName))) # comparison vector
  mainVec <- eval(parse(text = str_c("dataTibble$", mainColumnName))) # comparison vector

  compValVec <- unique(compVec) # unique values of comparison vector

  nameText <- str_sub(str_c(compValVec, " = NULL, ", collapse = ""), end = -3L)
  parseText <- str_c("nonMatchingRowsList = list( ", nameText, " )")
  eval(parse(text = parseText))

  for (i in 1:length(compValVec)) {
    currVal <- compValVec[i]
    vec1 <- str_detect(compVec, c(currVal)) * 1
    vec2 <- str_detect(mainVec, c(currVal)) * 1
    nonMatchingRowsVec <- which(vec1 != vec2)

    if (length(nonMatchingRowsVec) > 0) {
      midText <- str_sub(str_c(nonMatchingRowsVec, ", ", sep = "", collapse = ""), end = -3L)
      parseText <- str_c("nonMatchingRowsList$", currVal, " = c( ", midText, " )")
      eval(parse(text = parseText))
    }
  }

  return(nonMatchingRowsList)
}

#' Converts the original timepoints in TBRU to days.
#'
#' \code{convTimePoint} converts the codes for timepoints in the
#' original TBRU dataset to days.
#'
#' The conversions were done as follows.
#'
#' \tabular{cc}{
#' Original value \tab Converted value \cr
#' V08 \tab 0 \cr
#' V13 \tab 21 \cr
#' V15 \tab 35 \cr
#' V28 \tab 365 \cr
#' }
#'
#' @param studyDayVector character vector. The character vector
#' based on the column in the original full data table that contains
#' the timepoint information.
#' @export


convTimePoint <- function(studyDayVector) {
  # PRELIMINARIES
  nObs <- length(studyDayVector)
  timePointVec <- rep(-1, length(studyDayVector))

  # REPLACEMENT LOOP
  for (i in 1:nObs) {
    # PRELIMINARIES

    currStudyDay <- studyDayVector[i]

    # REPLACEMENT LOOP

    if (currStudyDay == "V08") {
      timePointVec[i] <- 0
    } else if (currStudyDay == "V13") {
      timePointVec[i] <- 21
    } else if (currStudyDay == "V15") {
      timePointVec[i] <- 35
    } else if (currStudyDay == "V28") {
      timePointVec[i] <- 365
    }
  }

  # OUTPUT

  return(timePointVec)
}

#' String convenience functions
#'
#' Wrapper functions around str_ functions.
#'
#' @param str1 character.
#' @param startLoc,endLoc integer.
#' @name strConv
#'
NULL

#' @rdname strConv
#' @export
editStr <- function(str1, startLoc, endLoc) {
  strStart <- str_sub(str1, startLoc, endLoc - 1)
  strEnd <- str_sub(str1, endLoc, endLoc) %>% str_to_lower()
  strOut <- str_c(strStart, strEnd)
  return(strOut)
}

#' @rdname strConv
#' @export
editNonANStr <- function(str1, startLoc, endLoc) {
  strStart <- str_sub(str1, startLoc, endLoc - 1)
  strEnd <- str_sub(str1, endLoc, endLoc) %>% repPosNegInd()
  strOut <- str_c(strStart, strEnd)
  return(strOut)
}

#' @rdname strConv
#' @export
repPosNegInd <- function(str1) {
  if (str1 == "+") {
    return("p")
  }
  if (str1 == "-") {
    return("n")
  }
}


#' A functional operator that returns a counting function.
#'
#' Returns a function that returns an integer
#' one larger than the last integer it returned, starting
#' from one. In other words, the first time the
#' outputted function is run, it returns one, the second
#' time two, the third time three, and so on.
#'
#' @return A function as described in the description.
#' @export

countFuncClosure <- function() {
  k <- 0
  function() {
    k <<- k + 1
    k
  }
}

#' Give stims for each vaccine
#'
#' Given a vector of vaccine number references, it
#' returns the stim for that vaccine. Designed
#' for the lOrigMainTbl.
#'
#' @export
nameStim <- function(x) {
  x <- as.character(x)
  plyr::laply(x, function(x) {
    if (x == "1") {
      return("ag85b")
    }
    if (x == "2") {
      return("NA")
    }
    if (x == "3") {
      return("m72ppl")
    }
    if (x == "4") {
      return("ag85a")
    }
    if (x == "5") {
      return("NA")
    }
    if (x == "6") {
      return("NA")
    }
  })
}
