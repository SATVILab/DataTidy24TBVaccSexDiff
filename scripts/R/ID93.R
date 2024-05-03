# Loading -----------------------------------------------------------------

# names of current objects
currObjVec <- ls()

# find library containing raw data
pkgPath <- system.file(package = "VaccCompData")

# libraries
# read in data
fileLoc <- projr::projr_path_get(
  "data-raw-public", "id93.xlsx"
)
idriTbl <- as_tibble(read.xlsx(fileLoc, sheet = "Sheet 1"))

# Data Prep ---------------------------------------------------------------

# na check

# table of NA or not
naTbl <- sapply(idriTbl[, 8:ncol(idriTbl)], Vectorize(is.na)) %>% as_tibble()

# columns with NAs
naTbl %>%
  summarise_all(function(x) sum(x * 1)) %>%
  gather(key = cytCombo, value = naCount, `CD8+/G+T+2+17+`:`CD4+/G-T-2-17+`) %>%
  filter(naCount > 0) # only for that one triple positive. # it's for a PHA anyway.

# unique values

unique(idriTbl$SAMPLE.ID) # "ID93+GLA/SE" "Placebo". Must remove Placebos
unique(idriTbl$QFT.Status) # "QFT-" "QFT+". Correct, but convert to binary
unique(idriTbl$TUBE.NAME) # "PHA" "Rv1813" "Rv2608" "Rv3619" "Rv3620" "UNS". Correct.
unique(idriTbl$SAMPLE.ID) # all numbers, fine.
unique(idriTbl$COHORT) # 1 2 3 4. Use group 1 and 3.
unique(idriTbl$Study.Day) #  0  14  42 112 126 196 294. Fine.

# filter rows and remove unnecessary columns
idriTbl %<>%
  filter(COHORT == 1 | COHORT == 3) %>% # chose the two cohorts that give the same id93 and gla-se dose, but to different infection statuses
  filter(RANDOMISATION == "ID93+GLA/SE") %>%
  `[`(-c(2, 5))
unique(idriTbl$SAMPLE.ID)
firstCytCol <- 6

# rename columns and edit rows
idriTbl %<>%
  rename(
    infxn = QFT.Status, stim = TUBE.NAME, ptid = SAMPLE.ID, timePoint = Study.Day,
    group = COHORT
  ) %>%
  mutate(
    infxn = sapply(infxn, function(x) ifelse(x == "QFT-", "0", "1")),
    stim = str_to_lower(stim),
    ptid = as.character(ptid),
    timePoint = as.character(timePoint)
  )

#  remove pha stim
idriTbl %<>%
  filter(stim != "pha")

idriTbl %>%
  group_by(group) %>%
  summarise(n_distinct(infxn))

### BOOLEAN COLUMN NAMES

# remove useless columns
# idriTbl %<>% select( -c( `CD8+.Total.cytokine.response`, `CD4+.TOTAL` ))

# get actual names
cytNameVec <- str_to_upper(tbl_vars(idriTbl)[firstCytCol:ncol(idriTbl)])

# get replacement names
for (i in 1:length(cytNameVec)) { # loop over names
  currCyt <- cytNameVec[i]
  # check if we have a positive in a given location
  # if so, add a p onto a string. If not, add an n
  locVec <- c(7, 9, 11, 14)
  signVec <- str_sub(currCyt, locVec, locVec)
  for (j in 1:length(signVec)) {
    if (signVec[j] == "+") {
      str_sub(currCyt, locVec[j], locVec[j]) <- "p"
    } else if (signVec[j] == "-") {
      str_sub(currCyt, locVec[j], locVec[j]) <- "n"
    }
  }
  cytNameVec[i] <- currCyt
}

# replace names

origCytNameVec <- tbl_vars(idriTbl)[firstCytCol:ncol(idriTbl)]
colNameCodeDF <- tibble(x = str_c("idriTbl %<>% rename( `", cytNameVec, "` = `", origCytNameVec, "`)"))

for (i in 1:nrow(colNameCodeDF)) eval(parse(text = colNameCodeDF[i, 1]))

### RE-ORDER

idriTbl <- changeCytOrder(
  dataTibble = idriTbl, firstCytColIndex = firstCytCol,
  firstCytNameLoc = 6, signLocVec = c(7, 9, 11, 14),
  orderLocVec = c(1, 3, 2, 4), cdPresent = TRUE
)
# arrange rows

idriTbl %<>% arrange(group, infxn, ptid, timePoint, stim)

# Data Checking -----------------------------------------------------------

# stim

# number of stims
idriTbl %>%
  group_by(infxn, group, ptid, timePoint) %>%
  summarise(stimCount = n()) %>%
  filter(stimCount != 5) # all have five stims.

# number of unique stims
idriTbl %>%
  group_by(infxn, group, ptid, timePoint) %>%
  summarise(uniqueStimCount = n_distinct(stim)) %>%
  filter(uniqueStimCount != 5) # all have five unique stims.

# Subtraction -------------------------------------------------------------

###########################
### SUB UNS FROM OTHERS ###
###########################

# merge
idriTbl %<>% arrange(group, infxn, ptid, timePoint, stim)

# subtraction
idriITbl <- idriTbl %>%
  group_by(group, infxn, ptid, timePoint) %>%
  mutate_if(is.numeric, function(x) x - x[5]) %>%
  ungroup() %>%
  filter(stim != "uns")


# summation
idriTbl %<>%
  group_by(group, infxn, ptid, timePoint) %>%
  summarise_if(is.numeric, function(x) sum(x[1:4]) - 4 * x[5]) %>%
  ungroup()

# Final Editing -----------------------------------------------------------

# idriITbl %<>% mutate( ptid = str_c( "a", ptid, sep = "" ) )

# idriTbl %<>% mutate( ptid = str_c( "a", ptid, sep = "" ) )


# Testing -----------------------------------------------------------------

idriTbl %>%
  filter(ptid %in% c("id93_1", "id93_7", "id93_32", "id93_43")) %>%
  group_by(ptid) %>%
  slice(1) %>%
  select(group, ptid)
idriITbl
testRow1 <- idriITbl %>% filter(ptid == "id93_1" & timePoint == 0 & stim == "rv3619")
if (testRow1$CD4Gp2nTn17n != (0.009664287 - 0.007563361)) stop("idriITbl error")
testRow2 <- idriITbl %>% filter(ptid == "id93_43" & timePoint == 294 & stim == "rv2608")
if (testRow2$CD8Gp2nTn17n != (0.00115 - 0.000528)) stop("idriITbl error")
testRow3 <- idriITbl %>% filter(ptid == "id93_7" & timePoint == 112 & stim == "rv3620")
if (testRow3$CD8Gp2nTp17n != (0.1967563 - 0.1723022)) stop("idriITbl error")

testRow1 <- idriTbl %>% filter(ptid == "id93_32" & timePoint == 126)
if (signif(testRow1$CD4Gn2nTp17n, 7) != signif((0.005361815 + 0.01608234 + 0.01048621 + 0.02476414 - 4 * 0.008231904), 7)) stop("idriTbl error")

# Clear workspace ---------------------------------------------------------

# remove all objects but initial objects (currObjVec) and processed data tibble (idriTbl)
rm(list = setdiff(ls(), c(currObjVec, "idriTbl", "idriITbl")))
