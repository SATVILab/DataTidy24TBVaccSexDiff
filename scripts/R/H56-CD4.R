# Loading -----------------------------------------------------------------

# names of current objects
currObjVec <- ls()

# read in data
fileLoc <- projr::projr_path_get(
  "data-raw-public", "h56.xlsx"
)
cd4Tbl <- as_tibble(read.xlsx(fileLoc, sheet = "CD4 Freq"))

# Data Prep ---------------------------------------------------------------

# UNIQUE VALUES

# unique values in id info
unique(cd4Tbl$Stimulation) # "Ag85B"  "ESAT6"  "PHA"    "Rv2660" "UNS"
unique(cd4Tbl$Study.Day) # 0  14  56  70 292 112 126. Correct
unique(cd4Tbl$LTBI) # "neg" "pos". Correct, but convert to 0 1
unique(cd4Tbl$Group) # 1 2 3 4. Correct
unique(cd4Tbl$`Vaccine/Placebo`) # "vaccine" "placebo". Correct, but convert to 0 1
unique(cd4Tbl$Dose) # 15 5 50 0 "?". Correct
unique(cd4Tbl$`Regimen.(#injections)`) # 2 1 3. Corrrect.

# renaming columns and converting to correct type
cd4Tbl %<>% mutate(Stimulation = str_to_lower(Stimulation)) %>%
  rename(group = Group, inf = LTBI, ptid = PTID, stim = Stimulation, timePoint = Study.Day, treatType = `Vaccine/Placebo`, dose = Dose) %>%
  mutate(group = as.character(group), inf = as.character(inf), dose = as.character(dose), timePoint = as.character(timePoint)) %>%
  rename(infxn = inf)

# cd4Tbl %>%
# filter(ptid == 'ID_03013' & timePoint == 70) # simply missing. Checked in raw data - not there either.

# dimension names
dimnames(cd4Tbl)[[2]][8] <- "doseCount"

for (i in 9:29) cd4Tbl[[i]] <- as.numeric(cd4Tbl[[i]])

# change doseCount variable type
cd4Tbl %<>% mutate(doseCount = as.character(doseCount))

# fill in ?s

# find the locations
# cd4Tbl %>%
#  filter(treatType == "?" | dose == "?") %>%
#  print(n = 10^5)

# finding replacement values

# cd8Tbl %>% filter(ptid == 'id_01015' & timePoint == 292) # dose = 50 & treatType = 1
# cd8Tbl %>% filter(ptid == 'id_02003' & timePoint == 0) # dose = 5 and treatType = 1
# cd8Tbl %>% filter(ptid == 'id_02012' & timePoint == 0) # dose = 5 & treatType = 1

# replacement
# cd4Tbl[ cd4Tbl$ptid == 'ID_01015', ]$dose = 50
# cd4Tbl[ cd4Tbl$ptid == 'ID_01015', ]$treatType = 'vaccine'
cd4Tbl[cd4Tbl$ptid == "h56_15", ]$dose <- "50"
cd4Tbl[cd4Tbl$ptid == "h56_15", ]$treatType <- "vaccine"

# cd4Tbl[ cd4Tbl$ptid == 'ID_02003', ]$dose = 5
# cd4Tbl[ cd4Tbl$ptid == 'ID_02003', ]$treatType = 'vaccine'
cd4Tbl[cd4Tbl$ptid == "h56_53", ]$dose <- "5"
cd4Tbl[cd4Tbl$ptid == "h56_53", ]$treatType <- "vaccine"


# cd4Tbl[ cd4Tbl$ptid == 'ID_02012', ]$dose = 5
# cd4Tbl[ cd4Tbl$ptid == 'ID_02012', ]$treatType = 'vaccine'
cd4Tbl[cd4Tbl$ptid == "h56_62", ]$dose <- "5"
cd4Tbl[cd4Tbl$ptid == "h56_62", ]$treatType <- "vaccine"

# editing entries
cd4Tbl %<>%
  mutate(
    infxn = sapply(infxn, function(x) ifelse(x == "neg", "0", "1")),
    ptid = str_to_lower(ptid),
    stim = str_to_lower(stim),
    treatType = sapply(treatType, function(x) ifelse(x == "vaccine", "1", "0"))
  )

### BOOLEAN COLUMN NAMES

# remove useless columns
cd4Tbl %<>% select(-c(`CD4+`, `g-T-2-17-`, `CD4+g+`, `CD4+T+`, `CD4+2+`, `CD4+17+`))

# get actual names
cytNameVec <- str_to_upper(tbl_vars(cd4Tbl)[9:23])

# get replacement names
for (i in 1:length(cytNameVec)) { # loop over names
  currCyt <- cytNameVec[i]
  # check if we have a positive in a given location
  # if so, add a p onto a string. If not, add an n
  locVec <- c(2, 4, 6, 9)
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

origCytNameVec <- tbl_vars(cd4Tbl)[9:23]
colNameCodeDF <- tibble(x = str_c("cd4Tbl %>% rename(cd4", cytNameVec, " = `", origCytNameVec, "`)"))

cd4Tbl <- eval(parse(text = colNameCodeDF[1, 1]))
cd4Tbl <- eval(parse(text = colNameCodeDF[2, 1]))
cd4Tbl <- eval(parse(text = colNameCodeDF[3, 1]))
cd4Tbl <- eval(parse(text = colNameCodeDF[4, 1]))
cd4Tbl <- eval(parse(text = colNameCodeDF[5, 1]))
cd4Tbl <- eval(parse(text = colNameCodeDF[6, 1]))
cd4Tbl <- eval(parse(text = colNameCodeDF[7, 1]))
cd4Tbl <- eval(parse(text = colNameCodeDF[8, 1]))
cd4Tbl <- eval(parse(text = colNameCodeDF[9, 1]))
cd4Tbl <- eval(parse(text = colNameCodeDF[10, 1]))
cd4Tbl <- eval(parse(text = colNameCodeDF[11, 1]))
cd4Tbl <- eval(parse(text = colNameCodeDF[12, 1]))
cd4Tbl <- eval(parse(text = colNameCodeDF[13, 1]))
cd4Tbl <- eval(parse(text = colNameCodeDF[14, 1]))
cd4Tbl <- eval(parse(text = colNameCodeDF[15, 1]))

### remove placebo
cd4Tbl %<>%
  filter(treatType == 1) %>%
  select(-treatType)

### RE-ORDER

cd4Tbl <- changeCytOrder(
  dataTibble = cd4Tbl, firstCytColIndex = 8,
  firstCytNameLoc = 4, signLocVec = c(5, 7, 9, 12),
  orderLocVec = c(1, 3, 2, 4), cdPresent = TRUE
)

# Data Checking -----------------------------------------------------------

# stim
# four stims
# cd4Tbl %>%
#  group_by(inf, group, ptid, timePoint) %>%
#  summarise(stimCount = n()) %>%
#  filter(stimCount != 5) # id_03008 & id_03019, both on day 14

# viewTbl = cd4Tbl %>%
#  filter(ptid == 'id_03008' | ptid == 'id_03019') %>%
#  filter(timePoint == 14)
viewTbl <- cd4Tbl %>%
  filter(ptid == " h56_86" | ptid == "h56_90") %>%
  filter(timePoint == 14) %>%
  select(group:doseCount) # both are missing unstim.

cd4Tbl %>%
  arrange(infxn, group, ptid, timePoint, stim) %>%
  group_by(infxn, group, ptid, timePoint) %>%
  summarise(stimVec = str_c(stim, sep = "", collapse = "")) %>%
  filter(stimVec != "ag85besat6pharv2660uns") # same as above, so everyone else has all the right stims for each day.

# cd4Tbl %<>%
#  filter(!(ptid == 'id_03008' & timePoint == '14')) %>%
#  filter(!(ptid == 'id_03019' & timePoint == '14'))

cd4Tbl %<>%
  filter(!(ptid == "h56_86" & timePoint == "14")) %>%
  filter(!(ptid == "h56_90" & timePoint == "14"))


# Subtraction ---------------------------------------------------------------

# save non-summed over table
cd4ITbl <- cd4Tbl %>%
  group_by(group, infxn, ptid, dose, doseCount, timePoint) %>%
  mutate_if(is.numeric, function(x) x - x[5]) %>%
  ungroup() %>%
  filter(stim %in% c("ag85b", "esat6", "rv2660"))

# summation
cd4Tbl %<>%
  group_by(group, infxn, ptid, dose, doseCount, timePoint) %>%
  summarise_if(is.numeric, function(x) sum(x[c(1, 2, 4)]) - 3 * x[5]) %>%
  ungroup()
# unique(cd4Tbl %>% filter(group ==)) %>% length()

# Editing -----------------------------------------------------------------

h564ITbl <- cd4ITbl %>%
  mutate(timePoint = as.numeric(timePoint))

h564Tbl <- cd4Tbl %>%
  mutate(timePoint = as.numeric(timePoint))

# Testing -----------------------------------------------------------------

# h564ITbl
testRow1 <- h564ITbl %>% filter(ptid == "h56_1" & timePoint == 0 & stim == "ag85b")
if (testRow1$CD4Gn2nTn17p != -0.001679) stop("h564ITbl error")
testRow2 <- h564ITbl %>% filter(ptid == "h56_32" & timePoint == 292 & stim == "esat6")
if (testRow2$CD4Gp2nTp17n != (0.000815 - 0.000465)) stop("h564ITbl error")
testRow3 <- h564ITbl %>% filter(ptid == "h56_97" & timePoint == 292 & stim == "esat6")
if (testRow3$CD4Gp2nTn17n != (0.030077 - 0.003363)) stop("h564ITbl error")
# h564Tbl
testRow1 <- h564Tbl %>% filter(ptid == "h56_1" & timePoint == 0)
if (signif(testRow1$CD4Gn2nTp17n, 6) != signif((0.009479 + 0.125443 + 0.010263 - 3 * 0.008241), 7)) stop("h564Tbl error")
if (testRow1$CD4Gn2pTn17n != (0.002916 + 0.000725 + 0.002932 - 3 * 0.000634)) stop("h564Tbl error")
if (testRow1$CD4Gp2pTp17n != (0.013853 + 0.008701 + 0 - 3 * 0)) stop("h564Tbl error")
# Clear Workspace ---------------------------------------------------------

# remove all objects but initial objects (currObjVec) and processed data tibble (h564Tbl)
rm(list = setdiff(ls(), c(currObjVec, "h564Tbl", "h564ITbl")))
