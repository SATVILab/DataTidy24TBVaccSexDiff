# Loading -----------------------------------------------------------------

# names of current objects
currObjVec <- ls()

# read in data
fileLoc <- projr::projr_path_get("data-raw-public", "h56.xlsx")
cd8Tbl <- as_tibble(read.xlsx(fileLoc, sheet = "CD8 Freq"))

# Data Prep ---------------------------------------------------------------

# UNIQUE VALUES

# unique values in id info
unique(cd8Tbl$Stimulation) # "Ag85B"  "ESAT6"  "PHA"    "Rv2660" "UNS". Make lower case.
unique(cd8Tbl$Study.Day) # 0  14  56  70 292 112 126. Correct
unique(cd8Tbl$LTBI) # "neg" "pos". Correct, but convert to 0 1
unique(cd8Tbl$Group) # 1 2 3 4. Correct
unique(cd8Tbl$`Vaccine/Placebo`) # "vaccine" "placebo". Correct, but convert to 0 1
unique(cd8Tbl$Dose) # 15 5 50 0 . Correct
unique(cd8Tbl$`Regimen.(#injections)`) # 2 1 3. Corrrect.

# renaming columns and converting to correct type
cd8Tbl %<>% mutate(Stimulation = str_to_lower(Stimulation)) %>%
  rename(group = Group, inf = LTBI, ptid = PTID, stim = Stimulation, timePoint = Study.Day, treatType = `Vaccine/Placebo`, dose = Dose) %>%
  mutate(group = as.character(group), inf = as.character(inf), dose = as.character(dose), timePoint = as.character(timePoint)) %>%
  rename(infxn = inf)

# correct day 292 and PTID ID_03017
# cd8Tbl[ cd8Tbl$ptid == 'ID_03017', "treatType" ] = 'vaccine'
# cd8Tbl[ cd8Tbl$ptid == 'ID_03017', "dose" ] = "5"
cd8Tbl[cd8Tbl$ptid == "h56_75", "treatType"] <- "vaccine"
cd8Tbl[cd8Tbl$ptid == "h56_75", "dose"] <- "5"

# dimension names
dimnames(cd8Tbl)[[2]][8] <- "doseCount"

# change doseCount variable type
cd8Tbl %<>% mutate(doseCount = as.character(doseCount))

# editing entries
cd8Tbl %<>%
  mutate(
    infxn = sapply(infxn, function(x) ifelse(x == "neg", "0", "1")),
    ptid = str_to_lower(ptid),
    stim = str_to_lower(stim),
    treatType = sapply(treatType, function(x) ifelse(x == "vaccine", "1", "0"))
  )

### BOOLEAN COLUMN NAMES
cd8Tbl2 <- cd8Tbl
# remove useless columns
cd8Tbl %<>% select(-c(`CD8+`, `g-T-2-17-`, `CD8+g+`, `CD8+T+`, `CD8+2+`, `CD8+17+`))

# get actual names
cytNameVec <- str_to_upper(tbl_vars(cd8Tbl)[9:23])

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

origCytNameVec <- tbl_vars(cd8Tbl)[9:23]
colNameCodeDF <- tibble(x = str_c("cd8Tbl %>% rename( cd8", cytNameVec, " = `", origCytNameVec, "`)"))

cd8Tbl <- eval(parse(text = colNameCodeDF[1, 1]))
cd8Tbl <- eval(parse(text = colNameCodeDF[2, 1]))
cd8Tbl <- eval(parse(text = colNameCodeDF[3, 1]))
cd8Tbl <- eval(parse(text = colNameCodeDF[4, 1]))
cd8Tbl <- eval(parse(text = colNameCodeDF[5, 1]))
cd8Tbl <- eval(parse(text = colNameCodeDF[6, 1]))
cd8Tbl <- eval(parse(text = colNameCodeDF[7, 1]))
cd8Tbl <- eval(parse(text = colNameCodeDF[8, 1]))
cd8Tbl <- eval(parse(text = colNameCodeDF[9, 1]))
cd8Tbl <- eval(parse(text = colNameCodeDF[10, 1]))
cd8Tbl <- eval(parse(text = colNameCodeDF[11, 1]))
cd8Tbl <- eval(parse(text = colNameCodeDF[12, 1]))
cd8Tbl <- eval(parse(text = colNameCodeDF[13, 1]))
cd8Tbl <- eval(parse(text = colNameCodeDF[14, 1]))
cd8Tbl <- eval(parse(text = colNameCodeDF[15, 1]))

### remove placebo
cd8Tbl %<>%
  filter(treatType == 1) %>%
  select(-treatType)

### RE-ORDER

cd8Tbl <- changeCytOrder(
  dataTibble = cd8Tbl, firstCytColIndex = 8,
  firstCytNameLoc = 4, signLocVec = c(5, 7, 9, 12),
  orderLocVec = c(1, 3, 2, 4), cdPresent = TRUE
)

# Data Checking -----------------------------------------------------------

# stim
# four stims
cd8Tbl %>%
  group_by(infxn, group, ptid, timePoint) %>%
  summarise(stimCount = n()) %>%
  filter(stimCount != 5)

viewTbl <- cd8Tbl %>%
  filter(ptid == "id_03008" | ptid == "id_03019") %>%
  filter(timePoint == 14)
# print( viewTbl, n =  10^19 ) # both are missing unstim.

cd8Tbl %>%
  arrange(infxn, group, ptid, timePoint, stim) %>%
  group_by(infxn, group, ptid, timePoint) %>%
  summarise(stimVec = str_c(stim, sep = "", collapse = "")) %>%
  filter(stimVec != "ag85besat6pharv2660uns") # same as above, so everyone else has all the right stims for each day.

# remove rows
cd8Tbl %<>%
  filter(!(ptid == "h56_86" & timePoint == "14")) %>%
  filter(!(ptid == "h56_90" & timePoint == "14"))


# Subtraction -------------------------------------------------------------

########################
### SUB UNS FROM BCG ###
########################

# check
cd8Tbl %>%
  arrange(infxn, group, ptid, timePoint, stim) %>%
  group_by(infxn, group, ptid, timePoint) %>%
  summarise(stimVec = str_c(stim, sep = "", collapse = "")) %>%
  filter(stimVec != "ag85besat6pharv2660uns") # none with missing values

# save non-summed over table
cd8ITbl <- cd8Tbl %>%
  group_by(group, infxn, ptid, dose, doseCount, timePoint) %>%
  mutate_if(is.numeric, function(x) x - x[5]) %>%
  ungroup() %>%
  filter(stim %in% c("ag85b", "esat6", "rv2660"))

# summation
cd8Tbl %<>%
  group_by(group, infxn, ptid, dose, doseCount, timePoint) %>%
  summarise_if(is.numeric, function(x) sum(x[c(1, 2, 4)]) - 3 * x[5]) %>%
  ungroup()

# Data Editing ------------------------------------------------------------

h568ITbl <- cd8ITbl %>%
  mutate(timePoint = as.numeric(timePoint))

h568Tbl <- cd8Tbl %>%
  mutate(timePoint = as.numeric(timePoint))

# Testing -----------------------------------------------------------------

# h564ITbl
testRow1 <- h568ITbl %>% filter(ptid == "h56_33" & timePoint == 0 & stim == "esat6")
if (testRow1$CD8Gp2nTn17n != (0.003297 - 0.000829)) stop("h568ITbl error")
testRow2 <- h568ITbl %>% filter(ptid == "h56_97" & timePoint == 126 & stim == "ag85b")
if (testRow2$CD8Gp2nTn17p != (0.001711 - 0.003881)) stop("h568ITbl error")
# h564Tbl
testRow1 <- h568Tbl %>% filter(ptid == "h56_96" & timePoint == 56)
if (testRow1$CD8Gn2pTn17n != (0.000539 + 0.002565 + 0.001957 - 3 * 0.000542)) stop("h568Tbl error")

# Clear workspace ---------------------------------------------------------


# remove all objects but initial objects (currObjVec) and processed data tibble (h564Tbl)
rm(list = setdiff(ls(), c(currObjVec, "h568Tbl", "h568ITbl")))
