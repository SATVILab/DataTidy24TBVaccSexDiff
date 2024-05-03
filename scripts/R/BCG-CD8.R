# Loading -----------------------------------------------------------------

# names of current objects
currObjVec <- ls()

# read in data
fileLoc <- projr::projr_path_get(
  "data-raw-public", "bcg.xlsx"
)
cd8Tbl <- as_tibble(read.xlsx(fileLoc, sheet = "CD8"))

# Data Prep I ---------------------------------------------------------------

### Goal: Put data into a format appropriate for later manipulation.
### That means that only multiple cytokine combinations are allowed,
### values under variables are consistent ( e.g. always OBI and never OBi ),
### and there are no unnecessary columns.

###################
### DATA BASICS ###
###################

nCol <- ncol(cd8Tbl)

#########################
### CONSISTENT VALUES ###
#########################

# unique values in id info
unique(cd8Tbl$Stimulation) # "BCG"   "ECppl" "PHA"   "UNS". Correct
unique(cd8Tbl$Study.Day) # "SC-3" "V01"  "V08"  "V13"  "V15"  "V28"  "V20"  "V23"  "V30".
unique(cd8Tbl$Group) # "IBO" "iBO" "OBI" "oBI". Incorrect
cd8Tbl$Group <- str_to_upper(cd8Tbl$Group) # correct above
unique(cd8Tbl$Group) # "IBO" "OBI". Corrected

# Sample
cd8Tbl$Sample <- str_to_upper(cd8Tbl$Sample) # all caps

# Stimulation
cd8Tbl$Stimulation <- ifelse(str_detect(cd8Tbl$Stimulation, "ECppl") == TRUE, "ECPP", cd8Tbl$Stimulation)

# Remove Non-AN
cd8Tbl %<>%
  mutate(rowNumber = 1:n()) %>%
  group_by(rowNumber) %>%
  do(removeNonANEntries(., columnNameVec = c("Sample", "Study.Day"))) %>%
  ungroup() %>%
  select(-rowNumber)

#############################################
### UPPER CASE CYTOKINE NAMES IN BOOLEANS ###
#############################################

cd8Tbl %<>%
  gather(key = cytCombo, value = frequency, `CD8+/2+17+g+22+T+`:`CD8+/2-17-g-22-T-`) %>%
  mutate(cytCombo = str_to_upper(cytCombo)) %>%
  spread(key = cytCombo, value = frequency) %>%
  arrange(Group, PTID, Study.Day, Stimulation)

#######################
### RENAME BOOLEANS ###
#######################

### Goal: Put dataTibble into format where cytokine combinations
### are consistently labelled with a p/n format, in a specific order.

### CHANGE POSITIVITY INDICATOR

cd8Tbl <- replaceCytPosIndicator(
  dataTibble = cd8Tbl, firstMultCytIndex = 6,
  posPattern = "+", negPattern = "-",
  posRep = "p", negRep = "n",
  alphaNumericOrigSign = TRUE,
  signLocVec = c(7, 10, 12, 15, 17)
)
### RE-ORDER

cd8Tbl <- changeCytOrder(
  dataTibble = cd8Tbl, firstCytColIndex = 6,
  firstCytNameLoc = 6, signLocVec = c(7, 10, 12, 15, 17),
  orderLocVec = c(2, 4, 1, 5, 3), cdPresent = TRUE
)

# Data Checking -----------------------------------------------------------

# Goal: Check for any errors that can be detected detect, and correct these.

####################
### STIMULATIONS ###
####################

# Goal: Check that the stimulation listed in the Sample column is
# the same as the stimulation listed in the Stimulation column,
# and there is a BCG and UNS for each study day and ptid combo.

### MATCHING STIMULATION AND SAMPLE

nonMatchingStimRowsList <- compToFullName(dataTibble = cd8Tbl, compColumnName = "Stimulation", mainColumnName = "Sample") # BCG - 1510. PHA - 1510.

cd8Tbl[nonMatchingStimRowsList$BCG, ] # "BCG"  "PHA"  "170176OBIV20BCGFCS". BCG in Sample, but PHA in Stim
# print(cd8Tbl %>% filter(PTID == 'ID_0176' & Study.Day == "V20" ), n = 28) # two PHAs in same day
cd8Tbl$Stimulation[nonMatchingStimRowsList$BCG] <- "BCG"
compToFullName(dataTibble = cd8Tbl, compColumnName = "Stimulation ", mainColumnName = "Sample") # All NULL.

##################
### STUDY DAYS ###
##################

### MATCHING STIMULATION AND SAMPLE

nonMatchingStudyDaysRowsList <- compToFullName(dataTibble = cd8Tbl, compColumnName = "Study.Day", mainColumnName = "Sample") # BCG - 1510. PHA - 1510.

cd8Tbl[nonMatchingStudyDaysRowsList$V13, ] # V13 in Study.Day, but V15 in Sample
# print(cd4Tbl %>% filter(PTID == 'ID_0063' & Study.Day == "V13" ), n = 28) # two V13s for same person in Study.Day
cd8Tbl$Study.Day[nonMatchingStudyDaysRowsList$V15] <- "V15"
compToFullName(dataTibble = cd8Tbl, compColumnName = "Stimulation ", mainColumnName = "Sample") # All NULL.

### STIMULATIONS BY STUDY DAY AND PTID COMBO

stimCountDF <- cd8Tbl %>%
  group_by(PTID, Study.Day) %>%
  summarise(stimCount = n(), bcgCount = sum(str_detect(Stimulation, "BCG") * 1), unsCount = sum(str_detect(Stimulation, "UNS") * 1))
unique(stimCountDF$stimCount) # 4. Each PTID and study day has four stims each.
unique(stimCountDF$bcgCount) # 1. Each PTID and Study day has one BCG
unique(stimCountDF$unsCount) # 1. Each PTID and study day has one UNS

# Calculation -------------------------------------------------------------

#####################
### SUM OVER IL22 ###
#####################

cd8Tbl <- cd8Tbl %>%
  gather(key = cytCombo, value = resp, CD8Gn2nTn17n22n:CD8Gp2pTp17p22p) %>%
  group_by(PTID, Stimulation, Study.Day, Group) %>%
  mutate(cytCombo = str_sub(cytCombo, end = -4)) %>%
  group_by(Sample, PTID, Stimulation, Study.Day, Group, cytCombo) %>%
  summarise(resp = sum(resp)) %>%
  ungroup() %>%
  spread(cytCombo, resp)

########################
### SUB UNS FROM BCG ###
########################
########################

# test that the order of stims is as expected
testStr <- cd8Tbl %>%
  group_by(PTID, Study.Day) %>%
  arrange(PTID, Study.Day, Stimulation) %>%
  summarise(stim = paste0(
    Stimulation[1], Stimulation[2],
    Stimulation[3], Stimulation[4]
  )) %>%
  ungroup() %>%
  `[[`("stim") %>%
  unique()

if (!identical(testStr, "BCGECPPPHAUNS")) stop("cd8Tbl does not have the same stims, or the same order of stims, for each individual.")

# calculation
cd8Tbl %<>%
  group_by(PTID, Study.Day) %>%
  arrange(PTID, Study.Day, Stimulation) %>%
  mutate_if(is.numeric, function(x) x[1] - x[4]) %>%
  slice(1) %>%
  ungroup()

cd8ITbl <- cd8Tbl %>%
  rename(stim = Stimulation) %>%
  select(-Sample)

cd8Tbl %<>%
  select(-c(Sample, Stimulation))

# Data Prep II -------------------------------------------------------------

#####################
### COLUMN RENAME ###
#####################

cd8ITbl %<>%
  rename(ptid = PTID, group = Group)

cd8Tbl %<>%
  rename(ptid = PTID, group = Group)

##################################
### CONVERT STUDY DAYS TO DAYS ###
##################################

l8ITbl <- cd8ITbl %>%
  gather(key = cytCombo, value = frequency, CD8Gn2nTn17n:CD8Gp2pTp17p) %>%
  mutate(timePoint = convTimePoint(Study.Day)) %>%
  select(-Study.Day) %>%
  filter(timePoint != -1) %>%
  filter(ptid != "bcg_31" & ptid != "bcg_12" & ptid != "bcg_17" & ptid != "bcg_30")

l8Tbl <- cd8Tbl %>%
  gather(key = cytCombo, value = frequency, CD8Gn2nTn17n:CD8Gp2pTp17p) %>%
  mutate(timePoint = convTimePoint(Study.Day)) %>%
  select(-Study.Day) %>%
  filter(timePoint != -1) %>%
  filter(ptid != "bcg_31" & ptid != "bcg_12" & ptid != "bcg_17" & ptid != "bcg_30")

# make stim lower case
l8ITbl %<>% mutate(stim = "bcg")


# Testing -----------------------------------------------------------------


# l8ITbl
testVal1 <- l8ITbl %>%
  filter(ptid == "bcg_10" & timePoint == 35 & stim == "bcg" & cytCombo == "CD8Gn2nTp17n") %>%
  extract2("frequency")
if (testVal1 != (0.001242 - 0.00062 + 0.009933 - 0.014876)) stop("l8ITbl error")
testVal2 <- l8ITbl %>%
  filter(ptid == "bcg_1" & timePoint == 0 & stim == "bcg" & cytCombo == "CD8Gn2nTp17n") %>%
  extract2("frequency")
if (testVal2 != (0.010349 - 0.00065)) stop("l8ITbl error")

# l8Tbl
testVal1 <- l8Tbl %>%
  filter(ptid == "bcg_10" & timePoint == 35 & cytCombo == "CD8Gn2nTp17n") %>%
  extract2("frequency")
if (testVal1 != (0.001242 - 0.00062 + 0.009933 - 0.014876)) stop("l8ITbl error")
testVal2 <- l8Tbl %>%
  filter(ptid == "bcg_1" & timePoint == 0 & cytCombo == "CD8Gn2nTp17n") %>%
  extract2("frequency")
if (testVal2 != (0.010349 - 0.00065)) stop("l8ITbl error")

# Clear Workspace ---------------------------------------------------------

# remove all objects but initial objects (currObjVec) and processed data tibble (l8Tbl)
rm(list = setdiff(ls(), c(currObjVec, "l8Tbl", "l8ITbl")))
