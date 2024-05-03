# Loading -----------------------------------------------------------------

# names of current objects
currObjVec <- ls()

# find library containing raw data
pkgPath <- system.file(package = "VaccCompData")

# read in data
fileLoc <- projr::projr_path_get(
  "data-raw-public", "bcg.xlsx"
)
cd4Tbl <- as_tibble(read.xlsx(fileLoc, sheet = "CD4"))

# Data Prep I ---------------------------------------------------------------

### Goal: Put data into a format appropriate for later manipulation.
### That means that only multiple cytokine combinations are allowed,
### values under variables are consistent ( e.g. always OBI and never OBi ),
### and there are no unnecessary columns.

###################
### DATA BASICS ###
###################

nCol <- ncol(cd4Tbl)

##############################
### NO UNNECESSARY COLUMNS ###
##############################

# cd4Tbl %<>% select( -c( Subset, X49:X52 ) )
cd4Tbl %<>% select(-Subset)
# exclude Subset and X49:X52
# X49 to X52 not found in raw data, and Subset not useful since
# CD4 or CD8 is specified later and differently.

#########################
### CONSISTENT VALUES ###
#########################

# unique values in id info
unique(cd4Tbl$Stimulation) # "BCG"   "ECppl" "PHA"   "UNS". Correct
unique(cd4Tbl$Study.Day) # "SC-3" "V01"  "V08"  "V13"  "V15"  "V28"  "V20"  "V23"  "V30". Formatting seems fine.
unique(cd4Tbl$Group) # "IBO" "iBO" "OBI" "oBI". Incorrect
cd4Tbl$Group <- str_to_upper(cd4Tbl$Group) # correct above
unique(cd4Tbl$Group) # "IBO" "OBI". Corrected

# Sample
cd4Tbl$Sample <- str_to_upper(cd4Tbl$Sample) # all caps

# Stimulation
cd4Tbl$Stimulation <- ifelse(str_detect(cd4Tbl$Stimulation, "ECppl") == TRUE, "ECPP", cd4Tbl$Stimulation)

# Remove Non-AN
cd4Tbl %<>%
  mutate(rowNumber = 1:n()) %>%
  group_by(rowNumber) %>%
  do(removeNonANEntries(., columnNameVec = c("Sample", "Study.Day"))) %>%
  ungroup() %>%
  select(-rowNumber)

######################
### ONLY MULTIPLES ###
######################

cd4Tbl %<>%
  gather(key = cytCombo, value = frequency, `CD4+/2+`:`CD4+/2-17-22-g-T-`) %>%
  mutate(cytCombo = str_to_upper(cytCombo)) %>%
  filter(str_length(cytCombo) > 10) %>%
  spread(key = cytCombo, value = frequency) %>%
  arrange(Group, PTID, Study.Day, Stimulation)

#######################
### RENAME BOOLEANS ###
#######################

### Goal: Put dataTibble into format where cytokine combinations
### are consistently labelled with a p/n format, in a specific order.

### CHECK LENGTHS OF VARIABLE NAMES
unique(str_length(tbl_vars(cd4Tbl)[6:ncol(cd4Tbl)])) # all 17

### CHANGE POSITIVITY INDICATOR

cd4Tbl <- replaceCytPosIndicator(
  dataTibble = cd4Tbl, firstMultCytIndex = 6,
  posPattern = "+", negPattern = "-",
  posRep = "p", negRep = "n",
  alphaNumericOrigSign = TRUE,
  signLocVec = c(7, 10, 13, 15, 17)
)

### RE-ORDER

cd4Tbl <- changeCytOrder(
  dataTibble = cd4Tbl, firstCytColIndex = 6,
  firstCytNameLoc = 6, signLocVec = c(7, 10, 13, 15, 17),
  orderLocVec = c(2, 4, 5, 1, 3), cdPresent = TRUE
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

nonMatchingStimRowsList <- compToFullName(dataTibble = cd4Tbl, compColumnName = "Stimulation", mainColumnName = "Sample") # BCG - 1510. PHA - 1510.

cd4Tbl[nonMatchingStimRowsList$BCG, ] # "BCG"  "PHA"  "170176OBIV20BCGFCS". BCG in Sample, but PHA in Stim
# print(cd4Tbl %>% filter(PTID == 'ID_0176' & Study.Day == "V20" ), n = 28) # two PHAs in same day
cd4Tbl$Stimulation[nonMatchingStimRowsList$BCG] <- "BCG"
compToFullName(dataTibble = cd4Tbl, compColumnName = "Stimulation ", mainColumnName = "Sample") # All NULL.

##################
### STUDY DAYS ###
##################

### MATCHING STIMULATION AND SAMPLE

nonMatchingStudyDaysRowsList <- compToFullName(dataTibble = cd4Tbl, compColumnName = "Study.Day", mainColumnName = "Sample") # BCG - 1510. PHA - 1510.

cd4Tbl[nonMatchingStudyDaysRowsList$V13, ] # V13 in Study.Day, but V15 in Sample
# print(cd4Tbl %>% filter(PTID == 'ID_0063' & Study.Day == "V13" ), n = 28) # two V13s for same person in Study.Day
cd4Tbl$Study.Day[nonMatchingStudyDaysRowsList$V15] <- "V15"
compToFullName(dataTibble = cd4Tbl, compColumnName = "Stimulation ", mainColumnName = "Sample") # All NULL.

### STIMULATIONS BY STUDY DAY AND PTID COMBO

stimCountDF <- cd4Tbl %>%
  group_by(PTID, Study.Day) %>%
  summarise(stimCount = n(), bcgCount = sum(str_detect(Stimulation, "BCG") * 1), unsCount = sum(str_detect(Stimulation, "UNS") * 1))
unique(stimCountDF$stimCount) # 4. Each PTID and study day has four stims each.
unique(stimCountDF$bcgCount) # 1. Each PTID and Study day has one BCG
unique(stimCountDF$unsCount) # 1. Each PTID and study day has one UNS

# Calculation -------------------------------------------------------------

#####################
### SUM OVER IL22 ###
#####################

cd4Tbl <- cd4Tbl %>%
  gather(key = cytCombo, value = resp, CD4Gn2nTn17n22n:CD4Gp2pTp17p22p) %>%
  group_by(PTID, Stimulation, Study.Day, Group) %>%
  mutate(cytCombo = str_sub(cytCombo, end = -4)) %>%
  group_by(Sample, PTID, Stimulation, Study.Day, Group, cytCombo) %>%
  summarise(resp = sum(resp)) %>%
  ungroup() %>%
  spread(cytCombo, resp)

########################
### SUB UNS FROM BCG ###
########################

# test that the order of stims is as expected
testStr <- cd4Tbl %>%
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

cd4Tbl %<>%
  group_by(PTID, Study.Day) %>%
  arrange(PTID, Study.Day, Stimulation) %>%
  mutate_if(is.numeric, function(x) x[1] - x[4]) %>%
  slice(1) %>%
  ungroup()

cd4ITbl <- cd4Tbl %>%
  rename(stim = Stimulation) %>%
  select(-Sample)

cd4Tbl %<>%
  select(-c(Sample, Stimulation)) %>%
  ungroup()

# Data Prep II -------------------------------------------------------------

#####################
### COLUMN RENAME ###
#####################

cd4ITbl %<>%
  rename(ptid = PTID, group = Group)

cd4Tbl %<>%
  rename(ptid = PTID, group = Group)

##################################
### CONVERT STUDY DAYS TO DAYS ###
##################################

# l4ITbl = cd4ITbl %>%
#  gather( key = cytCombo, value = frequency, CD4Gn2nTn17n:CD4Gp2pTp17p ) %>%
#  mutate( timePoint = convTimePoint( Study.Day ) ) %>%
#  select( - Study.Day ) %>%
#  filter( timePoint != - 1)  %>%
#  filter( ptid != 'ID_0102' & ptid != 'ID_0104' & ptid != 'ID_0131' & ptid != 'ID_0250' )

l4ITbl <- cd4ITbl %>%
  gather(key = cytCombo, value = frequency, CD4Gn2nTn17n:CD4Gp2pTp17p) %>%
  mutate(timePoint = convTimePoint(Study.Day)) %>%
  select(-Study.Day) %>%
  filter(timePoint != -1) %>%
  filter(ptid != "bcg_31" & ptid != "bcg_12" & ptid != "bcg_17" & ptid != "bcg_30")

l4Tbl <- cd4Tbl %>%
  gather(key = cytCombo, value = frequency, CD4Gn2nTn17n:CD4Gp2pTp17p) %>%
  mutate(timePoint = convTimePoint(Study.Day)) %>%
  select(-Study.Day) %>%
  filter(timePoint != -1) %>%
  filter(ptid != "bcg_31" & ptid != "bcg_12" & ptid != "bcg_17" & ptid != "bcg_30")

# make stim lower case
l4ITbl %<>% mutate(stim = "bcg")


# Testing -----------------------------------------------------------------

# l4ITbl
testVal1 <- l4ITbl %>%
  filter(ptid == "bcg_8" & timePoint == 365 & stim == "bcg" & cytCombo == "CD4Gn2pTn17n") %>%
  extract2("frequency")
if (testVal1 != (0.003385 - 0 + 0.052802 - 0.007228)) stop("l4ITbl error")
testVal2 <- l4ITbl %>%
  filter(ptid == "bcg_1" & timePoint == 0 & stim == "bcg" & cytCombo == "CD4Gn2nTp17n") %>%
  extract2("frequency")
if (testVal2 != (0.000614 + 0.155341 - 0.016429)) stop("l4ITbl error")

# l4Tbl
testVal1 <- l4Tbl %>%
  filter(ptid == "bcg_8" & timePoint == 365 & cytCombo == "CD4Gn2pTn17n") %>%
  extract2("frequency")
if (testVal1 != (0.003385 - 0 + 0.052802 - 0.007228)) stop("l4Tbl error")
testVal2 <- l4Tbl %>%
  filter(ptid == "bcg_1" & timePoint == 0 & cytCombo == "CD4Gn2nTp17n") %>%
  extract2("frequency")
if (testVal2 != (0.000614 + 0.155341 - 0.016429)) stop("l4Tbl error")

# Clear Workspace ---------------------------------------------------------

# remove all objects but initial objects (currObjVec) and processed data tibble (l4Tbl)
rm(list = setdiff(ls(), c(currObjVec, "l4Tbl", "l4ITbl")))
