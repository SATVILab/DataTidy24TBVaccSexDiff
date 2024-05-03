# Loading -----------------------------------------------------------------

# names of current objects
currObjVec <- ls()

# read in data
fileLoc <- projr::projr_path_get(
  "data-raw-public", "main.xlsx"
)
tbData <- as_tibble(read.xlsx(fileLoc, sheet = "Sheet 1"))

# Data Preparation ------------------------------------------------

# column editing

tbData %<>%
  rename(
    "ptid" = PatientID, "prid" = Protocol.ID, vaccine = Vaccine, infxn = Infxn,
    timePoint = TimePoint
  ) %>%
  select(-c(TubeName, UniqueID)) %>%
  mutate(
    vaccine = convVaccNameToInteger(vaccine),
    prid = convProtNameToInteger(prid),
    infxn = str_trim(infxn),
    infxn = ifelse(infxn == "positive", 1, 0),
    infxn = as.character(infxn)
  ) %>%
  arrange(vaccine, prid, infxn, ptid, timePoint)

# select only multiple cytokines

tbData %<>%
  gather(key = cytCombo, value = frequency, `CD4posGpos2pos17posTpos`:`TotCD8posTpos`) %>%
  filter(str_length(cytCombo) > 20) %>%
  spread(key = cytCombo, value = frequency) %>%
  arrange(vaccine, prid, infxn, ptid, timePoint)

# rename booleans

tbData %<>% rename(CD8posGNeg2pos17posTNeg = CDpos8GNeg2pos17posTNeg)

origCytVec <- dimnames(tbData)[[2]][6:35]

altCytVec <- sapply(origCytVec, function(x) str_c(str_sub(x, 1, 3), editStr(x, 7, 8), editStr(x, 11, 12), editStr(x, 20, 21), editStr(x, 15, 17), collapse = ""))

dimnames(tbData)[[2]][6:35] <- altCytVec

# make all Aeras402 uninfected
tbData[which(tbData$vaccine == 1), "infxn"] <- "0"

# make long
lOrigMainTbl <- tbData %>%
  gather(key = cytCombo, value = frequency, CD4Gn2nTp17n:CD8Gn2pTn17p) %>%
  filter(cytCombo != "CD4Gn2nTn17n" & cytCombo != "CD8n2nTn17n")

# add in stim column
lOrigMainITbl <- lOrigMainTbl %>%
  mutate(stim = nameStim(vaccine))

# Testing -----------------------------------------------------------------

# lOrigMainITbl
testVal1 <- lOrigMainITbl %>%
  filter(ptid == "main_1" & vaccine == 3 & timePoint == 0 & stim == "m72ppl" & cytCombo == "CD4Gn2pTp17n") %>%
  extract2("frequency")
if (testVal1 != 0.009) stop("lOrigMainITbl error")
testVal2 <- lOrigMainITbl %>%
  filter(ptid == "main_91" & vaccine == 4 & infxn == 0 & timePoint == 7 & stim == "ag85a" & cytCombo == "CD4Gp2pTp17p") %>%
  extract2("frequency")
if (testVal2 != 0.041) stop("lOrigMainITbl error")
testVal3 <- lOrigMainITbl %>%
  filter(ptid == "main_102" & vaccine == 4 & infxn == 1 & timePoint == 364 & stim == "ag85a" & cytCombo == "CD8Gn2nTn17p") %>%
  extract2("frequency")
if (testVal3 != 0.002) stop("lOrigMainITbl error")

# lOrigMainTbl
testVal1 <- lOrigMainTbl %>%
  filter(ptid == "main_1" & vaccine == 3 & timePoint == 0 & cytCombo == "CD4Gn2pTp17n") %>%
  extract2("frequency")
if (testVal1 != 0.009) stop("lOrigMainTbl error")
testVal2 <- lOrigMainTbl %>%
  filter(ptid == "main_91" & vaccine == 4 & infxn == 0 & timePoint == 7 & cytCombo == "CD4Gp2pTp17p") %>%
  extract2("frequency")
if (testVal2 != 0.041) stop("lOrigMainTbl error")
testVal3 <- lOrigMainTbl %>%
  filter(ptid == "main_102" & vaccine == 4 & infxn == 1 & timePoint == 364 & cytCombo == "CD8Gn2nTn17p") %>%
  extract2("frequency")
if (testVal3 != 0.002) stop("lOrigMainTbl error")

# Clear Workspace ---------------------------------------------------------

# remove all objects but initial objects (currObjVec) and processed data tibble (l8Tbl)
rm(list = setdiff(ls(), c(currObjVec, "lOrigMainTbl", "lOrigMainITbl")))
