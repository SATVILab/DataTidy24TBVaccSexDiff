# Set-up ------------------------------------------------------------------

# names of current objects
currObjVec <- ls()


# Calculation -------------------------------------------------------------

sub17IncTbl <- join17IncTbl %>%
  group_by(vaccine, infxn, cytCombo) %>%
  mutate(med = median(resp[timePoint == 0])) %>% # calculate median day zero response for each vaccine, infection status and cytCombo combination
  ungroup() %>%
  arrange(timePoint) %>%
  group_by(vaccine, infxn, cytCombo, ptid) %>% # subtract either an individual's on baseline, or the median baseline for its vaccine, infection status and cytokine combination if day zero is missing
  do(calcSub(.)) %>%
  select(-med) %>%
  ungroup()

sub17IncITbl <- join17IncITbl %>%
  group_by(vaccine, infxn, stim, cytCombo) %>%
  mutate(med = median(resp[timePoint == 0])) %>% # calculate median day zero response for each vaccine, infection status and cytCombo combination
  ungroup() %>%
  arrange(timePoint) %>%
  group_by(vaccine, infxn, cytCombo, stim, ptid) %>% # subtract either an individual's on baseline, or the median baseline for its vaccine, infection status and cytokine combination if day zero is missing
  do(calcSub(.)) %>%
  select(-med) %>%
  ungroup()

sub17ExcTbl <- join17ExcTbl %>%
  filter(vaccine != 8) %>%
  group_by(vaccine, infxn, cytCombo) %>%
  mutate(med = median(resp[timePoint == 0])) %>% # calculate median day zero response for each vaccine, infection status and cytCombo combination
  ungroup() %>%
  arrange(timePoint) %>%
  group_by(vaccine, infxn, cytCombo, ptid) %>% # subtract either an individual's on baseline, or the median baseline for its vaccine, infection status and cytokine combination if day zero is missing
  do(calcSub(.)) %>%
  select(-med) %>%
  ungroup() %>%
  bind_rows(joinMegTbl1)

sub17ExcITbl <- join17ExcITbl %>%
  filter(vaccine != 8) %>%
  group_by(vaccine, infxn, stim, cytCombo) %>%
  mutate(med = median(resp[timePoint == 0])) %>% # calculate median day zero response for each vaccine, infection status and cytCombo combination
  ungroup() %>%
  arrange(timePoint) %>%
  group_by(vaccine, infxn, cytCombo, stim, ptid) %>% # subtract either an individual's on baseline, or the median baseline for its vaccine, infection status and cytokine combination if day zero is missing
  do(calcSub(.)) %>%
  select(-med) %>%
  ungroup() %>%
  bind_rows(joinMegITbl1)

subSingle17Tbl <- single17Tbl %>%
  group_by(vaccine, infxn, cd) %>%
  mutate(med = median(resp[timePoint == 0])) %>% # calculate median day zero response for each vaccine, infection status and cytCombo combination
  ungroup() %>%
  arrange(timePoint) %>%
  group_by(vaccine, infxn, cd, ptid) %>% # subtract either an individual's on baseline, or the median baseline for its vaccine, infection status and cytokine combination if day zero is missing
  do(calcSub(.)) %>%
  select(-med) %>%
  ungroup()

subSingle17ITbl <- single17ITbl %>%
  group_by(vaccine, infxn, cd) %>%
  mutate(med = median(resp[timePoint == 0])) %>% # calculate median day zero response for each vaccine, infection status and cytCombo combination
  ungroup() %>%
  arrange(timePoint) %>%
  group_by(vaccine, infxn, cd, ptid) %>% # subtract either an individual's on baseline, or the median baseline for its vaccine, infection status and cytokine combination if day zero is missing
  do(calcSub(.)) %>%
  select(-med) %>%
  ungroup()

# Testing -----------------------------------------------------------------

calcVec <- sub17IncTbl %>%
  filter(ptid == 116 & cytCombo == "CD4Gp2pTp17p") %>%
  extract2("resp")
testVec <- join17IncTbl %>%
  filter(ptid == 116 & cytCombo == "CD4Gp2pTp17p") %>%
  mutate(resp = resp - resp[1]) %>%
  extract2("resp")
if (!identical(testVec, calcVec)) print("sub17IncTbl error")

calcVec <- sub17ExcTbl %>%
  filter(ptid == 165 & cytCombo == "CD4Gp2pTp") %>%
  extract2("resp")
testMed <- join17ExcTbl %>%
  filter(vaccine == 5 & infxn == 0 & timePoint == 0 & cytCombo == "CD4Gp2pTp") %>%
  summarise(med = median(resp)) %>%
  extract2("med")
testVec <- join17ExcTbl %>%
  filter(ptid == 165 & cytCombo == "CD4Gp2pTp") %>%
  mutate(resp = resp - testMed) %>%
  extract2("resp")
if (!identical(testVec, calcVec)) print("sub17IncTbl error")

# Clear Workspace ---------------------------------------------------------

# remove all objects but initial objects (currObjVec) and processed data tibble (l8Tbl)
rm(list = setdiff(ls(), c(currObjVec, "sub17IncTbl", "sub17ExcTbl", "subSingle17Tbl", "sub17IncITbl", "sub17ExcITbl", "subSingle17ITbl")))
