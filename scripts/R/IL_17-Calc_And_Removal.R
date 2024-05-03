# Set-up ------------------------------------------------------------------

# names of current objects
currObjVec <- ls()

# Megapool data preparation --------------------------------------------------------

# megTbl
# uniqueOrigPTIDVec = unique( megTbl$ptid )
# startPTID = max( as.numeric( join17ExcTbl$ptid ) ) + 1
# endPTID = startPTID + calcLU( uniqueOrigPTIDVec )
# repPTIDLabVec = setNames( startPTID:endPTID %>% as.character(), uniqueOrigPTIDVec )
joinMegTbl1 <- megTbl %>%
  # mutate( ptid = repPTIDLabVec[ptid] ) %>%
  select(ptid, infxn, timePoint, everything()) %>%
  mutate(vaccine = "8", prid = "10", timePoint = as.numeric(timePoint)) %>%
  gather(key = cytCombo, value = resp, CD4Gn2nTp:CD8Gp2pTp)

joinMegITbl1 <- megITbl %>%
  # mutate( ptid = repPTIDLabVec[ptid] ) %>%
  select(ptid, infxn, timePoint, everything()) %>%
  mutate(vaccine = "8", prid = "10", timePoint = as.numeric(timePoint)) %>%
  gather(key = cytCombo, value = resp, CD4Gn2nTp:CD8Gp2pTp)

# IL-17 Single -----------------------------------------------------------------

single17Tbl <- join17IncTbl %>%
  filter(str_sub(cytCombo, start = -1L) == "p") %>%
  mutate(cd = str_sub(cytCombo, start = 3, end = 3)) %>%
  group_by(vaccine, infxn, prid, ptid, cd, timePoint) %>%
  summarise(resp = sum(resp)) %>%
  ungroup()

single17ITbl <- join17IncITbl %>%
  filter(str_sub(cytCombo, start = -1L) == "p") %>%
  mutate(cd = str_sub(cytCombo, start = 3, end = 3)) %>%
  group_by(vaccine, infxn, prid, ptid, cd, timePoint, stim) %>%
  summarise(resp = sum(resp)) %>%
  ungroup()

# Sum over IL-17 ----------------------------------------------------------

# sum over il17
join17ExcTbl <- join17IncTbl %>%
  mutate(shortCytCombo = str_sub(cytCombo, end = -4)) %>%
  group_by(vaccine, prid, infxn, ptid, timePoint, shortCytCombo) %>%
  summarise(resp = sum(resp)) %>%
  ungroup() %>%
  rename(cytCombo = shortCytCombo) %>%
  filter(str_sub(cytCombo, 4) != "Gn2nTn") %>%
  filter(!(ptid == 28 & str_sub(cytCombo, 3, 3) == 4 & timePoint == 70)) %>%
  bind_rows(joinMegTbl1)

# sum over il17

join17ExcITbl <- join17IncITbl %>%
  mutate(shortCytCombo = str_sub(cytCombo, end = -4)) %>%
  group_by(vaccine, prid, infxn, ptid, stim, timePoint, shortCytCombo) %>%
  summarise(resp = sum(resp)) %>%
  ungroup() %>%
  rename(cytCombo = shortCytCombo) %>%
  filter(str_sub(cytCombo, 4) != "Gn2nTn") %>%
  filter(!(ptid == 28 & str_sub(cytCombo, 3, 3) == 4 & timePoint == 70)) %>%
  bind_rows(joinMegITbl1)

# Clear Workspace ---------------------------------------------------------

# remove all objects but initial objects (currObjVec) and processed data tibble (l8Tbl)
rm(list = setdiff(ls(), c(
  currObjVec, "join17IncTbl", "single17Tbl", "join17ExcTbl", "join17IncITbl",
  "single17ITbl", "join17ExcITbl", "joinMegTbl1", "joinMegITbl1"
)))
