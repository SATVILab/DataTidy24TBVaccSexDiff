# Set-up ------------------------------------------------------------------

# names of current objects
currObjVec <- ls()

# Data Join ---------------------------------------------------------------

### EDIT TABLES FOR JOINING

# Goal: Ensure tables all have the same columns before joining, and
# that the people who need to be excluded from the BCG tables are excluded.

### lOrigMainTbl

joinOrigMainTbl <- lOrigMainTbl %>% mutate(ptid = as.character(ptid))

joinOrigMainITbl <- lOrigMainITbl %>% mutate(ptid = as.character(ptid))

### h1Tbl

joinH1Tbl <- h1Tbl %>%
  rename(frequency = freq) %>%
  select(-c(ag85b, esat6)) %>%
  mutate(prid = "7", vaccine = "5")

joinH1ITbl <- h1ITbl %>%
  filter(stim != "freq") %>%
  rename(frequency = resp) %>%
  mutate(prid = "7", vaccine = "5")

### l4Tbl

join4Tbl <- l4Tbl %>%
  mutate(vaccine = "7", prid = "8", infxn = "1") %>%
  filter(group == "IBO") %>%
  select(-group)

join4ITbl <- l4ITbl %>%
  mutate(vaccine = "7", prid = "8", infxn = "1") %>%
  filter(group == "IBO") %>%
  select(-group)

### l8Tbl

join8Tbl <- l8Tbl %>%
  mutate(vaccine = "7", prid = "8", infxn = "1") %>%
  filter(group == "IBO") %>%
  select(-group)

join8ITbl <- l8ITbl %>%
  mutate(vaccine = "7", prid = "8", infxn = "1") %>%
  filter(group == "IBO") %>%
  select(-group)

### h564Tbl

join564Tbl <- h564Tbl %>%
  filter(dose == 5) %>%
  filter(group == 1 | group == 3) %>%
  select(-c(group, dose, doseCount)) %>%
  mutate(vaccine = "2", prid = "2") %>%
  gather(key = cytCombo, value = frequency, CD4Gp2pTp17p:CD4Gn2nTn17p)


join564ITbl <- h564ITbl %>%
  filter(dose == 5) %>%
  filter(group == 1 | group == 3) %>%
  select(-c(group, dose, doseCount)) %>%
  mutate(vaccine = "2", prid = "2") %>%
  gather(key = cytCombo, value = frequency, CD4Gp2pTp17p:CD4Gn2nTn17p)

### h568Tbl

join568Tbl <- h568Tbl %>%
  filter(dose == 5) %>%
  filter(group == 1 | group == 3) %>%
  select(-c(group, dose, doseCount)) %>%
  mutate(vaccine = "2", prid = "2") %>%
  gather(key = cytCombo, value = frequency, CD8Gp2pTp17p:CD8Gn2nTn17p)

join568ITbl <- h568ITbl %>%
  filter(dose == 5) %>%
  filter(group == 1 | group == 3) %>%
  select(-c(group, dose, doseCount)) %>%
  mutate(vaccine = "2", prid = "2") %>%
  gather(key = cytCombo, value = frequency, CD8Gp2pTp17p:CD8Gn2nTn17p)

# idriTbl

joinIdriTbl <- idriTbl %>%
  select(-group) %>%
  mutate(
    vaccine = "6", prid = "9",
    timePoint = as.numeric(timePoint)
  ) %>%
  gather(key = cytCombo, value = frequency, CD8Gp2pTp17p:CD4Gn2nTn17p)

joinIdriITbl <- idriITbl %>%
  select(-group) %>%
  mutate(
    vaccine = "6", prid = "9",
    timePoint = as.numeric(timePoint)
  ) %>%
  gather(key = cytCombo, value = frequency, CD8Gp2pTp17p:CD4Gn2nTn17p)

# aerasTbl

joinAerasTbl <- aerasTbl %>%
  mutate(
    prid = "1",
    timePoint = as.character(timePoint)
  ) %>%
  gather(key = cytCombo, value = frequency, Gn2nTn17p:Gp2pTp17p) %>%
  mutate(cytCombo = str_c("CD", cd, cytCombo)) %>%
  select(-cd)

joinAerasITbl <- aerasITbl %>%
  mutate(
    prid = "1",
    timePoint = as.character(timePoint)
  ) %>%
  gather(key = cytCombo, value = frequency, Gn2nTn17p:Gp2pTp17p) %>%
  mutate(cytCombo = str_c("CD", cd, cytCombo)) %>%
  select(-cd)

### JOIN TABLES

firstJoinTbl <- bind_rows(joinOrigMainTbl, join4Tbl, join8Tbl, join564Tbl, join568Tbl, joinIdriTbl, joinH1Tbl) %>%
  rbind(joinAerasTbl) %>%
  filter(str_sub(cytCombo, start = 4) != "Gn2nTn17n") %>%
  arrange(vaccine, infxn, prid, ptid, timePoint) %>%
  mutate(frequency = sapply(frequency, max, 0))

firstJoinITbl <- bind_rows(joinOrigMainITbl, join4ITbl, join8ITbl, join564ITbl, join568ITbl, joinIdriITbl, joinH1ITbl) %>%
  rbind(joinAerasITbl) %>%
  filter(str_sub(cytCombo, start = 4) != "Gn2nTn17n") %>%
  arrange(vaccine, infxn, prid, ptid, timePoint) %>%
  mutate(frequency = sapply(frequency, max, 0))

# Join tables -------------------------------------------------------------

join17IncTbl <- firstJoinTbl %>%
  filter(cytCombo != "CD4Gn2nTn17n" & cytCombo != "CD8Gn2nTn17n") %>%
  arrange(vaccine, prid, infxn, ptid, cytCombo, timePoint) %>%
  mutate(ptid = as.character(ptid)) %>%
  select(ptid:infxn, timePoint, cytCombo, frequency) %>%
  ungroup() %>%
  mutate(timePoint = as.numeric(timePoint)) %>%
  arrange(vaccine, prid, infxn, ptid, cytCombo, timePoint) %>%
  rename(resp = frequency) %>%
  mutate(resp = pmax(resp, 0))

join17IncITbl <- firstJoinITbl %>%
  filter(cytCombo != "CD4Gn2nTn17n" & cytCombo != "CD8Gn2nTn17n") %>%
  arrange(vaccine, prid, infxn, ptid, cytCombo, stim, timePoint) %>%
  mutate(ptid = as.character(ptid)) %>%
  select(ptid:infxn, stim, timePoint, cytCombo, frequency) %>%
  ungroup() %>%
  mutate(timePoint = as.numeric(timePoint)) %>%
  arrange(vaccine, prid, infxn, ptid, stim, cytCombo, timePoint) %>%
  rename(resp = frequency) %>%
  mutate(resp = pmax(resp, 0))

# Clear Workspace ---------------------------------------------------------

# remove all objects but initial objects (currObjVec) and processed data tibble (l8Tbl)
rm(list = setdiff(ls(), c(currObjVec, "join17IncTbl", "join17IncITbl")))
