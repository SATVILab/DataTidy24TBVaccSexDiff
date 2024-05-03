# Set-up ------------------------------------------------------------------

# names of current objects
currObjVec <- ls()

# tmax --------------------------------------------------------------------

### Sub

tmaxSub17IncTbl <- sub17IncTbl %>%
  filter(timePoint %in% c(182, 292, 210, 168, 364, 224, 294, 365, -1)) %>%
  mutate(cd = str_sub(cytCombo, start = 3, end = 3))

tmaxSub17IncITbl <- sub17IncITbl %>%
  filter(timePoint %in% c(182, 292, 210, 168, 364, 224, 294, 365, -1)) %>%
  mutate(cd = str_sub(cytCombo, start = 3, end = 3))

tmaxSub17ExcTbl <- sub17ExcTbl %>%
  filter(timePoint %in% c(182, 292, 210, 168, 364, 224, 294, 365, -1)) %>%
  mutate(cd = str_sub(cytCombo, start = 3, end = 3))

tmaxSub17ExcITbl <- sub17ExcITbl %>%
  filter(timePoint %in% c(182, 292, 210, 168, 364, 224, 294, 365, -1)) %>%
  mutate(cd = str_sub(cytCombo, start = 3, end = 3))

tmaxSubSingle17Tbl <- subSingle17Tbl %>%
  filter(timePoint %in% c(182, 292, 210, 168, 364, 224, 294, 365, -1))

tmaxSubSingle17ITbl <- subSingle17ITbl %>%
  filter(timePoint %in% c(182, 292, 210, 168, 364, 224, 294, 365, -1))

tmax17IncTbl <- join17IncTbl %>%
  filter(timePoint %in% c(182, 292, 210, 168, 364, 224, 294, 365, -1)) %>%
  mutate(cd = str_sub(cytCombo, start = 3, end = 3))

tmax17IncITbl <- join17IncITbl %>%
  filter(timePoint %in% c(182, 292, 210, 168, 364, 224, 294, 365, -1)) %>%
  mutate(cd = str_sub(cytCombo, start = 3, end = 3))

tmax17ExcTbl <- join17ExcTbl %>%
  filter(timePoint %in% c(182, 292, 210, 168, 364, 224, 294, 365, -1)) %>%
  mutate(cd = str_sub(cytCombo, start = 3, end = 3))

tmax17ExcITbl <- join17ExcITbl %>%
  filter(timePoint %in% c(182, 292, 210, 168, 364, 224, 294, 365, -1)) %>%
  mutate(cd = str_sub(cytCombo, start = 3, end = 3))

tmaxSingle17Tbl <- single17Tbl %>%
  filter(timePoint %in% c(182, 292, 210, 168, 364, 224, 294, 365, -1))

tmaxSingle17ITbl <- single17ITbl %>%
  filter(timePoint %in% c(182, 292, 210, 168, 364, 224, 294, 365, -1))

### Baseline---------------------------------------------------------

bl17IncTbl <- join17IncTbl %>%
  filter(timePoint == 0) %>%
  mutate(cd = str_sub(cytCombo, start = 3, end = 3))

bl17IncITbl <- join17IncITbl %>%
  filter(timePoint == 0) %>%
  mutate(cd = str_sub(cytCombo, start = 3, end = 3))

bl17ExcTbl <- join17ExcTbl %>%
  filter(timePoint == 0 | vaccine == 8) %>%
  mutate(cd = str_sub(cytCombo, start = 3, end = 3))

bl17ExcITbl <- join17ExcITbl %>%
  filter(timePoint == 0 | vaccine == 8) %>%
  mutate(cd = str_sub(cytCombo, start = 3, end = 3))

blSingle17Tbl <- single17Tbl %>% filter(timePoint == 0)

blSingle17ITbl <- single17ITbl %>% filter(timePoint == 0)

# Clear Workspace ---------------------------------------------------------

# remove all objects but initial objects (currObjVec) and processed data tibble (l8Tbl)
rm(list = setdiff(ls(), c(
  currObjVec, "tmaxSub17IncTbl", "tmaxSub17ExcTbl", "tmaxSubSingle17Tbl", "tmax17IncTbl", "tmax17ExcTbl", "tmaxSingle17Tbl", "bl17IncTbl", "bl17ExcTbl", "blSingle17Tbl",
  "tmaxSub17IncITbl", "tmaxSub17ExcITbl", "tmaxSubSingle17ITbl", "tmax17IncITbl", "tmax17ExcITbl", "tmaxSingle17ITbl", "bl17IncITbl", "bl17ExcITbl", "blSingle17ITbl"
)))
