# Loading -----------------------------------------------------------------

# names of current objects
currObjVec <- ls()



# read in data
fileLoc <- projr::projr_path_get(
  "data-raw-public", "h1.xlsx"
)
h1Tbl <- as_tibble(read.xlsx(fileLoc, sheet = "Sheet 1"))

# table with info of which IDs are in group 1
fileLoc <- projr::projr_path_get(
  "data-raw-public", "h1-group_info.xlsx"
)
fTbl <- as_tibble(read.xlsx(fileLoc))

# francesca's data
groupOnePtidVec <- fTbl %>%
  filter(Group == 1) %>%
  `[[`("SubjectID") %>%
  unique()

# Data Prep ---------------------------------------------------------------

# rename and select columns
h1Tbl %<>%
  rename(ptid = SAMPLE.ID, stim = TUBE.NAME, timePoint = Timepoint, group = Group) %>%
  select(-c(Sample, `$FIL`))

# h1Tbl$group %>% unique() # 1 2, so only two groups for some reason.

# further editing
h1Tbl %<>%
  mutate(
    stim = str_to_lower(stim),
    timePoint = as.character(timePoint) %>% str_trim(),
    ptid = as.character(ptid)
  ) %>%
  filter(ptid %in% groupOnePtidVec) %>%
  rename(infxn = group) %>%
  separate(col = timePoint, into = c("Useless", "Number")) %>%
  mutate(
    timePoint = as.numeric(Number),
    infxn = sapply(infxn, function(x) ifelse(x == 2, "1", "0"))
  ) %>%
  select(-c(Useless, Number)) %>%
  select(ptid, infxn, timePoint, stim, everything()) %>%
  arrange(infxn, ptid, timePoint, stim)

# h1Tbl$stim %>% unique() # ""Ag85B" "ESAT6" "PHA"   "UNS". make lower case
# h1Tbl$ptid %>% unique() # all numbers. Correct
# h1Tbl$timePoint %>% unique()  #  0  14  56  70 224, post transform. Correct.
# h1Tbl %>% group_by( ptid ) %>% slice( 1 ) %>% ungroup() %>% group_by( infxn ) %>% summarise( count = n() ) # 1 - 34. 2 - 24. So group is infxn status.

# edit cytCombo names

origCytVec <- dimnames(h1Tbl)[[2]][5:ncol(h1Tbl)]

altCytVec <- sapply(origCytVec, function(x) str_c(str_sub(x, 1, 3), editNonANStr(x, 6, 7), editNonANStr(x, 10, 11), editNonANStr(x, 8, 9), editNonANStr(x, 12, 14), collapse = ""))

dimnames(h1Tbl)[[2]][5:ncol(h1Tbl)] <- altCytVec

# check stims

invisible(h1Tbl %>%
  group_by(ptid, timePoint) %>%
  summarise(totStim = str_c(stim, collapse = "")) %>%
  ungroup() %>%
  group_by(totStim) %>%
  summarise(count = n())) # ag85besat6phauns   281. Everyone had all.

# sum over stims, and make long
h1Tbl %<>%
  gather(key = cytCombo, value = freq, CD4Gp2pTp17p:CD8Gn2nTn17n) %>%
  arrange(infxn, ptid, cytCombo, timePoint, stim) %>%
  group_by(infxn, ptid, timePoint, cytCombo) %>%
  summarise(ag85b = freq[1] - freq[4], esat6 = freq[2] - freq[4], freq = freq[1] + freq[2] - 2 * freq[4]) %>%
  ungroup() %>%
  arrange(infxn, ptid, cytCombo, timePoint)

h1ITbl <- h1Tbl %>%
  gather(key = stim, value = resp, ag85b:freq)

# remove all cytoCombo negatives
h1ITbl %<>% filter(cytCombo != "CD4Gn2nTn17n" & cytCombo != "CD8Gn2nTn17n" & stim != "freq")

h1Tbl %<>% filter(cytCombo != "CD4Gn2nTn17n" & cytCombo != "CD8Gn2nTn17n")

# Testing -----------------------------------------------------------------

# h1ITbl
testVal1 <- h1ITbl %>%
  filter(ptid == "h1_2" & timePoint == 0 & stim == "esat6" & cytCombo == "CD4Gp2nTn17n") %>%
  extract2("resp")
if (testVal1 != (0.001113883 - 0.0005162169)) stop("h1ITbl error")
testVal2 <- h1ITbl %>%
  filter(ptid == "h1_221" & timePoint == 224 & stim == "ag85b" & cytCombo == "CD8Gp2nTp17n") %>%
  extract2("resp")
if (testVal2 != (0.001595456 - 0.003927473)) stop("h1ITbl error")

# h1ITbl
testVal1 <- h1Tbl %>%
  filter(ptid == "h1_81" & timePoint == 14 & cytCombo == "CD4Gn2nTp17n") %>%
  extract2("freq")
if (testVal1 != (0.00165674 + 0.002524785 - 2 * 0.001658691)) stop("h1Tbl error")

testVal2 <- h1Tbl %>%
  filter(ptid == "h1_81" & timePoint == 14 & cytCombo == "CD4Gp2pTn17n") %>%
  extract2("freq")
if (testVal2 != (0.00414185 + 0.002524785 - 2 * 0)) stop("h1Tbl error")

# Clear workspace ---------------------------------------------------------

# remove all objects but initial objects (currObjVec) and processed data tibble (l4Tbl)
rm(list = setdiff(ls(), c(currObjVec, "h1Tbl", "h1ITbl")))
