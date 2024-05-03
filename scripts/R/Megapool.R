# Loading -----------------------------------------------------------------

# names of current objects
currObjVec <- ls()

# read in data
fileLoc <- projr::projr_path_get(
  "data-raw-public", "megapool.csv"
)
megTbl <- as_tibble(read.csv(fileLoc))

# Data Prep ---------------------------------------------------------------

# edit columns
testTbl <- megTbl %>% select(Donor.ID, Stimulation, contains("Freq"))
edTbl <- megTbl %>% select(Donor.ID, Stimulation, contains("Freq"))
nCol <- ncol(edTbl)
names(edTbl)[3:nCol] <- tbl_vars(edTbl)[3:nCol] %>% str_sub(end = -19)

edTbl %<>%
  gather(key = cytCombo, value = resp, CD4.Gneg2negTpos:CD8.Gpos2posTneg) %>%
  separate(cytCombo, c("cd", "cytCombo")) %>%
  mutate(
    cytCombo = str_to_lower(cytCombo),
    cytCombo = str_c(
      "G", str_sub(cytCombo, 2, 2), "2", str_sub(cytCombo, 6, 6),
      "T", str_sub(cytCombo, 10, 10)
    ),
    cd = str_sub(cd, start = 3)
  ) %>%
  rename(ptid = Donor.ID, stim = Stimulation) %>%
  mutate(
    ptid = as.character(ptid), stim = as.character(stim),
    stim = Vectorize(ifelse)(stim == "Unstim+Costim", "uns", stim),
    stim = Vectorize(ifelse)(stim == "MegaPool + Costim", "mega", stim)
  ) %>%
  filter(cytCombo != "Gn2nTn")

# check which have background and which don't
idVec <- edTbl %>%
  group_by(ptid, cd) %>%
  summarise(lu = calcLU(stim)) %>%
  ungroup() %>%
  filter(lu == "1") %>%
  `[[`("ptid") %>%
  unique()

#
invisible(edTbl %>%
  group_by(ptid, stim, cd) %>%
  slice(1) %>%
  ungroup() %>%
  filter(ptid %in% idVec)) # only have unstim readings

# remove these ptids
edTbl %<>% filter(!(ptid %in% idVec))

# subtract background
edTbl %<>%
  arrange(ptid, cd, cytCombo) %>%
  group_by(ptid, cd, cytCombo) %>%
  summarise(resp = resp[which(stim == "mega")] - resp[which(stim == "uns")]) %>%
  ungroup()

# add infxn status, prid
megTbl <- edTbl %>%
  mutate(cytCombo = str_c("CD", cd, cytCombo)) %>%
  select(-cd) %>%
  mutate(resp = Vectorize(ifelse)(resp < 0, 0, resp)) %>%
  spread(key = cytCombo, value = resp) %>%
  mutate(infxn = "1", timePoint = "-1")

megITbl <- megTbl %>% mutate(stim = "mega")

# Testing -----------------------------------------------------------------

calcVal <- megTbl$CD4Gp2nTn[which(megTbl$ptid == "megapool_1")]
testVal <- testTbl %>%
  filter(Donor.ID == "megapool_1") %>%
  summarise(out = CD4.Gpos2negTneg...Freq..of.Parent[2] - CD4.Gpos2negTneg...Freq..of.Parent[1])
if (calcVal != testVal) stop("megTbl error")
calcVal <- megTbl$CD8Gn2pTp[which(megTbl$ptid == "megapool_21")]
testVal <- testTbl %>%
  filter(Donor.ID == "megapool_21") %>%
  summarise(out = CD8.Gneg2posTpos...Freq..of.Parent[2] - CD8.Gneg2posTpos...Freq..of.Parent[1])
if (calcVal != testVal) stop("megTbl error")

# Clear workspace ---------------------------------------------------------

# remove all objects but initial objects (currObjVec) and processed data tibble (l8Tbl)
rm(list = setdiff(ls(), c(currObjVec, "megTbl", "megITbl")))
