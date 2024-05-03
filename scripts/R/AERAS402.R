# Loading -----------------------------------------------------------------

# names of current objects
currObjVec <- ls()

# read in data
fileLoc <- projr::projr_path_get(
  "data-raw-public", "aeras402.xlsx"
)
aerasTbl <- as_tibble(read.xlsx(fileLoc))

# Data Prep ---------------------------------------------------------------

# UNIQUE VALUES

# unique values in id info
unique(aerasTbl$PatientID) # "051-25" "139-23" "226-21" "234-28" "250-24" "265-27" "292-30" "355-40" "360-36"
unique(aerasTbl$TubeName) # "Ag85"   "BCG"    "TB10.4"
unique(aerasTbl$TimePoint) #  0   7  28  84 182

# edit columns
aerasTbl %<>%
  rename(
    ptid = PatientID,
    stim = TubeName,
    timePoint = TimePoint
  ) %>%
  mutate_at(vars(`CD4+2+17+G+T+`:`CD8+2-17-G-T+`), as.numeric) %>%
  mutate(
    stim = str_to_lower(stim),
    infxn = "0",
    vaccine = "1"
  ) %>%
  select(vaccine, ptid, infxn, stim, timePoint, everything()) %>%
  arrange(vaccine, ptid, infxn, stim, timePoint) %>%
  mutate(timePoint = as.character(timePoint))

aerasTbl %<>%
  filter(stim != "bcg") %>%
  mutate(stim = Vectorize(ifelse)(stim == "tb10.4", "tb104", stim)) %>%
  rename_if(is.numeric, function(x) str_rep_pos_neg(x)) %>%
  rename_if(is.numeric, function(x) str_to_upper_sel(x, c("p", "n"))) %>%
  gather(key = cytCombo, value = resp, CD4p2p17pGpTp:CD8p2n17nGnTp) %>%
  mutate(
    cd = str_sub(cytCombo, 3, 3),
    cytCombo = str_sub(cytCombo, 5)
  ) %>%
  spread(key = cytCombo, value = resp) %>%
  rename_if(is.numeric, function(x) str_chunk_reorder(x, c("p", "n"), c(2, 4, 1, 3))) %>%
  gather(key = cytCombo, value = resp, Gn2nTp17n:Gp2pTp17p) %>%
  mutate(timePoint = as.numeric(timePoint)) %>%
  arrange(vaccine, ptid, infxn, cd, timePoint, cytCombo, stim) %>%
  # mutate( ptid = str_c( "xyz", ptid ) ) %>%
  spread(key = cytCombo, value = resp) %>%
  arrange(vaccine, ptid, infxn, cd, timePoint, stim) %>%
  mutate(timePoint = as.character(timePoint))

# stim

# number of stims
aerasTbl %>%
  group_by(infxn, ptid, timePoint, cd) %>%
  summarise(stimCount = n()) %>%
  filter(stimCount != 3) # all have three stims per time point and cytCombo

# number of unique stims
aerasTbl %>%
  group_by(infxn, ptid, timePoint, cd) %>%
  summarise(uniqueStimCount = n_distinct(stim)) %>%
  filter(uniqueStimCount != 3) # all have five unique stims.

# all in same order
aerasTbl %>%
  group_by(vaccine, infxn, ptid, timePoint, cd) %>%
  summarise(stimStr = str_c(stim, collapse = "")) %>%
  extract2("stimStr") %>%
  calcLU()
# Subtraction -------------------------------------------------------------

aerasITbl <- aerasTbl

# summation

aerasTbl %<>%
  group_by(vaccine, ptid, infxn, timePoint, cd) %>%
  summarise_if(is.numeric, function(x) sum(x)) %>%
  ungroup()

# Tests -------------------------------------------------------------------

# aerasITbl
testRow1 <- aerasITbl %>% filter(ptid == "aeras_1" & timePoint == 0 & stim == "ag85" & cd == "8")
if (testRow1$Gn2pTn17n != 0.024) stop("aerasITbl error")
testRow2 <- aerasITbl %>% filter(ptid == "aeras_9" & timePoint == 182 & stim == "tb104" & cd == "4")
if (testRow2$Gn2pTp17n != 0.00141) stop("aerasITbl error")
testRow3 <- aerasITbl %>% filter(ptid == "aeras_9" & timePoint == 7 & stim == "ag85" & cd == "4")
if (testRow3$Gp2nTp17p != 0.00185) stop("aerasITbl error")
testRow4 <- aerasITbl %>% filter(ptid == "aeras_8" & timePoint == 0 & stim == "tb104" & cd == "4")
if (testRow4$Gn2pTp17n != 0.00404) stop("aerasITbl error")

# aerasTbl
testRow1 <- aerasTbl %>% filter(ptid == "aeras_7" & timePoint == 0 & cd == "8")
if (testRow1$Gn2pTn17n != (0.0085 + 0.014)) stop("aerasTbl error")


# Clear Workspace ---------------------------------------------------------

# remove all objects but initial objects (currObjVec) and processed data tibble (h564Tbl)
rm(list = setdiff(ls(), c(currObjVec, "aerasTbl", "aerasITbl")))
