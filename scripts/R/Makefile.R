# Initial set-up ----------------------------------------------------------

# saving results? clear wd first
currObjVec <- ls()

# find library containing raw data
dataprepPath <- system.file("dataprep", package = "VaccCompData")
dirFunc <- function(x) paste0(dataprepPath, "/", x)

# Individual datasets -----------------------------------------------------

# h564Tbl
source(file = dirFunc("H56-CD4.R"))

# h568Tbl
source(file = dirFunc("H56-CD8.R"))

# l4Tbl
source(file = dirFunc("BCG-CD4.R"))

# l8Tbl
source(file = dirFunc("BCG-CD8.R"))

# lOrigMainTbl
source(file = dirFunc("Main.R"))

# idri
source(file = dirFunc("ID93.R"))

# h1
source(file = dirFunc("H1.R"))

# megapool
source(file = dirFunc("Megapool.R"))

# aeras402
source(file = dirFunc("AERAS402.R"))

# Joining and calculating -------------------------------------------------

# join
source(file = dirFunc("Data_Join.R"))

# il17
source(file = dirFunc("IL_17-Calc_And_Removal.R"))

# sub time zero
source(file = dirFunc("Sub_Baseline.R"))

# summary statistics
source(file = dirFunc("Summ_Stat_Calc.R"))

# Saving ------------------------------------------------------------------

#' making availabe in package
if (1 == 0) { # run manually if neeeded
  devtools::use_data(bl17ExcTbl, overwrite = TRUE)
  devtools::use_data(bl17ExcITbl, overwrite = TRUE)
  devtools::use_data(join17ExcTbl, overwrite = TRUE)
  devtools::use_data(join17ExcITbl, overwrite = TRUE)
  devtools::use_data(join17IncITbl, overwrite = TRUE)
  devtools::use_data(megITbl, overwrite = TRUE)
  devtools::use_data(sub17ExcTbl, overwrite = TRUE)
  devtools::use_data(sub17ExcITbl, overwrite = TRUE)
  devtools::use_data(tmax17ExcTbl, overwrite = TRUE)
  devtools::use_data(tmax17ExcITbl, overwrite = TRUE)
  devtools::use_data(tmaxSub17ExcTbl, overwrite = TRUE)
  devtools::use_data(tmaxSub17ExcITbl, overwrite = TRUE)
  devtools::use_data(blSingle17Tbl, overwrite = TRUE)
  devtools::use_data(single17Tbl, overwrite = TRUE)
  devtools::use_data(single17ITbl, overwrite = TRUE)
  devtools::use_data(tmaxSingle17Tbl, overwrite = TRUE)
  devtools::use_data(tmaxSingle17ITbl, overwrite = TRUE)
  devtools::use_data(subSingle17Tbl, overwrite = TRUE)
  devtools::use_data(subSingle17ITbl, overwrite = TRUE)
  devtools::use_data(tmaxSubSingle17Tbl, overwrite = TRUE)
  devtools::use_data(tmaxSubSingle17ITbl, overwrite = TRUE)
}

# write to spreadsheet
if (1 == 0) {
  openxlsx::write.xlsx(
    join17IncITbl %>%
      spread(key = cytCombo, value = resp) %>%
      select(-prid),
    file = "C:/Users/migue/Dropbox/Rodo et al Vaccine Immune Response paper/Data/Publicly available/Vaccines.xlsx",
    overwrite = TRUE
  )
  openxlsx::write.xlsx(
    joinMegITbl1 %>%
      spread(key = cytCombo, value = resp) %>%
      select(-c(prid, timePoint, vaccine, stim, infxn)),
    file = "C:/Users/migue/Dropbox/Rodo et al Vaccine Immune Response paper/Data/Publicly available/Megapool.xlsx",
    overwrite = TRUE
  )
}
