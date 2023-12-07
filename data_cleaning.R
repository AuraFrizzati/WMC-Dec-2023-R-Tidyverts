## AF: 04 Dec 2023
## Data cleaning for WMC tutorial tidyverts


# [0] Load relevant libraries ---------------------------------------------

## from tidyverse
library(dplyr)    ## data wrangling
#library(stringr)  ## handling of strings
#library(purrr)    ## functional programming
#library(readr)    ## r objects' import/export
#library(ggplot2)  ## r plotting

## from tidyverts
library(tsibble)
#library(fable)
#library(fabletools)
#library(feasts)


# [1] Re-import the data saved locally ------------------------------------

HLTH0037_data_raw <- read.csv("data/HLTH0037_data_raw.csv")



# [2] Extract only relevant columns and rename them ------------------------

HLTH0037_data <-
  HLTH0037_data_raw |>
  dplyr::select(
    Date_ItemName_ENG,       ## Time as Month&Year
    Data,                    ## Number of ED Attendances
    Hospital_Hierarchy      ## Local Health Board code
  ) |>
  ## rename the columns
  dplyr::rename(
    MonthYear = Date_ItemName_ENG,
    Attendances = Data,
    LHB_code = Hospital_Hierarchy
  ) 


#  [3] create & link the lookup for LHB names/codes --------
## LHBs retrieved from 
## https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fwww.gov.wales%2Fsites%2Fdefault%2Ffiles%2Fstatistics-and-research%2F2021-09%2Fsensory-health-eye-care-and-hearing-statistics-april-2019-march-2021-tables-221.ods&wdOrigin=BROWSELINK

NHSWalesOrg_lookup <-
  data.frame(
    "Organisation" = c("Betsi Cadwaladr", 
                       "Powys Teaching", 
                       "Hywel Dda", 
                       "Swansea Bay",
                       "Abertawe Bro Morgannwg", 
                       "Cwm Taf Morgannwg",
                       "Cwm Taf", 
                       "Aneurin Bevan",
                       "Cardiff & Vale", 
                       "Wales"),
    
    "Geographical_code" = c("W11000023", 
                            "W11000024",
                            "W11000025", 
                            "W11000031",
                            "W11000026", 
                            "W11000030",
                            "W11000027", 
                            "W11000028",
                            "W11000029", 
                            "W92000004"))

HLTH0037_data_v2 <-
  HLTH0037_data |>
  dplyr::left_join(
    NHSWalesOrg_lookup,
    by = c("LHB_code" = "Geographical_code"))


# [4] Aggregate age groups, shorten LHB names -----------------------------
## group Attendances counts by all variables' combinations 
## (MonthYear x Hospital X Age group X Sex) 

HLTH0037_data_cln <-
  HLTH0037_data_v2 |>
  dplyr::mutate(
    
    ## Convert LHB full names into shorter ones
    LHB = dplyr::case_when(
      Organisation == "Aneurin Bevan" ~ "AB UHB",
      Organisation == "Cardiff & Vale" ~ "CAV UHB",
      Organisation == "Abertawe Bro Morgannwg" ~ "ABM",
      Organisation == "Powys Teaching" ~ "PT HB",
      Organisation == "Betsi Cadwaladr" ~ "BC UHB",
      Organisation == "Cwm Taf" ~ "Cwm Taf",
      Organisation == "Hywel Dda" ~ "HD UHB",
      Organisation == "Cwm Taf Morgannwg" ~ "CTM UHB",
      Organisation == "Swansea Bay" ~ "SB UHB"
      
    )) |>  
  select(-c(Organisation,LHB_code)) |>
  dplyr::group_by_at(dplyr::vars(-Attendances)) |>
  dplyr::summarise(Attendances = sum(Attendances)) |>
  dplyr::ungroup() |>
  dplyr::mutate(MonthYear = tsibble::yearmonth(MonthYear))

readr::write_rds(HLTH0037_data_cln, "data/HLTH0037_data_cln.rds")

