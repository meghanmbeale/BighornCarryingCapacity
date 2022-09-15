# Bighorn Sheep Carrying Capacity

Meghan Beale 13/09/2022

This script contains the code used for the analyses in Beale et al. 2022. Carrying capacity and cumulative effects management: A case study using bighorn sheep. Conservation Science and Practice.



## Code associated with Beale et al. (2022) Carrying capacity and cumulative effects management: A case study using bighorn sheep.
## Conservation Science and Practice

## Meghan Beale, 15 September 2022

#################################################################################################################################

# load libraries
library(tidyverse)

###### Equation for bighorn sheep (BHS) carrying capacity (CC) ######

# BHS CC = (sum of (F*U*A))*SUF / FR

# F = forage quality; measured in Mcal
# U = relative winter selection by BHS; proportion between 0-1
# A = area; based on winter range; proportion between 0-1
# SUF = safe use factor; proportion between 0-1
# FR = forage requirements for average BHS over entire winter; measured in Mcal

###### Read raw data for F, U, and A ######

## Note that we cannot provide raw data for BHS winter ranges, as this species is protected
## in the province of British Columbia (BC) and large-scale (i.e., detailed) range maps are 
## not publicly available. For more information on the distribution of BHS in BC, 
## please visit: https://a100.gov.bc.ca/pub/eswp/esr.do?id=16396.
## To assist readers with understanding our CC model, we have opted to include the raw data
## that fed into the CC model (see below csv) without spatial references for winter BHS ranges.

# load raw data
FUA <- read.csv("BHS_AvailableEnergy_WinterRange_RelativeSelection.csv") 

# clean up df
fua <- FUA %>%
  dplyr::select(-c(OID)) %>%
  mutate(SubPop = as.factor(SubPop)) %>%
  mutate(SubPop = fct_recode(SubPop,
                             "NA" = "")) # there are some areas that do not fall within a specific sub-population but are within study area

###### Calculate CC per Sub-Population ######

cc <- fua %>%
  filter(!is.na(PropWinter)) %>% # removes forage outside the winter range
  mutate(FUA = ForageQual*PropWinter*Rel_BHSUse) %>% # multiplies forage quality by proportion of pixel in winter range by the relative selection of that pixel by BHS
  group_by(SubPop) %>%
  summarise(TotalMcal = sum(FUA)) %>% # calculates total Mcal of forage avail per subpop of sheep
  ungroup() %>%
  pivot_wider(names_from = "SubPop", values_from = "TotalMcal") %>%
  dplyr::mutate(AllWinterRange = `NA` + `Crossing Ck` + `Crowsnest North` + `Erickson Sheep M` +
                  `EV West Hornaday` + `Ewin Ck` + `Fording` + `Upper Elk East` + `Upper Elk West`) %>% # add column for Mcal across all winter ranges
  pivot_longer(names_to = "SubPop", values_to = "TotalMcal", `NA`:AllWinterRange) %>%
  mutate(bm.kg = 72.1) %>% # average body weight in kg based on herd composition from Poole
  mutate(daily.kcal.rate = ((460*(bm.kg^0.75))*(1/4.184))) %>% # 1 kcal = 4.184 kJ; convert to calories
  mutate(days.winter = 141) %>% # days per winter season
  dplyr::rename(middle = daily.kcal.rate) %>% # apply three performance classes
  mutate(good = (0.10*middle)+middle) %>%
  mutate(maint = (middle/(1+0.10))) %>%
  dplyr::select(-middle,middle) %>% # reorganize column order
  pivot_longer(!(SubPop:days.winter), names_to = "performance", values_to = "daily.kcal") %>%
  mutate(FR.Mcal = daily.kcal*days.winter*(1/1000)) %>% # overall forage requirement is caloric requirement of average BHS per day, multiplied by number of days in average winter season, then converted to Mcal
  mutate(intense = 0.75) %>% # apply three SUF
  mutate(moderate = 0.50) %>%
  mutate(conservative = 0.25) %>%
  pivot_longer(!(SubPop:FR.Mcal), names_to = "scenario", values_to = "SUF") %>%
  mutate(CC = (TotalMcal*SUF)/FR.Mcal) # calculate CC for each subpop x perf class x SUF

# export results
setwd("I:/CLIENTS/TECK_COAL/Teck_Data_Master/Data/Discipline/RSA_Modelling/DevelopedModels/Carrying_Capacity/Final_CC_Estimates")
write.csv(cc,"CC_Results.csv")

###### Calculate area of winter range within each subpop ######

areas <- fua %>%
  dplyr::filter(!is.na(PropWinter)) %>%
  dplyr::mutate(area = 625) %>% # add area of each 25 x 25-m raster cell (i.e., 625 m sq) %>%
  dplyr::group_by(SubPop) %>%
  dplyr::summarise(total.area.m2 = sum(area*PropWinter)) %>%
  dplyr::mutate(total.area.km2 = total.area.m2*(1/1000000)) # 1 km sq = 10^6 m sq

# export results
setwd("I:/CLIENTS/TECK_COAL/Teck_Data_Master/Data/Discipline/RSA_Modelling/DevelopedModels/Carrying_Capacity/Final_CC_Estimates")
write.csv(areas,"CC_Winter_Range_Areas.csv")

## END OF CODE
