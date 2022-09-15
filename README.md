# Bighorn Sheep Carrying Capacity

Meghan Beale; 15 Sept 2022

This script contains the code used for the analyses in Beale et al. 2022. Carrying capacity and cumulative effects management: A case study using bighorn sheep. Conservation Science and Practice.


###### Equation for bighorn sheep carrying capacity ######

BHS CC = (sum of (F*U*A))*SUF / FR<br/>

F = forage quality; measured in Mcal<br/>
U = relative winter selection by BHS; proportion between 0-1<br/>
A = area; based on winter range; proportion between 0-1<br/>
SUF = safe use factor; proportion between 0-1<br/>
FR = forage requirements for average BHS over entire winter; measured in Mcal<br/>


###### Read raw data for F, U, and A ######

Note that we cannot provide raw data for BHS winter ranges, as this species is protected in the province of British Columbia (BC) and large-scale (i.e., detailed) range maps are not publicly available. For more information on the distribution of BHS in BC, please visit: https://a100.gov.bc.ca/pub/eswp/esr.do?id=16396.<br/>
To assist readers with understanding our CC model, we have opted to include the raw data that fed into the CC model without spatial references for winter BHS ranges.<br/>
```
# load raw data
FUA <- read.csv("BHS_AvailableEnergy_WinterRange.csv") 

# clean df
fua <- FUA %>%
  dplyr::select(-c(OID)) %>%
  mutate(SubPop = as.factor(SubPop)) %>%
  mutate(SubPop = fct_recode(SubPop,
                             "NA" = ""))
```
###### Calculate carrying capacity per bighorn sheep subpopulation ######

```
cc <- fua %>%
  filter(!is.na(PropWinter)) %>% 
  mutate(FUA = ForageQual*PropWinter*Rel_BHSUse) %>%
  group_by(SubPop) %>%
  summarise(TotalMcal = sum(FUA)) %>% 
  ungroup() %>%
  pivot_wider(names_from = "SubPop", values_from = "TotalMcal") %>%
  dplyr::mutate(AllWinterRange = `NA` + `Crossing Ck` + `Crowsnest North` + `Erickson Sheep M` +
                  `EV West Hornaday` + `Ewin Ck` + `Fording` + `Upper Elk East` + `Upper Elk West`) %>% 
  pivot_longer(names_to = "SubPop", values_to = "TotalMcal", `NA`:AllWinterRange) %>%
  mutate(bm.kg = 72.1) %>% 
  mutate(daily.kcal.rate = ((460*(bm.kg^0.75))*(1/4.184))) %>% # 1 kcal = 4.184 kJ; convert to calories
  mutate(days.winter = 141) %>%
  dplyr::rename(middle = daily.kcal.rate) %>% # apply three performance classes
  mutate(good = (0.10*middle)+middle) %>%
  mutate(maint = (middle/(1+0.10))) %>%
  dplyr::select(-middle,middle) %>%
  pivot_longer(!(SubPop:days.winter), names_to = "performance", values_to = "daily.kcal") %>%
  mutate(FR.Mcal = daily.kcal*days.winter*(1/1000)) %>%
  mutate(intense = 0.75) %>% # apply three SUF
  mutate(moderate = 0.50) %>%
  mutate(conservative = 0.25) %>%
  pivot_longer(!(SubPop:FR.Mcal), names_to = "scenario", values_to = "SUF") %>%
  mutate(CC = (TotalMcal*SUF)/FR.Mcal) # calculate CC for each subpop x perf class x SUF
```
###### Calculate area of winter range per subpopulation ######
```
areas <- fua %>%
  dplyr::filter(!is.na(PropWinter)) %>%
  dplyr::mutate(area = 625) %>% # area of each 25 x 25-m raster cell (i.e., 625 m sq)
  dplyr::group_by(SubPop) %>%
  dplyr::summarise(total.area.m2 = sum(area*PropWinter)) %>%
  dplyr::mutate(total.area.km2 = total.area.m2*(1/1000000)) # 1 km sq = 10^6 m sq
```
