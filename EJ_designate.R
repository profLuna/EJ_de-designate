# This is an analysis of Census Block Groups potentially eligible for de-designation under the state's MA EJ policy

## EJ policy
# (A) a neighborhood that meets 1 or more of the following criteria: (i) the annual median household income is not more than 65 per cent of the statewide annual median household income; (ii) minorities comprise 40 per cent or more of the population; (iii) 25 per cent or more of  households  lack  English  language  proficiency; or (iv)  minorities  comprise  25 per cent  or more  of the population  and  the  annual  median  household  income  of  the  municipality  in  which  the  neighborhood  is  located does not exceed 150 per cent of the statewide annual median household income; or (B) a geographic portion of a neighborhood designated by the Secretary as an environmental justice population in accordance with law. 

## EJ De-designation policy
# “the secretary may determine that a neighborhood, including any geographic portion thereof, shall not be designated an environmental justice population upon finding that: (A) the annual median household income of that neighborhood is greater than 125 per cent of the statewide median household income; (B) a majority of persons age 25 and older in that neighborhood have a college education; (C) the neighborhood does not bear an unfair burden of environmental pollution; and (D) the neighborhood has more than limited access to natural resources, including open spaces and water resources, playgrounds and other constructed outdoor recreational facilities and venues.”

# Download ACS data at Block Group and county subdivision level to acquire necessary demographic layers
library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(tmap)




v19 <- load_variables(year = 2019, dataset = "acs5")

# download variables at block group level for Massachusetts
maACS19_blkgrp <- get_acs(geography = "block group", 
                          variables = c(totalpop = "B03002_001", 
                                        whitepop = "B03002_003", 
                                        medhhinc = "B19013_001",
                                        pop25 = "B15003_001",
                                        bachelors25 = "B15003_022",
                                        masters25 = "B15003_023",
                                        professional25 = "B15003_024",
                                        doctorate25 = "B15003_025",
                                        households = "C16002_001",
                                        limitSpan = "C16002_004",
                                        limitIE = "C16002_007",
                                        limitAPI = "C16002_010",
                                        limitO = "C16002_013"),
                          state = "MA", output = "wide")

# create variables for EJ criteria
maACS19_blkgrp <- maACS19_blkgrp %>% 
  drop_na(totalpopE) %>% 
  filter(totalpopE > 0) %>% 
  mutate(minority = totalpopE - whitepopE, 
         pct_minority = minority/totalpopE*100,
         limitedEngHH = limitSpanE+limitIEE+limitAPIE+limitOE,
         pct_limitedEngHH = limitedEngHH/householdsE*100,
         college = bachelors25E+masters25E+professional25E+doctorate25E,
         pct_college = college/pop25E*100)

# download census block group polygons alone
ma_blkgrps <- block_groups(state = "MA", cb = TRUE)

# get county subdivisions with median HH income
maACS19_cosub <- get_acs(geography = "county subdivision", 
                         variables = c(MHHI = "B19013_001"), state = "MA",
                         geometry = TRUE)

# get statewide median household income
maACS19_stateMHI <- get_acs(geography = "state", variables = "B19013_001",
                            state = "MA") %>% 
  select(estimate) %>% 
  pull()

# assign town name and MHHI to each block group
ma_blkgrps <- ma_blkgrps %>% 
  select(-NAME) %>% 
  st_join(., maACS19_cosub, largest = TRUE, left = FALSE) %>% 
  rename(COSUB_MHHI = estimate) %>% 
  mutate(TOWN = str_extract(str_remove_all(NAME, " town| city| Town"), 
                            "[^,]+"))

# join block group demographics to polygons
maACS19_blkgrp <- ma_blkgrps %>% 
  left_join(., maACS19_blkgrp, by = c("GEOID.x" = "GEOID"))

# allocate EJ identifiers
maACS19_blkgrp <- maACS19_blkgrp %>% 
  mutate(MA_MHHI = maACS19_stateMHI,
         BG_PCTMAHHI = medhhincE/MA_MHHI*100,
         MUNIMHHI_PCTMAHHI = COSUB_MHHI/MA_MHHI*100,
         MEDIANHHI = if_else(BG_PCTMAHHI <= 65, "I",""),
         LIMITEDENGLISH = if_else(pct_limitedEngHH >= 25, "E",""),
         MINORITY = if_else(pct_minority >= 40 | 
                              (pct_minority >= 25 & 
                                 MUNIMHHI_PCTMAHHI <= 150), "M", ""),
         EJ = str_trim(gsub("NA","",if_else(MEDIANHHI == "I" | 
                        LIMITEDENGLISH == "E" | 
                        MINORITY == "M", "Yes", "No"))),
         EJCRITERIA = str_squish(
           str_trim(gsub
                    ("NA","",
             paste(MEDIANHHI, LIMITEDENGLISH, MINORITY, sep = " ")))),
         EJCRITERIA_CNT = nchar(EJCRITERIA) - str_count(EJCRITERIA, " "),
         EJ_ELIMINATE = if_else(EJ == "Yes" & BG_PCTMAHHI > 125 & pct_college > 50, "Eliminate?", " "))

# map it out
tmap_mode("view")
maACS19_blkgrp %>% 
  filter(EJ_ELIMINATE == "Eliminate?") %>% 
  tm_shape(.) + tm_fill(col = "red", alpha = 0.5)


# Bring in EJSCREEN data to identify BGs that "not bear an unfair burden of environmental pollution"
download.file(url = "https://gaftp.epa.gov/EJSCREEN/2020/EJSCREEN_2020_StatePctile.csv.zip", 
              destfile = "EJSCREEN_2020_StatePctile.csv.zip")

unzip("EJSCREEN_2020_StatePctile.csv.zip")

EJSCREEN_2020_StatePctile <- read_csv("EJSCREEN_2020_StatePctile.csv") %>% 
  filter(STATE_NAME == "Massachusetts") %>% 
  select(ID, P_PM25, P_OZONE, P_DSLPM, P_CANCR, P_RESP, P_PTRAF, P_LDPNT,
         P_PNPL, P_PRMP, P_PTSDF, P_PWDIS)

# join to maACS block groups
maACS19_blkgrp <- maACS19_blkgrp %>% 
  left_join(., EJSCREEN_2020_StatePctile, by = c("GEOID.x" = "ID"))

# identify areas that meet EJ de-designation criteria, assuming 'unfair burden of environmental pollution' to mean greater than 50th percentile for any given type of pollution
maACS19_blkgrp <- maACS19_blkgrp %>% 
  mutate(EJ_ELIMINATE = if_else(EJ == "Yes" & 
                                  BG_PCTMAHHI > 125 & 
                                  pct_college > 50 & 
                                  (P_PM25 < 90 & P_OZONE < 90 & 
                                     P_DSLPM < 90 & P_CANCR < 90 & 
                                     P_RESP < 90 & 
                                     P_LDPNT < 90 & P_PWDIS < 90), 
                                "Eliminate?", " "))
# save work
save(ma_blkgrps, maACS19_blkgrp, maACS19_cosub, EJSCREEN_2020_StatePctile, 
     file = "maACSblkgrps.Rds")
# reload work
load("maACSblkgrps.Rds")


# How many potentially eliminated?
maACS19_blkgrp %>% filter(EJ_ELIMINATE == "Eliminate?") %>% nrow()

# How many de-designated by town?
maACS19_blkgrp %>% 
  as.data.frame() %>% 
  filter(EJ_ELIMINATE == "Eliminate?" & EJ == "Yes") %>% 
  group_by(TOWN) %>% 
  summarize(COUNT = n()) %>% 
  arrange(-COUNT)

# Look more closely at certain municipalities
# How many EJ BGs in Arlington?
maACS19_blkgrp %>% 
  as.data.frame() %>% 
  filter(TOWN == "Arlington" & EJ == "Yes") %>%
  nrow()

# How many Arlington EJ BGs would be eliminated?
maACS19_blkgrp %>% 
  as.data.frame() %>% 
  filter(TOWN == "Arlington" & EJ_ELIMINATE == "Eliminate?") %>%
  nrow()

# peruse relevant variables to see spread of values in each
maACS19_blkgrp %>% 
  as.data.frame() %>% 
  filter(TOWN == "Arlington") %>% 
  select(P_PM25, P_OZONE, P_DSLPM, P_CANCR, P_RESP, P_PTRAF, P_LDPNT, P_PNPL,
         P_PRMP, P_PTSDF, P_PWDIS) %>%
  head(50)

maACS19_blkgrp %>% 
  as.data.frame() %>% 
  filter(TOWN == "Arlington") %>% 
  select(TOWN, P_PM25, P_OZONE, P_DSLPM, P_CANCR, P_RESP, P_PTRAF, P_LDPNT, P_PNPL,
         P_PRMP, P_PTSDF, P_PWDIS) %>%
  group_by(TOWN) %>% 
  summarize(across(P_PM25:P_PWDIS, ~ max(.x, na.rm = T)))

# map it out
library(tmap)
tmap_mode("view")
maACS19_blkgrp %>% 
  filter(EJ_ELIMINATE == "Eliminate?") %>% 
  tm_shape(.) + tm_fill(col = "red", alpha = 0.6)

