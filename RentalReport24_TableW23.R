#Table W23: Rentership Rates by Age, Race, and Household Income, 2022

library(data.table)
library(tidyverse)

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"
workbook_path <- "C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\RentalReport24_AppendixTables_WAO working copy.xlsx"

us_2022 <- fread(file.path(path, "ACS_2022_hhplus.csv"))
met_2022 <- fread(file.path(path, "ACS_2022hhplus_metro.csv"))


#table components-------
#national tabs
us_age <- us_2022 %>% 
  filter(sporder==1 & !is.na(tensimp)) %>% 
  mutate(agecat = case_when(agep<35 ~ "1 under35",
                            agep>=35 & agep<45 ~ "2 35-44",
                            agep>=45 & agep<55 ~ "3 45-54",
                            agep>=55 & agep<65 ~ "4 55-64",
                            agep>=65 ~ "5 65pl")) %>% 
  group_by(agecat, tensimp) %>% 
  summarise(tot = sum(wgtp)) %>%
  group_by(agecat) %>% 
  mutate(sh = tot/sum(tot) * 100) %>% 
  filter(tensimp==2) %>% 
  select(-tot, -tensimp) %>% 
  pivot_wider(names_from=agecat, values_from=sh)

us_race <- us_2022 %>% 
  mutate(race5cat = case_when(race5cat==1 ~ "1 white",
                              race5cat==2 ~ "2 black",
                              race5cat==3 ~ "3 hispanic",
                              race5cat==4 ~ "4 asian",
                              race5cat==5 ~ "5 another")) %>% 
  group_by(race5cat, tensimp) %>% 
  summarise(tot = sum(wgtp)) %>%
  group_by(race5cat) %>% 
  mutate(sh = tot/sum(tot) * 100) %>% 
  filter(tensimp==2) %>% 
  select(-tot, -tensimp) %>% 
  pivot_wider(names_from=race5cat, values_from=sh)

us_inc <- us_2022 %>% 
  mutate(hhincome = case_when(hincp < 15000 ~ "1 Less than $15000",
                              hincp >= 15000 & hincp <=29999 ~ "2 $15,000-29,999",
                              hincp >= 30000 & hincp <=44999 ~ "3 $30,000-44,999",
                              hincp >= 45000 & hincp <=74999 ~ "4 $45,000-74,999",
                              hincp >= 75000 ~ "5 $75,000 or More")) %>% 
  group_by(hhincome, tensimp) %>% 
  summarise(tot = sum(wgtp)) %>%
  group_by(hhincome) %>% 
  mutate(sh = tot/sum(tot) * 100) %>% 
  filter(tensimp==2) %>% 
  select(-tot, -tensimp) %>% 
  pivot_wider(names_from=hhincome, values_from=sh)

us_tot <- us_2022 %>% 
  filter(!is.na(tensimp)) %>% 
  group_by(tensimp) %>% 
  summarise(tot = sum(wgtp)) %>%
  ungroup() %>% 
  mutate(sh = tot/sum(tot) * 100) %>% 
  filter(tensimp==2) %>% 
  select(-tot, -tensimp)

us_table <- cbind(us_age, us_race, us_inc, us_tot)

#metro tabs
met_age <- met_2022 %>% 
  filter(poprank<=100 & sporder==1 & !is.na(tensimp)) %>% 
  mutate(agecat = case_when(agep<35 ~ "1 under35",
                            agep>=35 & agep<45 ~ "2 35-44",
                            agep>=45 & agep<55 ~ "3 45-54",
                            agep>=55 & agep<65 ~ "4 55-64",
                            agep>=65 ~ "5 65pl")) %>% 
  group_by(cbsaname20, agecat, tensimp) %>% 
  summarise(tot = sum(hhwtcbsa)) %>% 
  group_by(cbsaname20, agecat) %>% 
  mutate(sh=tot/sum(tot)* 100) %>% 
  filter(tensimp==2) %>% 
  select(-tot, -tensimp) %>% 
  pivot_wider(names_from=agecat, values_from=sh)

met_race <- met_2022 %>% 
  filter(poprank<=100 & sporder==1 & !is.na(tensimp)) %>% 
  mutate(race5cat = case_when(race5cat==1 ~ "1 white",
                              race5cat==2 ~ "2 black",
                              race5cat==3 ~ "3 hispanic",
                              race5cat==4 ~ "4 asian",
                              race5cat==5 ~ "5 another")) %>% 
  group_by(cbsaname20, race5cat, tensimp) %>% 
  summarise(tot = sum(hhwtcbsa)) %>% 
  group_by(cbsaname20, race5cat) %>% 
  mutate(sh=tot/sum(tot)* 100) %>% 
  filter(tensimp==2) %>% 
  select(-tot, -tensimp) %>% 
  pivot_wider(names_from=race5cat, values_from=sh)

met_inc <- met_2022 %>% 
  filter(poprank<=100 & sporder==1 & !is.na(tensimp)) %>% 
  mutate(hhincome = case_when(hincp < 15000 ~ "1 Less than $15000",
                              hincp >= 15000 & hincp <=29999 ~ "2 $15,000-29,999",
                              hincp >= 30000 & hincp <=44999 ~ "3 $30,000-44,999",
                              hincp >= 45000 & hincp <=74999 ~ "4 $45,000-74,999",
                              hincp >= 75000 ~ "5 $75,000 or More")) %>% 
  group_by(cbsaname20, hhincome, tensimp) %>% 
  summarise(tot = sum(hhwtcbsa)) %>% 
  group_by(cbsaname20, hhincome) %>% 
  mutate(sh=tot/sum(tot)* 100) %>% 
  filter(tensimp==2) %>% 
  select(-tot, -tensimp) %>% 
  pivot_wider(names_from=hhincome, values_from=sh)

met_tot <- met_2022 %>% 
  filter(poprank<=100 & sporder==1 & !is.na(tensimp)) %>% 
  group_by(cbsaname20, tensimp) %>% 
  summarise(tot = sum(hhwtcbsa)) %>% 
  group_by(cbsaname20) %>% 
  mutate(sh=tot/sum(tot)* 100) %>% 
  filter(tensimp==2) %>% 
  select(-tot, -tensimp)
  
met_table <- reduce(list(met_age, met_race, met_inc, met_tot), left_join, by = "cbsaname20")  

#export to excel------
title <- "Table W-23. US Metro Areas: Rentership Rates by Age, Race, and Household Income, 2022" #update the year
sources <- "Source: JCHS tabulations of US Census Bureau, American Community Survey 2022 1-Year Estimates and Missouri Census Data Center data."

#these commands load in the target workbook, tell the program where to paste each component, then writes the tables into the workbook
wb <- loadWorkbook(workbook_path) #loads in an existing workbook with template pasted in to a sheet called "StateCostBurdenShares"
writeData(wb, sheet = "W-23", x = title, #pastes the title in to cell A1
          startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, sheet = "W-23", x = us_table,
          startCol = 2, startRow = 7, colNames = FALSE) #colnames=FALSE suppresses the variable names since these are already in the template, the US line in the data doesn't have a label field for the row so paste into column 2 instead of 1
writeData(wb, sheet = "W-23", x = met_table, 
          startCol = 1, startRow = 8, colNames = FALSE)
writeData(wb, sheet = "W-23", x = sources, 
          startCol = 1, startRow = 110, colNames = FALSE)
saveWorkbook(wb, workbook_path, overwrite=TRUE)