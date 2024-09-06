#Table W22: Renter Households by Age, Race, and Household Income, 2022

library(data.table)
library(tidyverse)

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"
workbook_path <- "C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\RentalReport24_AppendixTables_WAO working copy.xlsx"

us_2022 <- fread(file.path(path, "ACS_2022_hhplus.csv"))
met_2022 <- fread(file.path(path, "ACS_2022hhplus_metro.csv"))


#table components-------
#national tabs
us_age <- us_2022 %>% 
  filter(tensimp==2) %>% 
  mutate(agecat = case_when(agep<35 ~ "1 under35",
                            agep>=35 & agep<45 ~ "2 35-44",
                            agep>=45 & agep<55 ~ "3 45-54",
                            agep>=55 & agep<65 ~ "4 55-64",
                            agep>=65 ~ "5 65pl")) %>% 
  group_by(agecat) %>% 
  summarise(tot = sum(wgtp)/1000) %>% 
  pivot_wider(names_from=agecat, values_from=tot)

us_race <- us_2022 %>% 
  filter(tensimp==2) %>% 
  mutate(race5cat = case_when(race5cat==1 ~ "1 white",
                              race5cat==2 ~ "2 black",
                              race5cat==3 ~ "3 hispanic",
                              race5cat==4 ~ "4 asian",
                              race5cat==5 ~ "5 another")) %>% 
  group_by(race5cat) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  pivot_wider(names_from = race5cat, values_from = tot)

us_inc <- us_2022 %>% 
  filter(tensimp==2) %>% 
  mutate(hhincome = case_when(hincp < 15000 ~ "1 Less than $15000",
                              hincp >= 15000 & hincp <=29999 ~ "2 $15,000-29,999",
                              hincp >= 30000 & hincp <=44999 ~ "3 $30,000-44,999",
                              hincp >= 45000 & hincp <=74999 ~ "4 $45,000-74,999",
                              hincp >= 75000 ~ "5 $75,000 or More")) %>% 
  group_by(hhincome) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  pivot_wider(names_from = hhincome, values_from = tot)

us_tot <- us_2022 %>% 
  filter(tensimp==2) %>% 
  summarise(tot=sum(wgtp)/1000)

us_table <- cbind(us_age, us_race, us_inc, us_tot)

#metro tabs
met_age <- met_2022 %>% 
  filter(poprank<=100 & tensimp==2) %>% 
  mutate(agecat = case_when(agep<35 ~ "1 under35",
                            agep>=35 & agep<45 ~ "2 35-44",
                            agep>=45 & agep<55 ~ "3 45-54",
                            agep>=55 & agep<65 ~ "4 55-64",
                            agep>=65 ~ "5 65pl")) %>% 
  group_by(cbsaname20, agecat) %>% 
  summarise(tot = sum(hhwtcbsa)/1000) %>% 
  pivot_wider(names_from=agecat, values_from=tot)

met_race <- met_2022 %>% 
  filter(poprank<=100 & tensimp==2) %>% 
  mutate(race5cat = case_when(race5cat==1 ~ "1 white",
                              race5cat==2 ~ "2 black",
                              race5cat==3 ~ "3 hispanic",
                              race5cat==4 ~ "4 asian",
                              race5cat==5 ~ "5 another")) %>% 
  group_by(cbsaname20, race5cat) %>% 
  summarise(tot = sum(hhwtcbsa)/1000) %>% 
  pivot_wider(names_from=race5cat, values_from=tot)

met_inc <- met_2022 %>% 
  filter(poprank<=100 & tensimp==2) %>% 
  mutate(hhincome = case_when(hincp < 15000 ~ "1 Less than $15000",
                              hincp >= 15000 & hincp <=29999 ~ "2 $15,000-29,999",
                              hincp >= 30000 & hincp <=44999 ~ "3 $30,000-44,999",
                              hincp >= 45000 & hincp <=74999 ~ "4 $45,000-74,999",
                              hincp >= 75000 ~ "5 $75,000 or More")) %>% 
  group_by(cbsaname20, hhincome) %>% 
  summarise(tot = sum(hhwtcbsa)/1000) %>% 
  pivot_wider(names_from=hhincome, values_from=tot)

met_tot <- met_2022 %>% 
  filter(poprank<=100 & tensimp==2) %>% 
  group_by(cbsaname20) %>% 
  summarise(tot=sum(hhwtcbsa)/1000)

met_table <- reduce(list(met_age, met_race, met_inc, met_tot), left_join, by = "cbsaname20")  

#export to excel------
title <- "Table W-22. US Metro Areas: Renter Households by Age, Race, and Household Income, 2022" #update the year
sources <- "Source: JCHS tabulations of US Census Bureau, American Community Survey 2022 1-Year Estimates and Missouri Census Data Center data."

#these commands load in the target workbook, tell the program where to paste each component, then writes the tables into the workbook
wb <- loadWorkbook(workbook_path) #loads in an existing workbook with template pasted in to a sheet called "StateCostBurdenShares"
writeData(wb, sheet = "W-22", x = title, #pastes the title in to cell A1
          startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, sheet = "W-22", x = us_table,
          startCol = 2, startRow = 7, colNames = FALSE) #colnames=FALSE suppresses the variable names since these are already in the template, the US line in the data doesn't have a label field for the row so paste into column 2 instead of 1
writeData(wb, sheet = "W-22", x = met_table, 
          startCol = 1, startRow = 8, colNames = FALSE)
writeData(wb, sheet = "W-22", x = sources, 
          startCol = 1, startRow = 110, colNames = FALSE)
saveWorkbook(wb, workbook_path, overwrite=TRUE)