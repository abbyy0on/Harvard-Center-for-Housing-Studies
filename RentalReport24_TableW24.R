#Table W24: Number of HOusing Cost-Burdened Renters, 2012-2022

library(data.table)
library(tidyverse)
library(openxlsx)

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"
workbook_path <- "C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\RentalReport24_AppendixTables_WAO working copy.xlsx"

us_2012 <- fread(file.path(path, "ACS_multiyear_hhplus_RentersPlusVacant.csv")) %>% 
  filter(year==2012)
met_2012 <- fread(file.path(path, "ACS_2012hhplus_metro.csv"))

us_2019 <- fread(file.path(path, "ACS_2019_hhplus.csv"))
met_2019 <- fread(file.path(path, "ACS_2019_hhplus_metro.csv"))

us_2022 <- fread(file.path(path, "ACS_2022_hhplus.csv"))
met_2022 <- fread(file.path(path, "ACS_2022hhplus_metro.csv"))


#table components--------
#national tabs
burdens_us12 <- us_2012 %>% 
  filter(tensimp==2 & year==2012) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% 
  bind_rows(mutate(., cost_burden = "all_renters")) %>%
  group_by(cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  pivot_wider(names_from = cost_burden, values_from=tot) %>% 
  select(-`1`)   %>% 
  rename_all(function(x) paste0(x, "_2012"))
  
burdens_us19 <- us_2019 %>% 
  filter(tensimp==2) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% 
  bind_rows(mutate(., cost_burden = "all_renters")) %>%
  group_by(cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  pivot_wider(names_from = cost_burden, values_from=tot) %>% 
  select(-`1`)   %>% 
  rename_all(function(x) paste0(x, "_2019"))

burdens_us22 <- us_2022 %>% 
  filter(tensimp==2) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% 
  bind_rows(mutate(., cost_burden = "all_renters")) %>%
  group_by(cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  pivot_wider(names_from = cost_burden, values_from=tot) %>% 
  select(-`1`)   %>% 
  rename_all(function(x) paste0(x, "_2022"))

burdens_us_table <- cbind(burdens_us12, burdens_us19, burdens_us22)


#metro tabs
burdens_met12 <- met_2012 %>% 
  mutate(tensimp = case_when(ten==1|ten==2 ~ 1,
                             ten==3|ten==4 ~ 2),
         costincr = (grntp*1200)/hincp,
         cost_burden = case_when(costincr <= 30 | ten==4 ~ 1,
                                 costincr >30 & costincr <=50 ~ 2,
                                 costincr >50 | (hincp<= 1 & ten!=4) ~ 3)) %>% 
  filter(poprank<=100 & tensimp==2) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% 
  bind_rows(mutate(., cost_burden = "all_renters")) %>%
  group_by(cbsaname20, cost_burden) %>% 
  summarise(tot = sum(hhwtcbsa)/1000) %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  select(-`1`)   %>% 
  rename_at(vars(-cbsaname20),function(x) paste0(x, "_2012"))

burdens_met19 <- met_2019 %>% 
  mutate(top100_2022 = case_when(CBSAName20 == "Scranton--Wilkes-Barre, PA" ~ 0,
                                 CBSAName20 == "Fayetteville-Springdale-Rogers, AR" ~ 1,
                                 CBSAName20 != "Scranton--Wilkes-Barre, PA" & CBSAName20 != "Fayetteville-Springdale-Rogers, AR" ~ top100)) %>% 
  filter(top100_2022==1 & tensimp==2) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% 
  bind_rows(mutate(., cost_burden = "all_renters")) %>%
  group_by(CBSAName20, cost_burden) %>% 
  summarise(tot = sum(hhwtcbsa)/1000) %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename(cbsaname20 = CBSAName20) %>% 
  select(-`1`)   %>% 
  rename_at(vars(-cbsaname20),function(x) paste0(x, "_2019"))

burdens_met22 <- met_2022 %>% 
  filter(poprank<=100 & tensimp==2) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% 
  bind_rows(mutate(., cost_burden = "all_renters")) %>%
  group_by(cbsaname20, cost_burden) %>% 
  summarise(tot = sum(hhwtcbsa)/1000) %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  select(-`1`)   %>% 
  rename_at(vars(-cbsaname20),function(x) paste0(x, "_2022"))
  
burdens_met_table <- reduce(list(burdens_met12, burdens_met19, burdens_met22), left_join, by = "cbsaname20")  


#export to excel-------
title <- "Table W-24. US Metro Areas: Number of Housing Cost-Burdened Renters: 2012-2022" #update the year
sources <- "Source: JCHS tabulations of US Census Bureau, American Community Survey 1-Year Estimates and Missouri Census Data Center data."

#these commands load in the target workbook, tell the program where to paste each component, then writes the tables into the workbook
wb <- loadWorkbook(workbook_path) #loads in an existing workbook with template pasted in to a sheet called "StateCostBurdenShares"
writeData(wb, sheet = "W-24", x = title, #pastes the title in to cell A1
          startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, sheet = "W-24", x = burdens_us_table,
          startCol = 2, startRow = 7, colNames = FALSE) #colnames=FALSE suppresses the variable names since these are already in the template, the US line in the data doesn't have a label field for the row so paste into column 2 instead of 1
writeData(wb, sheet = "W-24", x = burdens_met_table, 
          startCol = 1, startRow = 8, colNames = FALSE)
writeData(wb, sheet = "W-24", x = sources, 
          startCol = 1, startRow = 111, colNames = FALSE)
saveWorkbook(wb, workbook_path, overwrite=TRUE)