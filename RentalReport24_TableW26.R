#Table W26: Share of Renters with Cost Burdens by Race/Ethnicity: 2022

library(data.table)
library(tidyverse)

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"
workbook_path <- "C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\RentalReport24_AppendixTables_WAO working copy2.xlsx"

us_2022 <- fread(file.path(path, "ACS_2022_hhplus.csv"))
met_2022 <- fread(file.path(path, "ACS_2022hhplus_metro.csv"))

w26_us <- us_2022 %>% 
  filter(tensimp==2) %>% 
  mutate(race5cat = case_when(race5cat==1 ~ "1 white",
                              race5cat==2 ~ "2 black",
                              race5cat==3 ~ "3 hispanic",
                              race5cat==4 ~ "4 asian",
                              race5cat==5 ~ "5 another")) %>% 
  group_by(race5cat, cost_burden) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(race5cat) %>% 
  mutate(sh= tot/sum(tot) * 100) %>% 
  ungroup() %>% 
  filter(cost_burden>1) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = c("race5cat", "cost_burden"), values_from = sh)
  

w26_met_tots <- met_2022 %>% 
  filter(poprank<=100 & tensimp==2) %>% 
  mutate(race5cat = case_when(race5cat==1 ~ "1 white",
                              race5cat==2 ~ "2 black",
                              race5cat==3 ~ "3 hispanic",
                              race5cat==4 ~ "4 asian",
                              race5cat==5 ~ "5 another")) %>% 
  group_by(cbsaname20, race5cat, cost_burden) %>% 
  summarise(tot = sum(hhwtcbsa)) %>% 
  group_by(cbsaname20, race5cat) %>% 
  mutate(sh = tot/sum(tot)*100) %>% 
  ungroup() %>% 
  filter(cost_burden>1) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = c(race5cat, cost_burden), values_from = sh)


#sample code for writing table to excel workbook
title <- "Table W-26. US Metro Areas: Share of Renters with Cost Burdens by Race/Ethnicity, 2022" #update the year
sources <- "Source: JCHS tabulations of US Census Bureau, American Community Survey 2022 1-Year Estimates and Missouri Census Data Center data."

#these commands load in the target workbook, tell the program where to paste each component, then writes the tables into the workbook
wb <- loadWorkbook(workbook_path) #loads in an existing workbook with template pasted in to a sheet called "StateCostBurdenShares"
writeData(wb, sheet = "W-26", x = title, #pastes the title in to cell A1
          startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, sheet = "W-26", x = w26_us,
          startCol = 2, startRow = 7, colNames = FALSE) #colnames=FALSE suppresses the variable names since these are already in the template, the US line in the data doesn't have a label field for the row so paste into column 2 instead of 1
writeData(wb, sheet = "W-26", x = w26_met_tots, 
          startCol = 1, startRow = 8, colNames = FALSE)
writeData(wb, sheet = "W-26", x = sources, 
          startCol = 1, startRow = 111, colNames = FALSE)
saveWorkbook(wb, workbook_path, overwrite=TRUE)
