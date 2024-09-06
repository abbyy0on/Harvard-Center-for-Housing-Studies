#Table W27: Share of Renters with Cost Burdens by Income: 2022

library(data.table)
library(tidyverse)
library(openxlsx)

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"
workbook_path <- "C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\RentalReport24_AppendixTables_WAO working copy.xlsx"

us_2022 <- fread(file.path(path, "ACS_2022_hhplus.csv"))
met_2022 <- fread(file.path(path, "ACS_2022hhplus_metro.csv"))

w27_us <- us_2022 %>% 
  filter(tensimp==2) %>% 
  mutate(hh_inccat = case_when(hh_inccat==1 ~ "1 Less than $15k",
                            hh_inccat==2 ~ "2 $15k-29,999",
                            hh_inccat==3 ~ "3 $30k-44,999",
                            hh_inccat==4 ~ "4 $45k-74,999",
                            hh_inccat==5 ~ "5 $75kpl")) %>% 
  mutate_at(vars(c(cost_burden, hh_inccat)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., hh_inccat = "6 all incomes")) %>%
  group_by(hh_inccat, cost_burden) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(hh_inccat) %>% 
  mutate(sh= tot/sum(tot) * 100) %>% 
  ungroup() %>% 
  filter(cost_burden>1) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = c("hh_inccat", "cost_burden"), values_from = sh)
  

w27_met_tots <- met_2022 %>% 
  filter(poprank<=100 & tensimp==2) %>% 
  mutate(hh_inccat = case_when(hh_inccat==1 ~ "1 Less than $15k",
                               hh_inccat==2 ~ "2 $15k-29,999",
                               hh_inccat==3 ~ "3 $30k-44,999",
                               hh_inccat==4 ~ "4 $45k-74,999",
                               hh_inccat==5 ~ "5 $75kpl")) %>% 
  mutate_at(vars(c(cost_burden, hh_inccat)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., hh_inccat = "6 all incomes")) %>%
  group_by(cbsaname20, hh_inccat, cost_burden) %>% 
  summarise(tot = sum(hhwtcbsa)) %>% 
  ungroup()

w27_met_samplecheck <- met_2022 %>% 
  filter(poprank<=100 & tensimp==2) %>% 
  mutate(hh_inccat = case_when(hh_inccat==1 ~ "1 Less than $15k",
                               hh_inccat==2 ~ "2 $15k-29,999",
                               hh_inccat==3 ~ "3 $30k-44,999",
                               hh_inccat==4 ~ "4 $45k-74,999",
                               hh_inccat==5 ~ "5 $75kpl"),
         hh=1) %>% 
  mutate_at(vars(c(cost_burden, hh_inccat)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., hh_inccat = "6 all incomes")) %>%
  group_by(cbsaname20, hh_inccat) %>% 
  summarise(tot_check = sum(hh)) %>% 
  ungroup() %>% 
  mutate(tot_check = if_else(tot_check<=30, NA_integer_, tot_check))
##sample sizes are fine, no need to suppress

w27_met_final <- w27_met_tots %>% 
  group_by(cbsaname20, hh_inccat) %>% 
  mutate(sh = tot/sum(tot)*100) %>% 
  ungroup() %>% 
  filter(cost_burden>1) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = c(hh_inccat, cost_burden), values_from = sh) %>% 
  replace(is.na(.), 0)


#sample code for writing table to excel workbook
title <- "Table W-27. US Metro Areas: Share of Renters with Cost Burdens by Income: 2022" #update the year
sources <- "Source: JCHS tabulations of US Census Bureau, 2022 American Community Survey 1-Year Estimates and Missouri Census Data Center data."

#these commands load in the target workbook, tell the program where to paste each component, then writes the tables into the workbook
wb <- loadWorkbook(workbook_path) #loads in an existing workbook with template pasted in to a sheet called "StateCostBurdenShares"
writeData(wb, sheet = "W-27", x = title, #pastes the title in to cell A1
          startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, sheet = "W-27", x = w27_us,
          startCol = 2, startRow = 7, colNames = FALSE) #colnames=FALSE suppresses the variable names since these are already in the template, the US line in the data doesn't have a label field for the row so paste into column 2 instead of 1
writeData(wb, sheet = "W-27", x = w27_met_final, 
          startCol = 1, startRow = 8, colNames = FALSE)
writeData(wb, sheet = "W-27", x = sources, 
          startCol = 1, startRow = 111, colNames = FALSE)
saveWorkbook(wb, workbook_path, overwrite=TRUE)
