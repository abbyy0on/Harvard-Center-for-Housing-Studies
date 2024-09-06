#Table W13: Number of Housing Cost-Burdened Renters: 2001, 2019, 2022

# Load packages 
library(tidyverse)
library(data.table)
library(openxlsx)

setwd("C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\")

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"

multi_years <-c(2001) #years to select from multi file (2001:2014) if keeping all
yearly_years <- c(2019, 2022) #used for recent years not in multi file

states_fips_codes <- fread(file.path(path, "State FIPS codes.csv"))

multiyear <- fread(file.path(path, "ACS_multiyear_hhplus_RentersPlusVacant.csv")) %>% 
  left_join(states_fips_codes, by=c("st" = "FIPS Code")) %>% 
  filter(year==multi_years)

# yearly files
for(x in yearly_years) {
  inputfile <- paste0("ACS_", x, "_hhplus.csv")
  df <- fread(file.path(path, inputfile)) %>% 
    left_join(states_fips_codes, by = c("st" = "FIPS Code"))
  dfyr <- paste0("df_", x)
  assign(dfyr, df)
  rm(df)
}


# tabs: 2001 data - code for states ------------------------
multtab_states <- multiyear %>% 
  filter(sporder==1 & tensimp==2) %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  group_by(year, cost_burden, `State Name`) %>% 
  summarise(tot = sum(wgtp)/1000) %>%
  ungroup %>% 
  group_by(year, `State Name`) %>% 
  mutate(share = tot/sum(tot)*100) %>% 
  pivot_wider(names_from=cost_burden, values_from=c(tot, share)) %>% 
  mutate(all_renters = tot_1 + tot_2 + tot_3) %>%
  rename("ModBurden" = tot_2,
         "SevBurden" = tot_3) %>% 
  select(year, "ModBurden", "SevBurden", all_renters) %>%
  pivot_wider(names_from=year, values_from=c("ModBurden", "SevBurden", all_renters))  


# tabs: annual file years ------------------------
yearly_tabs_states_all <- function(x) {
  df <- paste0("df_", x)
  df <- get(df)
  tab <- df %>% 
    filter(sporder==1 & tensimp==2) %>% 
    bind_rows(mutate(., `State Name` = "United States")) %>%
    group_by(year, cost_burden, `State Name`) %>% 
    summarise(tot = sum(wgtp)/1000) %>%
    ungroup %>% 
    group_by(year, `State Name`) %>% 
    mutate(share = tot/sum(tot)*100) %>% 
    pivot_wider(names_from=cost_burden, values_from=c(tot, share)) %>% 
    mutate(all_renters = tot_1 + tot_2 + tot_3) %>%
    rename("ModBurden" = tot_2,
           "SevBurden" = tot_3) %>% 
    select(year, "ModBurden", "SevBurden", all_renters) %>%
    pivot_wider(names_from=year, values_from=c("ModBurden", "SevBurden", all_renters)) %>% 
  ungroup
}

tabslist_states <- map(yearly_years, yearly_tabs_states_all) 
tabs_states <- reduce(tabslist_states, left_join, by = "State Name")
  

# combine data across years ------------------------
full_table <- reduce(list(multtab_states, tabs_states), left_join, by = "State Name") %>% 
  arrange(`State Name` != 'United States') #move US total to top row 


# excel export
title <- "Table W-13. US States: Number of Housing Cost-Burdened Renters: 2001, 2019, and 2022"  
sources <- "Source: JCHS tabulation of US Census Bureau, American Community Survey 1-Year Estimates."

#these commands load in the target workbook, tell the program where to paste each component, then writes the tables into the workbook
wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-13", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE) 
writeData(wb, sheet = "W-13", x = full_table, 
          startCol = 1, startRow = 7, colNames = FALSE)  
writeData(wb, sheet = "W-13", x = sources, 
          startCol = 1, startRow = 63, colNames = FALSE)

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy.xlsx", overwrite=TRUE)
