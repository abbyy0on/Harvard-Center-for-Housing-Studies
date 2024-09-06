#Table W22: Number and Share of Rental Units by Structure Type, 2022

#Load packages 
library(tidyverse)
library(data.table)
library(openxlsx)
library(reldist)

setwd("C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\")

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"

df_2022 <- fread(file.path(path, "ACS_2022_hhplus.csv"))
states_fips_codes <- fread(file.path(path, "State FIPS codes.csv"))
# merge the datasets 
df_2022 <- left_join(df_2022, states_fips_codes, by = c("st" = "FIPS Code")) 


# rental unit - structure type (number in thousands)    ---------------------------
rental_unit_bldcat_number <- df_2022 %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf")) %>%
  bind_rows(mutate(., bldcat = "6 Total")) %>%
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  group_by(`State Name`, bldcat) %>% 
  summarise(tot = sum(wgtp)/1000) %>% 
  pivot_wider(names_from = bldcat, values_from = tot) %>%
  mutate(total_mult = sum(c_across("2 2-4":"4 20pl"))) %>%
  select(`State Name`, "1 sf", "2 2-4", "3 5-19", "4 20pl", total_mult, "5 manuf", "6 Total") 


# rental unit - structure type (share)   ---------------------------
rental_unit_bldcat <- df_2022 %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf")) %>%
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  group_by(`State Name`, bldcat) %>% 
  summarise(tot = sum(wgtp)) %>% 
  group_by(`State Name`) %>%
  mutate(sh = tot / sum(tot) * 100) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = bldcat, values_from = sh) %>%
  mutate(total_mult = sum(c_across("2 2-4":"4 20pl")),
         total = `1 sf` + total_mult + `5 manuf`, #this is redundant (all are 100) but using as a check
         total = if_else(is.na(total), 100, total)) %>%
  select(`State Name`, "1 sf", "2 2-4", "3 5-19", "4 20pl", total_mult, "5 manuf", total) 


# combine! ---------------------------
full_table <- reduce(list(rental_unit_bldcat_number, rental_unit_bldcat), left_join, by = "State Name") %>% 
  arrange(`State Name` != "United States")


## import to excel ---------------------------

title <- "Table W-20. US States: Number and Share of Rental Units by Structure Type: 2022"  
sources <- "Source: JCHS tabulation of US Census Bureau, 2022 American Community Survey 1-Year Estimates."

wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-20", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, sheet = "W-20", x = full_table, 
          startCol = 1, startRow = 8, colNames = FALSE)  
writeData(wb, sheet = "W-20", x = sources, 
          startCol = 1, startRow = 62, colNames = FALSE)

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy.xlsx", overwrite=TRUE)


