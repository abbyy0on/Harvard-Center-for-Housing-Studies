#Table W17: Basic Rental Housing Facts from the American Community Survey, 2022

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


## renter household table components ---------------------------
# renter household - total & share
renter_hh_total <- df_2022 %>%
  filter(sporder==1) %>%  # tensimp = 1 is homeowner, tensimp = 2 is renter
  bind_rows(mutate(., `State Name` = "United States")) %>%
  group_by(`State Name`, tensimp) %>% 
  summarise(tot = sum(wgtp)) %>%
  group_by(`State Name`) %>%
  mutate(share = tot/sum(tot)*100)  %>%
  filter(tensimp ==2) %>%
  select(-tensimp)

# renter household - cost burden
renter_hh_burden <- df_2022 %>%
  filter(sporder==1 & tensimp==2) %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  group_by(cost_burden, `State Name`) %>% 
  summarise(tot = sum(wgtp)/1000) %>%
  ungroup %>%
  group_by(`State Name`) %>%
  mutate(share = tot/sum(tot)*100) %>% 
  filter(cost_burden==2|cost_burden==3) %>% 
  pivot_wider(names_from=cost_burden, values_from=c(tot, share)) %>% 
  mutate(share = share_2 + share_3,
         sharemod = share_2,
         sharesev = share_3) %>% 
  rename("ModBurdenSh"= sharemod,
         "SevBurdenSh" = sharesev,
         "Any" = share) %>% 
  select(`State Name`, "ModBurdenSh", "SevBurdenSh", "Any") 

# renter household - median household income  
renter_hh_med <- df_2022 %>%
  bind_rows(mutate(., `State Name` = "United States")) %>%
  filter(sporder==1 & tensimp ==2) %>% 
  group_by(`State Name`) %>% 
  summarise(medinc = wtd.quantile(hincp, q=0.5, na.rm=TRUE, weight=wgtp))

# renter household - household income (share)
renter_hh_income <- df_2022 %>%
  filter(sporder==1 & tensimp==2) %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  mutate(hhincome = case_when(hincp < 15000 ~ "1 Less than $15000",
                              hincp >= 15000 & hincp <=29999 ~ "2 $15,000-29,999",
                              hincp >= 30000 & hincp <=44999 ~ "3 $30,000-44,999",
                              hincp >= 45000 & hincp <=74999 ~ "4 $45,000-74,999",
                              hincp >= 75000 ~ "5 $75,000 or More")) %>% 
  group_by(`State Name`, hhincome) %>% 
  summarise(tot = sum(wgtp)) %>%
  mutate(percentage = tot/sum(tot)*100) %>%   
  select(-tot) %>%
  pivot_wider(names_from = "hhincome", values_from = "percentage") 

# merge data for renter household characteristics
rental_household_table <- reduce(list(renter_hh_total, renter_hh_burden, renter_hh_med, renter_hh_income), left_join, by = "State Name")  


## rental stock table components  ---------------------------

# rental unit - totals
rental_unit_total <- df_2022 %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  group_by(`State Name`) %>% 
  filter(tensimp==2 | vacs==1 |vacs==2) %>% 
  summarise(tot=sum(wgtp))

# vacancy rate
rental_unit_vacrate <- df_2022 %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  filter(tensimp==2 | vacs==1 | vacs==2) %>% 
  mutate(vacant = case_when(vacs==1|vacs==2 ~ 1,
                            tensimp==2 ~ 0)) %>% 
  group_by(`State Name`, vacant) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(`State Name`) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(vacant==1) %>% 
  select(sh)

# rental unit - gross rent 
rental_unit_grntp <- df_2022 %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  filter(tensimp==2) %>% 
  group_by(`State Name`) %>% 
  summarise(medrnt = wtd.quantile(grntp, q=0.5, na.rm=TRUE, weight=wgtp))


# rental unit - rent distribution (share by contract rent)
rental_unit_rntlvl <- df_2022 %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  mutate(rntp = case_when(rntp<600 ~ 1,
                          rntp>=600 & rntp<800 ~ 2,
                          rntp>=800 & rntp<1000 ~ 3,
                          rntp>=1000 & rntp<1400 ~ 4,
                          rntp>=1400 & rntp<2000 ~ 5,
                          rntp>=2000 ~ 6)) %>% 
  filter(!is.na(rntp)) %>%
  group_by(`State Name`, rntp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(`State Name`) %>%
  mutate(sh=tot/sum(tot)*100) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = rntp, values_from = sh)

# rental unit - structure type (share)
rental_unit_bldcat <- df_2022 %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==1|bld==10 ~ "2 manuf",
                            bld==4|bld==5 ~ "3 2-4",
                            bld==6|bld==7 ~"4 5-19",
                            bld==8|bld==9 ~ "5 20pl")) %>% 
  group_by(`State Name`, bldcat) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(`State Name`) %>%
  mutate(sh=tot/sum(tot)*100) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = bldcat, values_from = sh)
 
# merge data for rental stock characteristics
rental_unit_table <- reduce(list(rental_unit_total, rental_unit_vacrate, rental_unit_grntp, rental_unit_rntlvl, rental_unit_bldcat), left_join, by = "State Name") 


  ## combine data ---------------------------
full_table <- reduce(list(rental_household_table, rental_unit_table), left_join, by = "State Name") %>% 
  arrange(`State Name`!="United States")

  
  ## import to excel ---------------------------
  
title <- "Table W-17. US States: Basic Rental Housing Facts from the American Community Survey, 2022"  
sources <- "Source: JCHS tabulation of US Census Bureau, 2022 American Community Survey 1-Year Estimates."

wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy2.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-17", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE) 
writeData(wb, sheet = "W-17", x = full_table, 
          startCol = 1, startRow = 8, colNames = FALSE)  
writeData(wb, sheet = "W-17", x = sources, 
          startCol = 1, startRow = 63, colNames = FALSE)

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy2.xlsx", overwrite=TRUE)
