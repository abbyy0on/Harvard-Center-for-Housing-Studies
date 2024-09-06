#Table W12: Rentership Rates by Age, Race, and Household Income, 2022

# Load packages 
library(tidyverse)
library(data.table)
library(openxlsx)

setwd("C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\")

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"

df_2022 <- fread(file.path(path, "ACS_2022_hhplus.csv"))
states_fips_codes <- fread(file.path(path, "State FIPS codes.csv"))
# merge the datasets 
df_2022 <- left_join(df_2022, states_fips_codes, by = c("st" = "FIPS Code")) 


# add important variables  ----------------------------------------
df_2022 <- df_2022 %>%
  filter(sporder == 1) %>%  #  i deleted tensimp == 2 
  mutate(hhincome = case_when(hincp < 15000 ~ "1 Less than $15,000",
                              hincp >= 15000 & hincp <= 29999 ~ "2 $15,000-29,999",
                              hincp >= 30000 & hincp <= 44999 ~ "3 $30,000-44,999",
                              hincp >= 45000 & hincp <= 74999 ~ "4 $45,000-74,999",
                              hincp >= 75000 ~ "5 $75,000 or More"),
         age = case_when(agep < 25 ~ "1 Under 25",
                         agep >= 25 & agep <=34 ~ "2 25-34",
                         agep >= 35 & agep <=44 ~ "3 35-44",
                         agep >= 45 & agep <=54 ~ "4 45-54",
                         agep >= 55 & agep <=64 ~ "5 55-64",
                         agep >= 65 & agep <=74 ~ "6 65-74",
                         agep >= 75 ~ "7 75+"),
         race6cat = case_when(rac1p == 1 & hisp == 1 ~ 1,
                              rac1p == 2 & hisp == 1 ~ 2,
                              hisp > 1 ~ 3,
                              rac1p == 6 & hisp == 1 ~ 4,
                              (rac1p == 3 | rac1p == 4 | rac1p == 5) ~ 5,
                              (rac1p == 7 | rac1p == 8 | rac1p == 9) & hisp == 1 ~ 6),
         race6cat = case_when(race6cat == 1 ~ "1 White",
                              race6cat == 2 ~ "2 Black",
                              race6cat == 3 ~ "3 Hispanic",
                              race6cat == 4 ~ "4 Asian",
                              race6cat == 5 ~ "5 American Indian/Alaskan Native",
                              race6cat == 6 ~ "6 Multiracial or Another Race"))
        

# rentership rates by age ----------------------------------------
rent_rate_age  <- df_2022 %>%
  filter(sporder==1) %>%  # tensimp = 1 is homeowner, tensimp = 2 is renter
  bind_rows(mutate(., `State Name` = "United States")) %>%
  group_by(`State Name`, age, tensimp) %>% 
  summarise(tot = sum(wgtp)) %>%
  group_by(`State Name`, age) %>%
  mutate(share = tot/sum(tot)*100)  %>%
  filter(tensimp ==2) %>%
  select(-tensimp, -tot) %>%
  pivot_wider(names_from = "age", values_from = "share") 


# rentership rates by race ----------------------------------------
rent_rate_race <- df_2022 %>%
  filter(sporder==1) %>%  # tensimp = 1 is homeowner, tensimp = 2 is renter
  bind_rows(mutate(., `State Name` = "United States")) %>%
  group_by(`State Name`, race6cat, tensimp) %>% 
  summarise(tot = sum(wgtp)) %>%
  group_by(`State Name`, race6cat) %>%
  mutate(share = tot/sum(tot)*100)  %>%
  filter(tensimp ==2) %>%
  select(-tensimp, -tot) %>%
  pivot_wider(names_from = "race6cat", values_from = "share") 


# renter household - household income (share)   ----------------------------------------
rent_rate_income <- df_2022 %>%
  filter(sporder==1) %>%  # tensimp = 1 is homeowner, tensimp = 2 is renter
  bind_rows(mutate(., `State Name` = "United States")) %>%
  bind_rows(mutate(., hhincome = "All Incomes")) %>% # creates the  "Tot" summary column at the end of the table
  group_by(`State Name`, hhincome, tensimp) %>% 
  summarise(tot = sum(wgtp)) %>%
  group_by(`State Name`, hhincome) %>%
  mutate(share = tot/sum(tot)*100)  %>%
  filter(tensimp ==2) %>%
  select(-tensimp, -tot) %>%
  pivot_wider(names_from = "hhincome", values_from = "share") 


# combine tables   ----------------------------------------
full_table = reduce(list(rent_rate_age, rent_rate_race, rent_rate_income), left_join, by = "State Name")  %>% 
  arrange(`State Name` != 'United States') #move US total to top row 



# export to excel ----------------------------------------
title <- "Table W-12. US States: Rentership Rates by Age, Race, and Household Income, 2022"  
sources <- "Source: JCHS tabulation of US Census Bureau, 2022 American Community Survey 1-Year Estimates."

wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-12", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE) 
writeData(wb, sheet = "W-12", x = full_table, 
          startCol = 1, startRow = 7, colNames = FALSE)
writeData(wb, sheet = "W-12", x = sources, 
          startCol = 1, startRow = 61, colNames = FALSE)

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy.xlsx", overwrite=TRUE)
