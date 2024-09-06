#Table W11: Renter Households by Age, Race, and Household Income, 2022

# Load packages 
library(tidyverse)
library(data.table)
library(openxlsx)
library(tidyquant)
library(dint)
library(pracma) 

setwd("C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\")

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"

df_2022 <- fread(file.path(path, "ACS_2022_hhplus.csv"))
states_fips_codes <- fread(file.path(path, "State FIPS codes.csv"))
# merge the datasets 
df_2022 <- left_join(df_2022, states_fips_codes, by = c("st" = "FIPS Code")) 


# add variables  ----------------------------------------
df_2022 <- df_2022 %>%
  filter(sporder == 1) %>%
  mutate(hhincome = case_when(
                    hincp < 15000 ~ "1 Less than $15,000",
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


# tab: state x age for renters (number of households in thousands) ------------------------------
state_age <- df_2022 %>%  
  filter(tensimp == 2 & sporder == 1) %>%
  mutate_at(vars(c(`State Name`)), list(~as.character(.))) %>% # preserves row for all hhs; total US numbers
  bind_rows(mutate(., `State Name` = "United States")) %>%
  group_by(age, `State Name`) %>%  
  summarise(tot = sum(wgtp)/1000) %>%
  pivot_wider(names_from = "age", values_from = "tot")   
 

# tab: state x race for renters (number of households in thousands) ------------------------------
state_race <- df_2022 %>%  
  filter(tensimp==2 & sporder==1) %>%
  mutate_at(vars(c(`State Name`)),list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., `State Name` = "United States")) %>%
  group_by(race6cat, `State Name`) %>%
  summarise(tot = sum(wgtp)/1000) %>%
  pivot_wider(names_from="race6cat", 
              values_from = "tot")  

# tab: state x income for renters (number of households in thousands) ------------------------------
state_income <- df_2022 %>%  
  filter(tensimp==2 & sporder==1) %>%
  mutate_at(vars(c(`State Name`)),list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., `State Name` = "United States")) %>%
  mutate_at(vars(c(hhincome)), list(~as.character(.))) %>% # ensures that the hhincome column is treated as a character rather than any other data type
  bind_rows(mutate(., hhincome = "All Incomes")) %>% # creates a summary column where "6 All Households" represents a category encompassing all households
  group_by(hhincome, `State Name`) %>%  
  summarise(tot = sum(wgtp)/1000) %>%
  pivot_wider(names_from="hhincome", 
              values_from = "tot")  

# merge tables ------------------------------
states_full_table <- reduce(list(state_age, state_race, state_income), left_join, by = "State Name") %>% 
  arrange(`State Name` != 'United States') #move US total to top row

# import to excel ------------------------------

title <- "Table W-11. US States: Renter Households by Age, Race, and Household Income, 2022"  
sources <- "Source: JCHS tabulation of US Census Bureau, 2022 American Community Survey 1-Year Estimates."

#these commands load in the target workbook, tell the program where to paste each component, then writes the tables into the workbook
wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-11", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE) 
writeData(wb, sheet = "W-11", x = states_full_table, 
          startCol = 1, startRow = 7, colNames = FALSE)  
writeData(wb, sheet = "W-11", x = sources, 
          startCol = 1, startRow = 61, colNames = FALSE) 

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy.xlsx", overwrite=TRUE)

