#Table W2: Characteristics of Renter Households by Household Income

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

## add variables----------
df_2022 <- df_2022 %>% 
  filter(tensimp == 2 & sporder == 1) %>%
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
    race5cat = case_when(rac1p == 1 & hisp == 1 ~ 1,
                         rac1p == 2 & hisp == 1 ~ 2,
                         hisp > 1 ~ 3,
                         rac1p == 6 & hisp == 1 ~ 4,
                         (rac1p == 3 | rac1p == 4 | rac1p == 5 | rac1p == 7 | rac1p == 8 | rac1p == 9) & hisp == 1 ~ 5),
    race5cat = case_when(race5cat == 1 ~ "1 White",
                         race5cat == 2 ~ "2 Black",
                         race5cat == 3 ~ "3 Hispanic",
                         race5cat == 4 ~ "4 Asian",
                         race5cat == 5 ~ "5 Mult_Oth"),
    householdtype = case_when(hhtype == 1 ~ "1 Married, no Children",
                             hhtype == 2 ~ "2 Married with Children",
                             hhtype == 3 ~ "3 Single Parent",
                             hhtype == 4 ~ "4 Other Family",
                             hhtype == 5 ~ "5 Single Person",
                             hhtype == 6 ~ "6 Other non-family"),
    nativity = case_when(nativity == 1 ~ "1 Native born",
                         nativity == 2 ~ "2 Foreign born"))  
 

## totals (row 7 in excel)----------

# total number of households (in thousands) per income bracket
income <- df_2022 %>% 
  filter(tensimp == 2 & sporder == 1) %>%
  mutate_at(vars(c(hhincome)), list(~as.character(.))) %>% # ensures that the hhincome column is treated as a character rather than any other data type
  bind_rows(mutate(., hhincome = "6 All Households")) %>% # creates a summary row where "6 All Households" represents a category encompassing all households
  group_by(hhincome) %>%
  summarise(tot = sum(wgtp)/1000) %>%
  pivot_wider(names_from = hhincome, values_from = tot)


# total percentages of income bracket  
incomeP <- df_2022 %>%  
  filter(tensimp==2 & sporder==1) %>% #tensimp is tenure, 2 selects renters, sporder is hh head, use these filters for all tabs
  mutate_at(vars(c(hhincome)), list(~as.character(.))) %>% 
  bind_rows(mutate(., hhincome = "6 All Households")) %>%
  group_by(hhincome) %>%
  summarise(tot = sum(wgtp)) %>%
  mutate(percentage = sum(tot)/sum(tot)*100) %>%
  select(-tot) %>%
  pivot_wider(names_from = hhincome, values_from = percentage)


## tabs for characteristics----------

# tab: age x income for renters (number of households in thousands)
age_income <- df_2022 %>%  
  filter(tensimp==2 & sporder==1) %>% 
  mutate_at(vars(c(hhincome)), list(~as.character(.))) %>% 
  bind_rows(mutate(., hhincome = "6 All Households")) %>%
  group_by(hhincome, age) %>%  
  summarise(tot = sum(wgtp)/1000) %>%
  pivot_wider(names_from="hhincome", 
              values_from = "tot")  

# tab: age x income for renters (percentage)
age_incomeP <- df_2022 %>%  
  filter(tensimp==2 & sporder==1) %>% #tensimp is tenure, 2 selects renters, sporder is hh head, use these filters for all tabs
  mutate_at(vars(c(hhincome, age)), list(~as.character(.))) %>% 
  bind_rows(mutate(., hhincome = "6 All Households")) %>%
  group_by(hhincome, age) %>% # helps with finding the share of households fulfilling a certain age & income bracket
  summarise(tot = sum(wgtp)) %>% # to get total # of households, you get sum of the weights (each person is representative of a certain number of households here; 'survey weight')
  mutate(percentage = tot/sum(tot)*100) %>%
  select(-tot) %>%
  pivot_wider(names_from="hhincome", 
              values_from = "percentage") 

# merge these tables
age_income_fulltable <- reduce(list(age_income, age_incomeP), left_join, by = "age")
#list includes the tables you want to merge, left_join is specified in next argument, by is the common variable
age_income_fulltable <- age_income_fulltable %>%  
    select(-age)

  
# tab: race x income-----
  
race_income <- df_2022 %>%  
  filter(tensimp==2 & sporder==1) %>% #tensimp is tenure, 2 selects renters, sporder is hh head, use these filters for all tabs
  mutate_at(vars(c(hhincome)), list(~as.character(.))) %>% 
  bind_rows(mutate(., hhincome = "6 All Households")) %>%
  group_by(hhincome, race5cat) %>%  
  summarise(tot = sum(wgtp)/1000) %>%  
  pivot_wider(names_from="hhincome", 
              values_from = "tot") 

# tab: race x income for renters (percentage)
race_incomeP <- df_2022 %>%  
  filter(tensimp==2 & sporder==1) %>% #tensimp is tenure, 2 selects renters, sporder is hh head, use these filters for all tabs
  mutate_at(vars(c(hhincome)), list(~as.character(.))) %>% 
  bind_rows(mutate(., hhincome = "6 All Households")) %>%
  group_by(hhincome, race5cat) %>%  
  summarise(tot = sum(wgtp)) %>%  
  mutate(percentage = tot/sum(tot)*100) %>%  
  select(-tot) %>%
  pivot_wider(names_from="hhincome", values_from = "percentage") 

# merge these tables
race_income_fulltable <- reduce(list(race_income, race_incomeP), left_join, by = "race5cat")
#list includes the tables you want to merge, left_join is specified in next argument, by is the common variable
race_income_fulltable <- race_income_fulltable %>%  
    select(-race5cat)
  

# tab: household type x income for renters-----
  
householdtype_income <- df_2022 %>%  
  filter(tensimp==2 & sporder==1) %>%
  mutate_at(vars(c(hhincome)), list(~as.character(.))) %>% 
  bind_rows(mutate(., hhincome = "6 All Households")) %>%
  group_by(hhincome, householdtype) %>% 
  summarise(tot = sum(wgtp)/1000) %>%
  pivot_wider(names_from="hhincome", 
              values_from = "tot") 
# tab: household type x income for renters (percentage)
householdtype_incomeP <- df_2022 %>%  
  filter(tensimp==2 & sporder==1) %>% #tensimp is tenure, 2 selects renters, sporder is hh head, use these filters for all tabs
  mutate_at(vars(c(hhincome)), list(~as.character(.))) %>% 
  bind_rows(mutate(., hhincome = "6 All Households")) %>%
  group_by(hhincome, householdtype) %>%  
  summarise(tot = sum(wgtp)) %>%  
  mutate(percentage = tot/sum(tot)*100) %>%  
  select(-tot) %>%
  pivot_wider(names_from="hhincome", 
              values_from = "percentage")  
  
# merge these tables
householdtype_income_fulltable <- reduce(list(householdtype_income, householdtype_incomeP), left_join, by = "householdtype")
#list includes the tables you want to merge, left_join is specified in next argument, by is the common variable
householdtype_income_fulltable <- householdtype_income_fulltable %>%  
    select(-householdtype)
  

# tab: nativity x income for renters-----
nativity_income <- df_2022 %>%  
  filter(tensimp==2 & sporder==1) %>% #tensimp is tenure, 2 selects renters, sporder is hh head, use these filters for all tabs
  mutate_at(vars(c(hhincome)), list(~as.character(.))) %>% 
  bind_rows(mutate(., hhincome = "6 All Households")) %>%
  group_by(hhincome, nativity) %>%
  summarise(tot = sum(wgtp)/1000) %>%  
  pivot_wider(names_from="hhincome", values_from = "tot")  

# tab: nativity x income for renters (percentage)
nativity_incomeP <- df_2022 %>%  
  filter(tensimp==2 & sporder==1) %>% #tensimp is tenure, 2 selects renters, sporder is hh head, use these filters for all tabs
  mutate_at(vars(c(hhincome)), list(~as.character(.))) %>% 
  bind_rows(mutate(., hhincome = "6 All Households")) %>%
  group_by(hhincome, nativity) %>% 
  summarise(tot = sum(wgtp)) %>%  
  mutate(percentage = tot/sum(tot)*100) %>%  
  select(-tot) %>%
  pivot_wider(names_from="hhincome", 
              values_from = "percentage")  
  
# merge these tables
nativity_income_fulltable <- reduce(list(nativity_income, nativity_incomeP), left_join, by = "nativity") 
#list includes the tables you want to merge, left_join is specified in next argument, by is the common variable
nativity_income_fulltable <- nativity_income_fulltable %>%  
  select(-nativity)
  


# excel
title <- "Table W-2. US National: Characteristics of Renter Households by Household Income: 2022"  
sources <- "Sources: JCHS tabulation of US Census Bureau, 2022 American Community Survey 1-Year Estimates."

#these commands load in the target workbook, tell the program where to paste each component, then writes the tables into the workbook
wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-2", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, sheet = "W-2", x = income, 
          startCol = 2, startRow = 7, colNames = FALSE)  
writeData(wb, sheet = "W-2", x = incomeP, 
          startCol = 8, startRow = 7, colNames = FALSE)  
writeData(wb, sheet = "W-2", x = age_income_fulltable, 
          startCol = 2, startRow = 9, colNames = FALSE) 
writeData(wb, sheet = "W-2", x = race_income_fulltable, 
          startCol = 2, startRow = 17, colNames = FALSE)  
writeData(wb, sheet = "W-2", x = householdtype_income_fulltable, 
          startCol = 2, startRow = 23, colNames = FALSE)  
writeData(wb, sheet = "W-2", x = nativity_income_fulltable, 
          startCol = 2, startRow = 30, colNames = FALSE)
writeData(wb, sheet = "W-2", x = sources, 
          startCol = 1, startRow = 34, colNames = FALSE)

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy.xlsx", overwrite=TRUE)
