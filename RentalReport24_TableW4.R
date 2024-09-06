#Table W4: Rentership Rates by Demographics and Household Income

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
    race5cat = case_when(race5cat == 1 ~ "1 White",
                         race5cat == 2 ~ "2 Black",
                         race5cat == 3 ~ "3 Hispanic",
                         race5cat == 4 ~ "4 Asian",
                         race5cat == 5 ~ "5 Mult_Oth"),
    hhtype = case_when(hhtype == 1 ~ "1 Married, no Children",
                              hhtype == 2 ~ "2 Married with Children",
                              hhtype == 3 ~ "3 Single Parent",
                              hhtype == 4 ~ "4 Other Family",
                              hhtype == 5 ~ "5 Single Person",
                              hhtype == 6 ~ "6 Other non-family"),
    nativity = case_when(nativity == 1 ~ "1 Native born",
                         nativity == 2 ~ "2 Foreign born"))  


# 'total' row   ----------------------------------------
rent_rate_total <- df_2022 %>%
  filter(sporder==1) %>%  # tensimp = 1 is homeowner, tensimp = 2 is renter
  mutate_at(vars(c(st)),list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., st = "Total US")) %>%
  bind_rows(mutate(., hhincome = "All Incomes")) %>% # creates the  "Tot" summary column at the end of the table
  group_by(hhincome, tensimp) %>% 
  summarise(tot = sum(wgtp)) %>%
  group_by(hhincome) %>%
  mutate(share = tot/sum(tot)*100)  %>%
  filter(tensimp ==2) %>%
  select(-tensimp, -tot) %>%
  pivot_wider(names_from = "hhincome", values_from = "share") 


# rentership rates by age ----------------------------------------
rent_rate_age  <- df_2022 %>%
  filter(sporder==1) %>%  # tensimp = 1 is homeowner, tensimp = 2 is renter
  bind_rows(mutate(., hhincome = "All Incomes")) %>% # creates the  "Tot" summary column at the end of the table
  group_by(hhincome, age, tensimp) %>% 
  summarise(tot = sum(wgtp)) %>%
  group_by(hhincome, age) %>%
  mutate(share = tot/sum(tot)*100)  %>%
  filter(tensimp ==2) %>%
  select(-tensimp, -tot) %>%
  pivot_wider(names_from = "hhincome", values_from = "share") 
rent_rate_age$age<-NULL


# rentership rates by race ----------------------------------------
rent_rate_race  <- df_2022 %>%
  filter(sporder==1) %>%  # tensimp = 1 is homeowner, tensimp = 2 is renter
  bind_rows(mutate(., hhincome = "All Incomes")) %>% # creates the  "Tot" summary column at the end of the table
  group_by(hhincome, race5cat, tensimp) %>% 
  summarise(tot = sum(wgtp)) %>%
  group_by(hhincome, race5cat) %>%
  mutate(share = tot/sum(tot)*100)  %>%
  filter(tensimp ==2) %>%
  select(-tensimp, -tot) %>%
  pivot_wider(names_from = "hhincome", values_from = "share") 
rent_rate_race$race5cat<-NULL


# rentership rates by household type ----------------------------------------
rent_rate_hhtype <- df_2022 %>%
  filter(sporder==1) %>%  # tensimp = 1 is homeowner, tensimp = 2 is renter
  bind_rows(mutate(., hhincome = "All Incomes")) %>% # creates the  "Tot" summary column at the end of the table
  group_by(hhincome, hhtype, tensimp) %>% 
  summarise(tot = sum(wgtp)) %>%
  group_by(hhincome, hhtype) %>%
  mutate(share = tot/sum(tot)*100)  %>%
  filter(tensimp ==2) %>%
  select(-tensimp, -tot) %>%
  pivot_wider(names_from = "hhincome", values_from = "share") 
rent_rate_hhtype$hhtype<-NULL



# rentership rates by nativity   ----------------------------------------
rent_rate_nativity <- df_2022 %>%
  filter(sporder==1) %>%  # tensimp = 1 is homeowner, tensimp = 2 is renter
  bind_rows(mutate(., hhincome = "All Incomes")) %>% # creates the  "Tot" summary column at the end of the table
  group_by(hhincome, nativity, tensimp) %>% 
  summarise(tot = sum(wgtp)) %>%
  group_by(hhincome, nativity) %>%
  mutate(share = tot/sum(tot)*100)  %>%
  filter(tensimp ==2) %>%
  select(-tensimp, -tot) %>%
  pivot_wider(names_from = "hhincome", values_from = "share") 
rent_rate_nativity$nativity<-NULL



# import to excel ----------------------------------------

title <- "Table W-4. US National: Rentership Rates by Demographics and Household Income: 2022"  
sources <- "Sources: JCHS tabulation of US Census Bureau, 2022 American Community Survey 1-Year Estimates."

wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-4", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, sheet = "W-4", x = rent_rate_total, 
          startCol = 2, startRow = 7, colNames = FALSE)  
writeData(wb, sheet = "W-4", x = rent_rate_age, 
          startCol = 2, startRow = 9, colNames = FALSE)  
writeData(wb, sheet = "W-4", x = rent_rate_race, 
          startCol = 2, startRow = 17, colNames = FALSE)  
writeData(wb, sheet = "W-4", x = rent_rate_hhtype, 
          startCol = 2, startRow = 23, colNames = FALSE)  
writeData(wb, sheet = "W-4", x = rent_rate_nativity, 
          startCol = 2, startRow = 30, colNames = FALSE)  
writeData(wb, sheet = "W-4", x = sources, 
          startCol = 1, startRow = 34, colNames = FALSE)

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy.xlsx", overwrite=TRUE)