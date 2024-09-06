#Table W16: Share of Renters with Cost Burdens by Household Income, 2022

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


# table ----------------------------------------
income_costburden <- df_2022 %>%
  filter(sporder==1 & tensimp==2) %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  mutate(hhincome = case_when(hincp < 15000 ~ "1 Less than $15000",
                              hincp >= 15000 & hincp <=29999 ~ "2 $15,000-29,999",
                              hincp >= 30000 & hincp <=44999 ~ "3 $30,000-44,999",
                              hincp >= 45000 & hincp <=74999 ~ "4 $45,000-74,999",
                              hincp >= 75000 ~ "5 $75,000 or More")) %>% 
  bind_rows(mutate(., hhincome = "All Incomes")) %>% # creates a summary column where "6 All Households" represents a category encompassing all households
  group_by(cost_burden, `State Name`, hhincome) %>% 
  summarise(tot = sum(wgtp)) %>%
  group_by(`State Name`, hhincome) %>% 
  mutate(share = tot/sum(tot)*100) %>% 
  filter(cost_burden==2|cost_burden==3) %>% 
  pivot_wider(names_from=cost_burden, values_from=c(tot, share)) %>% 
  mutate(share = share_2 + share_3,
         sharemod = share_2,
         sharesev = share_3) %>% 
  rename("ModBurdenSh"= sharemod,
         "SevBurdenSh" = sharesev) %>% 
  select("ModBurdenSh", "SevBurdenSh") %>%
  pivot_wider(names_from=hhincome, values_from=c("ModBurdenSh", "SevBurdenSh")) %>%
  select(`State Name`, ends_with("Less than $15000"), ends_with("$15,000-29,999"), ends_with("$30,000-44,999"), ends_with("$45,000-74,999"), ends_with("$75,000 or More"), ends_with("All incomes")) %>% 
  arrange(`State Name` != "United States")


# export to excel ----------------------------------------
title <- "Table W-16. US States: Share of Renters with Cost Burdens by Household Income: 2022"  
sources <- "Source: JCHS tabulation of US Census Bureau, 2022 American Community Survey 1-Year Estimates."

wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-16", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE) 
writeData(wb, sheet = "W-16", x = income_costburden, 
          startCol = 1, startRow = 7, colNames = FALSE)  
writeData(wb, sheet = "W-16", x = sources, 
          startCol = 1, startRow = 62, colNames = FALSE)

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy.xlsx", overwrite=TRUE)
