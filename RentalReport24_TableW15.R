#Table W15: Share of Renters with Cost Burdens by Race/Ethnicity, 2022

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
race_costburden <- df_2022 %>%
  filter(sporder==1 & tensimp==2) %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  mutate(race5cat = case_when(race5cat == 1 ~ "1 White",
                              race5cat == 2 ~ "2 Black",
                              race5cat == 3 ~ "3 Hispanic",
                              race5cat == 4 ~ "4 Asian",
                              race5cat == 5 ~ "5 Mult_Oth")) %>%
  group_by(cost_burden, `State Name`, race5cat) %>% 
  summarise(tot = sum(wgtp)) %>%
  group_by(`State Name`, race5cat) %>% 
  mutate(share = tot/sum(tot)*100) %>% 
  filter(cost_burden==2|cost_burden==3) %>% 
  pivot_wider(names_from=cost_burden, values_from=c(tot, share)) %>% 
  mutate(share = share_2 + share_3,
         sharemod = share_2,
         sharesev = share_3) %>% 
  rename("ModBurdenSh"= sharemod,
         "SevBurdenSh" = sharesev) %>% 
  select("ModBurdenSh", "SevBurdenSh") %>%
  pivot_wider(names_from=race5cat, values_from=c("ModBurdenSh", "SevBurdenSh")) %>%
  select(`State Name`, ends_with("White"), ends_with("Black"), ends_with("Hispanic"), ends_with("Asian"), ends_with("Oth")) %>% 
  arrange(`State Name`!="United States")



# export to excel ----------------------------------------

title <- "Table W-15. US States: Share of Renters with Cost Burdens by Race/Ethnicity: 2022"  
sources <- "Source: JCHS tabulation of US Census Bureau, 2022 American Community Survey 1-Year Estimates."

wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-15", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE) 
writeData(wb, sheet = "W-15", x = race_costburden, 
          startCol = 1, startRow = 7, colNames = FALSE)  
writeData(wb, sheet = "W-15", x = sources, 
          startCol = 1, startRow = 63, colNames = FALSE)

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy.xlsx", overwrite=TRUE)

