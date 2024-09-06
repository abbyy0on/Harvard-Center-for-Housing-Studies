#Table W21: Change in Rental Stock by Structure Type: 2010-2022

# Load packages 
library(tidyverse)
library(data.table)
library(openxlsx)

setwd("C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\")

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"

multi_years <-c(2010) #years to select from multi file (2001:2014) if keeping all
yearly_years <- c(2016, 2022) #used for recent years not in multi file

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


#table components - number of rental units by structure type------------------------

#years from multiyear file
bldcat_mult <- multiyear %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf")) %>%
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  group_by(year, `State Name`, bldcat) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  pivot_wider(names_from = bldcat, values_from = tot)  %>%
  mutate(all_units = sum(c_across("1 sf":"5 manuf"), na.rm = TRUE))  %>% 
  rename_at(vars(-c(year, `State Name`)),function(x) paste0(x, "_2010")) %>% 
  ungroup() %>% 
  select(-year)

#years from annual files
bldcat_16 <- df_2016 %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf")) %>%
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  group_by(`State Name`, bldcat) %>% 
  summarise(tot = sum(wgtp)/1000) %>% 
  pivot_wider(names_from = bldcat, values_from = tot) %>%
  mutate(all_units = sum(c_across("1 sf":"5 manuf"), na.rm = TRUE))   %>% 
  rename_at(vars(-`State Name`),function(x) paste0(x, "_2016"))
 
bldcat_22 <- df_2022 %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf")) %>%
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  group_by(`State Name`, bldcat) %>% 
  summarise(tot = sum(wgtp)/1000) %>% 
  pivot_wider(names_from = bldcat, values_from = tot) %>%
  mutate(all_units = sum(c_across("1 sf":"5 manuf"), na.rm = TRUE))   %>% 
  rename_at(vars(-`State Name`),function(x) paste0(x, "_2022"))

# combine all years ------------------------

full_table <- reduce(list(bldcat_mult, bldcat_16, bldcat_22), left_join, by = "State Name")  %>% 
  arrange(`State Name`!="United States") %>%
  mutate("1 sf change" = `1 sf_2022` - `1 sf_2010`,
         "2 2-4 change" = `2 2-4_2022` - `2 2-4_2010`,
         "3 5-19 change" = `3 5-19_2022` - `3 5-19_2010`,
         "4 20pl change" = `4 20pl_2022` - `4 20pl_2010`,
         "5 manuf change" = `5 manuf_2022` - `5 manuf_2010`,
         "allunits change" = all_units_2022 - all_units_2010) %>%
  ungroup() %>% 
  select(`State Name`, starts_with("1 sf"), starts_with("2 2-4"), starts_with("3 5-19"), starts_with("4 20pl"), starts_with("5 manuf"), starts_with("all_units"))
  

## export to excel ---------------------------

title <- "Table W-21. US States: Change in Rental Stock by Structure Type: 2010–2022"  
sources <- "Source: JCHS tabulation of US Census Bureau, American Community Survey 1-Year Estimates."

wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-21", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE) 
writeData(wb, sheet = "W-21", x = full_table, 
          startCol = 1, startRow = 7, colNames = FALSE)  
writeData(wb, sheet = "W-21", x = sources, 
          startCol = 1, startRow = 61, colNames = FALSE)

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy.xlsx", overwrite=TRUE)

