#Table W6: Characteristics of the Rental Housing Stock, 2022

#Load packages 
library(tidyverse)
library(data.table)
library(openxlsx)
library(reldist)

setwd("C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\")

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"

df_2022 <- fread(file.path(path, "ACS_2022_hhplus.csv"))


# rental unit - structure type (number in thousands)    ---------------------------
tot <- df_2022 %>% 
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf")) %>%
  bind_rows(mutate(., bldcat = "6 Total")) %>%
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  group_by(bldcat) %>% 
  summarise(tot = sum(wgtp)/1000) %>% 
  pivot_wider(names_from = bldcat, values_from = tot) %>%
  mutate(total_mult = sum(c_across("2 2-4":"4 20pl"))) %>%
  select("1 sf", "2 2-4", "3 5-19", "4 20pl", total_mult, "5 manuf", "6 Total") 

region <- df_2022 %>% 
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf"),
         region = case_when(region==1 ~ "1 Northeast",
                            region==2 ~ "2 Midwest",
                            region==3 ~ "3 South",
                            region==4 ~ "4 West")) %>%
  bind_rows(mutate(., bldcat = "6 Total")) %>%
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  group_by(region, bldcat) %>% 
  summarise(tot = sum(wgtp)/1000) %>% 
  pivot_wider(names_from = bldcat, values_from = tot) %>%
  mutate(total_mult = sum(c_across("2 2-4":"4 20pl"))) %>%
  select(region, "1 sf", "2 2-4", "3 5-19", "4 20pl", total_mult, "5 manuf", "6 Total") %>% 
  ungroup() %>% 
  select(-region)

yrblt <- df_2022 %>% 
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf"),
         yrblt = case_when(yrblt<1950 ~ "1 before 1950",
                           yrblt>=1950 & yrblt<1980 ~ "2 1950-1979",
                           yrblt>=1980 & yrblt<2000 ~ "3 1980-1999",
                           yrblt>=2000 & yrblt<2010 ~ "4 2000-2009",
                           yrblt>=2010 ~ "5 2010_later")) %>%
  bind_rows(mutate(., bldcat = "6 Total")) %>%
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  group_by(yrblt, bldcat) %>% 
  summarise(tot = sum(wgtp)/1000) %>% 
  pivot_wider(names_from = bldcat, values_from = tot) %>%
  mutate(total_mult = sum(c_across("2 2-4":"4 20pl"))) %>%
  select(yrblt, "1 sf", "2 2-4", "3 5-19", "4 20pl", total_mult, "5 manuf", "6 Total") %>% 
  ungroup() %>% 
  select(-yrblt)

#rent distribution
rentdist <- df_2022 %>% 
  filter(!is.na(rntp)) %>% 
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf"),
         rntp_cat = case_when(rntp<600 ~ "1 under600",
                              rntp>=600 & rntp<800 ~ "2 600-799",
                              rntp>=800 & rntp<1000 ~ "3 800-999",
                              rntp>=1000 & rntp<1400 ~ "4 1000-1399",
                              rntp>=1400 & rntp<2000 ~ "5 1400-1999",
                              rntp>=2000 ~ "6 2000pl")) %>%
  bind_rows(mutate(., bldcat = "6 Total")) %>%
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  group_by(rntp_cat, bldcat) %>% 
  summarise(tot = sum(wgtp)/1000) %>% 
  pivot_wider(names_from = bldcat, values_from = tot) %>%
  mutate(total_mult = sum(c_across("2 2-4":"4 20pl"))) %>%
  select(rntp_cat, "1 sf", "2 2-4", "3 5-19", "4 20pl", total_mult, "5 manuf", "6 Total") %>% 
  ungroup() %>% 
  select(-rntp_cat)

#median rents
mdnrent <- df_2022 %>% 
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf")) %>% 
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  group_by(bldcat) %>% 
  summarise(med_rent = wtd.quantile(rntp, q=0.5, na.rm=TRUE, weight=wgtp)) %>% 
  pivot_wider(names_from = bldcat, values_from = med_rent)

mdnrent_mult <- df_2022 %>% 
  filter((bld>=4 & bld<=9) & (tensimp==2|vacs==1|vacs==2)) %>% 
  summarise(all_mf = wtd.quantile(rntp, q=0.5, na.rm=TRUE, weight=wgtp))

mdnrent_all <- df_2022 %>% 
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  summarise(all = wtd.quantile(rntp, q=0.5, na.rm=TRUE, weight=wgtp))

mdnrent_complete <- cbind(mdnrent, mdnrent_mult, mdnrent_all) %>% 
  select(`1 sf`, `2 2-4`, `3 5-19`, `4 20pl`, all_mf, `5 manuf`, all)

#number of bedrooms
beds <- df_2022 %>% 
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf"),
         bdsp = if_else(bdsp>3, 3, bdsp)) %>%
  bind_rows(mutate(., bldcat = "6 Total")) %>%
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  group_by(bdsp, bldcat) %>% 
  summarise(tot = sum(wgtp)/1000) %>% 
  pivot_wider(names_from = bldcat, values_from = tot) %>%
  mutate(total_mult = sum(c_across("2 2-4":"4 20pl"))) %>%
  select(bdsp, "1 sf", "2 2-4", "3 5-19", "4 20pl", total_mult, "5 manuf", "6 Total") %>% 
  ungroup() %>% 
  select(-bdsp)


## export to excel ---------------------------

title <- "Table W-6. US States: Characteristics of the Rental Housing Stock, 2022"  
sources <- "Source: JCHS tabulation of US Census Bureau, 2022 American Community Survey 1-Year Estimates."

wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-6", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, sheet = "W-6", x = tot, 
          startCol = 2, startRow = 8, colNames = FALSE)  
writeData(wb, sheet = "W-6", x = region, 
          startCol = 2, startRow = 10, colNames = FALSE)  
writeData(wb, sheet = "W-6", x = yrblt, 
          startCol = 2, startRow = 15, colNames = FALSE) 
writeData(wb, sheet = "W-6", x = rentdist, 
          startCol = 2, startRow = 21, colNames = FALSE)  
writeData(wb, sheet = "W-6", x = mdnrent_complete, 
          startCol = 2, startRow = 27, colNames = FALSE) 
writeData(wb, sheet = "W-6", x = beds, 
          startCol = 2, startRow = 29, colNames = FALSE)
writeData(wb, sheet = "W-6", x = sources, 
          startCol = 1, startRow = 35, colNames = FALSE)

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy.xlsx", overwrite=TRUE)


