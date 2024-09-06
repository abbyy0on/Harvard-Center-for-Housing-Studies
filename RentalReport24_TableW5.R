#US National: Renter Household Mobility by Type of Move, 2000-2023

library(data.table)
library(tidyverse)
library(openxlsx)

setwd("C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\CPS")
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("cps_00008.xml")
data <- read_ipums_micro(ddi)

movetype_counts <- data %>% 
  rename_with(tolower) %>% 
  mutate(movercat = case_when(migrate1==1 ~ "1 nonmover",
                              migrate1==3 ~ "2 within_county",
                              migrate1==4|migrate1==5 ~ "3 within_state_interstate",
                              migrate1==6 ~ "4 abroad"),
         flag= if_else((gq==1 & qmigrat1==0 & qmigrat1g==0) | (gq==1 & year<1995), 1, 0),
         movercat2 = if_else(flag==0, NA_character_, movercat),
         bin_move = if_else(movercat2=="1 nonmover", 0, 1),
         ten = case_when(ownershp == 10 ~ "1 Own",
                         ownershp==21|ownershp==22 ~ "2 Rent")) %>% 
  filter(ten=="2 Rent" & year>=2000 & relate==101 & gq==1 & !is.na(movercat2)) %>% 
  group_by(year, movercat2) %>% 
  summarise(tot = sum(asecwth)) %>% 
  group_by(year) %>% 
  mutate(tot = tot/1000) %>% 
  pivot_wider(names_from=movercat2, values_from = tot) %>% 
  mutate(tot = `1 nonmover` + `2 within_county` + `3 within_state_interstate` + `4 abroad`)

movetype_sh <- data %>% 
  rename_with(tolower) %>% 
  mutate(movercat = case_when(migrate1==1 ~ "1 nonmover",
                              migrate1==3 ~ "2 within_county",
                              migrate1==4|migrate1==5 ~ "3 within_state_interstate",
                              migrate1==6 ~ "4 abroad"),
         flag= if_else((gq==1 & qmigrat1==0 & qmigrat1g==0) | (gq==1 & year<1995), 1, 0),
         movercat2 = if_else(flag==0, NA_character_, movercat),
         bin_move = if_else(movercat2=="1 nonmover", 0, 1),
         ten = case_when(ownershp == 10 ~ "1 Own",
                         ownershp==21|ownershp==22 ~ "2 Rent")) %>% 
  filter(ten=="2 Rent" & year>=2000 & relate==101 & gq==1 & !is.na(movercat2)) %>% 
  group_by(year, movercat2) %>% 
  summarise(tot = sum(asecwth)) %>% 
  group_by(year) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  select(-tot) %>% 
  pivot_wider(names_from=movercat2, values_from = sh) %>% 
  mutate(tot = `1 nonmover` + `2 within_county` + `3 within_state_interstate` + `4 abroad`)

movetype <- movetype_counts %>% 
  left_join(movetype_sh, by="year")

#write to worksheet-----
workbook_path <- ("C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\RentalReport24_AppendixTables.xlsx")
title <- "Table W-5. US National: Renter Household Mobility by Type of Move, 2000-2023" #update the year

#these commands load in the target workbook, tell the program where to paste each component, then writes the tables into the workbook
wb <- loadWorkbook(workbook_path) #loads in an existing workbook with template pasted in to a sheet called "StateCostBurdenShares"
writeData(wb, sheet = "W-5", x = title, #pastes the title in to cell A1
          startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, sheet = "W-5", x = movetype,
          startCol = 1, startRow = 7, colNames = FALSE) #colnames=FALSE suppresses the variable names since these are already in the template, the US line in the data doesn't have a label field for the row so paste into column 2 instead of 1
saveWorkbook(wb, workbook_path, overwrite=TRUE)
