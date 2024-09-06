#Table W30: Number and Share of Rental Units by Structure Type, 2022

library(data.table)
library(tidyverse)

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"
workbook_path <- "C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\RentalReport24_AppendixTables_WAO working copy2.xlsx"

us_2022 <- fread(file.path(path, "ACS_2022_hhplus.csv"))
met_2022 <- fread(file.path(path, "ACS_2022hhplus_metro.csv"))


#table components-------
#national tabs
us_bldcat <- us_2022 %>% 
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf")) %>%
  bind_rows(mutate(., bldcat = "6 Total")) %>%
  group_by(bldcat) %>% 
  summarise(tot = sum(wgtp)/1000) %>% 
  pivot_wider(names_from=bldcat, values_from=tot) %>% 
  mutate(total_mult = sum(c_across("2 2-4":"4 20pl"))) %>%
  select("1 sf", "2 2-4", "3 5-19", "4 20pl", total_mult, "5 manuf", "6 Total")

us_bldcat_sh <- us_2022 %>% 
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf")) %>%
  group_by(bldcat) %>% 
  summarise(tot = sum(wgtp)) %>% 
  ungroup() %>% 
  mutate(sh= tot/sum(tot)*100) %>% 
  select(-tot) %>% 
  pivot_wider(names_from=bldcat, values_from=sh) 

us_bldcat_mf_sh <- us_2022 %>% 
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf")) %>%
  bind_rows(mutate(., bldcat = "6 Total")) %>%
  group_by(bldcat) %>% 
  summarise(tot = sum(wgtp)) %>% 
  ungroup() %>% 
  pivot_wider(names_from=bldcat, values_from=tot) %>% 
  mutate(total_mult = sum(c_across("2 2-4":"4 20pl")),
         mult_sh=total_mult/`6 Total` *100) %>%
  select(mult_sh)

us_sh <- cbind(us_bldcat_sh, us_bldcat_mf_sh) %>% 
  select("1 sf", "2 2-4", "3 5-19", "4 20pl", mult_sh, "5 manuf")

us_table <- cbind(us_bldcat, us_sh)

#metro tabs
met_bldcat <- met_2022 %>% 
  filter(poprank<=100 & (tensimp==2|vacs==1|vacs==2)) %>% 
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf")) %>%
  bind_rows(mutate(., bldcat = "6 Total")) %>%
  group_by(cbsaname20, bldcat) %>% 
  summarise(tot = sum(hhwtcbsa)/1000) %>% 
  pivot_wider(names_from=bldcat, values_from=tot) %>% 
  mutate(total_mult = sum(c_across("2 2-4":"4 20pl"))) %>%
  select("1 sf", "2 2-4", "3 5-19", "4 20pl", total_mult, "5 manuf", "6 Total")

met_bldcat_sh <- met_2022 %>% 
  filter(poprank<=100 & (tensimp==2|vacs==1|vacs==2)) %>%  
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf")) %>%
  group_by(cbsaname20, bldcat) %>% 
  summarise(tot = sum(hhwtcbsa)) %>% 
  group_by(cbsaname20) %>% 
  mutate(sh= tot/sum(tot)*100) %>% 
  select(-tot) %>% 
  pivot_wider(names_from=bldcat, values_from=sh) 

met_bldcat_mf_sh <- met_2022 %>% 
  filter(poprank<=100 & (tensimp==2|vacs==1|vacs==2)) %>%  
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==4|bld==5 ~ "2 2-4",
                            bld==6|bld==7 ~"3 5-19",
                            bld==8|bld==9 ~ "4 20pl", 
                            bld==1|bld==10 ~ "5 manuf")) %>%
  bind_rows(mutate(., bldcat = "6 Total")) %>%
  group_by(cbsaname20, bldcat) %>% 
  summarise(tot = sum(hhwtcbsa)) %>% 
  group_by(cbsaname20) %>% 
  pivot_wider(names_from=bldcat, values_from=tot) %>% 
  mutate(total_mult = sum(c_across("2 2-4":"4 20pl")),
         mult_sh=total_mult/`6 Total` *100) %>%
  select(mult_sh)

met_sh <- cbind(met_bldcat_sh, met_bldcat_mf_sh) %>% 
  select("1 sf", "2 2-4", "3 5-19", "4 20pl", mult_sh, "5 manuf")

met_table <- cbind(met_bldcat, met_sh)
 

#export to excel------
title <- "Table W-30. US Metro Areas: Number and Share of Rental Units by Structure Type, 2022" #update the year
sources <- "Source: JCHS tabulations of US Census Bureau, American Community Survey 2022 1-Year Estimates and Missouri Census Data Center data."

#these commands load in the target workbook, tell the program where to paste each component, then writes the tables into the workbook
wb <- loadWorkbook(workbook_path) #loads in an existing workbook with template pasted in to a sheet called "StateCostBurdenShares"
writeData(wb, sheet = "W-30", x = title, #pastes the title in to cell A1
          startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, sheet = "W-30", x = us_table,
          startCol = 2, startRow = 8, colNames = FALSE) #colnames=FALSE suppresses the variable names since these are already in the template, the US line in the data doesn't have a label field for the row so paste into column 2 instead of 1
writeData(wb, sheet = "W-30", x = met_table, 
          startCol = 1, startRow = 9, colNames = FALSE)
writeData(wb, sheet = "W-30", x = sources, 
          startCol = 1, startRow = 111, colNames = FALSE)
saveWorkbook(wb, workbook_path, overwrite=TRUE)