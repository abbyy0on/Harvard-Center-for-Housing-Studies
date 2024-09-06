#Table W28: Basic Rental Housing Facts from the American Community Survey: 2022

library(data.table)
library(tidyverse)
library(openxlsx)
library(reldist)

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"
workbook_path <- "C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\RentalReport24_AppendixTables_WAO working copy2.xlsx"

us_2022 <- fread(file.path(path, "ACS_2022_hhplus.csv"))
met_2022 <- fread(file.path(path, "ACS_2022hhplus_metro.csv"))

##us-level tabs------
us_hhs <- us_2022 %>% 
  filter(tensimp==1|tensimp==2) %>% 
  group_by(tensimp) %>% 
  summarise(tot = sum(wgtp)/1000) %>% 
  ungroup() %>% 
  mutate(sh = tot/sum(tot) * 100) %>% 
  filter(tensimp==2) %>% 
  select(-tensimp)

us_burden <- us_2022 %>% 
  filter(tensimp==2) %>% 
  group_by(cost_burden) %>% 
  summarise(tot = sum(wgtp)) %>% 
  ungroup() %>% 
  mutate(sh=tot/sum(tot) * 100) %>% 
  filter(cost_burden>1) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = cost_burden, values_from = sh) %>% 
  mutate(any_burden = `2` + `3`)

us_medinc <- us_2022 %>% 
  filter(tensimp==2) %>% 
  summarise(medinc = wtd.quantile(hincp, q=0.5, na.rm=TRUE, weight=wgtp))

us_inccat <- us_2022 %>% 
  filter(tensimp==2) %>% 
  group_by(hh_inccat) %>% 
  summarise(tot=sum(wgtp)) %>% 
  ungroup() %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = hh_inccat, values_from = sh)

us_totrentals <- us_2022 %>% 
  filter(tensimp==2 | vacs==1 |vacs==2) %>% 
  summarise(tot=sum(wgtp/1000))

us_vacrate <- us_2022 %>% 
  filter(tensimp==2 | vacs==1 | vacs==2) %>% 
  mutate(vacant = case_when(vacs==1|vacs==2 ~ 1,
                            tensimp==2 ~ 0)) %>% 
  group_by(vacant) %>% 
  summarise(tot=sum(wgtp)) %>% 
  ungroup() %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(vacant==1) %>% 
  select(sh)

us_medrnt <- us_2022 %>% 
  filter(tensimp==2) %>% 
  summarise(medinc = wtd.quantile(grntp, q=0.5, na.rm=TRUE, weight=wgtp))

us_rntlvl <- us_2022 %>% 
  filter((tensimp==2|vacs==1|vacs==2) & !is.na(rntp)) %>% 
  mutate(rntcat = case_when(rntp<600 ~ 1,
                            rntp>=600 & rntp<1000 ~ 2,
                            rntp>=1000 & rntp<1400 ~ 3,
                            rntp>=1400 & rntp<2000 ~ 4,
                            rntp>=2000 ~ 5)) %>% 
  group_by(rntcat) %>% 
  summarise(tot=sum(wgtp)) %>% 
  ungroup() %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(!is.na(rntcat)) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = rntcat, values_from = sh)

us_bldcat <- us_2022 %>% 
  filter(tensimp==2|vacs==1|vacs==2) %>% 
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==1|bld==10 ~ "2 manuf",
                            bld==4|bld==5 ~ "3 2-4",
                            bld==6|bld==7 ~"4 5-19",
                            bld==8|bld==9 ~ "5 20pl")) %>% 
  group_by(bldcat) %>% 
  summarise(tot=sum(wgtp)) %>% 
  ungroup() %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = bldcat, values_from = sh)

us_all <- cbind(us_hhs, us_burden, us_medinc, us_inccat, us_totrentals, us_vacrate, us_medrnt, us_rntlvl, us_bldcat)


##metro-level tabs-----
met_hhs <- met_2022 %>% 
  filter(poprank<=100 & (tensimp==1|tensimp==2)) %>% 
  group_by(cbsaname20, tensimp) %>% 
  summarise(tot = sum(hhwtcbsa)/1000) %>% 
  group_by(cbsaname20) %>% 
  mutate(sh = tot/sum(tot) * 100) %>% 
  filter(tensimp==2) %>% 
  select(-tensimp)

met_burden <- met_2022 %>% 
  filter(tensimp==2 & poprank<=100) %>% 
  group_by(cbsaname20, cost_burden) %>% 
  summarise(tot = sum(hhwtcbsa)) %>% 
  group_by(cbsaname20) %>% 
  mutate(sh=tot/sum(tot) * 100) %>% 
  filter(cost_burden>1) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = cost_burden, values_from = sh) %>% 
  mutate(any_burden = `2` + `3`)

met_medinc <- met_2022 %>% 
  filter(tensimp==2 & poprank<=100) %>%
  group_by(cbsaname20) %>% 
  summarise(medinc = wtd.quantile(hincp, q=0.5, na.rm=TRUE, weight=hhwtcbsa))

met_inccat <- met_2022 %>% 
  filter(tensimp==2 & poprank<=100) %>% 
  group_by(cbsaname20, hh_inccat) %>% 
  summarise(tot=sum(hhwtcbsa)) %>% 
  group_by(cbsaname20) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = hh_inccat, values_from = sh)

met_totrentals <- met_2022 %>% 
  filter((tensimp==2 | vacs==1 |vacs==2) & poprank<=100) %>% 
  group_by(cbsaname20) %>% 
  summarise(tot=sum(hhwtcbsa/1000))

met_vacrate <- met_2022 %>% 
  filter((tensimp==2 | vacs==1 |vacs==2) & poprank<=100) %>% 
  mutate(vacant = case_when(vacs==1|vacs==2 ~ 1,
                            tensimp==2 ~ 0)) %>% 
  group_by(cbsaname20, vacant) %>% 
  summarise(tot=sum(hhwtcbsa)) %>% 
  group_by(cbsaname20) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(vacant==1) %>% 
  select(sh)

met_medrnt <- met_2022 %>% 
  filter(tensimp==2 & poprank<=100) %>% 
  group_by(cbsaname20) %>% 
  summarise(medinc = wtd.quantile(grntp, q=0.5, na.rm=TRUE, weight=hhwtcbsa))

met_rntlvl <- met_2022 %>% 
  filter((tensimp==2 | vacs==1 |vacs==2) & poprank<=100 & !is.na(rntp)) %>% 
  mutate(rntcat = case_when(rntp<600 ~ 1,
                            rntp>=600 & rntp<1000 ~ 2,
                            rntp>=1000 & rntp<1400 ~ 3,
                            rntp>=1400 & rntp<2000 ~ 4,
                            rntp>=2000 ~ 5)) %>% 
  group_by(cbsaname20, rntcat) %>% 
  summarise(tot=sum(hhwtcbsa)) %>% 
  group_by(cbsaname20) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(!is.na(rntcat)) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = rntcat, values_from = sh)

met_bldcat <- met_2022 %>% 
  filter((tensimp==2 | vacs==1 |vacs==2) & poprank<=100) %>% 
  mutate(bldcat = case_when(bld==2|bld==3 ~ "1 sf",
                            bld==1|bld==10 ~ "2 manuf",
                            bld==4|bld==5 ~ "3 2-4",
                            bld==6|bld==7 ~"4 5-19",
                            bld==8|bld==9 ~ "5 20pl")) %>% 
  group_by(cbsaname20, bldcat) %>% 
  summarise(tot=sum(hhwtcbsa)) %>% 
  group_by(cbsaname20) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  select(-tot) %>% 
  pivot_wider(names_from = bldcat, values_from = sh)

metlist <- c()
met_all <- reduce(list(met_hhs, met_burden, met_medinc, met_inccat, met_totrentals, met_vacrate, met_medrnt, met_rntlvl, met_bldcat), left_join, by='cbsaname20')

#write to worksheet-----
title <- "Table W-28. US Metro Areas: Basic Rental Housing Facts from the American Community Survey, 2022" #update the year
sources <- "Source: JCHS tabulations of US Census Bureau, 2022 American Community Survey 1-Year Estimates and Missouri Census Data Center data."

#these commands load in the target workbook, tell the program where to paste each component, then writes the tables into the workbook
wb <- loadWorkbook(workbook_path) #loads in an existing workbook with template pasted in to a sheet called "StateCostBurdenShares"
writeData(wb, sheet = "W-28", x = title, #pastes the title in to cell A1
          startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, sheet = "W-28", x = us_all,
          startCol = 2, startRow = 8, colNames = FALSE) #colnames=FALSE suppresses the variable names since these are already in the template, the US line in the data doesn't have a label field for the row so paste into column 2 instead of 1
writeData(wb, sheet = "W-28", x = met_all, 
          startCol = 1, startRow = 9, colNames = FALSE)
writeData(wb, sheet = "W-28", x = sources, 
          startCol = 1, startRow = 112, colNames = FALSE)
saveWorkbook(wb, workbook_path, overwrite=TRUE)
