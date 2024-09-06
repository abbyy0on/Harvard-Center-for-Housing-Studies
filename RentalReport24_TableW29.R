#Table W29: Number of Rental Units by Monthly Contract Rent: 2012-2022

library(data.table)
library(tidyverse)
library(openxlsx)
library(tidyquant)
library(pracma)

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"
workbook_path <- "C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\RentalReport24_AppendixTables_WAO working copy.xlsx"

currentyear<-2022

#load inflation factors
tickers <-c('CUUR0000SA0L2', #less shelter
            'CUSR0000SEHA', #rent of primary residence
            'CPIAUCNS' #cpi-u nsa
) 

myvars<-c('CPIU Less Shelter',
          'CPIU Rent of Primary Residence',
          'CPIU nsa')

mylookup<-data.frame(symbol=tickers, var=myvars) #just a df to see the variables that will be pulled with labels

cpi<- tickers %>% 
  tq_get(get="economic.data", from="2001-01-01") %>% #call to pull data from fred, can specify earlier start date
  spread(symbol, price) %>% #reshapes from long to wide
  mutate(cpi_allitems = CPIAUCNS, 
         cpi_ls = CUUR0000SA0L2,
         cpi_ai_12mav = movavg(CPIAUCNS, 12, type="s"), #calculates 12 month trailing avg
         cpi_ls_12mav = movavg(CUUR0000SA0L2, 12, type="s"),
         month = rep(1:12, length.out=n()),
         year = year(date)) %>%
  filter(month == 12 & year<=currentyear) %>% 
  select(year, cpi_ai_12mav, cpi_ls_12mav)

#create object for current data year's cpi
cpi_ls_current <- cpi$cpi_ls_12mav[cpi$year==currentyear]

us_2012 <- fread(file.path(path, "ACS_multiyear_hhplus_RentersPlusVacant.csv")) %>% 
  filter(year==2012) %>% 
  left_join(cpi, by="year") %>% 
  mutate(rntp_infl = rntp*cpi_ls_current/cpi_ls_12mav,
         rntp_infl_cat = case_when(rntp_infl<600 ~ "1 under600",
                                   rntp_infl>=600 & rntp_infl<800 ~ "2 600-799",
                                   rntp_infl>=800 & rntp_infl<1000 ~ "3 800-999",
                                   rntp_infl>=1000 & rntp_infl<1400 ~ "4 1000-1399",
                                   rntp_infl>=1400 & rntp_infl<2000 ~ "5 1400-1999",
                                   rntp_infl>=2000 ~ "6 2000pl"))
met_2012 <- fread(file.path(path, "ACS_2012hhplus_metro.csv")) %>% 
  mutate(year=2012) %>% 
  left_join(cpi, by="year") %>% 
  mutate(rntp_infl = rntp*cpi_ls_current/cpi_ls_12mav,
         rntp_infl_cat = case_when(rntp_infl<600 ~ "1 under600",
                                   rntp_infl>=600 & rntp_infl<800 ~ "2 600-799",
                                   rntp_infl>=800 & rntp_infl<1000 ~ "3 800-999",
                                   rntp_infl>=1000 & rntp_infl<1400 ~ "4 1000-1399",
                                   rntp_infl>=1400 & rntp_infl<2000 ~ "5 1400-1999",
                                   rntp_infl>=2000 ~ "6 2000pl"))

us_2019 <- fread(file.path(path, "ACS_2019_hhplus.csv")) %>% 
  left_join(cpi, by="year") %>% 
  mutate(rntp_infl = rntp*cpi_ls_current/cpi_ls_12mav,
         rntp_infl_cat = case_when(rntp_infl<600 ~ "1 under600",
                                   rntp_infl>=600 & rntp_infl<800 ~ "2 600-799",
                                   rntp_infl>=800 & rntp_infl<1000 ~ "3 800-999",
                                   rntp_infl>=1000 & rntp_infl<1400 ~ "4 1000-1399",
                                   rntp_infl>=1400 & rntp_infl<2000 ~ "5 1400-1999",
                                   rntp_infl>=2000 ~ "6 2000pl"))
met_2019 <- fread(file.path(path, "ACS_2019_hhplus_metro.csv")) %>% 
  left_join(cpi, by="year") %>% 
  mutate(rntp_infl = rntp*cpi_ls_current/cpi_ls_12mav,
         rntp_infl_cat = case_when(rntp_infl<600 ~ "1 under600",
                                   rntp_infl>=600 & rntp_infl<800 ~ "2 600-799",
                                   rntp_infl>=800 & rntp_infl<1000 ~ "3 800-999",
                                   rntp_infl>=1000 & rntp_infl<1400 ~ "4 1000-1399",
                                   rntp_infl>=1400 & rntp_infl<2000 ~ "5 1400-1999",
                                   rntp_infl>=2000 ~ "6 2000pl"))

us_2022 <- fread(file.path(path, "ACS_2022_hhplus.csv")) %>% 
  left_join(cpi, by="year") %>% 
  mutate(rntp_infl = rntp*cpi_ls_current/cpi_ls_12mav,
         rntp_infl_cat = case_when(rntp_infl<600 ~ "1 under600",
                                   rntp_infl>=600 & rntp_infl<800 ~ "2 600-799",
                                   rntp_infl>=800 & rntp_infl<1000 ~ "3 800-999",
                                   rntp_infl>=1000 & rntp_infl<1400 ~ "4 1000-1399",
                                   rntp_infl>=1400 & rntp_infl<2000 ~ "5 1400-1999",
                                   rntp_infl>=2000 ~ "6 2000pl"))
met_2022 <- fread(file.path(path, "ACS_2022hhplus_metro.csv")) %>% 
  left_join(cpi, by="year") %>% 
  mutate(rntp_infl = rntp*cpi_ls_current/cpi_ls_12mav,
         rntp_infl_cat = case_when(rntp_infl<600 ~ "1 under600",
                                   rntp_infl>=600 & rntp_infl<800 ~ "2 600-799",
                                   rntp_infl>=800 & rntp_infl<1000 ~ "3 800-999",
                                   rntp_infl>=1000 & rntp_infl<1400 ~ "4 1000-1399",
                                   rntp_infl>=1400 & rntp_infl<2000 ~ "5 1400-1999",
                                   rntp_infl>=2000 ~ "6 2000pl"))


#table components--------
#national tabs
rntcat_us12 <- us_2012 %>% 
  filter((tensimp==2|vacs==1|vacs==2) & year==2012 & !is.na(rntp)) %>% 
  group_by(rntp_infl_cat) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  pivot_wider(names_from = rntp_infl_cat, values_from=tot) %>% 
  rename_all(function(x) paste0(x, "_2012"))

rntcat_us19 <- us_2019 %>% 
  filter((tensimp==2|vacs==1|vacs==2) & !is.na(rntp)) %>% 
  group_by(rntp_infl_cat) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  pivot_wider(names_from = rntp_infl_cat, values_from=tot) %>% 
  rename_all(function(x) paste0(x, "_2019"))

rntcat_us22 <- us_2022 %>% 
  filter((tensimp==2|vacs==1|vacs==2) & !is.na(rntp)) %>% 
  group_by(rntp_infl_cat) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  pivot_wider(names_from = rntp_infl_cat, values_from=tot) %>% 
  rename_all(function(x) paste0(x, "_2022"))

rntcat_us_table <- cbind(rntcat_us12, rntcat_us19, rntcat_us22)


#metro tabs
rntcat_met12 <- met_2012 %>% 
  mutate(tensimp = case_when(ten==1|ten==2 ~ 1,
                             ten==3|ten==4 ~ 2)) %>% 
  filter(poprank<=100 & (tensimp==2|vacs==1|vacs==2) & !is.na(rntp)) %>% 
  group_by(cbsaname20, rntp_infl_cat) %>% 
  summarise(tot = sum(hhwtcbsa)/1000) %>% 
  pivot_wider(names_from=rntp_infl_cat, values_from=tot) %>% 
  rename_at(vars(-cbsaname20),function(x) paste0(x, "_2012"))

rntcat_met19 <- met_2019 %>% 
  mutate(top100_2022 = case_when(CBSAName20 == "Scranton--Wilkes-Barre, PA" ~ 0,
                                 CBSAName20 == "Fayetteville-Springdale-Rogers, AR" ~ 1,
                                 CBSAName20 != "Scranton--Wilkes-Barre, PA" & CBSAName20 != "Fayetteville-Springdale-Rogers, AR" ~ top100)) %>% 
  filter(top100_2022==1 & (tensimp==2|vacs==1|vacs==2) & !is.na(rntp)) %>% 
  group_by(CBSAName20, rntp_infl_cat) %>% 
  summarise(tot = sum(hhwtcbsa)/1000) %>% 
  pivot_wider(names_from=rntp_infl_cat, values_from=tot) %>% 
  rename(cbsaname20 = CBSAName20) %>% 
  rename_at(vars(-cbsaname20),function(x) paste0(x, "_2019"))

rntcat_met22 <- met_2022 %>% 
  filter(poprank<=100 & (tensimp==2|vacs==1|vacs==2) & !is.na(rntp)) %>% 
  group_by(cbsaname20, rntp_infl_cat) %>% 
  summarise(tot = sum(hhwtcbsa)/1000) %>% 
  pivot_wider(names_from=rntp_infl_cat, values_from=tot) %>% 
  rename_at(vars(-cbsaname20),function(x) paste0(x, "_2022"))

rntcat_met_table <- reduce(list(rntcat_met12, rntcat_met19, rntcat_met22), left_join, by = "cbsaname20")  


#export to excel-------
title <- "Table W-29. US Metro Areas: Number of Rental Units by Monthly Contract Rent: 2012-2022" #update the year
sources <- "Source: JCHS tabulations of US Census Bureau, American Community Survey 1-Year Estimates and Missouri Census Data Center data."

#these commands load in the target workbook, tell the program where to paste each component, then writes the tables into the workbook
wb <- loadWorkbook(workbook_path) #loads in an existing workbook with template pasted in to a sheet called "StateCostBurdenShares"
writeData(wb, sheet = "W-29", x = title, #pastes the title in to cell A1
          startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, sheet = "W-29", x = rntcat_us_table,
          startCol = 2, startRow = 7, colNames = FALSE) #colnames=FALSE suppresses the variable names since these are already in the template, the US line in the data doesn't have a label field for the row so paste into column 2 instead of 1
writeData(wb, sheet = "W-29", x = rntcat_met_table, 
          startCol = 1, startRow = 8, colNames = FALSE)
writeData(wb, sheet = "W-29", x = sources, 
          startCol = 1, startRow = 110, colNames = FALSE)
saveWorkbook(wb, workbook_path, overwrite=TRUE)