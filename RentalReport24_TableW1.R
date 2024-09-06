#Table W1: Characteristics of Growth in Renter Households, 2016-2022

# Load packages 
library(tidyverse)
library(data.table)
library(tidyquant)
library(pracma)
library(openxlsx)


setwd("C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\")

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"

yearly_years <- c(2016, 2019, 2022) #used for recent years not in multi file
currentyear <- 2022

#cpi for adjusting income
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
cpi_ai_current <- cpi$cpi_ai_12mav[cpi$year==currentyear]

# yearly files
for(x in yearly_years) {
  inputfile <- paste0("ACS_", x, "_hhplus.csv")
  df <- fread(file.path(path, inputfile)) %>% 
    left_join(cpi, by="year")
  dfyr <- paste0("df_", x)
  assign(dfyr, df)
  rm(df)
}


#table components------------------------

#totals
tot_16<-df_2016 %>% 
  filter(tensimp==2) %>% 
  summarise(tot = sum(wgtp)/1000) %>% 
  rename_all(function(x) paste0(x, "_2016"))

tot_19 <- df_2019 %>% 
  filter(tensimp==2) %>% 
  summarise(tot = sum(wgtp)/1000) %>% 
  rename_all(function(x) paste0(x, "_2019"))

tot_22 <- df_2022 %>% 
  filter(tensimp==2) %>% 
  summarise(tot = sum(wgtp)/1000) %>% 
  rename_all(function(x) paste0(x, "_2022"))

tot <- cbind(tot_16, tot_19, tot_22) %>% 
  mutate(ch1619_num = tot_2019-tot_2016,
         ch1619_pct = (tot_2019-tot_2016)/tot_2016*100,
         ch1922_num = tot_2022-tot_2019,
         ch1922_pct = (tot_2022-tot_2019)/tot_2019*100)

#income
inc_16 <- df_2016 %>% 
  filter(tensimp==2) %>% 
  mutate(hincp_infl = hincp*cpi_ai_current/cpi_ai_12mav,
         hh_inccat_infl = case_when(hincp_infl<15000 ~ "1 under15,000",
                                    hincp_infl>=15000 & hincp_infl<30000 ~ "2 15,000-29,999",
                                    hincp_infl>=30000 & hincp_infl<45000 ~ "3 30,000-44,999",
                                    hincp_infl>=45000 & hincp_infl<75000 ~ "4 45,000-74,999",
                                    hincp_infl>=75000 ~ "5 75,000pl")) %>% 
  group_by(hh_inccat_infl) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  rename_at(vars(-hh_inccat_infl), function(x) paste0(x, "_2016"))

inc_19 <- df_2019 %>% 
  filter(tensimp==2) %>% 
  mutate(hincp_infl = hincp*cpi_ai_current/cpi_ai_12mav,
         hh_inccat_infl = case_when(hincp_infl<15000 ~ "1 under15,000",
                                    hincp_infl>=15000 & hincp_infl<30000 ~ "2 15,000-29,999",
                                    hincp_infl>=30000 & hincp_infl<45000 ~ "3 30,000-44,999",
                                    hincp_infl>=45000 & hincp_infl<75000 ~ "4 45,000-74,999",
                                    hincp_infl>=75000 ~ "5 75,000pl")) %>% 
  group_by(hh_inccat_infl) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  rename_at(vars(-hh_inccat_infl), function(x) paste0(x, "_2019"))

inc_22 <- df_2022 %>% 
  filter(tensimp==2) %>% 
  mutate(hincp_infl = hincp*cpi_ai_current/cpi_ai_12mav,
         hh_inccat_infl = case_when(hincp_infl<15000 ~ "1 under15,000",
                                    hincp_infl>=15000 & hincp_infl<30000 ~ "2 15,000-29,999",
                                    hincp_infl>=30000 & hincp_infl<45000 ~ "3 30,000-44,999",
                                    hincp_infl>=45000 & hincp_infl<75000 ~ "4 45,000-74,999",
                                    hincp_infl>=75000 ~ "5 75,000pl")) %>% 
  group_by(hh_inccat_infl) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  rename_at(vars(-hh_inccat_infl), function(x) paste0(x, "_2022"))

income <- reduce(list(inc_16, inc_19, inc_22), left_join, by = "hh_inccat_infl") %>% 
          mutate(ch1619_num = tot_2019-tot_2016,
                 ch1619_pct = (tot_2019-tot_2016)/tot_2016*100,
                 ch1922_num = tot_2022-tot_2019,
                 ch1922_pct = (tot_2022-tot_2019)/tot_2019*100)
income$hh_inccat_infl <- NULL


#age
age_16 <- df_2016 %>% 
  filter(tensimp==2) %>% 
  mutate(agecat = case_when(agep < 25 ~ "1 Under 25",
                            agep >= 25 & agep <=34 ~ "2 25-34",
                            agep >= 35 & agep <=44 ~ "3 35-44",
                            agep >= 45 & agep <=54 ~ "4 45-54",
                            agep >= 55 & agep <=64 ~ "5 55-64",
                            agep >= 65 & agep <=74 ~ "6 65-74",
                            agep >= 75 ~ "7 75+")) %>% 
  group_by(agecat) %>% 
  summarise(tot=sum(wgtp)/1000)  %>% 
  rename_at(vars(-agecat), function(x) paste0(x, "_2016"))


age_19 <- df_2019 %>% 
  filter(tensimp==2) %>% 
  mutate(agecat = case_when(agep < 25 ~ "1 Under 25",
                            agep >= 25 & agep <=34 ~ "2 25-34",
                            agep >= 35 & agep <=44 ~ "3 35-44",
                            agep >= 45 & agep <=54 ~ "4 45-54",
                            agep >= 55 & agep <=64 ~ "5 55-64",
                            agep >= 65 & agep <=74 ~ "6 65-74",
                            agep >= 75 ~ "7 75+")) %>% 
  group_by(agecat) %>% 
  summarise(tot=sum(wgtp)/1000)  %>% 
  rename_at(vars(-agecat), function(x) paste0(x, "_2019"))


age_22 <- df_2022 %>% 
  filter(tensimp==2) %>% 
  mutate(agecat = case_when(agep < 25 ~ "1 Under 25",
                            agep >= 25 & agep <=34 ~ "2 25-34",
                            agep >= 35 & agep <=44 ~ "3 35-44",
                            agep >= 45 & agep <=54 ~ "4 45-54",
                            agep >= 55 & agep <=64 ~ "5 55-64",
                            agep >= 65 & agep <=74 ~ "6 65-74",
                            agep >= 75 ~ "7 75+")) %>% 
  group_by(agecat) %>% 
  summarise(tot=sum(wgtp)/1000)  %>% 
  rename_at(vars(-agecat), function(x) paste0(x, "_2022"))

age <- reduce(list(age_16, age_19, age_22), left_join, by = "agecat") %>% 
        mutate(ch1619_num = tot_2019-tot_2016,
               ch1619_pct = (tot_2019-tot_2016)/tot_2016*100,
               ch1922_num = tot_2022-tot_2019,
               ch1922_pct = (tot_2022-tot_2019)/tot_2019*100)
age$agecat <- NULL

#household size
hh_16 <- df_2016 %>% 
  filter(tensimp==2) %>% 
  mutate(np= if_else(np>3, 3, np)) %>% 
  group_by(np) %>% 
  summarise(tot=sum(wgtp)/1000)  %>% 
  rename_at(vars(-np), function(x) paste0(x, "_2016"))

hh_19 <- df_2019 %>% 
  filter(tensimp==2) %>% 
  mutate(np= if_else(np>3, 3, np)) %>% 
  group_by(np) %>% 
  summarise(tot=sum(wgtp)/1000)  %>% 
  rename_at(vars(-np), function(x) paste0(x, "_2019"))

hh_22 <- df_2022 %>% 
  filter(tensimp==2) %>% 
  mutate(np= if_else(np>3, 3, np)) %>% 
  group_by(np) %>% 
  summarise(tot=sum(wgtp)/1000)  %>% 
  rename_at(vars(-np), function(x) paste0(x, "_2022"))

hh <- reduce(list(hh_16, hh_19, hh_22), left_join, by = "np") %>% 
  mutate(ch1619_num = tot_2019-tot_2016,
         ch1619_pct = (tot_2019-tot_2016)/tot_2016*100,
         ch1922_num = tot_2022-tot_2019,
         ch1922_pct = (tot_2022-tot_2019)/tot_2019*100)
hh$np<-NULL

#household type
hht_16 <- df_2016 %>% 
  filter(tensimp==2) %>% 
  mutate(hhtype = case_when(hhtype==1 ~ "1 married_wo_kids",
                            hhtype==2 ~ "2 married_w_kids",
                            hhtype==3 ~ "3 single_parent",
                            hhtype==4 ~ "4 other_fam",
                            hhtype==5 ~ "5 single_person",
                            hhtype==6 ~ "6 other_nonfam")) %>% 
  group_by(hhtype) %>% 
  summarise(tot=sum(wgtp)/1000)  %>% 
  rename_at(vars(-hhtype), function(x) paste0(x, "_2016"))

hht_19 <- df_2019 %>% 
  filter(tensimp==2) %>% 
  mutate(hhtype = case_when(hhtype==1 ~ "1 married_wo_kids",
                            hhtype==2 ~ "2 married_w_kids",
                            hhtype==3 ~ "3 single_parent",
                            hhtype==4 ~ "4 other_fam",
                            hhtype==5 ~ "5 single_person",
                            hhtype==6 ~ "6 other_nonfam")) %>% 
  group_by(hhtype) %>% 
  summarise(tot=sum(wgtp)/1000)  %>% 
  rename_at(vars(-hhtype), function(x) paste0(x, "_2019"))

hht_22 <- df_2022 %>% 
  filter(tensimp==2) %>% 
  mutate(hhtype = case_when(hhtype==1 ~ "1 married_wo_kids",
                            hhtype==2 ~ "2 married_w_kids",
                            hhtype==3 ~ "3 single_parent",
                            hhtype==4 ~ "4 other_fam",
                            hhtype==5 ~ "5 single_person",
                            hhtype==6 ~ "6 other_nonfam")) %>% 
  group_by(hhtype) %>% 
  summarise(tot=sum(wgtp)/1000)  %>% 
  rename_at(vars(-hhtype), function(x) paste0(x, "_2022"))

hht <- reduce(list(hht_16, hht_19, hht_22), left_join, by = "hhtype") %>% 
  mutate(ch1619_num = tot_2019-tot_2016,
         ch1619_pct = (tot_2019-tot_2016)/tot_2016*100,
         ch1922_num = tot_2022-tot_2019,
         ch1922_pct = (tot_2022-tot_2019)/tot_2019*100)
hht$hhtype<-NULL


#nativity
nat_16 <- df_2016 %>% 
  filter(tensimp==2) %>% 
  mutate(nativity = if_else(nativity==1, "1 Native Born", "2 Foreign Born")) %>% 
  group_by(nativity) %>% 
  summarise(tot=sum(wgtp)/1000)  %>% 
  rename_at(vars(-nativity), function(x) paste0(x, "_2016"))

nat_19 <- df_2019 %>% 
  filter(tensimp==2) %>% 
  mutate(nativity = if_else(nativity==1, "1 Native Born", "2 Foreign Born")) %>% 
  group_by(nativity) %>% 
  summarise(tot=sum(wgtp)/1000)  %>% 
  rename_at(vars(-nativity), function(x) paste0(x, "_2019"))

nat_22 <- df_2022 %>% 
  filter(tensimp==2) %>% 
  mutate(nativity = if_else(nativity==1, "1 Native Born", "2 Foreign Born")) %>% 
  group_by(nativity) %>% 
  summarise(tot=sum(wgtp)/1000)  %>% 
  rename_at(vars(-nativity), function(x) paste0(x, "_2022"))

nativity <- reduce(list(nat_16, nat_19, nat_22), left_join, by = "nativity") %>% 
  mutate(ch1619_num = tot_2019-tot_2016,
         ch1619_pct = (tot_2019-tot_2016)/tot_2016*100,
         ch1922_num = tot_2022-tot_2019,
         ch1922_pct = (tot_2022-tot_2019)/tot_2019*100)
nativity$nativity <- NULL


## export to excel ---------------------------

title <- "Table W-1. US National: Characteristics of Growth in Renter Households, 2016–2019, 2019–2022"  
sources <- "Source: JCHS tabulation of US Census Bureau, American Community Survey 1-Year Estimates."

wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-1", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE) 
writeData(wb, sheet = "W-1", x = tot, 
          startCol = 2, startRow = 7, colNames = FALSE) 
writeData(wb, sheet = "W-1", x = income, 
          startCol = 2, startRow = 9, colNames = FALSE) 
writeData(wb, sheet = "W-1", x = age, 
          startCol = 2, startRow = 15, colNames = FALSE) 
writeData(wb, sheet = "W-1", x = hh, 
          startCol = 2, startRow = 23, colNames = FALSE) 
writeData(wb, sheet = "W-1", x = hht, 
          startCol = 2, startRow = 27, colNames = FALSE) 
writeData(wb, sheet = "W-1", x = nativity, 
          startCol = 2, startRow = 34, colNames = FALSE) 
writeData(wb, sheet = "W-1", x = sources, 
          startCol = 1, startRow = 38, colNames = FALSE)

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy.xlsx", overwrite=TRUE)