#Table W3: Rentership Rates by Household Characteristics, 2010-2022

# Load packages 
library(tidyverse)
library(data.table)
library(tidyquant)
library(pracma)
library(openxlsx)


setwd("C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\")

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"

multi_years <-c(2010) #years to select from multi file (2001:2014) if keeping all
yearly_years <- c(2019, 2022) #used for recent years not in multi file
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

#read files, merge on cpi
multiyear <- fread(file.path(path, "ACS_multiyear_hhplus_full.csv")) %>% 
  filter(year==multi_years) %>% 
  left_join(cpi, by="year")

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
tot_mult <- multiyear %>% 
  filter(tensimp!="") %>% 
  group_by(tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  ungroup() %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp=="Rent") %>% 
  select(-tensimp, -tot)

tot_19 <- df_2019 %>% 
  filter(!is.na(tensimp)) %>% 
  group_by(tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  ungroup() %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp==2) %>% 
  select(-tensimp, -tot)

tot_22 <- df_2022 %>% 
  filter(!is.na(tensimp)) %>% 
  group_by(tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  ungroup() %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp==2) %>% 
  select(-tensimp, -tot)

tot <- cbind(tot_mult, tot_19, tot_22)

#age
age_mult <- multiyear %>% 
  filter(tensimp!="") %>% 
  mutate(agecat = case_when(agep < 25 ~ "1 Under 25",
                         agep >= 25 & agep <=34 ~ "2 25-34",
                         agep >= 35 & agep <=44 ~ "3 35-44",
                         agep >= 45 & agep <=54 ~ "4 45-54",
                         agep >= 55 & agep <=64 ~ "5 55-64",
                         agep >= 65 & agep <=74 ~ "6 65-74",
                         agep >= 75 ~ "7 75+")) %>% 
  group_by(agecat, tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(agecat) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp=="Rent") %>% 
  select(-tensimp, -tot)

age_65pl_mult <- multiyear %>% 
  filter(tensimp!="" & agep>=65) %>% 
  group_by(tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  ungroup() %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp=="Rent") %>% 
  select(-tensimp, -tot) %>% 
  mutate(agecat = "65pl") %>% 
  select(agecat, sh)

age_mult_complete <- rbind(age_mult, age_65pl_mult) %>% 
  rename_at(vars(-agecat), function(x) paste0(x, "_2010"))

age_19 <- df_2019 %>% 
  filter(!is.na(tensimp)) %>% 
  mutate(agecat = case_when(agep < 25 ~ "1 Under 25",
                            agep >= 25 & agep <=34 ~ "2 25-34",
                            agep >= 35 & agep <=44 ~ "3 35-44",
                            agep >= 45 & agep <=54 ~ "4 45-54",
                            agep >= 55 & agep <=64 ~ "5 55-64",
                            agep >= 65 & agep <=74 ~ "6 65-74",
                            agep >= 75 ~ "7 75+")) %>% 
  group_by(agecat, tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(agecat) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp==2) %>% 
  select(-tensimp, -tot)

age_65pl_19 <- df_2019 %>% 
  filter(!is.na(tensimp) & agep>=65) %>% 
  group_by(tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  ungroup() %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp==2) %>% 
  select(-tensimp, -tot) %>% 
  mutate(agecat = "65pl") %>% 
  select(agecat, sh)

age_19_complete <- rbind(age_19, age_65pl_19) %>% 
  rename_at(vars(-agecat), function(x) paste0(x, "_2019"))


age_22 <- df_2022 %>% 
  filter(!is.na(tensimp)) %>% 
  mutate(agecat = case_when(agep < 25 ~ "1 Under 25",
                            agep >= 25 & agep <=34 ~ "2 25-34",
                            agep >= 35 & agep <=44 ~ "3 35-44",
                            agep >= 45 & agep <=54 ~ "4 45-54",
                            agep >= 55 & agep <=64 ~ "5 55-64",
                            agep >= 65 & agep <=74 ~ "6 65-74",
                            agep >= 75 ~ "7 75+")) %>% 
  group_by(agecat, tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(agecat) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp==2) %>% 
  select(-tensimp, -tot)

age_65pl_22 <- df_2022 %>% 
  filter(!is.na(tensimp) & agep>=65) %>% 
  group_by(tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  ungroup() %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp==2) %>% 
  select(-tensimp, -tot) %>% 
  mutate(agecat = "65pl") %>% 
  select(agecat, sh)

age_22_complete <- rbind(age_22, age_65pl_22) %>% 
  rename_at(vars(-agecat), function(x) paste0(x, "_2022"))

age <- reduce(list(age_mult_complete, age_19_complete, age_22_complete), left_join, by = "agecat")
age$agecat <- NULL

#race/ethnicity
race_mult <- multiyear %>% 
  filter(tensimp!="") %>% 
  mutate(race5cat = case_when(rac1p == 1 & hisp == 1 ~ 1,
                              rac1p == 2 & hisp == 1 ~ 2,
                              hisp > 1 ~ 3,
                              rac1p == 6 & hisp == 1 ~ 4,
                              (rac1p == 3 | rac1p == 4 | rac1p == 5 | rac1p == 7 | rac1p == 8 | rac1p == 9) & hisp == 1 ~ 5),
         race5cat = case_when(race5cat == 1 ~ "1 White",
                              race5cat == 2 ~ "2 Black",
                              race5cat == 3 ~ "3 Hispanic",
                              race5cat == 4 ~ "4 Asian",
                              race5cat == 5 ~ "5 Mult_Oth")) %>% 
  group_by(race5cat, tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(race5cat) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp=="Rent") %>% 
  select(-tensimp, -tot) %>% 
  rename_at(vars(-race5cat), function(x) paste0(x, "_2010"))

race_19 <- df_2019 %>% 
  filter(!is.na(tensimp)) %>% 
  mutate(race5cat = case_when(rac1p == 1 & hisp == 1 ~ 1,
                              rac1p == 2 & hisp == 1 ~ 2,
                              hisp > 1 ~ 3,
                              rac1p == 6 & hisp == 1 ~ 4,
                              (rac1p == 3 | rac1p == 4 | rac1p == 5 | rac1p == 7 | rac1p == 8 | rac1p == 9) & hisp == 1 ~ 5),
         race5cat = case_when(race5cat == 1 ~ "1 White",
                              race5cat == 2 ~ "2 Black",
                              race5cat == 3 ~ "3 Hispanic",
                              race5cat == 4 ~ "4 Asian",
                              race5cat == 5 ~ "5 Mult_Oth")) %>% 
  group_by(race5cat, tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(race5cat) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp==2) %>% 
  select(-tensimp, -tot) %>% 
  rename_at(vars(-race5cat), function(x) paste0(x, "_2019"))

race_22 <- df_2022 %>% 
  filter(!is.na(tensimp)) %>% 
  mutate(race5cat = case_when(rac1p == 1 & hisp == 1 ~ 1,
                              rac1p == 2 & hisp == 1 ~ 2,
                              hisp > 1 ~ 3,
                              rac1p == 6 & hisp == 1 ~ 4,
                              (rac1p == 3 | rac1p == 4 | rac1p == 5 | rac1p == 7 | rac1p == 8 | rac1p == 9) & hisp == 1 ~ 5),
         race5cat = case_when(race5cat == 1 ~ "1 White",
                              race5cat == 2 ~ "2 Black",
                              race5cat == 3 ~ "3 Hispanic",
                              race5cat == 4 ~ "4 Asian",
                              race5cat == 5 ~ "5 Mult_Oth")) %>% 
  group_by(race5cat, tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(race5cat) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp==2) %>% 
  select(-tensimp, -tot) %>% 
  rename_at(vars(-race5cat), function(x) paste0(x, "_2022"))

race <- reduce(list(race_mult, race_19, race_22), left_join, by = "race5cat")
race$race5cat <- NULL

#nativity
nat_mult <- multiyear %>% 
  filter(tensimp!="" & nativity!="") %>% 
  mutate(nativity = if_else(nativity=="Native born", 1, 2)) %>% 
  group_by(nativity, tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(nativity) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp=="Rent") %>% 
  select(-tensimp, -tot) %>% 
  rename_at(vars(-nativity), function(x) paste0(x, "_2010"))

nat_19 <- df_2019 %>% 
  filter(!is.na(tensimp)) %>% 
  group_by(nativity, tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(nativity) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp==2) %>% 
  select(-tensimp, -tot) %>% 
  rename_at(vars(-nativity), function(x) paste0(x, "_2019"))

nat_22 <- df_2022 %>% 
  filter(!is.na(tensimp)) %>% 
  group_by(nativity, tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(nativity) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp==2) %>% 
  select(-tensimp, -tot) %>% 
  rename_at(vars(-nativity), function(x) paste0(x, "_2022"))

nativity <- reduce(list(nat_mult, nat_19, nat_22), left_join, by = "nativity")
nativity$nativity <- NULL

#income
inc_mult <- multiyear %>% 
  filter(tensimp!="") %>% 
  mutate(hincp_infl = hincp*cpi_ai_current/cpi_ai_12mav,
         hh_inccat_infl = case_when(hincp_infl<15000 ~ "1 under15,000",
                                    hincp_infl>=15000 & hincp_infl<30000 ~ "2 15,000-29,999",
                                    hincp_infl>=30000 & hincp_infl<45000 ~ "3 30,000-44,999",
                                    hincp_infl>=45000 & hincp_infl<75000 ~ "4 45,000-74,999",
                                    hincp_infl>=75000 ~ "5 75,000pl")) %>% 
  group_by(hh_inccat_infl, tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(hh_inccat_infl) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp=="Rent") %>% 
  select(-tensimp, -tot) %>% 
  rename_at(vars(-hh_inccat_infl), function(x) paste0(x, "_2010"))

inc_19 <- df_2019 %>% 
  filter(!is.na(tensimp)) %>% 
  mutate(hincp_infl = hincp*cpi_ai_current/cpi_ai_12mav,
         hh_inccat_infl = case_when(hincp_infl<15000 ~ "1 under15,000",
                                    hincp_infl>=15000 & hincp_infl<30000 ~ "2 15,000-29,999",
                                    hincp_infl>=30000 & hincp_infl<45000 ~ "3 30,000-44,999",
                                    hincp_infl>=45000 & hincp_infl<75000 ~ "4 45,000-74,999",
                                    hincp_infl>=75000 ~ "5 75,000pl")) %>% 
  group_by(hh_inccat_infl, tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(hh_inccat_infl) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp==2) %>% 
  select(-tensimp, -tot) %>% 
  rename_at(vars(-hh_inccat_infl), function(x) paste0(x, "_2019"))

inc_22 <- df_2022 %>% 
  filter(!is.na(tensimp)) %>% 
  mutate(hincp_infl = hincp*cpi_ai_current/cpi_ai_12mav,
         hh_inccat_infl = case_when(hincp_infl<15000 ~ "1 under15,000",
                                    hincp_infl>=15000 & hincp_infl<30000 ~ "2 15,000-29,999",
                                    hincp_infl>=30000 & hincp_infl<45000 ~ "3 30,000-44,999",
                                    hincp_infl>=45000 & hincp_infl<75000 ~ "4 45,000-74,999",
                                    hincp_infl>=75000 ~ "5 75,000pl")) %>% 
  group_by(hh_inccat_infl, tensimp) %>% 
  summarise(tot=sum(wgtp)) %>% 
  group_by(hh_inccat_infl) %>% 
  mutate(sh=tot/sum(tot)*100) %>% 
  filter(tensimp==2) %>% 
  select(-tensimp, -tot) %>% 
  rename_at(vars(-hh_inccat_infl), function(x) paste0(x, "_2022"))

income <- reduce(list(inc_mult, inc_19, inc_22), left_join, by = "hh_inccat_infl")
income$hh_inccat_infl <- NULL



## export to excel ---------------------------

title <- "Table W-3. US National: Rentership Rates by Household Characteristics, 2010-2022"  
sources <- "Source: JCHS tabulation of US Census Bureau, American Community Survey 1-Year Estimates."

wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-3", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE) 
writeData(wb, sheet = "W-3", x = tot, 
          startCol = 2, startRow = 7, colNames = FALSE) 
writeData(wb, sheet = "W-3", x = age, 
          startCol = 2, startRow = 9, colNames = FALSE) 
writeData(wb, sheet = "W-3", x = race, 
          startCol = 2, startRow = 18, colNames = FALSE) 
writeData(wb, sheet = "W-3", x = nativity, 
          startCol = 2, startRow = 24, colNames = FALSE) 
writeData(wb, sheet = "W-3", x = income, 
          startCol = 2, startRow = 27, colNames = FALSE) 
writeData(wb, sheet = "W-3", x = sources, 
          startCol = 1, startRow = 34, colNames = FALSE)

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy.xlsx", overwrite=TRUE)