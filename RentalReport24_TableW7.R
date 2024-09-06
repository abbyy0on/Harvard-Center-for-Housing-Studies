#Table W7: Housing Cost-Burdened Renters by Demographic Characteristics, 2001, 2019, 2022

# Load packages 
library(tidyverse)
library(data.table)
library(tidyquant)
library(pracma)
library(openxlsx)

setwd("C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\")

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"

multi_years <-c(2001) #years to select from multi file (2001:2014) if keeping all
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
multiyear <- fread(file.path(path, "ACS_multiyear_hhplus_RentersPlusVacant.csv")) %>% 
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
  filter(tensimp==2) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_all(function(x) paste0(x, "_2001"))

tot_19 <- df_2019 %>% 
  filter(tensimp==2) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_all(function(x) paste0(x, "_2019"))

tot_22 <- df_2022 %>% 
  filter(tensimp==2) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_all(function(x) paste0(x, "_2022"))

tot <- cbind(tot_mult, tot_19, tot_22)

#age
age_mult <- multiyear %>% 
  filter(tensimp==2) %>% 
  mutate(agecat = case_when(agep<25 ~ "1 under25",
                            agep>=25 & agep<45 ~ "2 25-44",
                            agep>=45 & agep<65 ~ "3 45-64",
                            agep>=65 ~ "4 65pl")) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(agecat, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-agecat), function(x) paste0(x, "_2001"))

age_19 <- df_2019 %>% 
  filter(tensimp==2) %>% 
  mutate(agecat = case_when(agep<25 ~ "1 under25",
                            agep>=25 & agep<45 ~ "2 25-44",
                            agep>=45 & agep<65 ~ "3 45-64",
                            agep>=65 ~ "4 65pl")) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(agecat, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-agecat), function(x) paste0(x, "_2019"))

age_22 <- df_2022 %>% 
  filter(tensimp==2) %>% 
  mutate(agecat = case_when(agep<25 ~ "1 under25",
                            agep>=25 & agep<45 ~ "2 25-44",
                            agep>=45 & agep<65 ~ "3 45-64",
                            agep>=65 ~ "4 65pl")) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(agecat, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-agecat), function(x) paste0(x, "_2022"))

age <- reduce(list(age_mult, age_19, age_22), left_join, by = "agecat")
age$agecat <- NULL

#race/ethnicity
race_mult <- multiyear %>% 
  filter(tensimp==2) %>% 
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
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(race5cat, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-race5cat), function(x) paste0(x, "_2001"))

race_19 <- df_2019 %>% 
  filter(tensimp==2) %>% 
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
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(race5cat, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-race5cat), function(x) paste0(x, "_2019"))

race_22 <- df_2022 %>% 
  filter(tensimp==2) %>% 
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
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(race5cat, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-race5cat), function(x) paste0(x, "_2022")) 

race <- reduce(list(race_mult, race_19, race_22), left_join, by = "race5cat")
race$race5cat <- NULL

#income
inc_mult <- multiyear %>% 
  filter(tensimp==2) %>% 
  mutate(hincp_infl = hincp*cpi_ai_current/cpi_ai_12mav,
         hh_inccat_infl = case_when(hincp_infl<15000 ~ "1 under15,000",
                                    hincp_infl>=15000 & hincp_infl<30000 ~ "2 15,000-29,999",
                                    hincp_infl>=30000 & hincp_infl<45000 ~ "3 30,000-44,999",
                                    hincp_infl>=45000 & hincp_infl<75000 ~ "4 45,000-74,999",
                                    hincp_infl>=75000 ~ "5 75,000pl")) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(hh_inccat_infl, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-hh_inccat_infl), function(x) paste0(x, "_2001"))

inc_19 <- df_2019 %>% 
  filter(tensimp==2) %>% 
  mutate(hincp_infl = hincp*cpi_ai_current/cpi_ai_12mav,
         hh_inccat_infl = case_when(hincp_infl<15000 ~ "1 under15,000",
                                    hincp_infl>=15000 & hincp_infl<30000 ~ "2 15,000-29,999",
                                    hincp_infl>=30000 & hincp_infl<45000 ~ "3 30,000-44,999",
                                    hincp_infl>=45000 & hincp_infl<75000 ~ "4 45,000-74,999",
                                    hincp_infl>=75000 ~ "5 75,000pl")) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(hh_inccat_infl, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-hh_inccat_infl), function(x) paste0(x, "_2019"))

inc_22 <- df_2022 %>% 
  filter(tensimp==2) %>% 
  mutate(hincp_infl = hincp*cpi_ai_current/cpi_ai_12mav,
         hh_inccat_infl = case_when(hincp_infl<15000 ~ "1 under15,000",
                                    hincp_infl>=15000 & hincp_infl<30000 ~ "2 15,000-29,999",
                                    hincp_infl>=30000 & hincp_infl<45000 ~ "3 30,000-44,999",
                                    hincp_infl>=45000 & hincp_infl<75000 ~ "4 45,000-74,999",
                                    hincp_infl>=75000 ~ "5 75,000pl")) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(hh_inccat_infl, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-hh_inccat_infl), function(x) paste0(x, "_2022"))

income <- reduce(list(inc_mult, inc_19, inc_22), left_join, by = "hh_inccat_infl")
income$hh_inccat_infl <- NULL

#education
ed_mult <- multiyear %>% 
  filter(tensimp==2) %>% 
  mutate(ed2cat = case_when(ed2cat==1 ~ "1 no hs diploma",
                            ed2cat==2 ~ "2 hs_ged",
                            ed2cat==3 ~ "3 some college",
                            ed2cat==4 ~ "4 bach_higher")) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(ed2cat, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-ed2cat), function(x) paste0(x, "_2001"))

ed_19 <- df_2019 %>% 
  filter(tensimp==2) %>% 
  mutate(ed2cat = case_when(ed2cat==1 ~ "1 no hs diploma",
                            ed2cat==2 ~ "2 hs_ged",
                            ed2cat==3 ~ "3 some college",
                            ed2cat==4 ~ "4 bach_higher")) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(ed2cat, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-ed2cat), function(x) paste0(x, "_2019"))

ed_22 <- df_2022 %>% 
  filter(tensimp==2) %>% 
  mutate(ed2cat = case_when(ed2cat==1 ~ "1 no hs diploma",
                            ed2cat==2 ~ "2 hs_ged",
                            ed2cat==3 ~ "3 some college",
                            ed2cat==4 ~ "4 bach_higher")) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(ed2cat, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-ed2cat), function(x) paste0(x, "_2022"))

education <- reduce(list(ed_mult, ed_19, ed_22), left_join, by = "ed2cat")
education$ed2cat <- NULL

#employment
emp_mult <- multiyear %>% 
  filter(tensimp==2) %>% 
  mutate(emp12 = case_when(emp12==1 ~ "1 full emp",
                           emp12==2 ~ "2 st unemp",
                           emp12==3 ~ "3 lt unemp",
                           emp12==4 ~ "4 full unemp",
                           is.na(emp12) ~ "5 not in labor force")) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(emp12, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-emp12), function(x) paste0(x, "_2001"))

emp_19 <- df_2019 %>% 
  filter(tensimp==2) %>% 
  mutate(emp12 = case_when(emp12==1 ~ "1 full emp",
                           emp12==2 ~ "2 st unemp",
                           emp12==3 ~ "3 lt unemp",
                           emp12==4 ~ "4 full unemp",
                           is.na(emp12) ~ "5 not in labor force")) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(emp12, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-emp12), function(x) paste0(x, "_2019"))

emp_22 <- df_2022 %>% 
  filter(tensimp==2) %>% 
  mutate(emp12 = case_when(emp12==1 ~ "1 full emp",
                           emp12==2 ~ "2 st unemp",
                           emp12==3 ~ "3 lt unemp",
                           emp12==4 ~ "4 full unemp",
                           is.na(emp12) ~ "5 not in labor force")) %>% 
  mutate_at(vars(c(cost_burden)), list(~as.character(.))) %>% #preserves row for all hhs
  bind_rows(mutate(., cost_burden = "All Renters")) %>%
  group_by(emp12, cost_burden) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  filter(cost_burden!="1") %>% 
  pivot_wider(names_from=cost_burden, values_from=tot) %>% 
  rename_at(vars(-emp12), function(x) paste0(x, "_2022"))

employment <- reduce(list(emp_mult, emp_19, emp_22), left_join, by = "emp12")
employment$emp12 <- NULL


## export to excel ---------------------------

title <- "Table W-7. US National: Housing Cost-Burdened Renters by Demographic Characteristics: 2001, 2019, and 2022"  
sources <- "Source: JCHS tabulation of US Census Bureau, American Community Survey 1-Year Estimates."

wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-7", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE) 
writeData(wb, sheet = "W-7", x = tot, 
          startCol = 2, startRow = 7, colNames = FALSE) 
writeData(wb, sheet = "W-7", x = age, 
          startCol = 2, startRow = 9, colNames = FALSE) 
writeData(wb, sheet = "W-7", x = race, 
          startCol = 2, startRow = 14, colNames = FALSE) 
writeData(wb, sheet = "W-7", x = income, 
          startCol = 2, startRow = 20, colNames = FALSE) 
writeData(wb, sheet = "W-7", x = education, 
          startCol = 2, startRow = 26, colNames = FALSE) 
writeData(wb, sheet = "W-7", x = employment, 
          startCol = 2, startRow = 31, colNames = FALSE) 
writeData(wb, sheet = "W-7", x = sources, 
          startCol = 1, startRow = 43, colNames = FALSE)

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy.xlsx", overwrite=TRUE)

