#Table W21: Change in Rental Stock by Structure Type: 2010-2022

# Load packages 
library(tidyverse)
library(data.table)
library(openxlsx)

setwd("C:\\Users\\wla131\\Harvard University\\JCHS - Documents\\Rental Reports\\Rental Report 2024\\Collateral\\Appendix Tables\\")

path <- "C:\\Users\\wla131\\Harvard University\\SON rental chapter - Documents\\2022 Rental\\Data Sources\\ACS\\"

multi_years <-c(2012) #years to select from multi file (2001:2014) if keeping all
yearly_years <- c(2019, 2022) #used for recent years not in multi file
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

states_fips_codes <- fread(file.path(path, "State FIPS codes.csv"))

multiyear <- fread(file.path(path, "ACS_multiyear_hhplus_RentersPlusVacant.csv")) %>%
  filter(year==multi_years) %>% 
  left_join(states_fips_codes, by=c("st" = "FIPS Code")) %>% 
  left_join(cpi, by="year") %>% 
  mutate(rntp_infl = rntp*cpi_ls_current/cpi_ls_12mav,
         rntp_infl_cat = case_when(rntp_infl<600 ~ "1 under600",
                                   rntp_infl>=600 & rntp_infl<800 ~ "2 600-799",
                                   rntp_infl>=800 & rntp_infl<1000 ~ "3 800-999",
                                   rntp_infl>=1000 & rntp_infl<1400 ~ "4 1000-1399",
                                   rntp_infl>=1400 & rntp_infl<2000 ~ "5 1400-1999",
                                   rntp_infl>=2000 ~ "6 2000pl"))

# yearly files
for(x in yearly_years) {
  inputfile <- paste0("ACS_", x, "_hhplus.csv")
  df <- fread(file.path(path, inputfile)) %>% 
    left_join(states_fips_codes, by = c("st" = "FIPS Code")) %>% 
    left_join(cpi, by="year") %>% 
    mutate(rntp_infl = rntp*cpi_ls_current/cpi_ls_12mav,
           rntp_infl_cat = case_when(rntp_infl<600 ~ "1 under600",
                                     rntp_infl>=600 & rntp_infl<800 ~ "2 600-799",
                                     rntp_infl>=800 & rntp_infl<1000 ~ "3 800-999",
                                     rntp_infl>=1000 & rntp_infl<1400 ~ "4 1000-1399",
                                     rntp_infl>=1400 & rntp_infl<2000 ~ "5 1400-1999",
                                     rntp_infl>=2000 ~ "6 2000pl"))
  dfyr <- paste0("df_", x)
  assign(dfyr, df)
  rm(df)
}


#table components - number of rental units by structure type------------------------

#years from multiyear file
rentdist_mult <- multiyear %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  filter((tensimp==2|vacs==1|vacs==2) & !is.na(rntp)) %>% 
  group_by(year, `State Name`, rntp_infl_cat) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  pivot_wider(names_from = rntp_infl_cat, values_from = tot)  %>%
  rename_at(vars(-c(year, `State Name`)),function(x) paste0(x, "_2012")) %>% 
  ungroup() %>% 
  select(-year)

#years from annual files
rentdist_19 <- df_2019 %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  filter((tensimp==2|vacs==1|vacs==2) & !is.na(rntp)) %>% 
  group_by(year, `State Name`, rntp_infl_cat) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  pivot_wider(names_from = rntp_infl_cat, values_from = tot)  %>%
  rename_at(vars(-c(year, `State Name`)),function(x) paste0(x, "_2019")) %>% 
  ungroup() %>% 
  select(-year)

rentdist_22 <- df_2022 %>% 
  bind_rows(mutate(., `State Name` = "United States")) %>%
  filter((tensimp==2|vacs==1|vacs==2) & !is.na(rntp)) %>% 
  group_by(year, `State Name`, rntp_infl_cat) %>% 
  summarise(tot=sum(wgtp)/1000) %>% 
  pivot_wider(names_from = rntp_infl_cat, values_from = tot)  %>%
  rename_at(vars(-c(year, `State Name`)),function(x) paste0(x, "_2022")) %>% 
  ungroup() %>% 
  select(-year)


# combine all years ------------------------
full_table <- reduce(list(rentdist_mult, rentdist_19, rentdist_22), left_join, by = "State Name")  %>% 
  arrange(`State Name`!="United States") %>%
  mutate("1 under600 change" = `1 under600_2022` - `1 under600_2012`,
         "2 600-799 change" = `2 600-799_2022` - `2 600-799_2012`,
         "3 80-999 change" = `3 800-999_2022` - `3 800-999_2012`,
         "4 1000-1399 change" = `4 1000-1399_2022` - `4 1000-1399_2012`,
         "5 1400-1999 change" = `5 1400-1999_2022` - `5 1400-1999_2012`,
         "6 2000pl change" = `6 2000pl_2022` - `6 2000pl_2012`)


## export to excel ---------------------------

title <- "Table W-18. US States: Number of Rental Units by Monthly Contract Rent, 2012, 2019, 2022"  
sources <- "Source: JCHS tabulation of US Census Bureau, American Community Survey 1-Year Estimates."

wb <- loadWorkbook("RentalReport24_AppendixTables_WAO working copy.xlsx") #loads in an existing workbook with template pasted in to a sheet called "RenterCharacteristicsByIncome"

writeData(wb, sheet = "W-18", x = title, 
          startCol = 1, startRow = 1, colNames = FALSE) 
writeData(wb, sheet = "W-18", x = full_table, 
          startCol = 1, startRow = 7, colNames = FALSE)  
writeData(wb, sheet = "W-18", x = sources, 
          startCol = 1, startRow = 61, colNames = FALSE)

saveWorkbook(wb, "RentalReport24_AppendixTables_WAO working copy.xlsx", overwrite=TRUE)



