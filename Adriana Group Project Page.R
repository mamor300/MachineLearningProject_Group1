
library(pacman)
pacman::p_load(readr, tidyverse, VIM, caret)
library(tidyverse)

CFPB0 <- read_csv("sample26.01.csv")
ZIPCODES <- read_csv("zip_fips.csv")

# Recoding ZIP and FIPS
ZIPCODES <- ZIPCODES %>%
  mutate(FIPS = str_pad(STCOUNTYFP,5,"left",pad="0"),
         ZIP  = str_pad(ZIP,5,"left",pad="0"))

## recode target variable
CFPB0$Relief<- ifelse(CFPB0$Company.response.to.consumer %in% 
                        c("Closed with monetary relief",
                          "Closed with relief",
                          "Closed with non-monetary relief"),
                      1,0)
CFPB0 <- CFPB0[,-c(1)]%>%
  select(Relief,Date.received, Date.sent.to.company,everything())

# Dropping NAs
## 134 rows have NA values across 12 variables in original data
CFPB1 <-CFPB0 %>%
  drop_na(Date.sent.to.company)%>%
  filter(!State %in% c("NONE", "None", "DC", "AA","AE", "AP", "AS", "FM","GU", "MH", "MP", "PR", "VI", "UNITED STATES MINOR OUTLYING ISLANDS"))

# 2 - Imputing zip codes with means
## Each means-imputed value uses the average among the first 3 digits of 
##non-missing "siblings". This produces 95 NAs for zip codes 
##that do not have any non-missing siblings, creating a branch of 
##the CFPB data that prepares the missing zips for imputation
set.seed(03012026)
df <- CFPB1 %>%
  mutate(ZIP.char    = as.character(ZIP.code),
         prefix3     = substr(ZIP.char, 1, 3),
         ZIP.missing = ifelse(nchar(ZIP.code) < 5 | grepl("X$", ZIP.code), 1, 0))

# Creating a dataframe of the valid ZIP codes
## A numeric vector helps calculate the means
## Character vector manages the prefixes code.
valid_zipz<-ZIPCODES %>% 
  mutate(ZIP.char = formatC(as.numeric(ZIP), width = 5, flag = '0', format = 'd'), 
         prefix3 = substr(ZIP.char, 1, 3), 
         ZIP.num = as.numeric(ZIP.char)) %>% 
  dplyr::select(prefix3, ZIP.char, ZIP.num)

#Computing means for each 3 digit prefix
ZIP.means<- df %>% 
  filter(ZIP.missing == 0, nchar(trimws(ZIP.char)) == 5) %>% 
  group_by(prefix3) %>% 
  summarise(ZIP.mean.num = round(mean(as.numeric(ZIP.char), na.rm = TRUE)))

#Identifying the nearesr valid zip for each mean value
##ZIP.imp is 
ZIP.impute<- ZIP.means %>% 
  left_join(valid_zipz, by = 'prefix3') %>% 
  mutate(dist= abs(ZIP.num - ZIP.mean.num)) %>% 
  group_by(prefix3) %>% 
  slice_min(order_by = dist, n = 1, with_ties = FALSE) %>% 
  dplyr::select(prefix3, ZIP.imp = ZIP.char)

# Replacing partial ZIPs with the imputed ones 
CFPB2 <- df %>%
  left_join(ZIP.impute, by = "prefix3") %>%
  mutate(ZIP.Imputed = as.character(ifelse(ZIP.missing == 1, ZIP.imp, ZIP.char)),
         ZIP.missing = ifelse(nchar(ZIP.Imputed) < 5 | grepl("X$", ZIP.Imputed), 1, 0)) %>%
  dplyr::select(-c(ZIP.char,ZIP.imp))

# Checking to make sure states match
StateCheck <- CFPB2 %>%
  left_join(ZIPCODES, by = c("ZIP.Imputed" = "ZIP"))%>%
  mutate(state_match = (State == STATE))
table(StateCheck$state_match, useNA = "ifany")
# Creating a dataframe with all the obs that failed to impute
# try <- CFPB2[is.na(CFPB2$ZIP.missing),]

#3: 
# First major cleaning of CFPB data 
CFPB3<- CFPB2 %>% 
  mutate(Date.received                = as.Date(Date.received,"%m/%d/%y"),
         Date.sent.to.company         = as.Date(Date.sent.to.company,"%m/%d/%y"),
         Year                         = year(Date.received),
         Issue                        = as.factor(Issue),
         Sub.issue                    = as.factor(Sub.issue),
         Company.public.response      = as.factor(Company.public.response),
         Company                      = as.factor(Company),
         State                        = as.factor(State),
         ZIP                          = as.character(ZIP.Imputed),
         Tags                         = as.factor(Tags),
         Consumer.consent.provided.   = as.factor(Consumer.consent.provided.),
         Submitted.via                = as.factor(Submitted.via),
         Company.response.to.consumer = as.factor(Company.response.to.consumer),
         Timely.response.             = as.factor(Timely.response.),
         ZIP.missing                  = ifelse(nchar(ZIP) < 5 | grepl("XX$", ZIP), 1, 0),
         missing                      = as.numeric(ifelse(!complete.cases(CFPB2),1,0)))%>%
  left_join(ZIPCODES %>% dplyr::select(ZIP, FIPS), by = "ZIP")%>%
  relocate(c(ZIP,ZIP.missing,FIPS),.after = State)%>%
  mutate(FIPS = as.factor(FIPS))%>%
  rename(Received      = Date.received,
         Sent          = Date.sent.to.company,
         Cust.response = Company.response.to.consumer,
         Pub.response  = Company.public.response,
         Consent       = Consumer.consent.provided.,
         Timely        = Timely.response.)%>%
  mutate(Wait.time = as.numeric(Sent - Received)) %>%
  dplyr::select(-Product,
                -Sub.product,
                -Consumer.disputed.,
                -Consumer.complaint.narrative,
                -ZIP.Imputed,
                -ZIP.code)%>%
  dplyr::select(Relief, Received, Sent, Year, Wait.time,everything())
# Verifying that the only incomplete cases are ones which did not impute
# summary(!complete.cases(CFPB3))

# Added Variable
## Wait time (in days) could be helpful but values are heavily skewed to 0 and 1
wait <- CFPB[,"Wait.time"] %>%
  filter(Wait.time <= 50,
         Wait.time >  0)%>%
  rename(Time = Wait.time)
summary(na.omit(CFPB$Wait.time==0)) # 55,000+ zero wait times
hist(wait$Time,breaks = 50,right = FALSE)

## --------- 5) -----------#
#Adding Fed measure for household debt by county 
county_debt<- read.csv('/Users/adrisoto/Desktop/HW\ -\ Spring\ \'26/Machine\ Learning/household-debt-by-county/household-debt-by-county.csv')
#Clearing the rows that are not 2020-2025
#CFPB.household_debt<- make new data frame with this for question 5
#using the household debt data, cleaning and formatting it from 
#long to wide

##Reshaping the county_debt data to be merged with CFPB
colnames(CFPB)
colnames(county_debt)

#Alignging the FIPS codes to be the same in both data sets 
CFPB<- CFPB %>% 
  mutate(FIPS = str_pad(as.character(FIPS), width = 5, pad = '0'))
county_debt<- county_debt %>% 
  mutate(area_fips = str_pad(as.character(area_fips), width = 5, pad = '0'))

#Pivot from long to wide, each row will represent one county/year/quarter combo
#with 'low' and 'high' debt columns 

CFPB<- CFPB %>% 
  mutate(
    Received = as.Date(Received, format = '%Y-%m-%d'), 
    year = as.integer(format(Received, '%Y')), 
    qtr = quarter(Received)
  )

merged_debt_county<- CFPB %>% 
  left_join(county_debt, by = c('FIPS' = 'area_fips', 'year', 'qtr'))

#8: Additional data to merge for our analysis: 
#Idea 1) Residential building permit requests by county, https://www.census.gov/construction/bps/index.html
#Idea 2) SNAP/Food stamp enrollment by county https://www.ers.usda.gov/data-products/supplemental-nutrition-assistance-program-snap-data-system/time-series-data
#Idea 3) Bank branch density https://banks.data.fdic.gov/bankfind-suite/SOD

##Need to clear and keep 2020-2025

#------- 6) --------#
StudentLoan<- read_xlsx('/Users/adrisoto/Desktop/Machine Learning/StudentLoan.xlsx')
OverallDebt<- read_xlsx('/Users/adrisoto/Desktop/Machine Learning/Overall.xlsx')
AutoRetail<- read_xlsx('/Users/adrisoto/Desktop/Machine Learning/AutoRetail.xlsx')
#combinin all debt data 
DebtMetrics <- OverallDebt %>%
  left_join(AutoRetail %>% 
              dplyr::select(-`County Name`,
                            -`State Name`,
                            -`Auto/retail loan delinquency rate, All`,
                            -`Auto/retail loan delinquency rate, Comm of color`,
                            -`Auto/retail loan delinquency rate, White comm`,
                            -`Share of people of color`,
                            -`Average household income, All`,
                            -`Average household income, White comm`,
                            -`Average household income, Comm of color`),
            by = "County FIPS") %>%
  left_join(StudentLoan %>% 
              dplyr::select(-`County Name`,
                            -`State Name`,
                            -`Share of people of color`,
                            -`Average household income, All`,
                            -`Average household income, White comm`,
                            -`Average household income, Comm of color`,
                            -`Student loan delinquency rate (60+), All`,
                            -`Student loan delinquency rate (60+), Comm of color`,
                            -`Student loan delinquency rate (60+), White comm`),
            by = "County FIPS")%>%
  rename(FIPS = `County FIPS`)
# Joining CFPB and Debt data for inspection
CFPB.debt <- CFPB3 %>%
  left_join(DebtMetrics %>
              dplyr::select(-`State Name`),
            by ="FIPS")
#rm(CFPB.debt,AutoRetail,OverallDebt,StudentLoan,ZIPCODES)

#--------- 7) -----------
Insecure0<- read_xlsx('/Users/adrisoto/Desktop/Machine Learning/credit-insecurity-index-data-workbook.xlsx', 
                      sheet = 'County')
Insecure<- Insecure0 %>% 
  pivot_longer(cols = '2018':'2023', 
               names_to = 'Year', 
               values_to = 'value') %>% 
  mutate(Year = as.integer(Year)) %>% 
  pivot_wider(names_from = 'Credit Insecurity Measure',
              values_from = value) %>% 
  rename(FIPS = GEOID)

#Joining credit insecurity data with CFPB
##The only years of overlap are 2022 and 2023. This filters everything else out
CFPB.insecurity<- CFPB3 %>% 
  left_join(Insecure %>% 
              dplyr::select(-'County Name', 
                            -State), 
            by = c('FIPS', 'Year')) %>% 
  filter(Year %in% c('2018':'2023'))
rm(CFPB.insecurity, Insecure0)

#Joining Debt and Credit Insecurity datasets to CFPB
##Previous CFPB data frame and referenses to it are changes to CFPB3
CFPB<- CFPB3 %>% 
  left_join(Insecure %>% 
              dplyr::select(-'County Name', -State), 
            by = c('FIPS', 'Year')) %>% 
  dplyr::select(-ZIP.missing)
rm(Insecure, DebtMetrics, CFPB2, CFPB3)

#--------- 8) ----------
#Importing, cleaning, and combining fair market rent data 

FMR22 <- read_xlsx('/Users/adrisoto/Desktop/Machine Learning/Group project data/FY22_FMRs_revised.xlsx')%>%
  rename(fips = fips2010,
         "Studio_2022"    = fmr_0,
         "OneRoom_2022"   = fmr_1,
         "TwoRoom_2022"   = fmr_2,
         "ThreeRoom_2022" = fmr_3,
         "FourRoom_2022"  = fmr_4)
FMR23 <- read_xlsx('/Users/adrisoto/Desktop/Machine Learning/Group project data/FY23_FMRs_revised.xlsx') %>%
  rename("Studio_2023"    = fmr_0,
         "OneRoom_2023"   = fmr_1,
         "TwoRoom_2023"   = fmr_2,
         "ThreeRoom_2023" = fmr_3,
         "FourRoom_2023"  = fmr_4)
FMR24 <- read_xlsx('/Users/adrisoto/Desktop/Machine Learning/Group project data/FMR2024_final_revised.xlsx') %>%
  rename("Studio_2024"    = fmr_0,
         "OneRoom_2024"   = fmr_1,
         "TwoRoom_2024"   = fmr_2,
         "ThreeRoom_2024" = fmr_3,
         "FourRoom_2024"  = fmr_4)
FMR25 <- read_xlsx('/Users/adrisoto/Desktop/Machine Learning/Group project data/FY25_FMRs_revised.xlsx')%>%
  rename("Studio_2025"    = fmr_0,
         "OneRoom_2025"   = fmr_1,
         "TwoRoom_2025"   = fmr_2,
         "ThreeRoom_2025" = fmr_3,
         "FourRoom_2025"  = fmr_4)

FMR <- FMR22 %>%
  left_join(FMR23[,c('Studio_2023','OneRoom_2023','TwoRoom_2023','ThreeRoom_2023','FourRoom_2023','fips')], by = 'fips') %>%
  left_join(FMR24[,c('Studio_2024','OneRoom_2024','TwoRoom_2024','ThreeRoom_2024','FourRoom_2024','fips')], by = 'fips') %>%
  left_join(FMR25[,c('Studio_2025','OneRoom_2025','TwoRoom_2025','ThreeRoom_2025','FourRoom_2025','fips')], by = 'fips') %>%
  pivot_longer(cols = matches("Studio|OneRoom|TwoRoom|ThreeRoom|FourRoom"),
               names_to = c(".value", "Year"),
               names_sep = "_",
               values_to = "fmr") %>%
  group_by(fips, Year) %>%
  dplyr::select(fips,Year,Studio,OneRoom,TwoRoom,ThreeRoom,FourRoom) %>%
  mutate(fips = substr(fips,1,5),
         Year = as.double(Year)) %>%
  summarise(across(matches("Studio|OneRoom|TwoRoom|ThreeRoom|FourRoom"), ~ round(mean(.x, na.rm = TRUE), 0)),
            .groups = "drop") %>%
  rename(FIPS = fips) %>%
  rename_with(~ paste0(., "_fmr"), matches("Studio|OneRoom|TwoRoom|ThreeRoom|FourRoom"))

#Joining FMR data to CFPB
##Only includes metric for 

CFPB.FMR<- CFPB %>% 
  left_join(FMR, by = c('FIPS', 'Year'))


##### 8) ------ Adri Code: -------
##Adding in County level building permit requests 

# Function to pull and format BPS data by year
# Direct URL for 2022 County Annual Data
url_2022 <- "https://www2.census.gov/econ/bps/County/co2204y.txt"

# Read the data
bps_2022_raw <- read.csv(url_2022, skip = 2, header = FALSE)

# Format the data to match  CFPB.FMR dataset
bps_2022_clean <- bps_2022_raw %>%
  mutate(
    # Combine State (V2) and County (V3) to create a 5-digit FIPS
    # %02d ensures state has 2 digits (01, 02)
    # %03d ensures county has 3 digits (001, 002)
    FIPS = paste0(sprintf("%02d", V2), sprintf("%03d", V3)),
    
    # Label the Year so it matches existing data
    Year = 2022,
    
    # V7 is the standard column for 'Total Housing Units' authorized
    Permit_Units = V7,
    
    # V8 is the total 'Valuation' (dollar amount) of those permits
    Permit_Valuation = V8
  ) %>%
  select(FIPS, Year, Permit_Units, Permit_Valuation, County_Name = V6)

# Quick check
head(bps_2022_clean)
# This force-formats  existing FIPS to a 5-character string
CFPB.FMR <- CFPB.FMR %>%
  mutate(FIPS = sprintf("%05s", as.character(FIPS)))

# use left_join so we don't lose any of  original rows
CFPB_with_Permits <- CFPB.FMR %>%
  filter(Year == 2022) %>% # Let's isolate 2022 for now
  left_join(bps_2022_clean, by = "FIPS")

# View the result
summary(CFPB_with_Permits$Permit_Units)

#Pulling 2023 data 
url_2023 <- "https://www2.census.gov/econ/bps/County/co2304y.txt"
bps_2023_raw <- read.csv(url_2023, skip = 2, header = FALSE)

bps_2023_clean <- bps_2023_raw %>%
  mutate(
    FIPS = paste0(sprintf("%02d", V2), sprintf("%03d", V3)),
    Year = 2023,
    Permit_Units = V7,
    Permit_Valuation = V8
  ) %>%
  select(FIPS, Year, Permit_Units, Permit_Valuation, County_Name = V6)

#Pulling 2024 data 
url_2024 <- "https://www2.census.gov/econ/bps/County/co2404y.txt"
bps_2024_raw <- read.csv(url_2024, skip = 2, header = FALSE)

bps_2024_clean <- bps_2024_raw %>%
  mutate(
    FIPS = paste0(sprintf("%02d", V2), sprintf("%03d", V3)),
    Year = 2024,
    Permit_Units = V7,
    Permit_Valuation = V8
  ) %>%
  select(FIPS, Year, Permit_Units, Permit_Valuation, County_Name = V6)

##Stacking all years together: 
all_bps_years<- bind_rows(bps_2022_clean, bps_2023_clean, bps_2024_clean)

#making sure main df FIPS are formatted with 5 digits
CFPB.FMR<- CFPB.FMR %>% 
  mutate(FIPS = sprintf('%05s', as.character(FIPS))

#Verify the results 
# This shoudl show the counts for each year 
table(CFPB_Final$Year)

# Look at the first few FIPS in both
head(CFPB.FMR$FIPS)
head(all_bps_years$FIPS)

# This filters out the header/footer junk from the Census files
all_bps_years_clean <- all_bps_years %>%
  filter(!is.na(Permit_Units)) %>%
  filter(FIPS != "NA0NA")

CFPB_Final <- CFPB.FMR %>%
  left_join(all_bps_years_clean, by = c("FIPS", "Year"))


######## ------- 9) --------- #########







