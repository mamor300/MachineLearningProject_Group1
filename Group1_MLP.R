setwd('/Users/mattamor/Library/CloudStorage/OneDrive-Personal/School/ECON 6378 - Machines/DataProject/MachineLearningProject_Group1')
rm(list=ls())

pacman::p_load(
  readr,
  readxl,
  tidyverse,
  VIM)

CFPB0 <- read_csv("sample26.01.csv")
ZIPCODES <- read_csv("zip_fips.csv")

# Recoding ZIP and FIPS
ZIPCODES <- ZIPCODES %>%
  mutate(FIPS = str_pad(STCOUNTYFP,5,"left",pad="0"),
         ZIP  = str_pad(ZIP,5,"left",pad="0"))

# Recoding Response Variable
CFPB0$Relief<- ifelse(CFPB0$Company.response.to.consumer %in% 
                     c("Closed with monetary relief",
                       "Closed with relief",
                       "Closed with non-monetary relief"),
                    1,0)
CFPB0 <- CFPB0[,-c(1)]%>%
  dplyr::select(Relief,Date.received, Date.sent.to.company,everything())

# Dropping Observations
## Dropping 134 rows (<0.3% of total) have NA values across 12 variables in original data 
## Dropping observations not in the 50 states
CFPB1 <-CFPB0 %>%
  drop_na(Date.sent.to.company)%>%
  filter(!State %in% c("NONE", "None", "DC", "AA","AE", "AP", "AS", "FM","GU", "MH", "MP", "PR", "VI", "UNITED STATES MINOR OUTLYING ISLANDS"))

#2.
# Imputing zip codes with means
## Each means-imputed value uses the average among the first 3 digits of non-missing "siblings"
## This produces 95 NAs for zip codes that do not have any non-missing siblings
## Creating a branch of the CFPB data that prepares the missing zips for imputation
df <- CFPB1 %>%
  mutate(ZIP.char    = as.character(ZIP.code),
         prefix3     = substr(ZIP.char, 1, 3),
         ZIP.missing = ifelse(nchar(ZIP.code) < 5 | grepl("X$", ZIP.code), 1, 0))

# Creating a dataframe of the valid ZIP codes
## A numeric vector helps calculate the means
## Character vector manages the prefixes
valid_zips <- ZIPCODES %>%
  mutate(ZIP.char   = formatC(as.numeric(ZIP), width = 5, flag = "0", format = "d"),
         prefix3    = substr(ZIP.char, 1, 3),
         ZIP.num    = as.numeric(ZIP.char)) %>%
  dplyr::select(prefix3, ZIP.char, ZIP.num)

# Computing means for each 3-digit prefix
ZIP.means <- df %>%
  filter(ZIP.missing == 0, nchar(trimws(ZIP.char)) == 5) %>%
  group_by(prefix3) %>%
  summarise(ZIP.mean.num = round(mean(as.numeric(ZIP.char), na.rm = TRUE)))

# Identifying the nearest valid zip for each mean value
## ZIP.imp is 
ZIP.impute <- ZIP.means %>%
  left_join(valid_zips, by = "prefix3") %>%
  mutate(dist = abs(ZIP.num - ZIP.mean.num)) %>%
  group_by(prefix3) %>%
  slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%  
  dplyr::select(prefix3, ZIP.imp = ZIP.char)

# Replacing partial ZIP codes with imputed ones
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

#rm(StateCheck,CFPB0, CFPB1,df,try,ZIP.impute,ZIP.means,valid_zips)

#3.
## First major cleaning of CFPB data
CFPB3 <- CFPB2 %>%
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

#6.
AutoRetail <- read_xlsx("Question6/AutoRetail.xlsx")
StudentLoan <- read_xlsx("Question6/StudentLoan.xlsx")
OverallDebt <- read_xlsx("Question6/Overall.xlsx")
# Combining all debt data
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
  left_join(DebtMetrics %>%
              dplyr::select(-`State Name`),
            by ="FIPS")
#rm(CFPB.debt,AutoRetail,OverallDebt,StudentLoan,ZIPCODES)

#7.
INSECURE0 <- read_xlsx("Question07/credit-insecurity-index-data-workbook.xlsx", sheet = "County")
INSECURE <- INSECURE0 %>%
  pivot_longer(cols = "2018":"2023",
               names_to = "Year",
               values_to = "value") %>%
  mutate(Year = as.integer(Year))%>%
  pivot_wider(names_from = `Credit Insecurity Measure`,
              values_from = value)%>%
  rename(FIPS = GEOID)
# Joining credit insecurity data with CFPB
## The only years of overlap are 2022 and 2023. This filters everything else out
CFPB.insecurity <- CFPB3 %>%
  left_join(INSECURE %>%
              dplyr::select(-`County Name`,
                     -State),
            by = c("FIPS","Year"))%>%
  filter(Year %in% c("2018":"2023"))
#rm(CFPB.insecurity,INSECURE0)

# Joining Debt and Credit Insecurity datasets to CFPB
## Previous CFPB data frame and references to it are changed to CFPB3 
CFPB <- CFPB3 %>%
  left_join(INSECURE %>%
              dplyr::select(-`County Name`,
                     -State),
            by = c("FIPS","Year"))%>%
  left_join(DebtMetrics %>%
              dplyr::select(-`State Name`),
            by ="FIPS")
#rm(INSECURE, DebtMetrics,CFPB2,CFPB3)

#8
FMR22 <- read_xlsx("Question08/FY22_FMRs_revised.xlsx")%>%
  rename(fips = fips2010)
FMR23 <- read_xlsx("Question08/FY23_FMRs_revised.xlsx")
FMR24 <- read_xlsx("Question08/FMR2024_final_revised.xlsx")
FMR25 <- read_xlsx("Question08/FY25_FMRs_revised.xlsx")
FMR <- FMR22 %>%
  left_join(FMR23, by = "fips")
  