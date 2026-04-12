setwd("/Users/mattamor/MachineLearningProject_Group1")

install.packages("tidycensus")
install.packages("randomForest")

pacman::p_load(
  readr,
  readxl,
  writexl,
  tidyverse,
  VIM,
  caret,
  missMDA,
  tidycensus,
  randomForest,
  clustMixType,
  censusapi,
  forcats)

#1. 
{
CFPB0 <- read_csv("sample26.01.csv")
ZIPCODES <- read_csv("zip_fips.csv")

# Recoding ZIP and FIPS
ZIPCODES <- ZIPCODES |>
  mutate(FIPS = str_pad(STCOUNTYFP,5,"left",pad="0"),
         ZIP  = str_pad(ZIP,5,"left",pad="0"))

# Recoding Response Variable
CFPB0$Relief<- ifelse(CFPB0$Company.response.to.consumer %in% 
                     c("Closed with monetary relief",
                       "Closed with relief",
                       "Closed with non-monetary relief"),
                    1,0)
CFPB0 <- CFPB0[,-c(1)]|>
  select(Relief,Date.received, Date.sent.to.company,everything())
  #select(-Company.response.to.consumer)

# Dropping Observations
## Dropping 134 rows (<0.3% of total) have NA values across 12 variables in original data 
## Dropping observations not in the 50 states
CFPB1 <- CFPB0 |>
  drop_na(Date.sent.to.company)|>
  filter(!State %in% c("NONE", "None", "DC", "AA","AE", "AP", "AS", "FM","GU", "MH", "MP", "PR", "VI", "UNITED STATES MINOR OUTLYING ISLANDS"))
}
#2.
{
# Imputing zip codes with means
## Each means-imputed value uses the average among the first 3 digits of non-missing "siblings"
## This produces 95 NAs for zip codes that do not have any non-missing siblings
## Creating a branch of the CFPB data that prepares the missing zips for imputation
df <- CFPB1 |>
  mutate(ZIP.char    = as.character(ZIP.code),
         prefix3     = substr(ZIP.char, 1, 3),
         ZIP.missing = ifelse(nchar(ZIP.code) < 5 | grepl("X$", ZIP.code), 1, 0))

# Creating a dataframe of the valid ZIP codes
## A numeric vector helps calculate the means
## Character vector manages the prefixes
valid_zips <- ZIPCODES |>
  mutate(ZIP.char   = formatC(as.numeric(ZIP), width = 5, flag = "0", format = "d"),
         prefix3    = substr(ZIP.char, 1, 3),
         ZIP.num    = as.numeric(ZIP.char)) |>
  select(prefix3, ZIP.char, ZIP.num)

# Computing means for each 3-digit prefix
ZIP.means <- df |>
  filter(ZIP.missing == 0, nchar(trimws(ZIP.char)) == 5) |>
  group_by(prefix3) |>
  summarise(ZIP.mean.num = round(mean(as.numeric(ZIP.char), na.rm = TRUE)))

# Identifying the nearest valid zip for each mean value
## ZIP.imp is 
ZIP.impute <- ZIP.means |>
  left_join(valid_zips, by = "prefix3") |>
  mutate(dist = abs(ZIP.num - ZIP.mean.num)) |>
  group_by(prefix3) |>
  slice_min(order_by = dist, n = 1, with_ties = FALSE) |>  
  select(prefix3, ZIP.imp = ZIP.char)

# Replacing partial ZIP codes with imputed ones
CFPB2 <- df |>
  left_join(ZIP.impute, by = "prefix3") |>
  mutate(ZIP.Imputed = as.character(ifelse(ZIP.missing == 1, ZIP.imp, ZIP.char)),
         ZIP.missing = ifelse(nchar(ZIP.Imputed) < 5 | grepl("X$", ZIP.Imputed), 1, 0)) |>
  select(-c(ZIP.char,ZIP.imp, prefix3))

# Checking to make sure states match
StateCheck <- CFPB2 |>
  left_join(ZIPCODES, by = c("ZIP.Imputed" = "ZIP"))|>
  mutate(state_match = (State == STATE))
table(StateCheck$state_match, useNA = "ifany")
# Creating a dataframe with all the obs that failed to impute
# try <- CFPB2[is.na(CFPB2$ZIP.missing),]

#rm(StateCheck,CFPB0, CFPB1,df,try,ZIP.impute,ZIP.means,valid_zips)
}
#3.
{
## First major cleaning of CFPB data
CFPB3 <- CFPB2 |>
  mutate(Date.received                = as.Date(Date.received,"%m/%d/%y"),
         Date.sent.to.company         = as.Date(Date.sent.to.company,"%m/%d/%y"),
         Year                         = as.factor(year(Date.received)),
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
         ZIP.missing                  = ifelse(nchar(ZIP) < 5 | grepl("XX$", ZIP), 1, 0))|>
  left_join(ZIPCODES |> select(ZIP, FIPS), by = "ZIP")|>
  relocate(c(ZIP,ZIP.missing,FIPS),.after = State)|>
  mutate(FIPS = as.factor(FIPS))|>
  rename(Received      = Date.received,
         Sent          = Date.sent.to.company,
         Cust.response = Company.response.to.consumer,
         Pub.response  = Company.public.response,
         Consent       = Consumer.consent.provided.,
         Timely        = Timely.response.)|>
  select(-Product,
         -Sub.product,
         -Consumer.disputed.,
         -Consumer.complaint.narrative,
         -ZIP.Imputed,
         -ZIP.code)|>
  mutate(Wait.time = as.numeric(Sent - Received))|>
  select(Relief, Received, Sent, Year, Wait.time,everything())
# Verifying that the only incomplete cases are ones which did not impute
CFPB3 <- CFPB3 |>
  mutate(missing   = as.numeric(ifelse(!complete.cases(CFPB3),1,0))) 
# summary(!complete.cases(CFPB3))
# summary(is.na(CFPB3$ZIP))
}
#4.
{
  # Medical debt
  med_debt_raw <- read_excel("Question04/changing_med_debt_landscape_county.xlsx", 
                             sheet = 1,
                             .name_repair = "universal")
  
  # Cleaning
  med_debt_clean <- med_debt_raw %>%
    mutate(
      Year = as.factor(Year),
      FIPS = as.factor(str_pad(as.character(County.Fips), 5, "left", pad = "0")),
      "Share.with.medical.debt.in.collections"                          = as.numeric(Share.with.medical.debt.in.collections),                   
      "Median.medical.debt.in.collections.in..2023"                     = as.numeric(Median.medical.debt.in.collections.in..2023),                 
      "Share.with.medical.debt.in.collections...Majority.White"         = as.numeric(Share.with.medical.debt.in.collections...Majority.White),        
      "Median.medical.debt.in.collections.in..2023...Majority.White"    = as.numeric(Median.medical.debt.in.collections.in..2023...Majority.White),   
      "Share.with.medical.debt.in.collections...Majority.of.Color"      = as.numeric(Share.with.medical.debt.in.collections...Majority.of.Color),
      "Median.medical.debt.in.collections.in..2023...Majority.of.Color" = as.numeric(Median.medical.debt.in.collections.in..2023...Majority.of.Color),
      "Hospital.market.concentration..HHI."                             = as.numeric(Hospital.market.concentration..HHI.),
      "Number.of.Closures.and.Mergers"                                  = as.numeric(Number.of.Closures.and.Mergers),
      "Share.of.the.population.with.no.health.insurance.coverage"       = as.numeric(Share.of.the.population.with.no.health.insurance.coverage),
      "Share.of.non.elderly.adults.with.a.reported.disability"          = as.numeric(Share.of.non.elderly.adults.with.a.reported.disability),
      "Average.household.income.in..2023"                               = as.numeric(Average.household.income.in..2023),
    ) %>%
    # removing redundant variables, and ones missing more than 30%
    select(-c(County.Fips,
              State.Abbreviation,
              County.Name,
              Median.medical.debt.in.collections.in..2023...Majority.of.Color,
              Share.with.medical.debt.in.collections...Majority.of.Color,
              Median.medical.debt.in.collections.in..2023...Majority.White,
              Median.medical.debt.in.collections.in..2023))
  # Median impute for missing 2024 and 2025 years
  median_cols <- setdiff(names(med_debt_clean), c("Year", "FIPS"))
  new_rows <- med_debt_clean %>%
    group_by(FIPS) %>%
    summarise(across(all_of(median_cols), \(x) median(x, na.rm = TRUE)), .groups = "drop")
  
  # Duplicate for 2024 and 2025
  new_rows_2024 <- new_rows %>% mutate(Year = as.factor(2024))
  new_rows_2025 <- new_rows %>% mutate(Year = as.factor(2025))
  
  # Combine with original data
  med_debt_clean <- bind_rows(med_debt_clean, new_rows_2024, new_rows_2025) %>%
    arrange(FIPS, Year)
  # Left join
  CFPB4 <- CFPB3 %>%
    left_join(med_debt_clean, by = c("FIPS", "Year"))
  mean(!complete.cases(CFPB4))
  # median impute for remaining missing values (about 17% of medical debt)
  CFPB4 <- na.roughfix(CFPB4[22:28])
}
#5.
{
#Adding Fed measure for household debt by county 
county_debt<- read.csv("Question05/household-debt-by-county.csv")
#Clearing the rows that are not 2020-2025
#CFPB.household_debt<- make new data frame with this for question 5
#using the household debt data, cleaning and formatting it from 
#long to wide

##Reshaping the county_debt data to be merged with CFPB
colnames(CFPB3)
colnames(county_debt)

#Alignging the FIPS codes to be the same in both data sets 
CFPB.countydebt <- CFPB3 %>% 
  mutate(FIPS = str_pad(as.character(FIPS), width = 5, pad = '0'))
county_debt <- county_debt %>% 
  mutate(area_fips = str_pad(as.character(area_fips), width = 5, pad = '0'))%>%
  rename(Year=year)

#Pivot from long to wide, each row will represent one county/year/quarter combo
#with 'low' and 'high' debt columns 

CFPB.countydebt <- CFPB.countydebt %>% 
  mutate(
    Received = as.Date(Received, format = '%Y-%m-%d'), 
    Year = as.integer(format(Received, '%Y')), 
    qtr = quarter(Received)
  )

CFPB5 <- CFPB.countydebt %>% 
  left_join(county_debt, by = c('FIPS' = 'area_fips', 'Year', 'qtr'))
}
#6.
{
AutoRetail <- read_xlsx("Question06/AutoRetail.xlsx")
StudentLoan <- read_xlsx("Question06/StudentLoan.xlsx")
OverallDebt <- read_xlsx("Question06/Overall.xlsx")
# Combining all debt data
DebtMetrics <- OverallDebt |>
  left_join(AutoRetail |> 
              select(-`County Name`,
                     -`State Name`,
                     -`Auto/retail loan delinquency rate, All`,
                     -`Auto/retail loan delinquency rate, Comm of color`,
                     -`Auto/retail loan delinquency rate, White comm`,
                     -`Share of people of color`,
                     -`Average household income, All`,
                     -`Average household income, White comm`,
                     -`Average household income, Comm of color`),
            by = "County FIPS") |>
  left_join(StudentLoan |> 
              select(-`County Name`,
                     -`State Name`,
                     -`Share of people of color`,
                     -`Average household income, All`,
                     -`Average household income, White comm`,
                     -`Average household income, Comm of color`,
                     -`Student loan delinquency rate (60+), All`,
                     -`Student loan delinquency rate (60+), Comm of color`,
                     -`Student loan delinquency rate (60+), White comm`),
            by = "County FIPS")|>
  rename(FIPS = `County FIPS`)

# Changing character vectors to numeric
DebtMetrics.clean <- DebtMetrics |>
  mutate(across(-c(`County Name`,FIPS,`State Name`),~ as.numeric(gsub(",", "", .x))))

# Joining CFPB and Debt data
CFPB6 <- CFPB5 |>
  left_join(DebtMetrics.clean |>
              select(-`County Name`,-`State Name`),
            by ="FIPS")
}
#7.
{
INSECURE0 <- read_xlsx("Question07/credit-insecurity-index-data-workbook.xlsx", sheet = "County")
tier_lookup <- c(
    "Credit At Risk"  = 1,
    "Credit Insecure" = 2,   # <-- double-check this
    "Mid-Tier"        = 3,
    "Credit Likely"   = 4,
    "Credit Assured"  = 5
  )
year_cols <- as.character(2018:2023)

tier_numeric_rows <- INSECURE0 %>%
  filter(`Credit Insecurity Measure` == "Credit Tier") %>%
  mutate(`Credit Insecurity Measure` = "Credit Tier - Numeric",
         across(all_of(year_cols), ~ as.character(tier_lookup[as.character(.)])))
county_with_numeric <- INSECURE0 %>%
  bind_rows(tier_numeric_rows) %>%
  arrange(GEOID, factor(`Credit Insecurity Measure`, 
                        levels = c("Credit Tier", 
                                   "Credit Tier - Numeric",
                                   "CI Index Score", 
                                   "Not Credit Included", 
                                   "Credit Constrained")))
tier_rows <- county_with_numeric %>%
  filter(`Credit Insecurity Measure` == "Credit Tier") %>%
  mutate(`2024` = NA_character_,
         `2025` = NA_character_)

numeric_rows <- county_with_numeric %>%
  filter(`Credit Insecurity Measure` != "Credit Tier") %>%
  mutate(across(all_of(year_cols), ~ suppressWarnings(as.numeric(.)))) %>%
  rowwise() %>%
  mutate(
    `2024` = round(mean(c_across(all_of(year_cols)), na.rm = TRUE),1),
    `2025` = `2024`
  ) %>%
  ungroup() %>%
  mutate(across(c(`2024`, `2025`), as.character),
         across(all_of(year_cols), as.character))

INSECURE1 <- bind_rows(tier_rows, numeric_rows) %>%
  arrange(GEOID, factor(`Credit Insecurity Measure`,
                        levels = c("Credit Tier",
                                   "Credit Tier - Numeric",
                                   "CI Index Score",
                                   "Not Credit Included",
                                   "Credit Constrained")))

INSECURE2 <- INSECURE1 |>
  pivot_longer(cols = "2018":"2025",
               names_to = "Year",
               values_to = "value") |>
  mutate(Year = as.integer(Year))|>
  pivot_wider(names_from = `Credit Insecurity Measure`,
              values_from = value)|>
  rename(FIPS = GEOID)

# Joining credit insecurity data with CFPB
CFPB7 <- CFPB6 |>
  left_join(INSECURE2 |>
              select(-`County Name`,-State),
            by = c("FIPS","Year"))|>
  mutate(`CreditTier` = round(as.numeric(`Credit Tier - Numeric`),0))|>
  select(-c(`Credit Tier`,`Credit Tier - Numeric`))
}
#8.
{
# Importing, cleaning, combining fair market rent data from 
FMR22 <- read_xlsx("Question08/FY22_FMRs_revised.xlsx")|>
  rename(fips = fips2010,
         "Studio_2022"    = fmr_0,
         "OneRoom_2022"   = fmr_1,
         "TwoRoom_2022"   = fmr_2,
         "ThreeRoom_2022" = fmr_3,
         "FourRoom_2022"  = fmr_4)
FMR23 <- read_xlsx("Question08/FY23_FMRs_revised.xlsx") |>
  rename("Studio_2023"    = fmr_0,
         "OneRoom_2023"   = fmr_1,
         "TwoRoom_2023"   = fmr_2,
         "ThreeRoom_2023" = fmr_3,
         "FourRoom_2023"  = fmr_4)
FMR24 <- read_xlsx("Question08/FMR2024_final_revised.xlsx") |>
  rename("Studio_2024"    = fmr_0,
         "OneRoom_2024"   = fmr_1,
         "TwoRoom_2024"   = fmr_2,
         "ThreeRoom_2024" = fmr_3,
         "FourRoom_2024"  = fmr_4)
FMR25 <- read_xlsx("Question08/FY25_FMRs_revised.xlsx")|>
  rename("Studio_2025"    = fmr_0,
         "OneRoom_2025"   = fmr_1,
         "TwoRoom_2025"   = fmr_2,
         "ThreeRoom_2025" = fmr_3,
         "FourRoom_2025"  = fmr_4)
FMR <- FMR22 |>
  left_join(FMR23[,c('Studio_2023','OneRoom_2023','TwoRoom_2023','ThreeRoom_2023','FourRoom_2023','fips')], by = 'fips') |>
  left_join(FMR24[,c('Studio_2024','OneRoom_2024','TwoRoom_2024','ThreeRoom_2024','FourRoom_2024','fips')], by = 'fips') |>
  left_join(FMR25[,c('Studio_2025','OneRoom_2025','TwoRoom_2025','ThreeRoom_2025','FourRoom_2025','fips')], by = 'fips') |>
  pivot_longer(cols = matches("Studio|OneRoom|TwoRoom|ThreeRoom|FourRoom"),
               names_to = c(".value", "Year"),
               names_sep = "_",
               values_to = "fmr") |>
  group_by(fips, Year) |>
  select(fips,Year,Studio,OneRoom,TwoRoom,ThreeRoom,FourRoom) |>
  mutate(fips = substr(fips,1,5),
         Year = as.double(Year)) |>
  summarise(across(matches("Studio|OneRoom|TwoRoom|ThreeRoom|FourRoom"), ~ round(mean(.x, na.rm = TRUE), 0)),
            .groups = "drop") |>
  rename(FIPS = fips) |>
  rename_with(~ paste0(., "_fmr"), matches("Studio|OneRoom|TwoRoom|ThreeRoom|FourRoom"))

# Joining FMR data to CFPB
## Only includes metric for 
CFPB.FMR <- CFPB7 |>
  left_join(FMR,by = c('FIPS',"Year"))

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

# Look at the first few FIPS in both
head(CFPB.FMR$FIPS)
head(all_bps_years$FIPS)

# This filters out the header/footer junk from the Census files
all_bps_years_clean <- all_bps_years %>%
  filter(!is.na(Permit_Units)) %>%
  filter(FIPS != "NA0NA")

CFPB.bps <- CFPB.FMR %>%
  left_join(all_bps_years_clean, by = c("FIPS", "Year"))

#Verify the results 
# This should show the counts for each year 
table(CFPB.bps$Year)
Sys.setenv(CENSUS_KEY = "ba79958600ff02f01da8a857d6a3243c191cfc8a")
sahie_vars <- listCensusMetadata(
  name = "timeseries/healthins/sahie",
  type = "variables"
)
#this will show you what all the variables chosen below are
library(purrr)
#no 2024 or 2025 data
sahie <- map_dfr(2022:2023, ~getCensus(
  name   = "timeseries/healthins/sahie",
  vars   = c("NAME", "PCTUI_PT", "NUI_PT", "NIC_PT","NIPR_PT", "YEAR"),
  region = "county:*",
  time   = .x
))|>
  mutate(fips = paste0(state, county)) |>
  select(-time) |>
  rename(Year = YEAR, FIPS = fips)
sahie$Year <- as.numeric(sahie$Year)
CFPB.sahie <- CFPB.bps |>
  left_join(sahie,by = c('FIPS',"Year"))
compiled_county_education_measures <- read_csv("Question08/compiled_county_education_measures.csv")|>
  select(fips,pct_bach_degree,year)|>
  rename(FIPS = fips,
         Year = year)
CFPB8 <- CFPB.bps |>
  left_join(compiled_county_education_measures,by= c("FIPS","Year"))
}
#9.
{
# --- A. Setup Variable IDs ---
young_males   <- sprintf("P12_%03dN", 3:10)
young_females <- sprintf("P12_%03dN", 27:34)
senior_vars   <- c(paste0("P12_0", 20:25, "N"), paste0("P12_0", 44:49, "N"))

census_vars <- c(
  total_pop    = "P1_001N",
  female_total = "P12_026N",
  hispanic     = "P9_002N",
  white_nh     = "P9_005N",
  black_nh     = "P9_006N",
  black_female = "P12B_026N" 
)

# Combine all into one massive pull to avoid making multiple API calls
all_vars <- c(census_vars, 
              setNames(young_males, paste0("m_young_", 3:10)), 
              setNames(young_females, paste0("f_young_", 27:34)),
              setNames(senior_vars, paste0("senior_", 1:12)))

# --- B. Pull the Data ---
census_raw <- get_decennial(
  geography = "county",
  variables = all_vars,
  year = 2020,
  sumfile = "dhc",
  output = "wide"
)
# 2. Feature Engineering
# Now we create the proportions and the Older County dummy.

# --- C. Create Proportions and Dummies ---
census_features <- census_raw %>%
  mutate(
    # Summing the age groups
    total_young    = rowSums(select(., starts_with("m_young"), starts_with("f_young"))),
    total_65plus   = rowSums(select(., starts_with("senior_"))),
    
    # Proportions (Features)
    prop_young      = total_young / total_pop,
    prop_65plus     = total_65plus / total_pop,
    prop_female     = female_total / total_pop,
    prop_hispanic   = hispanic / total_pop,
    prop_black      = black_nh / total_pop,
    prop_black_fem  = black_female / total_pop,
    
    # Dummy variable (Requirement 1.a.ii.2)
    is_older_county = if_else(prop_65plus > 0.17, 1, 0)
  ) %>%
  select(FIPS = GEOID, starts_with("prop_"), is_older_county)

# --- D. Standardize for PCA (Requirement 1.c.i) ---
census_scaled <- census_features %>%
  mutate(across(starts_with("prop_"), ~as.vector(scale(.))))
# --- E. Tag Dummies (Requirement 1.v) ---
CFPB.dummies <- CFPB8 %>%
  mutate(
    is_servicemember = if_else(grepl("Servicemember", Tags, ignore.case = TRUE), 1, 0),
    is_older_american = if_else(grepl("Older American", Tags, ignore.case = TRUE), 1, 0),
    # Ensure FIPS matches Census format
    FIPS = sprintf("%05s", as.character(FIPS))
  )

# --- F. The Final Join ---
CFPB9 <- CFPB.dummies %>%
  left_join(census_scaled, by = "FIPS")
#rm(age_clean, age_vars, all_bps_years, all_bps_years_clean, census_demographics, 
#  census_features, census_raw, census_scaled, CFPB_Final_Analysis, CFPB_Final_Bias, 
#  CFPB_with_Permits)
}
#10.
{
# PCA on debt collection variables
## Note the following is a PCA on the county-level debt collection metrics
## There is severe missingness in this dataset

# Setting neighborhoods as row names
## You'll see a warning for 49 NAs introduced. This is by design
DebtMetrics1 <- DebtMetrics[,-c(1:3)]
DMClean <- DebtMetrics1 |>
  mutate(across(everything(),~ as.numeric(gsub(",", "", .x))))

# Checking percentage of columns that have NAs
# colMeans(is.na(DMClean)) |> sort(decreasing = TRUE)

# Removing variables with over 30% missing - keeping 23 out of 50 variables
missingRate <- colMeans(is.na(DMClean))
DMClean <- DMClean[,missingRate <= 0.3]

# Checking percentage of columns that have NAs again
# colMeans(is.na(DMClean)) |> sort(decreasing = TRUE)

# Imputing missing data
## estim_ncpPCA uses CV to determine a good number of Principal Components
## Then imputePCA uses PCA to predict missing values
nb <- estim_ncpPCA(DMClean) # estimated optimum number of PCs = 4       
DMImpute <- imputePCA(DMClean, ncp = nb$ncp)
DMImputed <- DMImpute$completeObs

DebtMetrics.pca <- prcomp(DMImputed, center = TRUE, scale. = TRUE)

# Loadings
Loadings <- data.frame(DebtMetrics.pca$rotation)|>
  mutate(DebtMetric = rownames(DebtMetrics.pca$rotation))|>
  select(DebtMetric,everything())
# write_xlsx(Loadings, 'Question10/loadings.xlsx')
## See loadings.xlsx for color-coded loadings map

var_explained <- DebtMetrics.pca$sdev^2 / sum(DebtMetrics.pca$sdev^2) * 100
cumvar        <- cumsum(var_explained)

Cumulative.pca <- data.frame(PC         = paste0("PC", 1:10),
                             individual = round(var_explained[1:10], 2),
                             cumulative = round(cumvar[1:10], 2))
# Variance Explained Plot
Cumulative.pca |>
  mutate(PC = factor(PC, levels = PC)) |>
  pivot_longer(cols = c(individual, cumulative),
               names_to = "type", values_to = "variance") |>
  mutate(type = recode(type,
                       "individual" = "Individual %",
                       "cumulative" = "Cumulative %")) |>
  ggplot(aes(x = PC)) +
  geom_col(data = ~ filter(.x, type == "Individual %"),
           aes(y = variance, fill = type), width = 0.6) +
  geom_line(data = ~ filter(.x, type == "Cumulative %"),
            aes(y = variance, group = 1, color = type), linewidth = 0.8) +
  geom_point(data = ~ filter(.x, type == "Cumulative %"),
             aes(y = variance, color = type), size = 2) +
  geom_hline(yintercept = 75, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = length(Cumulative.pca), y = 77, label = "75%", size = 3, color = "gray50") +
  scale_fill_manual(values = c("Individual %" = "#85B7EB")) +
  scale_color_manual(values = c("Cumulative %" = "#D85A30")) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title = "FIGURE 10.1 - Variance Explained per Principal Component",
    x = NULL, y = "Variance explained",
    fill = NULL, color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

scores <- as.data.frame(DebtMetrics.pca$x[,1:4]) |>
  mutate(FIPS = DebtMetrics[[1]])

# Joining section 10 with full CFPB dataset, and removing all but Census variables
CFPB10 <- CFPB9[,-c(25:45,50:58,60:71)] |>
  left_join(scores,by="FIPS")
}
#11.
{
  # 5 clusters
  #creating a matrix for mixed type cluster analysis
  #using share of people of color, average household income, whether older or younger, Principal Component 1, is_older_county, and whether a legislature is republican controlled or not
  # Create a named vector of Republican-controlled legislatures in 2024
  #republican_states <- c("AL", "AZ", "AR", "FL", "GA", "ID", "IN", "IA", 
  #"KS", "KY", "LA", "MS", "MO", "MT", "NH", "ND", 
  #"OH", "OK", "SC", "SD", "TN", "TX", "UT", "WV", "WY",
  #"WI", "NC")
  
  #CFPB$rep_legislature <- as.factor(ifelse(CFPB$State %in% republican_states, 1, 0))
  matrix1 <- bind_cols(c(CFPB10[,c("PC1", "Year", "Issue","Share of people of color","prop_young", "prop_65plus")]))
  CFPB_clust<- matrix1
  #I tried adding the variables I got from sahie but it made the lambda so large I don't think it's worth it (498314632027)
  #CFPB_clust$is_servicemember <- as.factor(CFPB_clust$is_servicemember)
  #CFPB_clust$is_older_county <- as.factor(CFPB_clust$is_older_county)
  #removing the older county thing here actually seems to improve it
  #for whatever reason (idk why) but adding issue and sub.issue as categorical variables is lowering the lambda here
  #my republic legislature thing also seemed to make it worse
  #the household income variable was shooting up the lambda to several million, deleting this greatly improved it
  kpres <- kproto(x = CFPB_clust, k = 5)
  #managed to get it down to 2.4
  kpres
  summary(kpres)
  library(wesanderson)
  #par(mfrow=c(2,2))
  par(mfrow = c(1,1))
  #choosing 5 clusters for now but can later find an optimal amount with lambdaest()
  complete_idx <- complete.cases(CFPB_clust)
  CFPB_clust_complete <- CFPB_clust[complete_idx, ]
  #this aint workin for some reason
  #clprofiles(kpres, CFPB_clust_complete,
  #col = wes_palette("Royal1", 5, type = "continuous")) # figure 1
  #plot(kpres)
  #Save cluster assignments
  CFPB10$cluster <- NA
  CFPB10$cluster[complete_idx] <- kpres$cluster
  CFPB10$cluster <- as.factor(CFPB10$cluster)
  CFPB11 <- CFPB10
  
}
#12
{
#random forest imputation
set.seed(12345)
sapply(CFPB11, class)
CFPBimpute <- CFPB11 %>%
  mutate(across(where(is.character), as.factor))%>%
  mutate(across(where(is.logical), as.factor))%>%
  mutate(across(where(~ inherits(., "Date")), as.numeric))
sapply(CFPBimpute[sapply(CFPBimpute, is.factor)], nlevels) %>% 
  sort(decreasing = TRUE) %>% 
  head(20)
#all of these had more than 53 categories (alot more) so I dropped them
## I wouldn't drop all of these. Since we have 60K observations, ZIP code could be predictive
drop_cols_CFPB <- c(
  "ZIP",
  "FIPS",
  "Company",
  "County_Name",
  "CI Index Score",
  "Not Credit Included",
  "Credit Constrained"
  )
CFPBimpute <- CFPBimpute %>%
  select(-all_of(drop_cols_CFPB)) %>%
  mutate(across(where(is.character), as.factor))
CFPBimpute$Received <- as.numeric(CFPBimpute$Received)
CFPBimpute$Sent <- as.numeric(CFPBimpute$Sent)
CFPBimpute$qtr <- as.numeric(CFPBimpute$qtr)
#CFPBimpute<- rfImpute(Cust.response~., iter = 5, ntree = 50 ,data=CFPBimpute)
sapply(CFPBimpute, class)
#for whatever reason these were factors not numeric
# cols_to_convert <- c(
#   "Share with medical debt in collections",
#   "Median medical debt in collections in $2023",
#   "Hospital market concentration (HHI)",
#   "Number of Closures and Mergers",
#   "Share of the population with no health insurance coverage",
#   "Share of non-elderly adults with a reported disability",
#   "Average household income in $2023",
#   "Median medical debt in collections in $2023 - Majority White",
#   "Median medical debt in collections in $2023 - Majority of Color",
#   "Share with medical debt in collections - Majority White",
#   "Share with medical debt in collections - Majority of Color",
#   "Median medical debt in collections in $2023"
# )
# CFPBimpute <- CFPBimpute %>%
#   mutate(across(all_of(cols_to_convert), ~ as.numeric(as.character(.x))))
#since rfImpute cant impute y and y is what we want to predict in the future I'm going to do a median imputation on y and random forest on everything else
#pre_median <- preProcess(CFPBimpute[, "Median medical debt in collections in $2023", drop = FALSE], 
#method = "medianImpute")
#imputedmeddebt <- predict(pre_median, CFPBimpute[, "Median medical debt in collections in $2023", drop = FALSE])
#CFPBimpute$`Median medical debt in collections in $2023` <- imputedmeddebt[[1]]
#hitting vector ceiling on everything need to take a sample
#CFPBimpute <- rfImpute(`Median medical debt in collections in $2023` ~ ., iter = 3, ntree = 20, 
#maxnodes = 50, data = CFPBimpute)

#CFPBimputed <- missForest(
#CFPBimpute,
#ntree = 20,
#maxiter = 3,
#maxnodes = 50
#)
CFPBimpute$Relief <- as.factor(CFPBimpute$Relief)
na_cols <- names(which(colSums(is.na(CFPBimpute)) > 0))
set.seed(12345)
idx <- sample(nrow(CFPBimpute), 10000)
CFPBsample <- CFPBimpute[idx, ]
CFPBsample_imputed <- rfImpute(
  Relief ~ .,
  iter     = 3,
  ntree    = 20,
  maxnodes = 50,
  data     = CFPBsample
)
# Build a random forest on the imputed sample for each NA column,
# then predict into the full dataset

CFPBimpute_out <- CFPBimpute

for (col in na_cols) {
  
  missing_idx <- which(is.na(CFPBimpute_out[[col]]))
  if (length(missing_idx) == 0) next
  
  # Predictors: everything except the target column
  predictors <- setdiff(names(CFPBsample_imputed), col)
  
  # Train RF on the clean imputed sample
  rf_model <- randomForest(
    x        = CFPBsample_imputed[, predictors],
    y        = CFPBsample_imputed[[col]],
    ntree    = 20,
    maxnodes = 50
  )
  
  # Predict only for rows missing this column
  # Use other already-imputed cols where possible, median/mode fill any remaining NAs in predictors
  newdata <- CFPBimpute_out[missing_idx, predictors]
  
  for (p in predictors) {
    if (any(is.na(newdata[[p]]))) {
      if (is.numeric(newdata[[p]])) {
        newdata[[p]][is.na(newdata[[p]])] <- median(CFPBsample_imputed[[p]], na.rm = TRUE)
      } else {
        mode_val <- names(sort(table(CFPBsample_imputed[[p]]), decreasing = TRUE))[1]
        newdata[[p]][is.na(newdata[[p]])] <- mode_val
      }
    }
  }
  
  CFPBimpute_out[[col]][missing_idx] <- predict(rf_model, newdata = newdata)
}
}
CFPB <- CFPBimpute_out
rm(list = setdiff(ls(), "CFPB"))
gc()
