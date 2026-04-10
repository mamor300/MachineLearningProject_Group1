setwd('/Users/mattamor/Library/CloudStorage/OneDrive-Personal/School/ECON 6378 - Machines/DataProject/MachineLearningProject_Group1')

pacman::p_load(
  readr,
  readxl,
  writexl,
  tidyverse,
  VIM,
  caret,
  missMDA)

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
{## First major cleaning of CFPB data
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
         ZIP.missing                  = ifelse(nchar(ZIP) < 5 | grepl("XX$", ZIP), 1, 0),
         missing                      = as.numeric(ifelse(!complete.cases(CFPB2),1,0)))|>
  left_join(ZIPCODES |> select(ZIP, FIPS), by = "ZIP")|>
  relocate(c(ZIP,ZIP.missing,FIPS),.after = State)|>
  mutate(FIPS = as.factor(FIPS))|>
  rename(Received      = Date.received,
         Sent          = Date.sent.to.company,
         Cust.response = Company.response.to.consumer,
         Pub.response  = Company.public.response,
         Consent       = Consumer.consent.provided.,
         Timely        = Timely.response.)|>
  mutate(Wait.time = as.numeric(Sent - Received)) |>
  select(-Product,
         -Sub.product,
         -Consumer.disputed.,
         -Consumer.complaint.narrative,
         -ZIP.Imputed,
         -ZIP.code)|>
  select(Relief, Received, Sent, Year, Wait.time,everything())
# Verifying that the only incomplete cases are ones which did not impute
# summary(!complete.cases(CFPB3))
}
#5.
{
#Adding Fed measure for household debt by county 
county_debt<- read.csv("household-debt-by-county.csv")
#Clearing the rows that are not 2020-2025
#CFPB.household_debt<- make new data frame with this for question 5
#using the household debt data, cleaning and formatting it from 
#long to wide

##Reshaping the county_debt data to be merged with CFPB
colnames(CFPB)
colnames(county_debt)

#Alignging the FIPS codes to be the same in both data sets 
CFPB <- CFPB3 %>% 
  mutate(FIPS = str_pad(as.character(FIPS), width = 5, pad = '0'))
county_debt <- county_debt %>% 
  mutate(area_fips = str_pad(as.character(area_fips), width = 5, pad = '0'))

#Pivot from long to wide, each row will represent one county/year/quarter combo
#with 'low' and 'high' debt columns 

CFPB <- CFPB %>% 
  mutate(
    Received = as.Date(Received, format = '%Y-%m-%d'), 
    year = as.integer(format(Received, '%Y')), 
    qtr = quarter(Received)
  )

merged_debt_county <- CFPB %>% 
  left_join(county_debt, by = c('FIPS' = 'area_fips', 'year', 'qtr'))
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

# Joining CFPB and Debt data for inspection
CFPB.debt <- CFPB3 |>
  left_join(DebtMetrics.clean |>
              select(-`County Name`,-`State Name`),
            by ="FIPS")

#rm(CFPB.debt,AutoRetail,OverallDebt,StudentLoan,ZIPCODES)
}
#7.
{
INSECURE0 <- read_xlsx("Question07/credit-insecurity-index-data-workbook.xlsx", sheet = "County")
INSECURE <- INSECURE0 |>
  pivot_longer(cols = "2018":"2023",
               names_to = "Year",
               values_to = "value") |>
  mutate(Year = as.integer(Year))|>
  pivot_wider(names_from = `Credit Insecurity Measure`,
              values_from = value)|>
  rename(FIPS = GEOID)
# Joining credit insecurity data with CFPB
## The only years of overlap are 2022 and 2023. This filters everything else out
CFPB.insecurity <- CFPB3 |>
  left_join(INSECURE |>
              select(-`County Name`,-State),
            by = c("FIPS","Year"))|>
  filter(Year %in% c("2018":"2023"))
#rm(CFPB.insecurity,INSECURE0)

# Joining Debt and Credit Insecurity datasets to CFPB
## Previous CFPB data frame and references to it are changed to CFPB3 
CFPB4 <- CFPB3 |>
  left_join(INSECURE |>
              select(-`County Name`,-State),
            by = c("FIPS","Year"))|>
  select(-ZIP.missing)
#rm(INSECURE, DebtMetrics,CFPB2,CFPB3)
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
CFPB.FMR <- CFPB4 |>
  left_join(FMR,by = c('FIPS',"Year"))
}
#9.
{
installed.packages('tidycensus')
library("tidyverse")
library('tidycensus')
library(tidycensus)
library(tidyverse)

library(tidycensus)
library(tidyverse)

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
2. Feature Engineering
Now we create the proportions and the Older County dummy.

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
CFPB.FMR <- CFPB.FMR %>%
  mutate(
    is_servicemember = if_else(grepl("Servicemember", Tags, ignore.case = TRUE), 1, 0),
    is_older_american = if_else(grepl("Older American", Tags, ignore.case = TRUE), 1, 0),
    # Ensure FIPS matches Census format
    FIPS = sprintf("%05s", as.character(FIPS))
  )

# --- F. The Final Join ---
CFPB_Census <- CFPB.FMR %>%
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

# setting neighborhoods as row names
DebtMetrics1 <- DebtMetrics[,-c(1:3)]
DMClean <- DebtMetrics1 |>
  mutate(across(everything(),~ as.numeric(gsub(",", "", .x))))

# colMeans(is.na(DMClean)) |> sort(decreasing = TRUE)

# Removing variables with over 30% missing - keeping 23 out of 50 variables
missingRate <- colMeans(is.na(DMClean))
DMClean <- DMClean[,missingRate <= 0.3]

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

# Joining PCs with full dataset
scores <- as.data.frame(DebtMetrics.pca$x[,1:4]) |>
  mutate(FIPS = DebtMetrics[[1]])

CFPB <- CFPB4 |>
  left_join(scores,by="FIPS")
}