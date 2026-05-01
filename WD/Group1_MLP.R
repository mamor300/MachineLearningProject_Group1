setwd("/Users/mattamor/MachineLearningProject_Group1/WD")
# Installing Packages and Loading Libraries
{
install.packages("caret")
install.packages("randomForest")
install.packages("rpart.plot")
install.packages("RANN")
install.packages("pacman")

pacman::p_load(
  readr,
  readxl,
  writexl,
  tidyverse,
  ggplot2,
  VIM,
  caret,
  missMDA,
  tidycensus,
  randomForest,
  clustMixType,
  censusapi,
  forcats,
  car,
  rpart,
  rpart.plot,
  RANN,
  randomForestExplainer,
  xgboost,
  Matrix,
  data.table,
  DiagrammeR,
  glmnet)
}
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
  # Imputing with K Nearest Neighbors
  # Flaggingg missing ZIPs in a new column
  df <- CFPB1 %>%
    mutate(ZIP.char    = as.character(ZIP.code),
           prefix3     = substr(ZIP.char, 1, 3),
           ZIP.missing = ifelse(nchar(ZIP.code) < 5 | grepl("X$", ZIP.code), 1, 0),
           ZIP.num     = ifelse(ZIP.missing == 0, as.numeric(ZIP.char), NA))
  
  # Building a dataframe that KNN can run on using only original columns
  ## Limiting factors that might effect imputation to ones that are location-based
  knn_df <- df %>%
    mutate(
      State       = as.factor(State),
      Company     = as.factor(Company),
      prefix3     = as.factor(prefix3)
    ) %>%
    select(ZIP.num, State, Company, prefix3)
  
  # Running KNN imputation
  knn_result <- kNN(knn_df, variable = "ZIP.num", k = 5)
  
  # Creating valid_zips df for reference 
  valid_zips <- ZIPCODES %>%
    mutate(ZIP.char   = formatC(as.numeric(ZIP), width = 5, flag = "0", format = "d"),
           prefix3    = substr(ZIP.char, 1, 3),
           ZIP.num    = as.numeric(ZIP.char)) %>%
    select(prefix3, ZIP.char, ZIP.num)
  
  # Building a function to snap to nearest prefix and valid ZIP in ZIPCODES
  valid_zips_vec <- valid_zips$ZIP.num  # numeric vector for fast lookup
  
  snap_to_valid <- function(zip_num, prefix = NULL) {
    if (!is.null(prefix) && nchar(prefix) == 3) {
      candidates <- valid_zips[valid_zips$prefix3 == prefix, ]
    } else {
      candidates <- valid_zips  # fallback: no prefix restriction
    }
    
    if (nrow(candidates) == 0) {
      candidates <- valid_zips
    }
    
    candidates$ZIP.char[which.min(abs(candidates$ZIP.num - zip_num))]
  }
  
  # Joining imputed ZIP codes with full dataset
  CFPB2 <- df %>%
    mutate(ZIP.knn.raw = round(knn_result$ZIP.num),
           imputed = ifelse(knn_result$ZIP.num_imp==TRUE,1,0)) %>%
    rowwise() %>%
    mutate(
      ZIP.knn = if (ZIP.missing == 1) {
        snap_to_valid(ZIP.knn.raw, prefix = prefix3)  
      } else {
        snap_to_valid(ZIP.knn.raw)                     
      }
    ) %>%
    ungroup()%>%
    mutate(ZIP.knn.ch = as.character(ZIP.knn),
           ZIP.missing = ifelse(nchar(ZIP.knn) < 5 | grepl("X$", ZIP.knn), 1, 0),
           ZIP.knn = as.factor(ZIP.knn.ch),
           ZIP.match = as.numeric(substr(ZIP.knn.ch, 1, 3) != substr(ZIP.code, 1, 3))) %>%
    left_join(ZIPCODES[,c(2,4)],join_by("ZIP.knn.ch"=="ZIP"))%>%
    mutate(ZIP.state.match = ifelse(STATE==State,0,1))%>%
    select(c(Relief,State,ZIP.knn,everything()))%>%
    select(-c('ZIP.match',
              'ZIP.missing',
              'ZIP.code',
              'ZIP.knn.ch',
              'STATE',
              'ZIP.num',
              'prefix3',
              'ZIP.state.match','ZIP.match',
              'ZIP.missing',
              'ZIP.code',
              'ZIP.knn.ch',
              'STATE',
              'ZIP.num',
              'prefix3',
              'ZIP.state.match',
              'ZIP.char',
              'ZIP.knn.raw'))
}
#3.
{
  ## First major cleaning of CFPB data
  CFPB3 <- CFPB2 |>
    mutate(Date.received                = as.Date(Date.received,"%m/%d/%y"),
           Date.sent.to.company         = as.Date(Date.sent.to.company,"%m/%d/%y"),
           Year                         = as.factor(year(Date.received)),
           Month                        = as.factor(month(Date.received)),
           Issue                        = as.factor(Issue),
           Sub.issue                    = as.factor(Sub.issue),
           Company.public.response      = as.factor(Company.public.response),
           Company                      = as.factor(Company),
           State                        = as.factor(State),
           Tags                         = as.factor(Tags),
           Consumer.consent.provided.   = as.factor(Consumer.consent.provided.),
           Submitted.via                = as.factor(Submitted.via),
           Timely.response.             = as.factor(Timely.response.),
           ZIP                          = ZIP.knn)|>
    left_join(ZIPCODES |> select(ZIP, FIPS), by = "ZIP")|>
    relocate(c(ZIP,FIPS),.after = State)|>
    mutate(FIPS = as.factor(FIPS))|>
    rename(Received      = Date.received,
           Sent          = Date.sent.to.company,
           Pub.response  = Company.public.response,
           Consent       = Consumer.consent.provided.,
           Timely        = Timely.response.)|>
    select(-Product,
           -Sub.product,
           -Consumer.disputed.,
           -Consumer.complaint.narrative,
           -ZIP.knn,
           -Company.response.to.consumer,
           -Complaint.ID)|>
    mutate(Wait.time = as.numeric(Sent - Received))|>
    select(Relief, Received, Sent, Year, Wait.time,everything())
}
#4.
{
  # Medical debt
  med_debt_raw <- read_excel("changing_med_debt_landscape_county.xlsx", 
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
  colMeans(is.na(CFPB4))
  # median impute for remaining missing values (about 17% of medical debt)
  CFPB4 <- na.roughfix(CFPB4[19:25])
}
#5.
{
  #Adding Fed measure for household debt by county 
  county_debt <- read_csv("household-debt-by-county.csv")
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
  AutoRetail <- read_excel("Debt in America County-Level AutoRetail Debt.xlsx")
  StudentLoan <- read_xlsx("Debt in America County-Level Student Loan Debt.xlsx")
  OverallDebt <- read_xlsx("Debt in America County-Level Overall Debt.xlsx")
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
  INSECURE0 <- read_xlsx("credit-insecurity-index-data-workbook.xlsx", sheet = "County")
  tier_lookup <- c(
    "Credit At Risk"  = 1,
    "Credit Insecure" = 2,   
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
  # Fair Market Rent
  {
  # Importing, cleaning, combining fair market rent data from 
  FMR22 <- read_xlsx("FY22_FMRs_revised.xlsx")|>
    rename(fips = fips2010,
           "Studio_2022"    = fmr_0,
           "OneRoom_2022"   = fmr_1,
           "TwoRoom_2022"   = fmr_2,
           "ThreeRoom_2022" = fmr_3,
           "FourRoom_2022"  = fmr_4)
  FMR23 <- read_xlsx("FY23_FMRs_revised.xlsx") |>
    rename("Studio_2023"    = fmr_0,
           "OneRoom_2023"   = fmr_1,
           "TwoRoom_2023"   = fmr_2,
           "ThreeRoom_2023" = fmr_3,
           "FourRoom_2023"  = fmr_4)
  FMR24 <- read_xlsx("FMR2024_final_revised.xlsx") |>
    rename("Studio_2024"    = fmr_0,
           "OneRoom_2024"   = fmr_1,
           "TwoRoom_2024"   = fmr_2,
           "ThreeRoom_2024" = fmr_3,
           "FourRoom_2024"  = fmr_4)
  FMR25 <- read_xlsx("FY25_FMRs_revised.xlsx")|>
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
  }
  # Building Permits
  {
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
  }
  # Percent Uninsured
  {
  # This should show the counts for each year 
  table(CFPB.bps$Year)
  Sys.setenv(CENSUS_KEY = "ba79958600ff02f01da8a857d6a3243c191cfc8a")
  sahie_vars <- listCensusMetadata(
    name = "timeseries/healthins/sahie",
    type = "variables"
  )
  
  #no 2024 or 2025 data
  sahie <- map_dfr(2022:2023, ~getCensus(
    name   = "timeseries/healthins/sahie",
    vars   = c("NAME", "PCTUI_PT", "YEAR"),
    region = "county:*",
    time   = .x
  ))|>
    mutate(fips = paste0(state, county)) |>
    select(-time) |>
    rename(Year = YEAR, FIPS = fips)
  sahie$Year <- as.numeric(sahie$Year)
  CFPB.sahie <- CFPB.bps |>
    left_join(sahie,by = c('FIPS',"Year"))
  }
  # Percent 25+ with Bachelor's Degree or Higher
  {
    # https://data.census.gov/table?q=S1501:+Educational+Attainment&g=010XX00US$0500000
    # --- 1. SETUP FILENAMES ---
    edu_files <- c(
      "ACSST5Y2020.S1501-Data.csv",
      "ACSST5Y2021.S1501-Data.csv",
      "ACSST5Y2022.S1501-Data.csv",
      "ACSST5Y2023.S1501-Data.csv",
      "ACSST5Y2024.S1501-Data.csv")
    
    # https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-total.html
    pop_file <- "co-est2025-alldata.csv"
    
    process_edu <- function(file_path) {
      # Extract year from the file name
      year_val <- str_extract(file_path, "\\d{4}")
      
      # Load data: Skip 2 rows to get to data, then manually apply headers from row 1
      data <- read_csv(file_path, skip = 2, col_names = FALSE)
      headers <- names(read_csv(file_path, n_max = 0))
      colnames(data) <- headers
      
      data %>%
        mutate(
          fips = str_sub(GEO_ID, -5),
          year = as.numeric(year_val)
        ) %>%
        # S1501_C01_006E: Total Population 25 years and over
        # S1501_C01_015E: Bachelor's degree or higher
        select(fips, NAME, year, 
               total_25plus = S1501_C01_006E, 
               bach_or_higher = S1501_C01_015E) %>%
        mutate(pct_bach_degree = (bach_or_higher / total_25plus) * 100)
    }
    
    # COMBINE ---
    all_edu_data <- map_df(edu_files, process_edu)
    
    # --- 4. PROCESS POPULATION DATA (The Bridge) ---
    # Skip 3 rows: Title, Headers, and the "United States" total row
    pop_data <- read_csv(pop_file, skip = 3) %>%
      rename(county_full_name = 1) %>%
      # Clean the ".Autauga County, Alabama" format to "Autauga County, Alabama"
      mutate(match_name = str_remove(county_full_name, "^\\.")) 
    
    # FINAL JOIN & CLEANUP ---
    final_compiled_data <- all_edu_data %>%
      left_join(pop_data, by = c("NAME" = "match_name")) %>%
      select(fips, NAME, year, total_25plus, bach_or_higher, pct_bach_degree)
    
    # --- 6. EXPORT THE RESULT ---
    # Saves the compiled file to the current working directory
    output_filename <- "compiled_county_education_measures.csv"
    write_csv(final_compiled_data, output_filename)
    
  compiled_county_education_measures <- read_csv("compiled_county_education_measures.csv")|>
    select(fips,pct_bach_degree,year)|>
    rename(FIPS = fips,
           Year = year)
  
  # Final Join for Section 8
  CFPB8 <- CFPB.sahie |>
    left_join(compiled_county_education_measures,by= c("FIPS","Year"))
  }
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
    left_join(census_features, by = "FIPS")
  #rm(age_clean, age_vars, all_bps_years, all_bps_years_clean, census_demographics, 
  #  census_features, census_raw, census_scaled, CFPB_Final_Analysis, CFPB_Final_Bias, 
  #  CFPB_with_Permits)
  gc()
}
#10.
{
  # PCA on debt collection variables
  ## Note the following is a PCA on the county-level debt collection metrics
  ## There is severe missingness in this dataset
  
  # Setting neighborhoods as row names
  ## You'll see 49 warnings for NAs introduced. This is by design
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
    mutate(type = recode_values(type,
                         "individual" ~ "Individual %",
                         "cumulative" ~ "Cumulative %")) |>
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
  
  CFPB.nocensus <- c(
    'Share with any debt in collections, All',
    'Share with any debt in collections, Comm of color',
    'Share with any debt in collections, White comm',
    'Median debt in collections, All',
    'Median debt in collections, Comm of color',
    'Median debt in collections, White comm',
    'Share with medical debt in collections, All',
    'Share with medical debt in collections, Comm of color',
    'Share with medical debt in collections, White comm',
    'Student loan delinquency rate (60+), All',
    'Student loan delinquency rate (60+), Comm of color',
    'Student loan delinquency rate (60+), White comm',
    'Auto/retail loan delinquency rate, All',
    'Auto/retail loan delinquency rate, Comm of color',
    'Auto/retail loan delinquency rate, White comm',
    'Credit card debt delinquency rate, All',
    'Credit card debt delinquency rate, Comm of color',
    'Credit card debt delinquency rate, White comm',
    'Median credit card delinquent debt, All',
    'Median credit card delinquent debt, Comm of color',
    'Median credit card delinquent debt, White comm',
    'Auto/retail loan delinquency rate by credit score - Subprime, All',
    'Auto/retail loan delinquency rate by credit score - Near prime, All',
    'Auto/retail loan delinquency rate by credit score - Prime, All',
    'Share with auto loans, All',
    'Share with auto loans, Comm of color',
    'Share with auto loans, White comm',
    'Share with auto/retail loan debt, All',
    'Share with auto/retail loan debt, Comm of color',
    'Share with auto/retail loan debt, White comm',
    'Share of people in rural areas, White comm',
    'Share with student loan debt, All',
    'Share with student loan debt, Comm of color',
    'Share with student loan debt, White comm',
    'Median student loan debt, All',
    'Median student loan debt, Comm of color',
    'Median student loan debt, White comm',
    'Median student loan deliquent debt, All',
    'Median student loan deliquent debt, Comm of color',
    'Median student loan deliquent debt, White comm',
    'Median monthly student loan payment, All',
    'Median monthly student loan payment, Comm of color',
    'Median monthly student loan payment, White comm'
  )
  DebtMetrics.names <- names(DebtMetrics)[c(4:24,26:53)]
  
  # Joining section 10 with full CFPB dataset, and removing all but Census variables
  CFPB10 <- CFPB9 |>
    select(-all_of(DebtMetrics.names),
           -County_Name)|>
    left_join(scores,by="FIPS")
}
#11.
{
  # 5 clusters
  #creating a matrix for mixed type cluster analysis
  #using share of people of color, 
  # average household income, whether older or younger, 
  # Principal Component 1, 
  # is_older_county, and 
  # whether a legislature is republican controlled or not
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
  # Random Forest
  set.seed(12345)
  CFPB.mutate <- CFPB11 %>%
    mutate(across(where(is.character), as.factor),
           across(where(is.logical), as.factor),
           across(where(~ inherits(., "Date")), as.numeric),
           `CI Index Score`= as.numeric(`CI Index Score`),
           `Not Credit Included` = as.numeric(`Not Credit Included`),
           `Credit Constrained` = as.numeric(`Credit Constrained`))
  drop_cols_CFPB <- c(
    "ZIP",
    "FIPS",
    "Company",
    "NAME",
    "county"
  )
  CFPBimpute <- CFPB.mutate[,!names(CFPB.mutate) %in% drop_cols_CFPB]
  CFPBheldout <- CFPB.mutate[,names(CFPB.mutate) %in% drop_cols_CFPB]
  CFPBimpute$Relief <- as.factor(CFPBimpute$Relief)
  na_cols <- names(which(colSums(is.na(CFPBimpute)) > 0))
  set.seed(2345)
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
    
    #use this below as the y if you changed the seed in the sample
    train_y <- CFPBsample_imputed[[col]]
    is_categorical <- is.factor(train_y) || length(unique(na.omit(train_y))) <= 5
    if (is_categorical) {
      train_y <- as.factor(train_y)
    }
    if (is.factor(train_y)) train_y <- droplevels(train_y)
    
    # Train RF on the clean imputed sample
    rf_model <- randomForest(
      x        = CFPBsample_imputed[, predictors],
      y        = train_y,#CFPBsample_imputed[[col]],
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
  CFPB12 <- CFPBimpute_out|>
    cbind(CFPB11$Company)|>
    rename(Company = "CFPB11$Company")
}
# Output
{
CFPB <- CFPB12|>
  mutate(
    Issue = as.character(Issue),
    Sub.issue = as.character(Sub.issue),
    Issue_combined = paste0(Issue,Sub.issue))|>
  rename(MorePermits = high,
         LessPermits = low,
         Quarter = qtr)|>
  select(-c(Wait.time,
            PC1,
            PC2,
            PC3,
            PC4,
            Issue,
            Sub.issue,
            Received,
            Sent,
            Tags,
            sample,
            state,
            imputed))|>
  mutate(Company = as.character(Company),
         freq = n(),
         .by = Company)|>
  mutate(Company = case_when(
    freq     >= 10 ~ Company,
    .default = paste0("Other_", freq)),
    Company  = as.factor(Company))|>
  select(-freq)|>
  rename(ShareOfPeopleOfColor = `Share of people of color`,
         CI_score = `CI Index Score`,
         NotCreditIncluded = `Not Credit Included`,
         CreditConstrained = `Credit Constrained`)

write_xlsx(CFPB, "CFPB.xlsx")
saveRDS(CFPB,"CFPB.rds")
}
# Removing Objects not needed for the Models
{rm(list = setdiff(ls(), "CFPB"))
gc()}
# Models - Not Chosen
{
  # Ordinary Least Squares
  {
    df <- CFPB
    
    # Ensure Relief is treated as numeric
    df$Relief <- as.numeric(as.character(df$Relief))
    
    ols_full <- lm(Relief ~ .,
                   data = df)
    
    # summary(ols_full)
    
    # Prediction for accuracy
    CFPB.test <- CFPB
    CFPB.pred.raw <- predict(ols_full, newdata = CFPB.test)
    CFPB.pred <- factor(round(CFPB.pred.raw),levels = c("0","1"))
    confusionMatrix(CFPB.pred,reference = CFPB.test$Relief, mode = "everything")
  }
  # Logit
  {
    df <- CFPB
    logit_full <- glm(Relief ~ .,
                      data = df,
                      family = binomial(link = "logit"))
    #summary(logit_full)
    
    # Generate predicted probabilities
    fitted_probs <- predict(logit_full, type = "response")
    
    # Convert to binary predictions using 0.5 threshold
    predicted_class <- ifelse(fitted_probs > 0.5, 1, 0)
    
    # Actual values
    actual <- as.numeric(df$Relief) - 1  # converts factor to 0/1
    
    # Accuracy
    accuracy <- mean(predicted_class == actual)
    
    # Confusion matrix for more detail
    table(Actual = actual, Predicted = predicted_class)
  }
  # Lasso
  {
    train_data <- CFPB
    # 1. Align and Clean Data (The fix for the TRUE/FALSE error)
    # We create the matrix first; it automatically handles NAs in predictors
    X_dense <- model.matrix(Relief ~ . - 1, data = train_data)
    
    # We subset Y to match the rows that survived in X
    Y_train <- train_data$Relief[as.numeric(rownames(X_dense))]
    keep_idx <- !is.na(Y_train)
    X_final  <- Matrix(X_dense[keep_idx, ], sparse = TRUE)
    Y_final  <- Y_train[keep_idx]
    
    # 2. LOGISTIC LASSO (Part C)
    set.seed(12345)
    cv_lasso <- cv.glmnet(
      x = X_final, 
      y = Y_final, 
      family = "binomial", 
      alpha = 1,
      nfolds = 10,
      type.measure = "auc"
    )
    
    # Output for Part C
    plot(cv_lasso)
    best_coefs <- coef(cv_lasso, s = "lambda.min")
    
    # 1. Convert the sparse matrix to a standard data frame
    coef_matrix <- as.matrix(best_coefs)
    active_variables <- data.frame(
      Variable = rownames(coef_matrix),
      Coefficient = coef_matrix[,1]
    )
    
    # 2. Filter out the zeros (the variables Lasso discarded)
    significant_drivers <- active_variables[active_variables$Coefficient != 0, ]
    
    # Confusion Matrix
    CFPB.test <- model.matrix(Relief ~ . - 1, data = CFPB)
    CFPB.pred.raw <- predict(cv_lasso, newx = CFPB.test, s = "lambda.min", type = "response")
    CFPB.pred <- factor(round(CFPB.pred.raw),levels = c("0","1"))
    confusionMatrix(CFPB.pred,reference = CFPB$Relief, mode = "everything")
  }
  # CART
  {
    # 1. UPDATED HYBRID WRAPPING FUNCTION
    # Caps the root at 40 chars, wraps every 12, line-to-line verticality.
    wrap_and_cap <- function(x, labs, digits, varlen, faclen) {
      labs <- sapply(labs, function(l) {
        if (nchar(l) > 40) {
          l <- paste0(substr(l, 1, 40), "...")
        }
        paste(strwrap(l, width = 12), collapse = "\n")
      })
      return(labs)
    }
    
    # 2. RUN MODELS AT THREE RESOLUTIONS
    set.seed(12345)
    
    train_data_final <- CFPB
    # Resolution A: Executive (Depth 2)
    tree_exec <- rpart(Relief ~ ., data = train_data_final, method = "class",
                       control = rpart.control(cp = 0.01, maxdepth = 2))
    
    # Resolution B: Intermediate (Depth 4) - NEW
    tree_inter <- rpart(Relief ~ ., data = train_data_final, method = "class",
                        control = rpart.control(cp = 0.005, maxdepth = 4))
    
    # Resolution C: Deep Forensic (Depth 6)
    tree_deep <- rpart(Relief ~ ., data = train_data_final, method = "class",
                       control = rpart.control(cp = 0.0005, maxdepth = 6))
    
    # 3. PLOTTING FUNCTION
    plot_audit <- function(model, title) {
      prp(model, extra = 101, box.palette = "RdYlGn", 
          split.fun = wrap_and_cap, faclen = 0, varlen = 0, 
          nn = TRUE, main = title)
    }
    
    # Generate the Set
    plot_audit(tree_exec, "I. Executive Snapshot (High-Level Filter)")
    plot_audit(tree_inter, "II. Intermediate Summary (Structural Drivers)")
    plot_audit(tree_deep, "III. Deep Forensic Audit (Granular Evidence)")
    
    # Confusion Matrix
    CFPB.test <- CFPB
    CFPB.pred <- as.factor(predict(tree_deep, newdata = CFPB.test,type = "class"))
    confusionMatrix(CFPB.pred,reference = CFPB.test$Relief, mode = "everything")
    
     }
  # Random Forest
  {
    CFPB <- readRDS("CFPB.rds")
    
    set.seed(124)
    df <- CFPB[sample(1:nrow(CFPB), 20000),]
    
    # Single Random Forest - Commented to avoid rerunning
    ctrl <- trainControl(method = "repeatedcv")
    tunegrid <- expand.grid(.mtry = (10:17))
    CFPB.rf <- train(Relief ~ .,
                     data = df,
                     method = 'rf',
                     metric = 'Accuracy',
                     trControl = ctrl,
                     tuneGrid = tunegrid,
                     importance = TRUE,
                     ntree = 500)
    saveRDS(CFPB.rf,"CFPB_rf.rds")
    CFPB.rf <- readRDS("RandomForest/CFPB_rf.rds")
    CFPB.rf$finalModel
    # Testing model on full dataset
    CFPB.test <- CFPB
    CFPB.pred <- predict(CFPB.rf, newdata = CFPB.test)
    confusionMatrix(CFPB.pred,reference = CFPB.test$Relief, mode = "everything")
  }
  # xgBoost
  {
    #XGBoost model
    CFPB <- read_excel("CFPB.xlsx")
    y <- as.numeric(CFPB$Relief)
    y_factor <- as.factor(CFPB$Relief)
    #this is removing tags from the xgboost model parameters
    #I had to do this colnames thing since sparse.model.matrix was giving a name mismatch error
    colnames(CFPB) <- make.names(colnames(CFPB))
    x <- sparse.model.matrix(Relief ~ ., CFPB)
    train_control <- trainControl(method = "cv", number = 5)
    XGBoostdata <- xgb.DMatrix(data = x, label = y)
    XGBparams <- list(
      booster = "gbtree",
      objective = "binary:logistic",
      eta = 0.05,
      max_depth = 6,
      min_child_weight = 10,
      max_delta_step = 0,
      gamma = 0,
      colsample_bytree = 1,
      subsample = 0.8,
      verbosity = 1
    )
    set.seed(123)
    #find optimal number of rounds
    system.time ({
      XGBoostCV <- xgb.cv(
        data = XGBoostdata,
        params = XGBparams,
        nrounds = 200,
        nfold = 5,
        metrics = "error"
      )
    })
    XGBnrounds <- which.min(XGBoostCV$evaluation_log$test_error_mean)
    XGBnrounds
    system.time({
      XGBmodel <- xgb.train(
        params = XGBparams,
        data = XGBoostdata,
        nrounds = XGBnrounds
      )
    })
    XGBpredictions <- predict(XGBmodel, XGBoostdata)
    XGBresiduals <- y - XGBpredictions
    tune_grid <- expand.grid(
      nrounds = seq(from = 200, to =500, by =50),
      eta = c(0.05, 0.1, 0.3),
      max_depth = c(2, 4, 6),
      gamma = c(0, 0.1, 0.5),
      colsample_bytree = c(0.5, 0.8, 1),
      min_child_weight = c(1, 10, 100),
      subsample = c(0.8, 1)
      #alpha = (0:2),
      #lambda = (0:5)
    )
    tune_control <- caret::trainControl(
      method = "cv",
      number = 3,
      verboseIter = TRUE, #training log
      allowParallel = TRUE #FALSE for reproducible results
    )
    system.time({xgb_tune <- caret::train(
      x = x, y = y_factor,
      trControl = tune_control,
      tuneGrid = tune_grid,
      method = "xgbTree",
      verbose = TRUE
    )
    })
    xgb_tune$bestTune
    max(xgb_tune$results$Accuracy)
    qqnorm(XGBresiduals)
    xgb.plot.tree(model = xgb_tune$finalModel, trees = 1)
    ### plot
    #get the first three trees
    xgb.plot.tree(model = xgb_tune$finalModel, trees = 0:2)
    xgb.plot.multi.trees(xgb_tune$finalModel)
    importance_matrix <- xgb.importance(model = xgb_tune$finalModel)
    xgb.plot.importance(importance_matrix, xlab = "Feature Importance")
    #accuracy
    XGBpredictions_class <- ifelse(XGBpredictions > 0.5, 1, 0)
    accuracy <- mean(XGBpredictions_class == y)
    #confusion matrix
    table(XGBpredictions_class, y)
    caret::confusionMatrix(as.factor(XGBpredictions_class), as.factor(y))
  }
  # Neural Net
  {
  library(reshape2)
  library(arrow)
  library(nnet)
    install.packages("NeuralNetTools")
  library(NeuralNetTools)
  set.seed(27514)
  char_cols <- sapply(CFPB, is.character)
  
  if (any(char_cols)) {
    cat("Converting character columns to factors:", 
        paste(names(char_cols[char_cols]), collapse = ", "), "\n")
    CFPB[char_cols] <- lapply(CFPB[char_cols], as.factor)
  } else {
    cat("No character columns found.\n")
  }
  CFPBnnet <- CFPB
  CFPBnnet$Relief <- as.factor(CFPBnnet$Relief)
  #narrows it down to 18 variables
  #removed month and quarter since neural net needs less variables and I'm keeping year
  #company was found to be very unimportant
  CFPBnnet<- CFPBnnet[,-c(4:6, 8:9, 14, 16, 18:21, 27, 34, 36)]
  #I had previously changed characters up above but if you reimport and change the is.factor to is.character you can run this without needing to run the code above
  CFPBnnet[sapply(CFPBnnet, is.factor)] <- lapply(CFPBnnet[sapply(CFPBnnet, is.factor)], as.numeric)
  CFPBnnet <- CFPBnnet %>%
    rename(
      Credit_Constrained       = `Credit Constrained`
    )
  CFPBnnet$Relief <- as.factor(CFPBnnet$Relief)
  mm<- model.matrix(~. -1 -Relief, data=CFPBnnet)
  library(scales)
  ## rescale all the variables
  mm2 <- as.data.frame(apply(mm, 2, rescale))
  mm2$Relief <- CFPBnnet$Relief
  myControl <- trainControl(## 3-fold CV
    method = "cv",
    number = 3)
  nnGrid <- expand.grid(size = seq(3, 21, 3),
                        decay = c(0, 0.2, 0.4, 0.8))
  set.seed(27543)
  nnetFit <- train(Relief ~ .,
                   data = mm2,
                   method = "nnet",
                   maxit = 1000,
                   tuneGrid = nnGrid,
                   trControl = myControl)
  plotnet(nnetFit)
  olden(nnetFit) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  lekprofile(nnetFit)+ theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  
  # neural network with 2 hidden nodes
  #modifying to mimic the one below
  smp_size1 <- floor(0.75 * nrow(mm2))
  ## set the seed to make your partition reproducible
  set.seed(123)
  train_ind1 <- sample(seq_len(nrow(mm2)), size = smp_size1)
  
  ###### set up dataset for lightgbm
  train1 <- mm2[train_ind1, ]
  test1  <- mm2[-train_ind1, ]
  train1$Relief <- as.factor(as.character(train1$Relief))
  test1$Relief  <- as.factor(as.character(test1$Relief))
  nn_train1 <- neuralnet(Relief~., data=train1, hidden=c(4), linear.output=F, rep=3, algorithm = "backprop", learningrate = 0.03, err.fct = "ce", stepmax = 1e5)
  # on test data
  nn_test1 <- neuralnet(Relief~., data=test1, hidden = c(5),
                        linear.output = F)
  plot(nn_train1, rep = "best")
  set.seed(42132) # set the random seed for reproducibility
  # Compute fitted values from the training data
  predictions_train <- predict(nn_train1, newdata = train1)
  # Test the neural networks out of sample performance
  predictions_test <- predict(nn_train1, newdata = test1)
  p.test<-round(predictions_test,0 )
  cm<- table(p.test, test1[,1])
  cm
  pred_classes <- predict(nnetFit, newdata = mm2)
  confusionMatrix(pred_classes, mm2$Relief)
}
}
# Model - Chosen
## catBoost
{
  # Installing necessary library and packages
  {
  install.packages('remotes')
  remotes::install_url('https://github.com/catboost/catboost/releases/download/v1.2.10/catboost-R-windows-x86_64-1.2.10.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))
  library(catboost)
  }
  # Building and tuning catBoost model
  {
  CFPB <- readRDS("CFPB.rds")
  feature_cols <- CFPB[, 1:37]
  feature_cols_no_target <- feature_cols[, colnames(feature_cols) != "Relief"]
  
  cat_feature_indices <- which(sapply(feature_cols_no_target, is.factor)) - 1  # 0-based
  
  y_factor <- CFPB$Relief
  
  # Build CatBoost Pool (equivalent to xgb.DMatrix)
  CBdata <- catboost.load_pool(
    data  = feature_cols_no_target,
    label = y
  )
  
  CBparams <- list(
    loss_function    = "Logloss",        # binary:logistic equivalent
    eval_metric      = "Accuracy",
    learning_rate    = 0.05,             # eta
    depth            = 6,                # max_depth
    min_data_in_leaf = 10,               # min_child_weight
    l2_leaf_reg      = 3,                # regularization (gamma analog)
    rsm              = 1,                # colsample_bytree
    subsample        = 0.8,
    iterations       = 200              # nrounds
    #verbose          = 50                # print every 50 rounds (verbosity analog)[doesn't work with catboost]
  )
  # ── Cross-validation to find optimal iterations ──────────────────────────────
  set.seed(123)
  system.time({
    CBcv <- catboost.cv(
      pool       = CBdata,
      params     = CBparams,
      fold_count = 5,              # nfold
      type       = "Classical"
    )
  })
  # Find best iteration (lowest test error = highest test accuracy)
  XGBnrounds <- which.max(CBcv$test.Accuracy.mean)
  cat("Optimal iterations:", XGBnrounds, "\n")
  
  # ── Train final model ────────────────────────────────────────────────────────
  CBparams$iterations <- XGBnrounds
  
  system.time({
    CBmodel <- catboost.train(
      learn_pool = CBdata,
      params     = CBparams
    )
  })
  
  # ── Predictions ──────────────────────────────────────────────────────────────
  CBpredictions <- catboost.predict(CBmodel, CBdata, prediction_type = "Probability")
  CBpredictions_class <- ifelse(CBpredictions > 0.5, 1, 0)
  CBresiduals <- y - CBpredictions
  
  # ── Accuracy & Confusion Matrix ──────────────────────────────────────────────
  accuracy <- mean(CBpredictions_class == y)
  table(CBpredictions_class, y)
  caret::confusionMatrix(as.factor(CBpredictions_class), as.factor(y), mode="everything")
  
  # ── QQ plot of residuals ─────────────────────────────────────────────────────
  qqnorm(CBresiduals)
  
  # ── Feature Importance ───────────────────────────────────────────────────────
  importance <- catboost.get_feature_importance(CBmodel, CBdata)
  importance_df <- data.frame(
    Feature    = colnames(feature_cols_no_target),
    Importance = importance
  ) |> arrange(desc(Importance))
  
  ggplot(importance_df[1:20, ], aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "CatBoost Feature Importance", x = "Feature", y = "Importance")
  
  # ── Hyperparameter Tuning via caret ─────────────────────────────────────────
  # using the catboost caret wrapper
  tune_grid <- expand.grid(
    depth            = c(2, 4, 6),
    learning_rate    = c(0.05, 0.1, 0.3),
    iterations       = c(200, 350, 500),
    l2_leaf_reg      = c(1, 3, 5),
    rsm              = c(0.8, 1),
    border_count     = 128
  )
  
  tune_control <- caret::trainControl(
    method       = "cv",
    number       = 3,
    verboseIter  = FALSE,
    allowParallel = FALSE
  )
  
  system.time({
    cb_tune <- caret::train(
      x          = feature_cols_no_target,
      y          = y_factor,
      method     = catboost.caret,
      trControl  = tune_control,
      tuneGrid   = tune_grid#,
      #verbose    = FALSE
    )
  })
  
  cb_tune$bestTune
  max(cb_tune$results$Accuracy)
  }
  # Measuring Accuracy on Full Data
  {
  CATBpredictions_class <- ifelse(CBpredictions > 0.5, 1, 0)
  accuracy <- mean(CBpredictions_class == y)
  table(CBpredictions_class, y)
  caret::confusionMatrix(as.factor(CBpredictions_class), as.factor(y), mode = "everything")
  }
  # Plotting Probability Density
  {
    library(PRROC)
    
    pr_obj <- pr.curve(
      scores.class0 = CBpredictions[y == 1],
      scores.class1 = CBpredictions[y == 0],
      curve = TRUE
    )
    
    pr_df <- as.data.frame(pr_obj$curve)
    colnames(pr_df) <- c("Recall", "Precision", "Threshold")
    
    ggplot(pr_df, aes(x = Recall, y = Precision)) +
      geom_line(color = "steelblue", linewidth = 1) +
      labs(
        title = paste0("Precision-Recall Curve (AUC = ", round(pr_obj$auc.integral, 3), ")"),
        x = "Recall",
        y = "Precision"
      ) +
      theme_minimal()
    
    prob_df <- data.frame(
      Probability = CBpredictions,
      Outcome     = factor(y, levels = c(0, 1), labels = c("No Relief", "Relief"))
    )
    
    ggplot(prob_df, aes(x = Probability, fill = Outcome)) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = c("steelblue", "tomato")) +
      labs(
        title = "Predicted Probabilities by Actual Outcome",
        x     = "Predicted Probability of Relief",
        y     = "Density",
        fill  = "Actual Outcome"
      ) +
      theme_minimal()
    
    # Get predicted probabilities from cb_tune
    cb_tune_probs <- predict(cb_tune, newdata = feature_cols_no_target, type = "prob")
    
    # cb_tune_probs will be a data frame with columns "0" and "1" (or "X0", "X1")
    # Use the column for class "1" (Relief)
    prob_df <- data.frame(
      Probability = cb_tune_probs[, "1"],  # or cb_tune_probs$`1`
      Outcome     = factor(y, levels = c(0, 1), labels = c("No Relief", "Relief"))
    )
    
    ggplot(prob_df, aes(x = Probability, fill = Outcome)) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = c("steelblue", "tomato")) +
      labs(
        title = "Predicted Probabilities by Actual Outcome (Tuned CatBoost)",
        x     = "Predicted Probability of Relief",
        y     = "Density",
        fill  = "Actual Outcome"
      ) +
      theme_minimal()
  }
  # Measuring accuracy on test data
  {
  CFPB_test <- readRDS("CFPB_test.rds")
  colnames(CFPB_test) <- make.names(colnames(CFPB_test))
  
  # Aligning factor levels to match training data
  for (col in names(CFPB)) {
    if (is.factor(CFPB[[col]]) && col %in% names(CFPB_test)) {
      CFPB_test[[col]] <- factor(CFPB_test[[col]], levels = levels(CFPB[[col]]))
    }
  }
  
  # Creating sparse matrix using the SAME formula
  x_test <- sparse.model.matrix(Relief ~ ., CFPB_test)
  
  # Verifying column names match
  stopifnot(all(colnames(x_test) == colnames(x)))
  
  # Creating Confusion Matrix for catBoost
  CFPB_predCAT <- predict(cb_tune, newdata = CFPB_test)
  confusionMatrix(CFPB_predCAT,reference = CFPB_test$Relief, mode = "everything")
}
  # Measuring impact on older Americans
  {
  CFPB_old <- CFPB_test|>filter(is_older_american==1)
  CFPB_pred <- predict(cb_tune,newdata = CFPB_old)
  confusionMatrix(CFPB_pred,reference = CFPB_old$Relief, mode = "everything")
  
  # Processing CFPB_old the same way as training data
  colnames(CFPB_old) <- make.names(colnames(CFPB_old))
  
  # Ensuring character columns are made into factors
  char_cols_old <- sapply(CFPB_old, is.character)
  CFPB_old[char_cols_old] <- lapply(CFPB_old[char_cols_old], as.factor)
  
  # Aligning factor levels to match training data
  for (col in names(CFPB)) {
    if (is.factor(CFPB[[col]]) && col %in% names(CFPB_old)) {
      CFPB_old[[col]] <- factor(CFPB_old[[col]], levels = levels(CFPB[[col]]))
    }
  }
  
  # Creating a sparse matrix using the SAME formula
  x_old <- sparse.model.matrix(Relief ~ ., CFPB_old[, c(1:37)])
  
  # Verifying column names match
  stopifnot(all(colnames(x_old) == colnames(x)))
  CFPB_predoldxgb <- predict(xgb_tune,newdata = x_old)
  confusionMatrix(CFPB_predoldxgb,reference = CFPB_old$Relief,mode = "everything")
}
  # Measuring impact on Black Americans
  {
    df2 <- CFPB %>%
      mutate(
        majority_black = as.factor(if_else(prop_black > 0.144, 1, 0)),
        Relief = as.factor(Relief)  # ensure outcome is also a factor
      ) %>% 
      filter(majority_black == 1)
    # Converting df2 into catBoost pool
    feature_df <- df2 %>% 
      select(-Relief) %>%
      mutate(Issue_combined = as.factor(Issue_combined)) 
    pool_df2 <- catboost.load_pool(data = feature_df)
    
    predicted_prob  <- catboost.predict(CBmodel, pool = pool_df2, prediction_type = "Probability")
    predicted_class <- catboost.predict(CBmodel, pool = pool_df2, prediction_type = "Class")
    ydf2 <- as.factor(df2$Relief)
    # Convert predicted_class to a factor with the same levels as your outcome
    predicted_class_factor <- as.factor(predicted_class)
    
    # Make sure levels match
    levels(predicted_class_factor) <- levels(ydf2)
    
    # Now run confusionMatrix with the CLASS predictions, not probabilities
    caret::confusionMatrix(predicted_class_factor, ydf2)
    
    df2 <- CFPB %>%
      mutate(Relief = as.factor(Relief)) %>%
      filter(is_older_american == 0)
    
    # Convert your new data into a CatBoost Pool
    feature_df <- df2 %>% 
      select(-Relief) %>%
      mutate(Issue_combined = as.factor(Issue_combined))
    
    pool_df2 <- catboost.load_pool(data = feature_df)
    predicted_prob  <- catboost.predict(CBmodel, pool = pool_df2, prediction_type = "Probability")
    predicted_class <- catboost.predict(CBmodel, pool = pool_df2, prediction_type = "Class")
    
    ydf2 <- as.factor(df2$Relief)
    predicted_class_factor <- as.factor(predicted_class)
    levels(predicted_class_factor) <- levels(ydf2)
    caret::confusionMatrix(predicted_class_factor, ydf2)
  }
  # Cost of Living Analysis
  {
    # 1. PERCENTILE CUTPOINTS
    q25 <- quantile(CFPB_test$prop_black_fem, 0.25, na.rm = TRUE)
    q75 <- quantile(CFPB_test$prop_black_fem, 0.75, na.rm = TRUE)
    
    # 2. REPRESENTATIVE CONSUMER (median/mode baseline)
    rep_obs <- CFPB_test[1, ] |>
      mutate(across(where(is.numeric), ~median(CFPB_test[[cur_column()]], na.rm = TRUE)))
    
    # Set categoricals to modal value
    for (col in names(rep_obs)[sapply(rep_obs, is.factor)]) {
      rep_obs[[col]] <- names(sort(table(CFPB_test[[col]]), decreasing = TRUE))[1] |> 
        factor(levels = levels(CFPB_test[[col]]))
    }
    
    # After building rep_obs, force column types to match training data exactly
    rep_obs$Issue_combined <- factor(
      rep_obs$Issue_combined, 
      levels = levels(feature_cols_no_target$Issue_combined)
    )
    
    # Verify
    class(rep_obs$Issue_combined)
    
    # 3. SIMULATE LOW vs HIGH COL SCENARIOS
    make_pool <- function(obs, col_name, col_val) {
      obs[[col_name]] <- col_val
      obs_subset <- obs[, colnames(feature_cols_no_target)]
      catboost.load_pool(data = obs_subset)
    }
    
    prob_low  <- catboost.predict(CBmodel, make_pool(rep_obs, "prop_black_fem", q25), prediction_type = "Probability")
    prob_high <- catboost.predict(CBmodel, make_pool(rep_obs, "prop_black_fem", q75), prediction_type = "Probability")
    
    # 4. MARGINAL EFFECT
    abs_change <- (prob_high - prob_low) * 100
    rel_change <- ((prob_high - prob_low) / prob_low) * 100
    
    cat("--- CETERIS PARIBUS FORENSIC AUDIT ---\n")
    cat("P(Relief | Low COL, 25th pct): ", round(prob_low  * 100, 2), "%\n")
    cat("P(Relief | High COL, 75th pct):", round(prob_high * 100, 2), "%\n")
    cat("Absolute Swing:", round(abs_change, 2), "pp\n")
    cat("Relative Change:", round(rel_change, 2), "%\n")
    
    library(kableExtra)
    
    data.frame(
      Scenario       = c("Low COL (25th pct)", "High COL (75th pct)"),
      COL_Value      = c(round(q25, 2), round(q75, 2)),
      P_Relief       = c(round(prob_low * 100, 2), round(prob_high * 100, 2)),
      Abs_Swing_pp   = c("—", round((prob_high - prob_low) * 100, 2)),
      Rel_Change_pct = c("—", round(((prob_high - prob_low) / prob_low) * 100, 2))
    ) |>
      kable(col.names = c("Scenario", "COL Value", "P(Relief) %", "Absolute Swing (pp)", "Relative Change (%)"),
            align = "lrrrr") |>
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) |>
      row_spec(2, bold = TRUE)
  }
}