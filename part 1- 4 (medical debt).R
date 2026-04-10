library(tidyverse)
library(readxl)

# SETUP
setwd("C:/Users/-----/Downloads/productDownload_2026-04-07T194031")

CFPB0 <- read_csv("sample26.01.csv")
ZIPCODES <- read_csv("zip_fips.csv")

ZIPCODES$FIPS <- str_pad(as.character(ZIPCODES$STCOUNTYFP), 5, "left", pad = "0")
ZIPCODES$ZIP  <- str_pad(as.character(ZIPCODES$ZIP), 5, "left", pad = "0")

CFPB0$Relief <- ifelse(CFPB0$Company.response.to.consumer %in% 
                         c("Closed with monetary relief",
                           "Closed with relief",
                           "Closed with non-monetary relief"),
                       1, 0)

# 3. PART 2: CREATE CFPB2 (The Merge)
# We drop the first ID column and filter states
CFPB1 <- CFPB0[, -1] 
CFPB1 <- subset(CFPB1, !is.na(Date.sent.to.company))
CFPB1 <- subset(CFPB1, !State %in% c("NONE", "None", "DC", "AA", "AS", "FM", "GU", "MH", "MP", "PR", "VI"))

# Standardize the ZIP code column name to "ZIP" for the merge
# Using the corrected column name: ZIP.code
CFPB1$ZIP <- str_pad(as.character(CFPB1$ZIP.code), 5, "left", pad = "0")

# Join the data to get FIPS
CFPB2 <- left_join(CFPB1, unique(ZIPCODES[, c("ZIP", "FIPS")]), by = "ZIP")

# 4. PART 3: CREATE CFPB3 (Date Calculations)
CFPB3 <- CFPB2
# Using the corrected column names with dots
CFPB3$date_received <- as.Date(CFPB3$Date.received, format = "%m/%d/%y")
CFPB3$date_sent <- as.Date(CFPB3$Date.sent.to.company, format = "%m/%d/%y")

CFPB3$Year <- as.numeric(format(CFPB3$date_received, "%Y"))
CFPB3$Wait.time <- as.numeric(CFPB3$date_sent - CFPB3$date_received)

# Final filter for logical errors
CFPB3 <- subset(CFPB3, Wait.time >= 0)

# VERIFICATION: This should print the row count
cat("Rows in CFPB3:", nrow(CFPB3), "\n")

# Medical debt
med_debt_raw <- read_excel("changing_med_debt_landscape_county.xlsx", 
                           sheet = 1,
                           .name_repair = "universal")

# Cleaning
med_debt_clean <- med_debt_raw %>%
  mutate(
    Year = as.numeric(Year),
    FIPS = str_pad(as.character(County.Fips), 5, "left", pad = "0")
  ) %>%
  filter(nchar(FIPS) == 5)

# Left join
CFPB4 <- CFPB3 %>%
  mutate(FIPS = as.character(FIPS),
         Year = as.numeric(Year)) %>%
  left_join(med_debt_clean, by = c("FIPS", "Year"))

cat("Original rows:", nrow(CFPB3), "\n")
cat("Merged rows:", nrow(CFPB4), "\n")
cat("Total columns now:", ncol(CFPB4), "\n")

print(head(CFPB4[, c("FIPS", "Year", "Share.with.medical.debt.in.collections")]))