library(tidyverse)
library(readxl)
setwd("/Users/mattamor/MachineLearningProject_Group1")
# --- 1. SETUP FILE PATHS ---
folder_path <- "C:/Users/alanj/Downloads/productDownload_2026-04-07T194031/"
folder_path <- "/Users/mattamor/MachineLearningProject_Group1/"
edu_files <- c("2020 educational attainment.csv", "2021 educational attainment.csv", 
               "2022 educational attainment.csv", "2023 educational attainment.csv", 
               "2024 educational attainment.csv")
pop_file <- paste0(folder_path, "population_at_couny_level.xlsx")

process_edu <- function(file_name) {
  year <- str_extract(file_name, "\\d{4}")
  path <- paste0(folder_path, file_name)
  
  data <- read_csv(path, skip = 2, col_names = FALSE)
  headers <- names(read_csv(path, n_max = 0))
  colnames(data) <- headers
  
  data %>%
    mutate(
      fips = str_sub(GEO_ID, -5),
      year = as.numeric(year)
    ) %>%
    # S1501_C01_006E: Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over
    # S1501_C01_015E: Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree or higher
    select(fips, NAME, year, 
           total_25plus = S1501_C01_006E, 
           bach_or_higher = S1501_C01_015E) %>%
    mutate(pct_bach_degree = (bach_or_higher / total_25plus) * 100)
}

# COMBINE ---
all_edu_data <- map_df(edu_files, process_edu)

# --- 4. PROCESS POPULATION DATA (The Bridge) ---
# Skip 3 rows: Title, Headers, and the "United States" total row
pop_data <- read_excel(pop_file, skip = 3) %>%
  rename(county_full_name = 1) %>%
  # Clean the ".Autauga County, Alabama" format to "Autauga County, Alabama"
  mutate(match_name = str_remove(county_full_name, "^\\.")) 

# FINAL JOIN & CLEANUP ---
final_compiled_data <- all_edu_data %>%
  left_join(pop_data, by = c("NAME" = "match_name")) %>%
  select(fips, NAME, year, total_25plus, bach_or_higher, pct_bach_degree)

# --- 6. EXPORT THE RESULT ---
write_csv(final_compiled_data, paste0(folder_path, "compiled_county_education_measures.csv"))

message("Success! File saved to: ", folder_path)

library(tidyverse)
library(readxl)

# 1. Define your folder path
folder_path <- "C:/Users/alanj/Downloads/productDownload_2026-04-07T194031/"

# 2. Load and clean the 2020 Education Data
edu_2020_path <- paste0(folder_path, "2020 educational attainment.csv")
# Skip 2 rows to get past headers, then re-apply the correct names from row 1
edu_2020_raw <- read_csv(edu_2020_path, skip = 2, col_names = FALSE)
headers <- names(read_csv(edu_2020_path, n_max = 0))
colnames(edu_2020_raw) <- headers

edu_2020 <- edu_2020_raw %>%
  mutate(fips = str_sub(GEO_ID, -5)) %>%
  select(fips, NAME)

# 3. Load and clean the Population Data
pop_path <- paste0(folder_path, "population at couny level.xlsx")
pop_data <- read_excel(pop_path, skip = 3) %>%
  rename(county_full_name = 1) %>%
  mutate(match_name = str_remove(county_full_name, "^\\."))

# 4. AUDIT: Find which rows in Education are NOT in Population
dropped_counties <- edu_2020 %>%
  anti_join(pop_data, by = c("NAME" = "match_name"))

# 5. Output results
cat("Total records in 2020 Education file:", nrow(edu_2020), "\n")
cat("Number of counties dropped during merge:", nrow(dropped_counties), "\n\n")

cat("Example of dropped counties (First 15):\n")
print(head(dropped_counties$NAME, 15))