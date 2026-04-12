#I will be importing SAHIE(small area health insurance estimates) data from census bureau
library(censusapi)
Sys.setenv(CENSUS_KEY = "ba79958600ff02f01da8a857d6a3243c191cfc8a")
sahie_vars <- listCensusMetadata(
  name = "timeseries/healthins/sahie",
  type = "variables"
)
#this will show you what all the variables chosen below are
View(sahie_vars)
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
CFPB.sahie <- CFPB |>
  left_join(sahie,by = c('FIPS',"Year"))
