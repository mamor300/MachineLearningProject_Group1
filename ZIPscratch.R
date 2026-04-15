setwd('')
library(tigris)
library(ggplot2)
library(dplyr)

#2.
# Imputing zip codes with means
## Each means-imputed value uses the average among the first 3 digits of non-missing "siblings"
## This produces 95 NAs for zip codes that do not have any non-missing siblings
## Creating a branch of the CFPB data that prepares the missing zips for imputation
df <- CFPB1 %>%
  mutate(ZIP.char    = as.character(ZIP.code),
         prefix3     = substr(ZIP.char, 1, 3),
         ZIP.missing = ifelse(nchar(ZIP.code) < 5 | grepl("X$", ZIP.code), 1, 0))

# Get all valid ZIPs from your lookup, with their numeric value
valid_zips <- ZIPCODES %>%
  mutate(zip_char   = formatC(as.numeric(ZIP), width = 5, flag = "0", format = "d"),
         prefix3    = substr(zip_char, 1, 3),
         zip_num    = as.numeric(zip_char)) %>%
  select(prefix3, zip_char, zip_num)

# Compute mean ZIP per prefix from non-missing obs (same as before)
ZIP.means <- df %>%
  filter(ZIP.missing == 0, nchar(trimws(ZIP.char)) == 5) %>%
  group_by(prefix3) %>%
  summarise(ZIP.mean.num = round(mean(as.numeric(ZIP.char), na.rm = TRUE)))

# For each prefix, find the valid ZIP closest to the mean
ZIP.impute <- ZIP.means %>%
  left_join(valid_zips, by = "prefix3") %>%
  mutate(dist = abs(zip_num - ZIP.mean.num)) %>%
  group_by(prefix3) %>%
  slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%  
  select(prefix3, ZIP.imputed = zip_char)

# Join back and impute
CFPB2 <- df %>%
  left_join(ZIP.impute, by = "prefix3") %>%
  mutate(ZIP.Imputed = as.factor(ifelse(ZIP.missing == 1, ZIP.imputed, ZIP.char)),
         ZIP.missing = ifelse(nchar(ZIP.imputed) < 5 | grepl("X$", ZIP.imputed), 1, 0)) %>%
  select(-c(ZIP.char))

# Checking to make sure states match
StateCheck <- CFPB.compare%>%
  left_join(ZIPCODES, by = c("ZIP.knn" = "ZIP"))%>%
  mutate(state_match = (State == STATE))
table(StateCheck$state_match, useNA = "ifany")
# Creating a dataframe with all the obs that failed to impute
try <- CFPB2[is.na(CFPB2$ZIP.missing),]


# Imputing with K Nearest Neighbors
# Flaggingg missing ZIPs in a separate df
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
  dplyr::select(ZIP.num, State, Company, prefix3)

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
  mutate(ZIP.knn.raw = round(knn_result$ZIP.num)) %>%
  rowwise() %>%
  mutate(
    ZIP.knn = if (ZIP.missing == 1) {
      snap_to_valid(ZIP.knn.raw, prefix = prefix3)  
    } else {
      snap_to_valid(ZIP.knn.raw)                     
    }
  ) %>%
  ungroup() %>%
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
           -Company.response.to.consumer)|>
    mutate(Wait.time = as.numeric(Sent - Received))|>
    select(Relief, Received, Sent, Year, Wait.time,everything())
}

# Playing around with zip code maps
## Beyond the scope of this project
zips <- zctas(year = 2023)  # downloads zip code shapefiles

zip_counts <- df %>%
  count(ZIP.Imputed) %>%
  rename(region = ZIP.Imputed, value = n)

merged <- zips %>%
  left_join(zip_counts, by = c("ZCTA5CE20" = "region"))

ggplot(merged) +
  geom_sf(aes(fill = value), color = "white", linewidth = 0.1) +
  coord_sf(xlim = c(-76, -78), ylim = c(38, 39)) +
  scale_fill_gradient(low = "lightyellow", high = "darkred",
                      na.value = "grey90",    # zips not in your data → grey
                      name = "# Listings") +
  theme_minimal() +
  labs(title = "Listing Frequency by Zip Code")

# Separate your clean and partial zips
clean_zips <- df %>%
  filter(!grepl("X", ZIP.code)) %>%
  count(ZIP.code) %>%
  rename(region = ZIP.code, value = n)

partial_zips <- df %>%
  filter(grepl("X", ZIP.code)) %>%
  mutate(prefix = substr(ZIP.code, 1, 3))  # extract e.g. "123" from "123XX"

# Get all real zip codes from the shapefile
all_zips <- zips$ZCTA5CE20

# For each partial prefix, find all matching full zips
expanded <- partial_zips %>%
  rowwise() %>%
  mutate(matches = list(all_zips[startsWith(all_zips, prefix)])) %>%
  unnest(matches) %>%
  group_by(region = matches) %>%
  summarise(value = n())

# Combine clean and expanded partial zips
zip_counts <- bind_rows(clean_zips, expanded) %>%
  group_by(region) %>%
  summarise(value = sum(value))

merged <- zips %>%
  left_join(zip_counts, by = c("ZCTA5CE20" = "region"))

ggplot(merged) +
  geom_sf(aes(fill = value), color = "white", linewidth = 0.1) +
  coord_sf(xlim = c(-76, -78), ylim = c(38, 39)) +
  scale_fill_gradient(low = "lightyellow", high = "darkred",
                      na.value = "grey90",    # zips not in your data → grey
                      name = "# Listings") +
  theme_minimal() +
  labs(title = "Listing Frequency by Zip Code")

