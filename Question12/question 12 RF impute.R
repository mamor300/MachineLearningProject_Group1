#12
#code below will just left join everything selected into CFPB
#do not run these again if you have the dataset with CFPB already combined
CFPB <- CFPB10 |>
  left_join(sahie, by = c('FIPS', "Year")) |>
  select(-c(state, county, NAME))
CFPB <- CFPB |>
  left_join(FMR,by = c('FIPS',"Year"))
CFPB <- CFPB |>
  left_join(DebtMetrics.clean |>
              select(-`County Name`,-`State Name`),
            by ="FIPS")
CFPB <- CFPB %>%
  left_join(census_scaled, by = "FIPS")
CFPB <- CFPB %>%
  mutate(qtr = quarter(Received)) 
CFPB <- CFPB %>%
  mutate(high = CFPB5$high[match(paste(FIPS, Year, qtr), 
                                 paste(CFPB5$FIPS, CFPB5$Year, CFPB5$qtr))],
         low  = CFPB5$low[match(paste(FIPS, Year, qtr), 
                                paste(CFPB5$FIPS, CFPB5$Year, CFPB5$qtr))])
CFPB <- CFPB %>%
  left_join(compiled_county_education_measures, by = c("FIPS" = "fips", "Year" = "year"))|>
  select(-c(NAME))
CFPB <- CFPB %>%
  left_join(changing_med_debt_landscape_county, by = c("FIPS" = "County Fips", "Year"))|>
  select(-c(`State Abbreviation`, `County Name`))
#random forest imputation
library(randomForest)
set.seed(12345)
#looking for # of na's in variable of choice
noy<-is.na(CFPB$Cust.response)
#remove rows where the above variable is NA
CFPBNAremover<-CFPB[which(noy==0),]
sapply(CFPB, class)
library(forcats)
#should only use the line below for sparse (min = is number of observations) categories
#SFNArmv2$property_type<-(fct_lump_min(SFNArmv2$property_type, min = 10))
#price can't have na's in y to run this so I'm just gonna use the median imputed value for price
CFPBimpute <- CFPB
CFPBimpute <- CFPB %>%
  mutate(across(where(is.character), as.factor))%>%
  mutate(across(where(is.logical), as.factor))%>%
  mutate(across(where(~ inherits(., "Date")), as.numeric))
sapply(CFPBimpute[sapply(CFPBimpute, is.factor)], nlevels) %>% 
  sort(decreasing = TRUE) %>% 
  head(20)
#all of these had more than 53 categories (alot more) so I dropped them
drop_cols_CFPB <- c("ZIP", "FIPS", "Company", "prefix3", 
                    "CI Index Score", "Not Credit Included", 
                    "Credit Constrained")
CFPBimpute <- CFPBimpute %>% select(-all_of(drop_cols_CFPB))
CFPBimpute <- CFPBimpute %>%
  mutate(across(where(is.character), as.factor))
CFPBimpute$Received <- as.numeric(CFPBimpute$Received)
CFPBimpute$Sent <- as.numeric(CFPBimpute$Sent)
CFPBimpute$qtr <- as.numeric(CFPBimpute$qtr)
#CFPBimpute<- rfImpute(Cust.response~., iter = 5, ntree = 50 ,data=CFPBimpute)
sapply(CFPBimpute, class)
#for whatever reason these were factors not numeric
cols_to_convert <- c(
  "Share with medical debt in collections",
  "Median medical debt in collections in $2023",
  "Hospital market concentration (HHI)",
  "Number of Closures and Mergers",
  "Share of the population with no health insurance coverage",
  "Share of non-elderly adults with a reported disability",
  "Average household income in $2023",
  "Median medical debt in collections in $2023 - Majority White",
  "Median medical debt in collections in $2023 - Majority of Color",
  "Share with medical debt in collections - Majority White",
  "Share with medical debt in collections - Majority of Color",
  "Median medical debt in collections in $2023"
)
CFPBimpute <- CFPBimpute %>%
  mutate(across(all_of(cols_to_convert), ~ as.numeric(as.character(.x))))
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
