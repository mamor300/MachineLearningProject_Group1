#12
#code below will just left join everything selected into CFPB
#CFPB <- CFPB |>
  #left_join(CFPB.sahie |> select(FIPS, Year, choose col here), by = c('FIPS' = 'FIPS', 'Year' = 'Year')) |>
  #left_join(CFPB.FMR |> select(FIPS, Year, choose col here), by = c('FIPS' = 'FIPS', 'Year' = 'Year'))
  #left_join(CFPB.insecurity |> select(FIPS, Year, choose col here), by = c('FIPS' = 'FIPS', 'Year' = 'Year'))
  #left_join(CFPB.debt |> select(FIPS, Year, choose col here), by = c('FIPS' = 'FIPS', 'Year' = 'Year'))
  #left_join(CFPB_clust |> select(FIPS, Year, choose col here), by = c('FIPS' = 'FIPS', 'Year' = 'Year'))
  #left_join(CFPB_Census |> select(FIPS, Year, choose col here), by = c('FIPS' = 'FIPS', 'Year' = 'Year'))
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
CFPBimpute<- rfImpute(Cust.response~., iter = 5, data=CFPBimpute)
