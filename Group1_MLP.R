setwd('/Users/mattamor/Library/CloudStorage/OneDrive-Personal/School/ECON 6378 - Machines/DataProject/MachineLearningProject_Group1')
rm(list=ls())

library(readr)
library(tidyverse)

CFPB0 <- read_csv("sample26.01.csv")

## recode target variable
CFPB0$Relief<- ifelse(CFPB0$Company.response.to.consumer %in% 
                     c("Closed with monetary relief",
                       "Closed with relief",
                       "Closed with non-monetary relief"),
                    1,0)
CFPB0 <- CFPB0[,-c(1)]%>%
  select(Relief,Date.received, Date.sent.to.company,everything())

# Dropping NAs
## 134 rows have NA values across 12 variables in original data
CFPB1 <-CFPB0 %>%
  drop_na(Date.sent.to.company)

# Matt recommends
CFPB <- CFPB1 %>%
  mutate(Date.received                = as.Date(Date.received,"%m/%d/%y"),
         Date.sent.to.company         = as.Date(Date.sent.to.company,"%m/%d/%y"),
         Issue                        = as.factor(Issue),
         Sub.issue                    = as.factor(Sub.issue),
         Company.public.response      = as.factor(Company.public.response),
         Company                      = as.factor(Company),
         State                        = as.factor(State),
         ZIP.code                     = as.factor(ZIP.code),
         Tags                         = as.factor(Tags),
         Consumer.consent.provided.   = as.factor(Consumer.consent.provided.),
         Submitted.via                = as.factor(Submitted.via),
         Company.response.to.consumer = as.factor(Company.response.to.consumer),
         Timely.response.             = as.factor(Timely.response.))%>%
  rename(Received      = Date.received,
         Sent          = Date.sent.to.company,
         Cust.response = Company.response.to.consumer,
         Pub.response  = Company.public.response,
         Consent       = Consumer.consent.provided.,
         Timely        = Timely.response.)%>%
  mutate(Wait.time = as.numeric(Sent - Received)) %>%
  select(-Product,
         -Sub.product,
         -Consumer.disputed.,
         -Consumer.complaint.narrative,
         -Complaint.ID)%>%
  select(Relief, Received, Sent, Wait.time,everything())

# Below is just to show you why I ended up with CFPB. This will be removed from final code.

# This gives a good idea about the challenges with the data
## There are a lot of factor levels, particularly under Company and Zip.codes
## Zip codes have lots of missing values
## Nearly all variables have NA factor levels
summary(CFPB)
FACT_CFPB <- sapply(CFPB,is.factor)
fact_CFPB <- CFPB[,FACT_CFPB]
lapply(fact_CFPB,unique)

# Dropped the following variables
## No Variation
unique(CFPB0$Product)
unique(CFPB0$Sub.product)
unique(CFPB0$Consumer.disputed.)
## Too Much Variation
unique(CFPB0$Consumer.complaint.narrative)
unique(CFPB0$Complaint.ID)

# Added Variable
## Wait time (in days) could be helpful but values are heavily skewed to 0 and 1
wait <- CFPB2[,"Wait.time"] %>%
  filter(Wait.time <= 50,
         Wait.time >  0)%>%
  rename(Time = Wait.time)
summary(na.omit(CFPB2$Wait.time==0)) # 55,000+ zero wait times
hist(wait$Time,breaks = 50,right = FALSE)

