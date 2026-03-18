setwd('/Users/mattamor/Library/CloudStorage/OneDrive-Personal/School/ECON 6378 - Machines/DataProject/MachineLearningProject_Group1')
rm(fact_CFPB,result,wait,FACT_CFPB,RandomZip)

AutoRetail <- read_xlsx("AutoRetail.xlsx")
StudentLoan <- read_xlsx("StudentLoan.xlsx")
OverallDebt <- read_xlsx("Overall.xlsx")

DebtMetrics <- OverallDebt %>%
  left_join(AutoRetail %>% 
              select(-`County Name`,
                     -`State Name`,
                     -`Auto/retail loan delinquency rate, All`,
                     -`Auto/retail loan delinquency rate, Comm of color`,
                     -`Auto/retail loan delinquency rate, White comm`,
                     -`Share of people of color`,
                     -`Average household income, All`,
                     -`Average household income, White comm`,
                     -`Average household income, Comm of color`),
            by = "County FIPS") %>%
  left_join(StudentLoan %>% 
              select(-`County Name`,
                     -`State Name`,
                     -`Share of people of color`,
                     -`Average household income, All`,
                     -`Average household income, White comm`,
                     -`Average household income, Comm of color`,
                     -`Student loan delinquency rate (60+), All`,
                     -`Student loan delinquency rate (60+), Comm of color`,
                     -`Student loan delinquency rate (60+), White comm`),
            by = "County FIPS")%>%
  rename(FIPS = `County FIPS`)
colnames(DebtMetrics)

CFPB <- CFPB %>%
  left_join(DebtMetrics %>%
              select(-`State Name`),
            by="FIPS")
