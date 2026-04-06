setwd('/Users/mattamor/Library/CloudStorage/OneDrive-Personal/School/ECON 6378 - Machines/DataProject/MachineLearningProject_Group1/Question10')

# PCA on merged debt collection variables using base R
CFPB_debt <- CFPB.debt %>%
  select(FIPS, `Share with any debt in collections, All`:
           `Share without a bachelor's degree, White comm`) %>%
  distinct(FIPS,.keep_all = TRUE)

CFPB_FIPS <- CFPB_debt$FIPS
CFPB_debtNum <- CFPB_debt %>%
  select(-FIPS)

missingRate <- colMeans(is.na(CFPB_debtNum))
CFPB_debtNum_full <- CFPB_debtNum[,missingRate <= 0.3]
length(CFPB_debtNum_full) # 31 variables

nb <- estim_ncpPCA(CFPB_debtNum_full) # estimates 2 Principal Components is optimal        
DMImpute <- imputePCA(CFPB_debtNum_full, ncp = nb$ncp)
DMImputed <- DMImpute$completeObs

DebtMetrics.pca <- prcomp(DMImputed, center = TRUE, scale. = TRUE)

sdev         <- DebtMetrics.pca$sdev
var_exp      <- sdev^2 / sum(sdev^2) * 100
cumvar       <- cumsum(var_exp)

scores <- as.data.frame(DebtMetrics.pca$x) |>
  mutate(FIPS = CFPB_FIPS)

DebtMetrics.pca$rotation

print(data.frame(
  PC         = paste0("PC", 1:10),
  individual = round(var_exp[1:10], 2),
  cumulative = round(cumvar[1:10], 2)
))
