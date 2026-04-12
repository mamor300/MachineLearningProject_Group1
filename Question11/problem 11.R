#11 (my code now)
# 5 clusters
library(clustMixType)
#creating a matrix for mixed type cluster analysis
#using share of people of color, average household income, whether older or younger, Principal Component 1, is_older_county, and whether a legislature is republican controlled or not
# Create a named vector of Republican-controlled legislatures in 2024
#republican_states <- c("AL", "AZ", "AR", "FL", "GA", "ID", "IN", "IA", 
                       #"KS", "KY", "LA", "MS", "MO", "MT", "NH", "ND", 
                       #"OH", "OK", "SC", "SD", "TN", "TX", "UT", "WV", "WY",
                       #"WI", "NC")

#CFPB$rep_legislature <- as.factor(ifelse(CFPB$State %in% republican_states, 1, 0))
matrix1 <- bind_cols(c(CFPB[,c("PC1", "Year", "Issue")], CFPB.debt[,c("Share of people of color")], CFPB_Census[,c("prop_young", "prop_65plus")]))
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
plot(kpres)
#Save cluster assignments
CFPB$cluster <- NA
CFPB$cluster[complete_idx] <- kpres$cluster
CFPB$cluster <- as.factor(CFPB$cluster)
