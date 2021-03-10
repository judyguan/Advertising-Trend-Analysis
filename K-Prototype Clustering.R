#Part1: Data Cleaning and Preparation 
#read campaign performance table
setwd("~/Downloads/Analytic Trends Project Data /CampaignPerformanceTable")
library(dplyr)
library(readr)
library(clustMixType)
#Get the file name in csv format
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
CampaignPerformanceTable = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE,header=FALSE)))
#include column names 
names(CampaignPerformanceTable) = c("Brand","creative_size","dt","device_category","Region","age","gender","income","impressions","click","CampaignID")
head(CampaignPerformanceTable)
str(CampaignPerformanceTable)
#change NAs in click to zero
summary(CampaignPerformanceTable$click)
CampaignPerformanceTable$click[which(is.na(CampaignPerformanceTable$click))] = 0
#change blank in region to missing value
CampaignPerformanceTable$Region[which(CampaignPerformanceTable$Region == "")] = NA
summary(factor(CampaignPerformanceTable$Region))
summary(factor(CampaignPerformanceTable$creative_size))
summary(factor(CampaignPerformanceTable$device_category))
summary(factor(CampaignPerformanceTable$Brand))
CampaignPerformanceTable$Brand[which(CampaignPerformanceTable$Brand == "")] = NA
summary(factor(CampaignPerformanceTable$age))
CampaignPerformanceTable$age[which(CampaignPerformanceTable$age == "")] = NA
summary(factor(CampaignPerformanceTable$gender))
CampaignPerformanceTable$gender[which(CampaignPerformanceTable$gender == "")] = NA
CampaignPerformanceTable$gender[which(CampaignPerformanceTable$gender == "FALSE")] = NA
summary(factor(CampaignPerformanceTable$CampaignID))
CampaignPerformanceTable$income[which(CampaignPerformanceTable$income == "")] = NA
summary(factor(CampaignPerformanceTable$income))
#drop missing values
#drop no clicks
CampaignPerformanceTable <- na.omit(CampaignPerformanceTable)
str(CampaignPerformanceTable)
#no clicks-> 88383 obs
#12132042 observations -> 10096288 observations
#converting all character columns to factors 
CampaignPerformanceTable$Region <- as.factor(CampaignPerformanceTable$Region)
CampaignPerformanceTable$age <- as.factor(CampaignPerformanceTable$age)
CampaignPerformanceTable$gender <- as.factor(CampaignPerformanceTable$gender)
CampaignPerformanceTable$income <- as.factor(CampaignPerformanceTable$income)
CampaignPerformanceTable$device_category <- as.factor(CampaignPerformanceTable$device_category)
CampaignPerformanceTable$creative_size <- as.factor(CampaignPerformanceTable$creative_size)
CampaignPerformanceTable$CampaignID <- as.factor(CampaignPerformanceTable$CampaignID)
#clean outlier in click
CampaignPerformanceTable$click_clear = CampaignPerformanceTable$click
CampaignPerformanceTable$click_clear[which(CampaignPerformanceTable$click_clear > 100)] = 100
summary(CampaignPerformanceTable$click_clear)
summary(CampaignPerformanceTable$click)
#clean outlier in impressions
CampaignPerformanceTable$impressions_clear = CampaignPerformanceTable$impressions
CampaignPerformanceTable$impressions_clear[which(CampaignPerformanceTable$impressions_clear > 4000)] = 4000
summary(CampaignPerformanceTable$impressions)
summary(CampaignPerformanceTable$impressions_clear)


#Part2: K-Prototype Clustering(for large mixed dataset)
#region 1: Midwest 1967716 obs.
Midwest <- CampaignPerformanceTable[which(CampaignPerformanceTable$Region == "Midwest"),]
head(Midwest)
str(Midwest)
#selecting age,gender,income and impressions
#clustering_midwest <- Midwest[,6:10]
clustering_midwest <- Midwest[,c("age","gender","income","click_clear","impressions_clear")]

#random sample 196771 obs
#set.seed(123)
#split=sample(1:nrow(clustering_midwest),0.1*nrow(clustering_midwest))
#Midwestdata=clustering_midwest[split,]

#stratified sampling
library(splitstackshape)
set.seed(1)
Midwestdata <- stratified(clustering_midwest,c("age","gender","income","impressions_clear","click_clear"),10)

# Elbow Method for finding the optimal number of clusters
#https://stats.stackexchange.com/questions/293877/optimal-number-of-clusters-using-k-prototypes-method-in-r
library(clustMixType)
set.seed(123)
# Compute and plot wss for k = 2 to k = 9.
k.max <- 9
wss <- sapply(1:k.max, 
              function(k){kproto(Midwestdata, k)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")    #picked cluster 4
#cluster 4 is the optimal choice
library(clustMixType)
kpres <- kproto(Midwestdata, 4)
clprofiles(kpres, Midwestdata)
kpres #output cluster prototypes

#region 2: Northeast
Northeast <- CampaignPerformanceTable[which(CampaignPerformanceTable$Region == "Northeast"),]
clustering_Northeast <- Northeast[,6:10]
set.seed(1)
Northeast_sample <- stratified(clustering_Northeast,c("age","gender","income","impressions","click"),10)
#create elbow graph to determine the optimal cluster number 
k2.max <- 9
wss2 <- sapply(1:k2.max, 
              function(k2){kproto(Northeast_sample, k2)$tot.withinss})
wss2
plot(1:k2.max, wss2,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#run K protype model using the optimal cluster number 
kpres2 <- kproto(Northeast_sample, 4) 
clprofiles(kpres2, Northeast_sample)
kpres2 #output cluster prototypes

#region3: Southeast
Southeast <- CampaignPerformanceTable[which(CampaignPerformanceTable$Region == "Southeast"),]
clustering_Southeast <- Southeast[,6:10]
set.seed(3)
Southeast_sample <- stratified(clustering_Southeast,c("age","gender","income","impressions","click"),10)
#create elbow graph to determine the optimal cluster number 
k3.max <- 9
wss3 <- sapply(1:k3.max, 
              function(k3){kproto(Southeast_sample, k3)$tot.withinss})
wss3
plot(1:k3.max, wss3,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#run K protype model using the optimal cluster number 
kpres3 <- kproto(Southeast_sample, 4)  #4 clusters
clprofiles(kpres3, Southeast_sample)
kpres3 #output cluster prototypes

#region4: Southwest
Southwest <- CampaignPerformanceTable[which(CampaignPerformanceTable$Region == "Southwest"),]
clustering_Southwest <- Southwest[,6:10]
set.seed(4)
Southwest_sample <- stratified(clustering_Southwest,c("age","gender","income","impressions","click"),10)
#create elbow graph to determine the optimal cluster number 
k4.max <- 9
wss4 <- sapply(1:k4.max, 
              function(k4){kproto(Southwest_sample, k4)$tot.withinss})
wss4
plot(1:k4.max, wss4,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#run K protype model using the optimal cluster number 
kpres4 <- kproto(Southwest_sample, 3)  #3 clusters
clprofiles(kpres4, Southwest_sample)
kpres4 #output cluster prototypes

#region5: West
West <- CampaignPerformanceTable[which(CampaignPerformanceTable$Region == "West"),]
clustering_West <- West[,6:10]
set.seed(5)
West_sample <- stratified(clustering_West,c("age","gender","income","impressions","click"),10)
#create elbow graph to determine the optimal cluster number 
k5.max <- 9
wss5 <- sapply(1:k5.max, 
              function(k5){kproto(West_sample, k5)$tot.withinss})
wss5
plot(1:k5.max, wss5,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#run K protype model using the optimal cluster number 
kpres5 <- kproto(West_sample, 3)  #3 clusters
clprofiles(kpres5, West_sample)
kpres5 #output cluster prototypes