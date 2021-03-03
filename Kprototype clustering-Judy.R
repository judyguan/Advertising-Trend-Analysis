#read campaign performance table
setwd("~/Downloads/Analytic Trends Project Data /CampaignPerformanceTable")
library(dplyr)
library(readr)
library(clustMixType)
#Get the file names
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

###################################################clustering altogether##################################################################################
###region 1: Midwest 1967716 obs.
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
#kproto!!!
#http://www.imsbio.co.jp/RGM/R_rdfile?f=clustMixType/man/kproto.Rd&d=R_CC
library(clustMixType)
kpres <- kproto(Midwestdata, 4)
clprofiles(kpres, Midwestdata)
kpres
#Cluster prototypes:
#age gender income impressions     click
#1: 35-44      F   <75k  1908.68000 1.1709091
#2: 55-64      F   <75k   497.69787 0.3212273
#3: 35-44      M   <75k    85.23576 0.1937867
#4:   65+      F   <75k    67.46559 0.2556075


#kpres <- kproto(smalldata, 2,lambda=0.1)
#clprofiles(kpres, smalldata)
#kpres <- kproto(smalldata, 2,lambda=25)
#clprofiles(kpres, smalldata)

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
kpres2
#age gender   income impressions     click
#1: 35-44      M 75-<125k    113.8719 0.2223166
#2: 25-34      F     <75k    205.0366 0.2507994
#3:   <25      M    125k+     79.2344 0.2651638
#4: 25-34      F     <75k   1658.2082 0.7755544
  
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
kpres3
#Cluster prototypes:
#  age gender   income impressions     click
#1:   65+      F     <75k   968.18192 0.6910755
#2:   65+      F 75-<125k    47.77333 0.2740162
#3: 45-54      M     <75k    80.62556 0.2108053
#4:   65+      M    125k+   145.88246 0.1705226




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
kpres4
#Cluster prototypes:
#age gender income impressions     click
#1: 55-64      M   <75k    50.31117 0.1940068
#2: 45-54      F  125k+    69.41027 0.1505698
#3: 55-64      F   <75k   603.59631 0.4016393



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
kpres5


#K-protptyping
for large mixed dataset


###################################################clustering by beauty and fashion##################################################################################
###Region 1: Midwest 1967716 obs.
Midwest_beauty = Midwest[(Midwest$CampaignID=='162554' | 
                            Midwest$CampaignID=='167587' |
                            Midwest$CampaignID=='165864' |
                            Midwest$CampaignID=='168250' |
                            Midwest$CampaignID=='166489' |
                            Midwest$CampaignID=='184598' |
                            Midwest$CampaignID=='163695' |
                            Midwest$CampaignID=='193334' ), ]

#selecting age,gender,income and impressions
clustering_midwest_beauty <- Midwest_beauty[,6:10]

#stratified sampling
library(splitstackshape)
set.seed(11)
Midwestdata_beauty <- stratified(clustering_midwest_beauty,c("age","gender","income","impressions","click"),10)

# Elbow Method for finding the optimal number of clusters
library(clustMixType)
set.seed(12)
# Compute and plot wss for k = 2 to k = 9.
k6.max <- 9
wss6 <- sapply(1:k6.max, 
              function(k6){kproto(Midwestdata_beauty, k6)$tot.withinss})
wss6
plot(1:k6.max, wss6,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")    #picked cluster 3
#cluster 4 is the optimal choice
#kproto!!!
#http://www.imsbio.co.jp/RGM/R_rdfile?f=clustMixType/man/kproto.Rd&d=R_CC
library(clustMixType)
kpres6 <- kproto(Midwestdata_beauty, 3)
clprofiles(kpres6, Midwestdata_beauty)
kpres6
#Cluster prototypes:
#  age gender income impressions     click
#1:   65+      F  125k+   100.09518 0.1663626
#2: 35-44      M   <75k  1106.62398 0.7113821
#3: 25-34      F   <75k    74.94583 0.2205103

#midwest fashion
Midwest_fashion = Midwest[(Midwest$CampaignID=='167179' | 
                             Midwest$CampaignID=='164742' |
                             Midwest$CampaignID=='164706' |
                             Midwest$CampaignID=='167137' |
                             Midwest$CampaignID=='182273' |
                             Midwest$CampaignID=='183204' |
                             Midwest$CampaignID=='184491' |
                             Midwest$CampaignID=='184120' |
                             Midwest$CampaignID=='188753' |
                             Midwest$CampaignID=='189877' |
                             Midwest$CampaignID=='167392'), ]
#selecting age,gender,income and impressions
clustering_midwest_fashion <- Midwest_fashion[,6:10]

#stratified sampling
library(splitstackshape)
set.seed(13)
Midwestdata_fashion <- stratified(clustering_midwest_fashion,c("age","gender","income","impressions","click"),10)

# Elbow Method for finding the optimal number of clusters
library(clustMixType)
set.seed(14)
# Compute and plot wss for k = 2 to k = 9.
k7.max <- 9
wss7 <- sapply(1:k7.max, 
               function(k7){kproto(Midwestdata_fashion, k7)$tot.withinss})
wss7
plot(1:k7.max, wss7,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")    #picked cluster 3
library(clustMixType)
kpres7 <- kproto(Midwestdata_fashion, 3)
clprofiles(kpres7, Midwestdata_fashion)
kpres7

##############################################################Region2: 

