rm(list=ls())
# Step 1: Import data on citizen science air quality measurements and topography data for Sofia. ----
setwd("") # put the path to your working directory here 
getwd() #check WD

# installing library to unzip the "gz" files
# install.packages("R.utils") # in case you don't have this package installed
library(R.utils)

if(file.exists("./data_bg_2017.csv.gz")){
  gunzip("./Air Tube/data_bg_2017.csv.gz")
}

if(file.exists("./data_bg_2018.csv.gz")){
  gunzip("./Air Tube/data_bg_2018.csv.gz")
}

# reading in the "citizen science air quality measurements" data into R, strings imported as strings (rather than as factors) ----
data_bg_2017<-read.csv("./Air Tube/Data_bg_2017.csv", stringsAsFactors = FALSE)
data_bg_2018<-read.csv("./Air Tube/Data_bg_2018.csv", stringsAsFactors = FALSE)

# reading in the topography data for Sofia
sofia_topo<-read.csv("./TOPO-DATA/sofia_topo.csv")

# Step 2: Inspect data structure and correct if any inconsistencies ----

# Let's view some basic characteristics to get a "feeling" for the data ----
head(data_bg_2017)
head(data_bg_2018)

# now let's make a quick summary
summary(data_bg_2017)

# For the 2017 dataset we can already observe that:
# - the P1 and P2 features have a very high maximum value.
# - the "temperature" feature has a minimum value of -148 and a maximum value of 63, which seem practically not possible
# - the maximum amount for "pressure" should also be looked into at a later stage

summary(data_bg_2018)

# analogically for 2018 we see some pretty obvious potential outliers in all of the variables




# now let's check the class of the variables ----
names(data_bg_2017)==names(data_bg_2018)
# colnames of the two "citizen" datasets are the same, so we can work with both of the datasets

data_class<-data.frame()
for (i in 1:length(names(data_bg_2017))){
  data_class[i,1]<-names(data_bg_2017)[i]
  data_class[i,2]<-class(data_bg_2017[,i])
  data_class[i,3]<-class(data_bg_2018[,i])
}
names(data_class)<-c("Feature", "data_bg_2017", "data_bg_2018")
data_class

# it looks like all variables are of the appropriate class, except for "time". Let's fix that
# install.packages("lubridate") # in case we don't have the library installed
library(lubridate)
data_bg_2017$time<-ymd_hms(data_bg_2017$time)
data_bg_2018$time<-ymd_hms(data_bg_2018$time)
class(data_bg_2017$time); class(data_bg_2018$time)
# All good - now let's check the topo data

head(sofia_topo)
summary(sofia_topo)
data_class<-data.frame()
for (i in 1:length(names(sofia_topo))){
  data_class[i,1]<-names(sofia_topo)[i]
  data_class[i,2]<-class(sofia_topo[,i])
}
data_class
# all features are of class "numeric" - that's good
rm(data_class, i)

# Step 3 ----

# Now let's calculate the number of unique stations for 2017 and 2018
unique_2017<-unique(data_bg_2017$geohash)
length(unique_2017)
# 383 unique stations in 2017

unique_2018<-unique(data_bg_2018$geohash)
length(unique_2018)
# 1254 unique stations in 2018


# It is not reasonable to make predictions for stations that were not observed in 2018, 
# so we'll find and remove data for stations only observed in 2017

only_in_2017<-setdiff(data_bg_2017$geohash, data_bg_2018$geohash)
length(only_in_2017)
# 11 stations have data only for 2017, let's take them out of the dataset

data_bg_2017[-which(data_bg_2017$geohash==only_in_2017[1]),]
data_bg_2017_new<-data_bg_2017[-which(data_bg_2017$geohash==only_in_2017),] #THIS SUBSETTING DID NOT WORK PROPERLY!!!
unique_2017_new<-length(unique(data_bg_2017_new$geohash))
head(unique_2017_new)
length(unique_2017_new)

