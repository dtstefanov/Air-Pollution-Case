#Week 3---------
#Step 1: Consider the datasets on official air quality measurements:------

# OFFICIAL STATIONS ------
#Import EU Data for 2017 & 2018 only in a list
# Set WD to the "EEA Data" folder
setwd("C:\\Users\\kgenov\\Documents\\Trainings\\Sofia University\\Monthly Challenge - October\\EEA Data")
eu17=list.files(path="C:\\Users\\kgenov\\Documents\\Trainings\\Sofia University\\Monthly Challenge - October\\EEA Data", pattern = "2017_timeseries.csv")
eu18=list.files(path="C:\\Users\\kgenov\\Documents\\Trainings\\Sofia University\\Monthly Challenge - October\\EEA Data", pattern = "2018_timeseries.csv")
eu<-c(eu17,eu18)
rm(eu17, eu18)
eu

data_eu=lapply(eu,read.csv,na.string=c("","NA"," "), stringsAsFactors=F, fileEncoding="UTF-16LE")
#data_eu<-readRDS("/Users/kiril/Documents/Sofia University/Monthly Challenge/data/data_eu.RDS") # that was in case of troubles with encoding

for (i in 1:length(eu)){
  eu[i]=gsub("BG_5_","st", eu[i])
  eu[i]=gsub("_timeseries.csv", "", eu[i])
  names(data_eu)[i]=eu[i]
}
rm(eu,i)
setwd("C:\\Users\\kgenov\\Documents\\Trainings\\Sofia University\\Monthly Challenge - October")
#Check the class 
sapply(data_eu[[1]], class) #We need to fix the class of  DatetimeBegin, DatetimeEnd, Validity and Verification

#Merge the 2 datasets for 2017 &2018
library (plyr)
df_eu <- ldply (data_eu, data.frame)

#Fix the class of the variables
if (!require(lubridate)) {
  install.packages("lubridate")
  require(lubridate)
}
df_eu$DatetimeBegin<-ymd_hms(df_eu$DatetimeBegin)
df_eu$DatetimeEnd<-ymd_hms(df_eu$DatetimeEnd)
class(df_eu$DatetimeBegin) #[1] "POSIXct" "POSIXt", same for DatetimeEnd
class(df_eu$DatetimeEnd)

# According to http://dd.eionet.europa.eu/vocabulary/aq/observationvalidity/view the validity should be 1
# Furthermore this seems like a factor variable
df_eu$Validity<-as.factor(df_eu$Validity)
# let's check the factor levels
levels(df_eu$Validity) # [1] "1"

# Same procedure for verifitation http://dd.eionet.europa.eu/vocabulary/aq/observationverification/view
df_eu$Verification<-as.factor(df_eu$Verification)
# let's check the factor levels
levels(df_eu$Verification) # [1] "1"

# plot the P1 official concentration
df_eu<-df_eu[order(df_eu$DatetimeEnd),]
plot(df_eu$DatetimeBegin, df_eu$Concentration, type="l")

#Let's see the period for which we have data from the official staitons
min(df_eu$DatetimeEnd) #[1] "2017-11-28 13:00:00 UTC"
max(df_eu$DatetimeEnd) #[1] "2018-09-14 21:00:00 UTC"

# There's data fom the end of November 2017. 
# !!!!!!! N.B.:
# That means that if we want to use this dataset's variables as predictors, we'll have to restrain ourselves in the period November 2017 - August 2018

# Check for missing values
sapply(df_eu, function(x) sum(is.na(x)))
# This is great! The only missing values in this data frame are in the "Sample" column, which won't be part of our analysis

# Loading meta info about the official stations
setwd("C:\\Users\\kgenov\\Documents\\Trainings\\Sofia University\\Monthly Challenge - October")
if (!require(readxl)) {
  install.packages("readxl")
  require(readxl)
}
metadata<-as.data.frame(read_excel("./EEA Data/metadata.xlsx", na=""))
head(metadata)
# Subset only station id, common name, and topography
metadata<-metadata[,c("AirQualityStationEoICode","CommonName", "Longitude", "Latitude", "Altitude")]
metadata
# let's compare with the official dataset:
unique(df_eu$AirQualityStationEoICode)
# There's no data for Orlov Most Station, (as we know from the data.chat platform it is not functional in 2018)
# Let's remove it from the metadata as well
metadata<-metadata[metadata$CommonName!="Orlov Most",]




# Let's create time series data for every official station
df_eu_short<-df_eu[,c("AirQualityStationEoICode", "Concentration", "DatetimeEnd")]
names(df_eu_short)<-c("AirQualityStationEoICode", "P1_official", "time")
head(df_eu_short)

if (!require(tidyr)) {
  install.packages("tidyr")
  require(tidyr)
}


#NADEZHDA
nadezhda<-df_eu_short[df_eu_short$AirQualityStationEoICode=="BG0040A",]
nadezhda<-nadezhda[order(nadezhda$time),]
# introducing complete time series 
nadezhda<-nadezhda %>% 
  complete (time = seq(min(nadezhda$time), max(nadezhda$time), by="hour"))
nadezhda<-as.data.frame(nadezhda)
# Check for missing values (and visualise it)
sapply(nadezhda, function(x) sum(is.na(x))) # 258
par(mfrow=c(2,1))
plot(nadezhda$time, nadezhda$P1_official, type="l") # there is some missing data in January
# Interpolation of missing values
if (!require(imputeTS)) {
  install.packages("imputeTS")
  require(imputeTS)
}
nadezhda$P1_official<-na.interpolation(nadezhda$P1_official, option = "linear") # SPLINE DID NOT WORK WELL, WE CAN TRY AGAIN
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# YOU CAN TRY REPLACING THE OPTION "linear" ABOVE WITH "spline" AND RUNNING THE CODE FOR NADEZHDA STATION ALL OVER, TO SEE THE TWO PLOTS
nadezhda$AirQualityStationEoICode[is.na(nadezhda$AirQualityStationEoICode)]<-"BG0040A"
sapply(nadezhda, function(x) sum(is.na(x))) # good, 0
plot(nadezhda$time, nadezhda$P1_official, type="l")
names(nadezhda)<-c("time", "AirQualityStationEoICode", "P1_nadezhda")
# WE DO THIS FOR EACH STATION


# HIPODRUMA
hipodruma<-df_eu_short[df_eu_short$AirQualityStationEoICode=="BG0050A",]
hipodruma<-hipodruma[order(hipodruma$time),]
# introducing complete time series 
hipodruma<-hipodruma %>% 
  complete (time = seq(min(hipodruma$time), max(hipodruma$time), by="hour"))
hipodruma<-as.data.frame(hipodruma)
# Check for missing values (and visualise it)
sapply(hipodruma, function(x) sum(is.na(x))) # 189
par(mfrow=c(2,1))
plot(hipodruma$time, hipodruma$P1_official, type="l")
# Interpolation of missing values
hipodruma$P1_official<-na.interpolation(hipodruma$P1_official, option = "linear")
hipodruma$AirQualityStationEoICode[is.na(hipodruma$AirQualityStationEoICode)]<-"BG0050A"
sapply(hipodruma, function(x) sum(is.na(x))) # good, 0
plot(hipodruma$time, hipodruma$P1_official, type="l")
names(hipodruma)<-c("time", "AirQualityStationEoICode", "P1_hipodruma")





# DRUZHBA
druzhba<-df_eu_short[df_eu_short$AirQualityStationEoICode=="BG0052A",]
druzhba<-druzhba[order(druzhba$time),]
# introducing complete time series 
druzhba<-druzhba %>% 
  complete (time = seq(min(druzhba$time), max(druzhba$time), by="hour"))
druzhba<-as.data.frame(druzhba)
# Check for missing values (and visualise it)
sapply(druzhba, function(x) sum(is.na(x))) # 320
par(mfrow=c(2,1))
plot(druzhba$time, druzhba$P1_official, type="l")
# Interpolation of missing values
druzhba$P1_official<-na.interpolation(druzhba$P1_official, option = "linear")
druzhba$AirQualityStationEoICode[is.na(druzhba$AirQualityStationEoICode)]<-"BG0052A"
sapply(druzhba, function(x) sum(is.na(x))) # good, 0
plot(druzhba$time, druzhba$P1_official, type="l")
names(druzhba)<-c("time", "AirQualityStationEoICode", "P1_druzhba")





# PAVLOVO
pavlovo<-df_eu_short[df_eu_short$AirQualityStationEoICode=="BG0073A",]
pavlovo<-pavlovo[order(pavlovo$time),]
# introducing complete time series 
pavlovo<-pavlovo %>% 
  complete (time = seq(min(pavlovo$time), max(pavlovo$time), by="hour"))
pavlovo<-as.data.frame(pavlovo)
# Check for missing values (and visualise it)
sapply(pavlovo, function(x) sum(is.na(x))) # 838
par(mfrow=c(2,1))
plot(pavlovo$time, pavlovo$P1_official, type="l")
# Interpolation of missing values
pavlovo$P1_official<-na.interpolation(pavlovo$P1_official, option = "linear")
pavlovo$AirQualityStationEoICode[is.na(pavlovo$AirQualityStationEoICode)]<-"BG0073A"
sapply(pavlovo, function(x) sum(is.na(x))) # good, 0
plot(pavlovo$time, pavlovo$P1_official, type="l")
names(pavlovo)<-c("time", "AirQualityStationEoICode", "P1_pavlovo")


# MLADOST
mladost<-df_eu_short[df_eu_short$AirQualityStationEoICode=="BG0079A",]
mladost<-mladost[order(mladost$time),]
# introducing complete time series 
mladost<-mladost %>% 
  complete (time = seq(min(mladost$time), max(mladost$time), by="hour"))
mladost<-as.data.frame(mladost)
# Check for missing values (and visualise it)
sapply(mladost, function(x) sum(is.na(x))) # 165
par(mfrow=c(2,1))
plot(mladost$time, mladost$P1_official, type="l")
# Interpolation of missing values
mladost$P1_official<-na.interpolation(mladost$P1_official, option = "linear")
mladost$AirQualityStationEoICode[is.na(mladost$AirQualityStationEoICode)]<-"BG0079A"
sapply(mladost, function(x) sum(is.na(x))) # good, 0
plot(mladost$time, mladost$P1_official, type="l")
names(mladost)<-c("time", "AirQualityStationEoICode", "P1_mladost")

# let's turn back the plot option
par(mfrow=c(1,1))


# OFFICIAL METEROLOGICAL DATA ----
# Before we move on, let's just take a quick look at the official meteorological data
# Now let's load the official meteorogical data in order to compare the intervals

official<-read.csv("./METEO-data/lbsf_20120101-20180917_IP.csv",
                   na.string = c("", "NA", " ", -9999),
                   stringsAsFactors = FALSE)
# Of course, we need data for only 2017 and 2018
official<-official[official$year==2017|official$year==2018,]

summary(official)

# Let's make it time series per day (for now)
if (!require(lubridate)) {
  install.packages("lubridate")
  require(lubridate)
}
official$date<-ymd(paste(official$year, official$Month, official$day))

# the visibility data seems to have outliers that are not real
official$VISIB[(official$VISIB<0)]<-NA
summary(official$VISIB) # Much better now, despite the NA's

# let's see what the period of the official meterological data is
min(official$date, na.rm = TRUE) #[1] "2017-01-01"
max(official$date, na.rm = TRUE) #[1] "2018-09-17"
# Good, we have official data for the whole period. The thing here is that it is per day, not hour, but we'll deal with this next week
# Let's check missing values
sapply(official, function(x) sum(is.na(x)))
# That's good - there are no missing values for most of the variables, which means no interpolation is necessary
# We have to address the missing value in the date column later



#Step 2: Add information on official air quality measurements to the dataset of every geo-unit----

# Okay, before we move on we've got to make a step back and review our approach to the geo units
# At the end of week 2 we had 113 clusters of the 568 citizen stations. 
# Looking forward to the next stages of the project, we'll have to analyze correlation between variables and create a prediction model based on them
# Considering the large scale of work - 113 different datasets to analyze, we decided to create few clusters to work with
# The number of clusters we'll create is 5. If we are confident enough working with such a small number of datasets,
# we can easily go back to the 113 clusters, if of course this would help strenghten the predictive power of our model

# Now let's load again the objects saved from our work during Week 1 and Week 2
load("Kiwi week 2")

# First, we are going to get info about our 113 geo units
z_units<-z4[,c("pnt", "lat", "lng")]
z_unique<-subset(z_units, !duplicated(pnt)) # unique cluster numbers and topographic data

# Let's add the topography of the official stations
official_st_topo<-as.matrix(metadata[,c("Longitude", "Latitude")])

# Using K-means, we create 5 clusters within the geo units list in order to better facilitate our work on the prediction model
# we also set seed, to ensure reproducibility of the analysis
set.seed(123)
z_unique$cluster<-kmeans(x = z_unique[,c("lng","lat")], centers = 5)$cluster

# Three quick plots to get an overview of the clustering process



if (!require(leaflet)) {
  install.packages("leaflet")
  require(leaflet)
}


# Here are the 568 citizen stations and the official stations
official_st_topo %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(sofia_summary$lng, sofia_summary$lat, color = "green", weight = 0.1) %>%
  addMarkers()



# Here are the 113 geo units, formed by joining citizen stations, close to each other

official_st_topo %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(z_unique$lng, z_unique$lat, color="green") %>%
  addMarkers()


# And that's how our 5 clusters look like - the ones that we created with kmeans() to make the prediction model

pal <- colorFactor("Set1", domain = z_unique$cluster)
official_st_topo %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(z_unique$lng, z_unique$lat, color=pal(z_unique$cluster)) %>%
  addMarkers()


# Next, we're going to aggregate the data for each of the 5 clusters using the mean() function for the variables
# Now let's add the number of the respective cluster (1 to 5) to the data frame with all variables z4
cluster_units<-unique(z_unique$cluster)
z4<-plyr::join(z4, z_unique[,c("pnt","cluster")], by="pnt", type = "left", match = "first")

# Let's create a list, containing 5 dataframes. Every dataframe contains information about each cluster, using the mean value for P1, P2, humidity, temperature, pressure 
cluster_list<-list()
# The function below is slow: using system.time for the loop below (around 107 sec. on a Intel i5-7200U CPU 2.50 GHz 2.70GHz)!!!!!!!
# We'll perform this operation using a modified version of the for loop, that we used in week 2

for (i in 1:length(cluster_units)){
  # Create a temporary data frame with the data for each cluster of geo units
  cluster_temp<-z4[z4$cluster==cluster_units[i],]
  cluster_temp<-cluster_temp[order(cluster_temp$time),]
  
  
  # Aggregate the data for all stations within the geo unit using the mean value for each observation, aggregated by the time
  # P1:
  P1_temp<-aggregate(cluster_temp$P1,
                     by = list(cluster_temp$time),
                     mean,
                     na.rm = TRUE)
  colnames(P1_temp) <- c("time","P1")
  # P2
  P2_temp<-aggregate(cluster_temp$P2,
                     by = list(cluster_temp$time),
                     mean,
                     na.rm = TRUE)
  colnames(P2_temp) <- c("time","P2")
  # Temperature
  temperature_temp<-aggregate(cluster_temp$temperature,
                              by = list(cluster_temp$time),
                              mean,
                              na.rm = TRUE)
  colnames(temperature_temp) <- c("time","temperature")
  # Humidity
  humidity_temp<-aggregate(cluster_temp$humidity,
                           by = list(cluster_temp$time),
                           mean,
                           na.rm = TRUE)
  colnames(humidity_temp) <- c("time","humidity")
  # Pressure
  pressure_temp<-aggregate(cluster_temp$pressure,
                           by = list(cluster_temp$time),
                           mean,
                           na.rm = TRUE)
  colnames(pressure_temp) <- c("time","pressure")
  
  join_temp<-join_all(list(P1_temp, P2_temp, temperature_temp, humidity_temp, pressure_temp),
                      by="time",
                      type='left')
  
  # Fill in missing steps in the time series sequence !!!NB: NA values are introduced here
  join_temp<-join_temp %>% 
    complete (time = seq(min(join_temp$time), max(join_temp$time), by="hour"))
  join_temp<-as.data.frame(join_temp)
  
  # now let's add center of the cluster and cluster number
  join_temp$lat<-rep(mean(cluster_temp$lat), length(join_temp$time))
  join_temp$lng<-rep(mean(cluster_temp$lng), length(join_temp$time))
  join_temp$clust_no<-rep(cluster_units[i], length(join_temp$time))
  
  # Save the result as a data frame in the list with the appropriate name (e.g. cluster 1's data frame is called "cluster_1")
  cluster_list[[i]]<-assign (paste0("cluster_",cluster_units[i]),
                             data.frame(join_temp))
  
  names(cluster_list)[i]<-paste0("cluster_",cluster_units[i])
  # Clean environment from temporary files created within this loop
  rm(list=paste0("cluster_",cluster_units[i]), 
     cluster_temp, P1_temp, P2_temp, temperature_temp, 
     humidity_temp, pressure_temp, join_temp)
  
}

# Now we have a list of 5 clusters, containing average P1, P2, humidity, temperature, pressure of all geo units in it,
# also the topo center of the cluster and its respective number


#let's check once again the min and max dates that we have for the official stations and the clusters 
check_dates_clusters<-data.frame("min"=c(min(cluster_list[[1]]$time),
                                         min(cluster_list[[2]]$time),
                                         min(cluster_list[[3]]$time),
                                         min(cluster_list[[4]]$time),
                                         min(cluster_list[[5]]$time)),
                                 
                                 "max"=c(max(cluster_list[[1]]$time),
                                         max(cluster_list[[2]]$time),
                                         max(cluster_list[[3]]$time),
                                         max(cluster_list[[4]]$time),
                                         max(cluster_list[[5]]$time)))
check_dates_clusters
# the TS data for all clusters starts on 2017-09-06 20:00:00 and finishes on 2018-08-16 12:00:00

check_dates_stations<-data.frame("min"=c(min(nadezhda$time),
                                         min(hipodruma$time),
                                         min(druzhba$time),
                                         min(pavlovo$time),
                                         min(mladost$time)),
                                 
                                 "max"=c(max(nadezhda$time),
                                         max(hipodruma$time),
                                         max(druzhba$time),
                                         max(pavlovo$time),
                                         max(mladost$time)),
                                 
                                 row.names = c("nadezhda", "hipodruma", "druzhba", "pavlovo", "mladost"))

check_dates_stations
# we have to think how to approach mladost station data, because it starts in 2018




# For the time being, let's join the dataframes, we'll handle NA values and periods later 

for (i in 1: length(cluster_list)){
  cluster_list[[i]]<-join_all(list(cluster_list[[i]],
                                   nadezhda[,c("time", "P1_nadezhda")],
                                   hipodruma[,c("time", "P1_hipodruma")],
                                   druzhba[,c("time", "P1_druzhba")],
                                   pavlovo[,c("time", "P1_pavlovo")],
                                   mladost[,c("time", "P1_mladost")]),
                              by="time",
                              type='full')
  }

# let's view the result
head(cluster_list[[1]])

stations_list<-list()

stations_list[[1]]<-nadezhda
stations_list[[2]]<-hipodruma
stations_list[[3]]<-druzhba
stations_list[[4]]<-pavlovo
stations_list[[5]]<-mladost

names(stations_list)<- c("nadezhda",
                         "hipodruma",
                         "druzhba",
                         "pavlovo",
                         "mladost")

rm(check_dates_clusters,
   check_dates_stations,
   clust_summary,
   data_bg_final,
   data_eu,
   df_eu,
   df_eu_short,
   ts_list,
   z_unique,
   z_units,
   i,
   cluster_units,
   pal,
   nadezhda,
   hipodruma,
   pavlovo,
   druzhba,
   mladost,
   sofia_summary,
   sofia_topo)

save(list=ls(),file="Kiwi week 3")

