# Week 1: ----
# put the path to your working directory here:
wd<-"/Users/kiril/Documents/Sofia University/Monthly Challenge/data"

# Step 1: Import data on citizen science air quality measurements and topography data for Sofia ----
rm(list=ls())
setwd(wd) 
getwd() #check WD

# installing library to unzip the "gz" files
# install.packages("R.utils") # in case you don't have this package installed

if (!require(R.utils)) {
  install.packages("R.utils")
  require(R.utils)
}

if(file.exists("./Air Tube/data_bg_2017.csv.gz")){
  gunzip("./Air Tube/data_bg_2017.csv.gz")
}

if(file.exists("./Air Tube/data_bg_2018.csv.gz")){
  gunzip("./Air Tube/data_bg_2018.csv.gz")
}

# reading in the "citizen science air quality measurements" data into R, strings imported as strings (rather than as factors) 
data_bg_2017<-read.csv("./Air Tube/Data_bg_2017.csv", stringsAsFactors = FALSE)
data_bg_2018<-read.csv("./Air Tube/Data_bg_2018.csv", stringsAsFactors = FALSE)

# reading in the topography data for Sofia
sofia_topo<-read.csv("./TOPO-DATA/sofia_topo.csv")

# Step 2: Inspect data structure and correct if any inconsistencies ----

# Let's view some basic characteristics to get a "feeling" for the data
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




# now let's check the class of the variables
names(data_bg_2017)==names(data_bg_2018)
# colnames of the two datasets are the same, so we can work with both of the datasets

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

if (!require(lubridate)) {
  install.packages("lubridate")
  require(lubridate)
}

data_bg_2017$time<-ymd_hms(data_bg_2017$time)
data_bg_2018$time<-ymd_hms(data_bg_2018$time)
class(data_bg_2017$time); class(data_bg_2018$time)
# All good - now let's check the topo data

head(sofia_topo)
summary(sofia_topo)
data_class_topo<-data.frame()
for (i in 1:length(names(sofia_topo))){
  data_class_topo[i,1]<-names(sofia_topo)[i]
  data_class_topo[i,2]<-class(sofia_topo[,i])
}
data_class_topo
# all features are of class "numeric" - that's good
rm(i, data_class, data_class_topo)

# Step 3: Handle unique stations and merge datasets on citizen science air quality measurements ----

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
only_in_2017 <- as.data.frame(only_in_2017)

data_bg_2017_new <- subset(data_bg_2017, !(data_bg_2017$geohash %in% only_in_2017$only_in_2017))

# check the removal
length(unique(data_bg_2017$geohash))
length(unique(data_bg_2017_new$geohash))

# binding datasets

data_bg_full <- rbind(data_bg_2017_new, data_bg_2018)
rm(data_bg_2017_new, unique_2018, data_bg_2017, data_bg_2018)

# Step 4: Summarize availability of merged data by stations ----

if (!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}

#Let's check for NAs
sum(is.na(data_bg_full$geohash)) #[1] 0 There are no NAs
sum(data_bg_full$geohash == "") # [1] 4 There are 4 empty geohashes

#Clean missing geohashes
data_bg_clean <- data_bg_full%>%
  filter(data_bg_full$geohash != "")

# summarize the observations
freq <- data.frame(table(data_bg_clean$geohash)) 

min <- aggregate(time ~ geohash, data = data_bg_clean, min) #min
max <- aggregate(time ~ geohash, data = data_bg_clean, max) #max

# merge min and max and prepare for merging to the final summary
minmax <- merge(min, max, by="geohash")

# building the final summary dataset
colnames(freq)[colnames(freq) == 'Var1'] <- 'geohash'
summary <- merge(freq, minmax, by="geohash")
summary$days <- difftime(summary$time.y, summary$time.x, units = "days")

# do some renaming
colnames(summary)[colnames(summary) == 'time.x'] <- 'tmin'
colnames(summary)[colnames(summary) == 'time.y'] <- 'tmax'
colnames(summary)[colnames(summary) == 'Freq'] <- 'obs'

# check the summary
head(summary)

rm(min, max, freq, minmax)

# Step 5: Summarize stations on the map ----


if (!require(geohash)) {
  install.packages("geohash")
  require(geohash)
}

# getting geohashes decoded

geohash_decoded <- as.data.frame(gh_decode(summary$geohash))
summary_decoded <- data.frame(summary, geohash_decoded)


# plotting some maps

if (!require(leaflet)) {
  install.packages("leaflet")
  require(leaflet)
}

# plot all stations on the map

summary_decoded %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())

# looks like there are stations outside Sofia

# extract Sofia stations only
colnames(sofia_topo)[colnames(sofia_topo) == 'Lat'] <- 'lat'
colnames(sofia_topo)[colnames(sofia_topo) == 'Lon'] <- 'lng'
sofia_summary <- summary_decoded[which(summary_decoded$lat < max(sofia_topo$lat) & summary_decoded$lat > min(sofia_topo$lat)& summary_decoded$lng < max(sofia_topo$lng) & summary_decoded$lng > min(sofia_topo$lng) ), ]


# plot stations in Sofia only
sofia_summary %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(weight = 0.1, color = "red") %>%
  addRectangles(lat1 = (min(sofia_summary$lat)-0.01), lng1 = (max(sofia_summary$lng)-0.18), 
                lat2 = (min(sofia_summary$lat)+0.13), lng2 = (min(sofia_summary$lng)+0.18),
                fillColor = "transparent")


# Step 6: Filter stations by space and time ----
#filter geohashes > 10
summary <-  filter(summary,obs > 10) #1248 observations left
#filter days>7
summary <-  filter(summary,days > 7) #1216 observations left 

# now that we have selected the stations we're going to use for the analysis,
# subset the dataset with the clear citizen data for Sofia, using the filters we defined in step 4 and 5 above 
data_bg_final <- subset(data_bg_clean, (data_bg_clean$geohash %in% sofia_summary$geohash))
data_bg_final <- data.frame(data_bg_final, as.data.frame(gh_decode(data_bg_final$geohash)))

rm(summary_decoded, unique_2017, unique_full_set, only_in_2017)

# Week 2: ----
# Step 1: Decide on the final list of geo-units that are subject to predictive analysis ----

# getting Sofia points clustered
sofia_topo$point <- 1:nrow(sofia_topo) 

# create distance matrix

if (!require(geosphere)) {
  install.packages("geosphere")
  require(geosphere)
}

dist_mat <- distm(sofia_summary[,c('lng','lat')], sofia_topo[,c('lng','lat')], fun=distVincentyEllipsoid)

sofia_summary$Elev <- sofia_topo$Elev[max.col(-dist_mat)]
sofia_summary$pnt <- sofia_topo$point[max.col(-dist_mat)]

# plot the stations based on the clusters (points from sofia_topo)

pal <- colorFactor("viridis", domain = sofia_summary$pnt)

sofia_summary %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    color = ~pal(pnt),
    stroke = FALSE, fillOpacity = 0.5) %>%
  addRectangles(lat1 = (min(sofia_summary$lat)-0.01), lng1 = (max(sofia_summary$lng)-0.18), 
                lat2 = (min(sofia_summary$lat)+0.13), lng2 = (min(sofia_summary$lng)+0.18),
                fillColor = "transparent")


if (!require(plotly)) {
  install.packages("plotly")
  require(plotly)
}

axx <- list(
  title = "Lattitude"
)

axy <- list(
  title = "Longitude"
)

axz <- list(
  title = "Elevation"
)
plot_ly(x = ~sofia_summary$lat,
        y = ~sofia_summary$lng,
        z = ~sofia_summary$Elev,
        type = "scatter3d",
        color = ~sofia_summary$pnt) %>% 
  layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

rm(axx, axy, axz)


# Step 2: Merge cluster information to the citizen science air quality measurements ----
z1 <- unique(sofia_summary[,c("pnt", "Elev")]) # clust, elev
colnames(z1) <- c("clust","elev")

z2 <- as.data.frame(table(sofia_summary$pnt)) # st.num
colnames(z2) <- c("clust","st.num")

z3 <- aggregate(obs ~ pnt, data = sofia_summary, sum) # obs
colnames(z3) <- c("clust","obs")

z4 <- merge(x = data_bg_final, y = sofia_summary[ , c("geohash", "pnt")], by = "geohash", all.x=TRUE)

z5 <- aggregate(z4$time, by = list(z4$pnt), min) # t.min
colnames(z5) <- c("clust","t.min")

z6 <- aggregate(z4$time, by = list(z4$pnt), max) # t.max
colnames(z6) <- c("clust","t.max")

z7 <- aggregate(z4$P1, by = list(z4$pnt), min) # p1.min
colnames(z7) <- c("clust","p1.min")

z8 <- aggregate(z4$P1, by = list(z4$pnt), max) # p1.max
colnames(z8) <- c("clust","p1.max")

z9 <- aggregate(z4$P2, by = list(z4$pnt), min) # p2.min
colnames(z9) <- c("clust","p2.min")

z10 <- aggregate(z4$P2, by = list(z4$pnt), max) # p2.max
colnames(z10) <- c("clust","p2.max")

z11 <- aggregate(z4$temperature, by = list(z4$pnt), min) # temp.min
colnames(z11) <- c("clust","temp.min")

z12 <- aggregate(z4$temperature, by = list(z4$pnt), max) # temp.max
colnames(z12) <- c("clust","temp.max")

z13 <- aggregate(z4$pressure, by = list(z4$pnt), min) # press.min
colnames(z13) <- c("clust","press.min")

z14 <- aggregate(z4$pressure, by = list(z4$pnt), max) # press.max
colnames(z14) <- c("clust","press.max")

z15 <- aggregate(z4$humidity, by = list(z4$pnt), min) # hum.min
colnames(z15) <- c("clust","hum.min")

z16 <- aggregate(z4$humidity, by = list(z4$pnt), max) # hum.max
colnames(z16) <- c("clust","hum.max")

# join all
if (!require(plyr)) {
  install.packages("plyr")
  require(plyr)
}

clust_summary <- join_all(list(z1, z2, z3, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14, z15, z16), by='clust', type='left')
rm(z1, z2, z3, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14, z15, z16)

# Step 3: Clean data for mismeasurements and inconsistencies ------

#Check our features for inconsistencies:
min(z4$temperature) #[1] -5573 -> impossible, indicates potencial inconsistencies
max(z4$temperature) #[1] 63
max(z4$humidity) #[1] 110
min(z4$humidity) # [1] -999
max(z4$pressure) #[1] 164990
min(z4$pressure) #[1] 0

#Let's create our own rules to localize potential errors
if (!require(editrules)) {
  install.packages("editrules")
  require(editrules)
}

E <- editmatrix(c(
  "P1 >= 0.09", 
  "P1 <= 690",
  "temperature >= -25",
  "temperature <= 40",
  "pressure >= 99018",
  "pressure <= 105113.5",
  "humidity >= 5",
  "humidity <= 100"
))

#Localize all errors in the data
err <- localizeErrors(E, z4,verbose=TRUE)
plot(err) #Most errors come from pressure
sum(err$adapt) #[1] 1 929 341 errors detected
sum(err$adapt[, "P1"]) #70324 errors in P1

z4$mismeasurements <- ifelse(err$adapt[,"P1"]==T,1,0)

detach("package:plyr", unload=TRUE)
z4mm=z4 %>%
  group_by(geohash) %>%
  summarise(freq=n(), mm=sum(mismeasurements), perc=mm/freq) %>%
  arrange(desc(perc)) 
#Removing 100% mismeasurements in P1
list1 <-z4mm$geohash[z4mm$perc==1]
z4=z4[!(z4$geohash %in% list1),]

err_new <- localizeErrors(E, z4,verbose=TRUE)
sum(err_new$adapt) #[1] 1 830 150 errors detected
rm(list1, z4mm)
prout=boxplot.stats(z4$pressure, coef=2, do.conf = F, do.out = F)
prout$stats
cc["Pmin"]=prout$stats

#Clean our geo unit fro inconsistencies and return NA on mismeasurements

z4$P1=ifelse(z4$P1>690,NA,ifelse(z4$P1<0.09,NA, z4$P1))
z4$humidity=ifelse(z4$humidity<5,NA,z4$humidity)
min_temp <- -25
z4$temperature=ifelse(z4$temperature>40,NA,ifelse(z4$temperature < min_temp,NA, z4$temperature))
z4$pressure=ifelse(z4$pressure>105113.5,NA,ifelse(z4$pressure<99018,NA, z4$pressure))
sum(is.na(z4))#[1] 1 928 965

rm(prout, E, err, err_new)
#They look much better now and we can move on


# Step 4: Aggregate data by geo-units ----

if (!require(tidyr)) {
  install.packages("tidyr")
  require(tidyr)
}

# Get the numbers of all geo units (clusters)
geo_units<-unique(z4$pnt) # z4 FROM STEP 2 IS USED; CHANGE THIS AFTER THE RULES CREATED STEP 3
length(geo_units) # [1] 116

# Create an empty list object to save the time series data for each cluster
ts_list<-list()
library(plyr) 
# The function below is slow: using system.time for the loop below (around 107 sec. on a Intel i5-7200U CPU 2.50 GHz 2.70GHz)!!!!!!!
# Since there are 116 clusters, we'll perform this operation using a for loop
for (i in 1:length(geo_units)){
  # Create a temporary data frame with the data for each geo unit
  cluster_temp<-z4[z4$pnt==geo_units[i],]
  
  # Fill in missing steps in the time series sequence !!!NB: NA values are introduced here
  cluster_temp<-cluster_temp %>% 
    complete (time = seq(min(cluster_temp$time), max(cluster_temp$time), by="hour"))
  cluster_temp<-as.data.frame(cluster_temp)
  
  # Aggregate the data for all stations within the geo unit using the mean value for each observation, aggregated by the time
  # P1:
  P1_temp<-aggregate(cluster_temp$P1,
                     by = list(cluster_temp$time),
                     mean,
                     na.action=NULL)
  colnames(P1_temp) <- c("time","P1")
  # P2
  P2_temp<-aggregate(cluster_temp$P2,
                     by = list(cluster_temp$time),
                     mean,
                     na.action=NULL)
  colnames(P2_temp) <- c("time","P2")
  # Temperature
  temperature_temp<-aggregate(cluster_temp$temperature,
                              by = list(cluster_temp$time),
                              mean,
                              na.action=NULL)
  colnames(temperature_temp) <- c("time","temperature")
  # Humidity
  humidity_temp<-aggregate(cluster_temp$humidity,
                           by = list(cluster_temp$time),
                           mean,
                           na.action=NULL)
  colnames(humidity_temp) <- c("time","humidity")
  # Pressure
  pressure_temp<-aggregate(cluster_temp$pressure,
                           by = list(cluster_temp$time),
                           mean,
                           na.action=NULL)
  colnames(pressure_temp) <- c("time","pressure")
  
  # Save the result as a data frame in the list with the appropriate name (e.g. cluster 1's data frame is called "cluster_1")
  ts_list[[i]]<-assign (paste0("cluster_",geo_units[i]),
                        data.frame(join_all(list(P1_temp, P2_temp, temperature_temp, humidity_temp, pressure_temp),
                                            by="time",
                                            type='left')))
  
  # Clean environment from temporary files created within this loop
  rm(list=paste0("cluster_",geo_units[i]), cluster_temp, P1_temp, P2_temp, temperature_temp, humidity_temp, pressure_temp)
  
}


# Step 5: Inspect and summarize the most important statistical characteristics --------
if (!require( tseries)) {
  install.packages(" tseries")
  require( tseries)
}
library(fBasics)

#Ljung-Box Test for stationarity
box=rep(NA,length(ts_list))
for(i in 1:length(ts_list)){
  b=Box.test((ts_list[[i]][,"P1"]), lag=1, type= c("Ljung-Box"), fitdf = 0)
  box [i]=b$p.value
  rm(b, i)
}
box<-as.data.frame(box)
plot(box)

rm(adf,axx,axy,axz,box, dist_mat,filtered, geo_units, geohash_decoded, i,pal, min_temp, stats, ts,summary,data_bg_clean, data_bg_full)
save(list=ls(),file="Kiwi week 2")
# Week 3: ---------
# Step 1: Consider the datasets on official air quality measurements------

# OFFICIAL STATIONS
#Import EU Data for 2017 & 2018 only in a list
# Set WD to the "EEA Data" folder
setwd("/Users/kiril/Documents/Sofia University/Monthly Challenge/data/EEA Data")
eu17=list.files(path="/Users/kiril/Documents/Sofia University/Monthly Challenge/data/EEA Data", pattern = "2017_timeseries.csv")
eu18=list.files(path="/Users/kiril/Documents/Sofia University/Monthly Challenge/data/EEA Data", pattern = "2018_timeseries.csv")
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
setwd("/Users/kiril/Documents/Sofia University/Monthly Challenge")
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
setwd("/Users/kiril/Documents/Sofia University/Monthly Challenge/data")
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


# OFFICIAL METEROLOGICAL DATA 
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



# Step 2: Add information on official air quality measurements to the dataset of every geo-unit----

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
geo_units<-unique(z4$pnt)


for (i in 1:length(geo_units)){
  # Create a temporary data frame with the data for each cluster of geo units
  cluster_temp<-z4[z4$pnt==geo_units[i],]
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
  join_temp$clust_no<-rep(geo_units[i], length(join_temp$time))
  
  # Save the result as a data frame in the list with the appropriate name (e.g. cluster 1's data frame is called "cluster_1")
  cluster_list[[i]]<-assign (paste0("cluster_",geo_units[i]),
                             data.frame(join_temp))
  
  names(cluster_list)[i]<-paste0("cluster_",geo_units[i])
  # Clean environment from temporary files created within this loop
  rm(list=paste0("cluster_",geo_units[i]), 
     cluster_temp, P1_temp, P2_temp, temperature_temp, 
     humidity_temp, pressure_temp, join_temp)
  
}

# Now we have a list of 113 clusters, containing average P1, P2, humidity, temperature, pressure of all geo units in it,
# also the topo center of the cluster and its respective number


#THIS SHOULD BE DONE FOR ALL 113 CLUSTERS let's check once again the min and max dates that we have for the official stations and the clusters 
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

# add the wheather data (might be useful for the model)

if (!require(data.table)) {
  install.packages("data.table")
  require(data.table)
}

cluster_df <- rbindlist(cluster_list)
cluster_df$date <- as.Date(cluster_df$time)

official_avg <- data.frame(official$date, official$TASAVG, official$DPAVG, official$RHAVG, official$sfcWindAVG, official$PSLAVG, official$PRCPAVG)
colnames(official_avg)[1] <- "date"
cluster_df <- merge(cluster_df, official_avg, by = "date", all.x = TRUE)
cluster_df$date <- NULL
head(cluster_df)
# save the environment

# Step 3: Design and implement a decomposition procedure ----

# check the correlations

if (!require(corrplot)) {
  install.packages("corrplot")
  require(corrplot)
}

cluster_df_omited <- na.omit(cluster_df)

cluster_corr <- cor(cluster_df_omited[,2:20])
corrplot(cluster_corr, method = "square", type = "upper", addCoef.col = "black")

# do the stationary test

if (!require(TSA)) {
  install.packages("TSA")
  require(TSA)
}

stat_test <- function(df){
  p <- ncol(df)
  df_multi <- data.frame(var=names(df),
                         box.pvalue=sapply(df, function(v) Box.test(ts(v),lag=20,type="Ljung-Box")$p.value),
                         adf.pvalue=sapply(df, function(v) adf.test(ts(v),alternative = "stationary")$p.value),
                         kpss.pvalue=sapply(df, function(v) kpss.test(ts(v))$p.value)
  )
  df_multi$box <- df_multi$box.pvalue < 0.05
  df_multi$adf <- df_multi$adf.pvalue < 0.05
  df_multi$kpss <- df_multi$kpss.pvalue > 0.05
  row.names(df_multi) <- c()
  df_multi
}
head(cluster_df_omited)
stats_df_omited <- stat_test(cluster_df_omited)
head(stats_df_omited)

# lm

fit <- lm(P1 ~ ., data = cluster_df_omited) 
summary(fit)

save(list=ls(),file="Kiwi week 3")

# Week 4: ----
# Step 1: Perform exploratory analysis on the statistical characteristics of the response variable (i.e. the adjusted PM10 concentration) for each geo-unit ----
# loading environment from provious weeks
setwd("/Users/kiril/Documents/Sofia University/Monthly Challenge/data")
load("Kiwi week 3")

# calc the NAs in the dataset

na_count <- data.frame(names(cluster_df))
row.names(na_count) <- na_count$names.cluster_df

for (i in 1:max(cluster_df$clust_no, na.rm = TRUE)){
  na_count[i] <- as.data.frame(round((colSums(is.na(cluster_df[which(cluster_df$clust_no == i),]))/(lengths(cluster_df[which(cluster_df$clust_no == i),])))*100, digits = 2))
  colnames(na_count)[i] <- paste0("perc_NAs_clust_", i)
}
head(cluster_list[[1]])
na_count <- na_count[,!(na_count[1,]=='NaN')]
head(na_count)
# after omiting the NAs the dataset's size significantly dropped (from 786k to 1k)
# let's try interpolate the NAs instead of omiting

if (!require(imputeTS)) {
  install.packages("imputeTS")
  require(imputeTS)
}

sum(is.na(cluster_df$P1))

par(mfrow=c(2,1))
plot(cluster_df$time, cluster_df$P1, type="l")

cluster_df$P1 <- na.interpolation(cluster_df$P1, option = "linear")

plot(cluster_df$time, cluster_df$P1, type="l")

save(list=ls(),file="Kiwi week 4")

### SATURDAY, 17TH NOVEMBER 2018
# Kiril: I think working on the list for each cluster would be a better option than dataframe containing all info
# or at least I can't make it work with the dataframe :)))

# Adding the official average information for each cluster in the cluster_list
head(official_avg)
official_avg_by_hours<-official_avg
head(official_avg_by_hours)
names(official_avg_by_hours)
#let's make it for each hour:
colnames(official_avg_by_hours)[which(names(official_avg_by_hours) == "official.date")] <- "time"
official_avg_by_hours$time<-paste(official_avg_by_hours$time, rep("00:00:00",length(official_avg_by_hours$time)))
official_avg_by_hours$time<-ymd_hms(official_avg_by_hours$time, tz="Europe/Athens") # warning message: 1 failed to parse
which(is.na(official_avg_by_hours$time)) #274
official_avg_by_hours[270:280,]
# hehehe, there's a date 31st of September
# we directly remove it, there's no explanation in the readme file, no questions in the forums, no info in Boryana's file, and most importantly: no time :)
official_avg_by_hours<-official_avg_by_hours[-which(is.na(official_avg_by_hours$time)),]
library(tidyr)
official_avg_by_hours<-official_avg_by_hours %>% 
  complete (time = seq(min(official_avg_by_hours$time), max(official_avg_by_hours$time), by="hour"))
official_avg_by_hours<-as.data.frame(official_avg_by_hours)

if (!require(imputeTS)) {
  install.packages("imputeTS")
  require(imputeTS)
}
sum(is.na(official_avg_by_hours)) # [1] 86130
# Very cool function from the tidyr package for filling in missing data
official_avg_by_hours<-fill(official_avg_by_hours,names(official_avg_by_hours))
sum(is.na(official_avg_by_hours)) # [1] 0

# just to check:
dim(cluster_list[[1]]) # [1] 8461   14
# alright, now let's add it to each cluster:
for (i in 1:length(cluster_list)){
  cluster_list[[i]] <- join_all(list(cluster_list[[i]], official_avg_by_hours), by = "time")
}
# check again
dim(cluster_list[[1]]) # [1] 8461   20
head(cluster_list[[1]])
