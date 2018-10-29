rm(list=ls())
# Week 1: ----
# Step 1: Import data on citizen science air quality measurements and topography data for Sofia ----
setwd("C:\\Users\\kgenov\\Documents\\Trainings\\Sofia University\\Monthly Challenge - October") # put the path to your working directory here 
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
min(clust_summary$temp.min) #[1] -5573 -> impossible, indicates potencial inconsistencies
max(clust_summary$temp.max) #[1] 63
min(clust_summary$press.min) #[1] 0
max(clust_summary$press.max) #[1] 164990

#Let's create our own rules to localize potential errors
if (!require(editrules)) {
  install.packages("editrules")
  require(editrules)
}

E <- editmatrix(c(
  "temp.min > -50",
  "temp.max < 80",
  "press.max > 0",
  "hum.min >= 0",
  "hum.max >= 0"))

#Localize all errors in the data
err <- localizeErrors(E, clust_summary,verbose=TRUE)
plot(err) #Most errors come from pressure

#And clean our geo units from inconsistencies
clust_summary_filtered <-  filter(clust_summary, temp.min > -50, temp.max < 80,
                                  press.max > 0, hum.min >= 0, hum.max >= 0)

#See the sample quantiles corresponding to the given probabilities:
quantile(clust_summary_filtered$temp.min, probs=seq(0, 1, 0.25), names=TRUE, na.rm=FALSE)
quantile(clust_summary_filtered$temp.max, probs=seq(0, 1, 0.25), names=TRUE, na.rm=FALSE)
quantile(clust_summary_filtered$press.max, probs=seq(0, 1, 0.25), names=TRUE, na.rm=FALSE)
#They look much better now and we can move on


if (!require(tidyr)) {
  install.packages("tidyr")
  require(tidyr)
}


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


