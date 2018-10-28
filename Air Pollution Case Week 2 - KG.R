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
rm(i)

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
unique_full_set <- data.frame(unique(data_bg_clean$geohash)) #1254 unique stations == length of frequency

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

#filter geohashes with observations over 10 and results for more than 7 days
summary <-  filter(summary,obs > 10)
summary <-  filter(summary,days > 7)

# check the summary
head(summary)

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

# now that we have selected the stations we're going to use for the analysis,
# subset the dataset with the clear citizen data for Sofia, using the filters we defined in step 4 and 5 above 
data_bg_final <- subset(data_bg_clean, (data_bg_clean$geohash %in% sofia_summary$geohash))
data_bg_final <- data.frame(data_bg_final, as.data.frame(gh_decode(data_bg_final$geohash)))

#It's important to keep our environment clean:
rm(data_bg_full, data_class, freq, data_class_topo, data_bg_2017_new, max, min, minmax, only_in_2017, unique_2017, unique_2018, unique_full_set)


# Week 2 ----

# A bit more COOL visualisations :) ----
# Saving the final dataset from step 1 to the working directory, so that we can access it later without running all the code
# saveRDS(data_bg_final, "./data_bg_final_step1.rds")
# Access file later:
# data_bg_final<-as.data.frame(readRDS("./data_bg_final_step1.rds"))


# This is a cool way to plot the topography of Sofia, accounting for elevation as well:
if (!require(plotly)) {
  install.packages("plotly")
  require(plotly)
}

# That's how the topography looks in 2D:
plot(sofia_topo$lat, sofia_topo$lng)

# And now - in 3D:
# !!!NB might take some time to plot, but it's INTERACTIVE - you can play with the plot
# We can try something like this when we create the Jupyter notebook

plot_ly(x = ~sofia_topo$lat,
        y = ~sofia_topo$lng,
        z = ~sofia_topo$Elev,
        type = "scatter3d")

# Week 2 ----
# Step 1: Decide on the final list of geo-units that are subject to predictive analysis ----

# first, let's again take unique stations from the final Step1 dataset to
unique_sofia<-unique(data_bg_final$geohash)

data_sofia <- subset(data_bg_final, !(data_bg_2017$geohash %in% only_in_2017$only_in_2017))


if (!require(geosphere)) {
  install.packages("geosphere")
  require(geosphere)
}

# checking dimensions of the data_bg_final dataset
length(data_bg_final$geohash) # [1] 1742668
length(unique(data_bg_final$geohash)) # [1] 549

# subset to unique stations and only three columns
cluster_test<-data_bg_final[!duplicated(cluster_test$geohash),c("geohash", "lat", "lng")]

head(cluster_test)
length(cluster_test$geohash) # [1] 566

# K-means clustering with 100 centers:
kmeans_sofia<-kmeans(data.frame(cluster_test[,c("lat","lng")]), centers = 50)
?kmeans
cluster_test$clusterNum<-kmeans_sofia$cluster
head(cluster_test)

if (!require(ggplot2)) {
  install.packages("ggplot2")
  require(ggplot2)
}


theme_set(theme_bw()) 
# Plotting Sofia stations without cluster:
ggplot(cluster_test, aes(x=lat, y=lng)) +
  geom_point(size=3)+
  labs(subtitle="No clusters", 
       y="Longitude", 
       x="Lattitude", 
       title="Plot of Stations in Sofia")


# Plotting Sofia staitons with cluster coloured
ggplot(cluster_test, aes(x=lat, y=lng)) +
  geom_point(size=3, col = cluster_test$clusterNum)+
  labs(subtitle="Number of clusters: 50", 
       y="Longitude", 
       x="Lattitude", 
       title="Clustered Stations in Sofia")
  

if (!require(ggalt)) {
  install.packages("ggalt")
  require(ggalt)
}


# Plotting Sofia staitons with cluster coloured and encircled
ggplot(cluster_test, aes(x=lat, y=lng)) +
  geom_point(size=3, col = cluster_test$clusterNum) +
  geom_encircle(aes(group=cluster_test$clusterNum,
                    expand=0.01)) +
  labs(subtitle="Number of clusters: 50", 
       y="Longitude", 
       x="Lattitude", 
       title="Clustered Stations in Sofia - Encircled")

    
    
    
  
# Step 2: Merge cluster information to the citizen science air quality measurements and get descriptive summary by clusters.----
# Step 3: Clean data for mismeasurements and inconsistencies ----
# Step 4: Step 4: Aggregate data by geo-units ----
# Step 5: Step 5: Inspect and summarize the most important statistical characteristics of the set of time series subject to further analysis ----

