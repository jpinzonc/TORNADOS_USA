# This script intends to provide data on storms/tornados events close to a selected location
# R vesion 3.3.1 (2016-06-21)
# Data was collected in October 2016
# These libraries are needed:
library(XML)
library(R.utils)
library(plyr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(data.table)
library(tidyr)
library(ggmap)
library(scales)
library(dismo) # this package masks some of dplyr functions. 
library(highcharter)
library(htmlwidgets)
library(viridisLite)

########################## DOWNLOAD AND LOAD DATA INTO R ###################################
# The data is from NOAA. 
# The files can be found @ 
# http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/
# The batch download step is adapted from 
# http://www.guru-gis.net/batch-download-files-using-r/

# Set the working directory
initial.dir<-getwd()
dir()

# Create a link to the data
url <- c("http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/")
site <- htmlParse(url)

# Get the <a> nodes. Basically the list of files in the site
a_nodes <- getNodeSet(site,"//a")

# Select the files of interest. 
# In this case, files with details of the storms.
# There is one file per year, starting in 1951 and ending in 2016. 
# The information on the data contained in these files is in the file:
# "Storm-Data-Export-Format.docx"
files <- grep("*details",sapply(a_nodes, function(Anode) xmlGetAttr(Anode,"href")),value=TRUE)

# Make the full url for each file. Change the separator acordingly, keep the last '/' in the url.  
urls <- paste(url,files,sep="")

# Download files.
mapply(function(x,y) download.file(x,y),urls,files)

# Unzip files (R.utils)
# Create a list with the path to all the files
filenames <- list.files(path= getwd(), full.names=TRUE ,  pattern= "*.gz")

# Gunzip the files in the current directory
mapply(function(x) gunzip(x),filenames)

# Rename files to make it easier to load later.
# Create a list with current names
cur_names = gsub('\\./','',list.files('.', pattern = '*.csv', full.names = TRUE))

# Create a list the new names
New_Names = gsub('_details-ftp_v1.0_d','',cur_names)
New_Names = gsub('_c[0-9]{8}','_10_2016', New_Names) # Here the "_10_2016" is the date the files were downloaded

# Rename the files. 
file.rename(from = file.path('.', cur_names), to = file.path('.', New_Names))

# Import all files into a list. 
storms <- lapply(list.files(path='.', pattern ="*.csv"), read.csv) # This is a list and each component is a dataframe

# Name each file in the inputfiles with the actual name
names(storms)  <- sub("\\.csv", "", list.files(path = '.', pattern='*.csv') ) 

# Combine all data frames from the list into one data.frame
all_storms = ldply(storms, rbind)

# Writing the all_storms data.frame to a file ("3-16" is the date of download)
write.csv(all_storms, 'all_storms_10-16.csv')

# Creating a subset with tornados ONLY
# The data includes other than tornado events. 
tornados = all_storms[all_storms$EVENT_TYPE=="Tornado",]

# Write this data.frame to a file
write.csv(tornados, file = "tornados_10-16.csv")

########################### NO NEED FOR DOWLOADING IF STARTING HERE #######################
# At this point the data is in the computer, in two files:
# all_storms.csv and tornados.css. 
# No need to RE-download.
# Read either of the files created above.
# The all_storms file needs to be filter after loading it into R
all_storms = read.csv('all_storms_10-16.csv')
tornados = read.csv('tornados_10-16.csv')


########################## CLEANING THE TORNADO DATA #############################
# There are some longitudes (BEGIN_LON) that have mistakes
# The mistakes may change as the data is constantly updated by NOAA. 
# For example, in January 2016, there were Positive longitudes that should have been negative.
# In February 2016, NOAA corrected them, but left 4 records that had the BEG_LON
# and END_LON in other columns

# This command checks for these 4 records:
# If it has been corrected, the outcome should be <0 rows>
tornados[tornados$BEGIN_LON > -13 & tornados$BEGIN_LON < 0 & !is.na(tornados$BEGIN_LON),]

# Check here for positive longitutes
tornados[tornados$BEGIN_LON > 0 & !is.na(tornados$BEGIN_LON),]

# It is possible that there are other errors, I advice to run View() and check the data manually.
# View() has more functionality in Rstudio
View(tornados)

# This command cleans both the positive longitudes (Removed as of March 2016) and 
# the 4 records: 
# Check data and make adjustments as you go:
tornados_clean = tornados %>%dplyr::select(.id, YEAR, MONTH_NAME,BEGIN_DAY, BEGIN_YEARMONTH,  
                      BEGIN_TIME, STATE, CZ_NAME, TOR_F_SCALE, TOR_LENGTH, TOR_WIDTH,BEGIN_LAT,BEGIN_LON,END_LAT,END_LON,
                      INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, EVENT_ID)%>%mutate(BEGIN_LON = ifelse(BEGIN_LON < 0 & BEGIN_LON >-13 & !is.na(BEGIN_LON), 
                      END_LON, BEGIN_LON))%>%mutate(BEGIN_LON = ifelse(BEGIN_LON > -10, -BEGIN_LON, BEGIN_LON), END_LON=ifelse(END_LON>0, -END_LON,END_LON))

# Check if the correction worked
# For misplaced records:  
tornados_clean[tornados_clean$BEGIN_LON < 0 & tornados_clean$BEGIN_LON > -13 & !is.na(tornados_clean$BEGIN_LON),]
# For positive values
tornados_clean[tornados_clean$BEGIN_LON > 0 & !is.na(tornados_clean$BEGIN_LON),]

# Write this data table on a file
write.csv(tornados_clean, file = "tornados_clean_10-16.csv")

#################### NOW, THE FUN PART - DATA ANALYSIS ################################
# Read the cleaned file:
tornados_clean = read.csv('tornados_clean_10-16.csv')

# Total number of Tornados since 1951 in the US
cat("According to NOAA, there has been",nrow(tornados_clean)," tornados reported in the US since 1951")

# Tornados per year
tor_Year = tornados_clean%>%group_by(YEAR)%>%dplyr::summarise(total.count=n())
as.data.table(tor_Year)

# Average tonados/year
ave_tor= as.integer(tor_Year%>%summarise(average=mean(total.count, na.rm=T)))

cat("According to NOAA, there has been",nrow(tornados_clean)," tornados reported in the US since 1951. With an average of", ave_tor,"tornados per year.")

# Plot Year vs. No. of Tornados
ggplot(tor_Year, aes(YEAR, total.count)) + geom_line()+
  ggtitle("Tornados in the US (1951-2016)") +
  xlab("YEAR") + ylab("No. of Tornados") +
  geom_hline(yintercept = ave_tor, col ="blue")

# Calculate the total number of tornados per scale:
# The Enhance F-scale (EF) was adopted in 2007. 
# See this site for additional explanations: http://www.spc.noaa.gov/faq/tornado/ef-scale.html
tornados_clean%>%group_by(TOR_F_SCALE)%>%dplyr::summarise(total.count=n())

# For a particular year
# Change the year variable to the desired value (data base goes from 1951 to 2015)
year = 2014
tornados_clean %>% filter(YEAR == year)%>% group_by(TOR_F_SCALE)%>%dplyr::summarise(total.count=n())

# Combining columns
# Create a vector with the columns you want to combine:
# Add variables as you which. This examples is for Year and Scale
tor_cols <- c("YEAR", "TOR_F_SCALE")

# Convert the vector into a list of symbols
col_com <- lapply(tor_cols, as.symbol)

# Determine the total number of tornados for the combination of Year and Scale
tor_scale = tornados_clean %>%group_by_(.dots=col_com) %>%dplyr::summarise(n = n())
tor_scale

# The plot. It is not very informative
ggplot(all_tor,aes(x = YEAR ,y = count)) + 
  geom_bar(stat="identity") + guides(fill=FALSE)+
  xlab("") + ylab("No. of Tornados")

# It is probably better to run one for a particular year. 
year2="1965"
tor_scale_year = tornados_clean%>%filter(YEAR==year2)%>%group_by_(.dots=col_com)%>%dplyr::summarise(n=n())
ggplot(tor_scale_year,aes(x = TOR_F_SCALE,y = n)) + 
  geom_bar(stat="identity", aes(fill = TOR_F_SCALE),position = "dodge")+ guides(fill=FALSE) +
  xlab("") + ylab("No. of Tornados")


###### COMPARISON BETWEEN TWO CITIES ############
locationCoords = tornados_clean%>%dplyr::select(originalPlace=X, lon=BEGIN_LON, lat=BEGIN_LAT)

# Coordinates for the City of Interest (Change the city as much as you can!)
yourcity = "GandPraire, TX"
yourcity1 = "Dallas, TX"

# Enter the distance around the cities to search for Tornados
# This is the radius of a circle centered at the city
distance_from_city = 10

# Getting the geographic coordinates for each city
your_city =  mutate(suppressMessages(geocode(yourcity)), originalPlace = (yourcity[1]))
your_city = your_city%>%dplyr::select(originalPlace, lon=longitude, lat=latitude)
your_city1 =  mutate(suppressMessages(geocode(yourcity1)), originalPlace = (yourcity1[1]))
your_city1 = your_city1%>%dplyr::select(originalPlace, lon=longitude, lat=latitude)


# Getting the events in a radios from your_city
deg2rad <- function(deg) return(deg*pi/180)
gcd.slc <- function(long1, lat1, long2, lat2) {
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  d <- d*0.621371 #in miles
  return(d) # Distance in miles
}

# Creating a matrix with the locations around the cities
dist <- gcd.slc(your_city$lon, your_city$lat, locationCoords$lon, locationCoords$lat)

dist_to_city <- which( dist <= distance_from_city) 
events_near_your_city = rbind((filter(locationCoords, originalPlace %in% dist_to_city)),your_city)

events=tor_DT%>%filter(X %in% events_near_your_city$originalPlace)%>%dplyr::select(X,BEGIN_YEARMONTH, BEGIN_DAY, BEGIN_LAT, BEGIN_LON, DEATHS)

n_e = nrow(events)

dist1 <- gcd.slc(your_city1$lon, your_city1$lat, locationCoords$lon, locationCoords$lat)

dist_to_city1 <- which( dist1 <= distance_from_city ) 

events_near_your_city1 = rbind((filter(locationCoords, originalPlace %in% dist_to_city1)),your_city1)

events_1=tor_DT%>%filter(X %in% events_near_your_city1$originalPlace)%>%dplyr::select(X, BEGIN_YEARMONTH, BEGIN_DAY, BEGIN_LAT, BEGIN_LON, DEATHS)

n_e1= nrow(events_1)
common_events = merge(x=events, y=events_1)
com_ev=nrow(common_events)

# The result
cat("There were",n_e, "tornados", distance_from_city, "miles around", yourcity, "and", n_e1, "in", yourcity1,", Of these,",com_ev,"were common.")


######################   CREATE MAPS LOCALIZING THE TORNADOS ####################################:

# Upload the US MAP
getTheMap <- suppressMessages(get_map(location = 'united states', zoom = 4, source = 'google'))

# Plot the location of tornados for a particular year (Change the year)

# Select the data for the map
# Adjust the filter option at the end for another year:
mapDT_year <- distinct(tor_DT%>%dplyr::select(YEAR,BEGIN_LON, BEGIN_LAT, DEATHS)%>%filter(YEAR==2015))

# Create a map with the tornados for that year
tor_one_year <- suppressMessages(ggmap(getTheMap) + 
                          geom_point(aes(x=BEGIN_LON, y=BEGIN_LAT), 
                                     data = mapDT_year,
                                     size= mapDT_year $DEATHS*5, # size is proportional to the number. Their scale can be changed here. 
                                     colour="red", 
                                     alpha=.5)+
                          ggtitle("Tornado-related deaths in US (2015)")) # CHANGE YEAR HERE!!
tor_one_year

# For all years. Noticed the change in the size value.
mapDT_All <- distinct(tor_DT%>%dplyr::select(YEAR,BEGIN_LON, BEGIN_LAT, DEATHS))
tornados_US_50 <- suppressMessages(ggmap(getTheMap) + 
                                  geom_point(aes(x=BEGIN_LON, y=BEGIN_LAT),
                                             data = mapDT_All,
                                             size=mapDT_All$DEATHS*.1,
                                             colour="red",
                                             alpha=.5) +
                                  ggtitle("Tornado-related deaths in the US 1950-2015"))
tornados_US_50



################# PLOT TORNADOS AND RELATED DEATHS ON MAP OF AN AREA OF INTEREST ##########
# Isolating the coordinates for all tornados in the dataset
locationCoords = tor_DT%>%dplyr::select(originalPlace=X, lon=BEGIN_LON, lat=BEGIN_LAT)

# Coordinates for the City of Interest
# Enter the name of the city or location
city_of_interest = "Tulsa, OK"
cityOI = mutate(suppressMessages(geocode(city_of_interest)), originalPlace = (city_of_interest[1]))
cityOI = cityOI%>%dplyr::select(originalPlace, lon=longitude, lat=latitude)

# Sorting the locations closer to the city of interest (cityOI)
deg2rad <- function(deg) return(deg*pi/180)
gcd.slc <- function(long1, lat1, long2, lat2) {
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  d <- d*0.621371 #in miles
  return(d) # Distance in miles
}

distance <- gcd.slc(cityOI$lon, cityOI$lat, locationCoords$lon, locationCoords$lat)
distance_matrix <- which( distance <= 75 ) # 75 miles from the center
Area_COI = rbind((filter(locationCoords, originalPlace %in% distance_matrix)),cityOI)

############### PLOTING THE DATA ON A MAP ##################
# Map of the area of interest
cityMap <- gmap(city_of_interest, zoom=8, scale = 2)
plot(cityMap)

coordinates(cityOI) <- ~ lon + lat
projection(cityOI) <- "+init=epsg:4326"
centralLocation <- spTransform(cityOI, CRS = CRS(projection(cityMap)))

mile2meter <- function(x) {  x * 1609.344 }
distLayer100<- rgeos::gBuffer(centralLocation, width = mile2meter(100))
distLayer50  <- rgeos::gBuffer(centralLocation, width = mile2meter(50))
distLayer25  <- rgeos::gBuffer(centralLocation, width = mile2meter(25))

plot(distLayer25, col = alpha("red", .0), add = TRUE)
plot(distLayer50, col = alpha("red", .0), add = TRUE)
plot(distLayer100, col = alpha("red", .0), add = TRUE)

# Tornado locations around city of interest
coordinates(Area_COI) <- ~ lon + lat
projection(Area_COI) <- "+init=epsg:4326"
tor_aroud_COI <- spTransform(Area_COI, CRS = CRS(projection(cityMap)))
points(tor_aroud_COI, cex = 1, pch = 20, col='blue')

# Add the Tornado related deaths around city of interest
# Generate a list of locations with DEATHS in the defined area.
DT_COI = filter(tor_DT, X %in% distance_matrix, DEATHS >0)

coordinates(DT_COI) <- ~ BEGIN_LON + BEGIN_LAT
projection(DT_COI) <- "+init=epsg:4326"
allIncidents2 <- spTransform(DT_COI, CRS = CRS(projection(cityMap))) # coordinate reference system (CRS) 
points(allIncidents2, cex = 1, pch = 2, col='red')


# THERE ARE MANY OTHER THINGS THAT CAN BE DONE --- ENJOY !!!!
