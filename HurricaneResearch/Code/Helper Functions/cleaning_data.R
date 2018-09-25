
#Libraries
library(maps); library(gridExtra); library(ggmap); library(ggplot2); library(plyr); library(raster); library(RColorBrewer); library(maptools); library(rgeos); library(sp); library(rgdal); library(geosphere); library(stringi); library(data.table)

#Parsing Data for Best Track Line from NHC
best_track = fread("https://www.nhc.noaa.gov/data/hurdat/hurdat2-1851-2017-050118.txt", blank.lines.skip = TRUE, header = FALSE, fill = TRUE)
#Create variable for recording category of hurricanes
best_track$category = vector()
best_track$category[best_track$V7 > 64] = 1
best_track$category[best_track$V7 > 83] = 2
best_track$category[best_track$V7 > 96] = 3
best_track$category[best_track$V7 > 113] = 4
best_track$category[best_track$V7 > 137] = 5
best_track$category[is.na(best_track$category)] = 0 

path = "~/shared/dae_ham/Project/Hurricane/rawdata/NHC/wwpts_us.txt"
#Parsing Data for Watchzone from NHC
cities = read.table(path, fill = TRUE, col.names = rep("V", 10))
#now clean up data to look like what we want
index_cities = grep("=", cities[, 2])
number_index_list = gregexpr("[0-9]", cities[,2])
number_index = which(sapply(number_index_list, "[[", 1) == 1)
reps = diff(number_index) - 1
code = cities[number_index, 7]
date = paste0(cities[number_index, 1], "/", cities[number_index, 2], "/", cities[number_index, 3])
name = cities[number_index, 5]
hour = cities[number_index, 4]

test = sort(c(index_cities, number_index))

#get rid of improper coding
city = paste0(cities[index_cities, 2], cities[index_cities, 3], cities[index_cities, 4], cities[index_cities, 5], cities[index_cities, 6], cities[index_cities, 7], cities[index_cities, 8], cities[index_cities, 9])
city[city == "=PuertoRico=Vieques=Culebra="] = "=PuertoRico=Culebra="
city[city == "=LakeOkeechobee="] = "=LakeOkeechobee=LakeOkeechobee="


lst = strsplit(city, "=")
city_origin = sapply(lst, "[[", 2)
city_destination = sapply(lst, "[[", 3)
city_warning = (cities[index_cities, 1]) 
name = rep(name[-length(name)], reps)
code = rep(code[-length(code)], reps)
date = rep(date[-length(date)], reps)
hour = rep(hour[-length(hour)], reps)

#Create final dataframe for the watchzone data
final_df = data.frame(code = code, name = name, date = date, hour = hour, city_origin = city_origin, city_destination = city_destination, warning = city_warning)

#Geocoding - Getting Lat and Longtitude of our cities from watchzone (from Google API)

library("devtools")
install_github("DerekYves/placement")
library(placement)
get_coordset = function(address) {
  address <- as.character(address)
  coordset <- geocode_url(address, auth="standard_api", privkey="AIzaSyC5xCEeJ_0hPXTG9d8qwRWEYAZ0IAMDDeA", clean=TRUE)[, c(1,2)]
  return(coordset)
}

fill_NA = function(coordset, address, col) {
  index = which(is.na(coordset[, col]) == TRUE)
  coordset[index, ] = geocode_url(as.character(address[index]), auth="standard_api", privkey="AIzaSyC5xCEeJ_0hPXTG9d8qwRWEYAZ0IAMDDeA", clean=TRUE)[, c(1, 2)]
  return(coordset)
}
check_NA = function(coordset, col) {
  length(which(is.na(coordset[, col])) == TRUE)
}

destination_geo = get_coordset(final_df$city_destination)
destination_geo = fill_NA(destination_geo, final_df$city_destination, 1)
destination_geo = fill_NA(destination_geo, final_df$city_destination, 2)
check_NA(destination_geo, 1)
check_NA(destination_geo, 2)

origin_geo = get_coordset(final_df$city_origin)
origin_geo = fill_NA(origin_geo, final_df$city_origin, 1)
origin_geo = fill_NA(origin_geo, final_df$city_origin, 2)
check_NA(origin_geo, 1)
check_NA(origin_geo, 2)

final_df$origin_lat = origin_geo$lat
final_df$origin_long = origin_geo$lng

final_df$destin_lat = destination_geo$lat
final_df$destin_long = destination_geo$lng

#Helper clean up data file functions
#First two functions for regular expression matching
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

substrLeft = function(x, n){
  substr(x, 1, n)
}

#This function is to extract the relevant best track line data for any specific hurricane name we desire
best_track_line = function(name, year, idf) {
  name = tolower(name)
  index = which(tolower(best_track$V2) == name)
  start = index[which(substrRight(best_track$V1[index], 4) == year)]
  hurricane = grep("[A-Z]", best_track$V2)
  end = hurricane[which(hurricane == start) + 1]
  in_df = best_track[start:(end-1), ]
  in_df = in_df[-1, ]
  date = as.character(idf$date)
  in_date = substr(date, nchar(date) - 6, nchar(date) -5)
  dex = which(substrRight(in_df$V1, 2) == in_date)
  in_df_1 = in_df[1:dex[length(dex)], ]
  x = substrLeft(in_df$V6, 4); y = substrLeft(in_df$V5, 4); x = gsub("[^0-9\\.]", "", x); y = gsub("[^0-9\\.]", "", y) 
  x = -as.numeric(x); y = as.numeric(y)
  bt_df = data.frame(x = x, y = y, category = in_df$category)
  return(bt_df)
}

#This function is to clean up the hurricane dataset the way we want
cleanup_df = function(name, year) {
  name = tolower(name)
  index = which(tolower(c_df$name) == name)
  l = strsplit(as.character(c_df$date[index]), "/")
  final_index = which(sapply(l, "[[", 3) == year)
  final_index = index[final_index]
  df = c_df[final_index, ]
  a = unique(df$date)
  lst = list()
  for (i in 1:length(a)) {
    l = which(df$date == a[i])
    d = df[l, ]
    l = unique(d$hour)
    for(j in 1:length(l)) {
      aa = which(d$hour == l[j])
      if (length(aa) == 1) {
        d[aa, ] = d[aa, ]
      } 
      if (length(aa) != 1) {
        dist = (d$origin_lat[aa] - d$destin_lat[aa])^2 + (d$origin_long[aa] - d$destin_long[aa])^2
        chosen = which.max(dist)
        index = which(aa != aa[chosen])
        d = d[-c(aa[index]), ]
      }
    }
    lst[[i]] = d
  }
  df = ldply(lst, data.frame)
  return(df)
}

