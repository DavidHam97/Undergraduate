
#Grid generation
#Create a gridpoint level of 1 million grid points on the whole of US
us <- getData('GADM', country = 'USA', level = 1)
a = makegrid(us, n = 1000000)
grid = SpatialPoints(a, proj4string = CRS(proj4string(us)))
grid = grid[us, ]
grid = as.data.frame(grid)

#Database for all hurricanes - Extracting all the hurricane names and dates from the NHC data
#Hurricane names:
hurricane_names = unique(c_df[, 3])

#Extract dates
extract_date = function(hurricane, data = c_df) {
  index = which(c_df[, 3] == hurricane)
  index = index[1]
  out = c_df[, 4][index]
  a = substrRight(x = as.character(out), n = 4)
  return(a)
}
dates = vector(length = 0)
for (i in 1:length(hurricane_names)) {
  dates[i] = extract_date(hurricane = hurricane_names[i], data = c_df)
}

hurricane_names = as.character(hurricane_names)

#name database in the best_track data: (basically for all storms)
best_track_names = unique(best_track[, 2])
best_track_names = best_track_names[grepl("[a-z]", tolower(best_track_names))]

#Helper functions for getting intersection and distance to watchzone
watch_coord_distance_updated = function(watchlist_df, coords) {
  watchlist_df$dist = distHaversine(coords, watchlist_df[, c("long", "lat")])/1609
  index = which.min(watchlist_df$dist)
  dis = watchlist_df$dist[index]
  return(watchlist_df[index, ])
}

intersection_updated = function(bt_df, i_start, i_end, dist = 50, m = 1) {
  line_df = bt_df[, -3]
  a = i_start - dist 
  b = i_end + dist
  if (a < 0) {
    a = 1
  }
  if (b > 7243) {
    b = 7243
  }
  shape_df = all_state[(a):(b), ]
  shape_df = shape_df[ , ]
  d = list()
  for (i in 1:length(shape_df$long)) {
    d[[i]] = distHaversine(line_df, shape_df[i, c("long", "lat")])/1609
  }
  test = sapply(d, min)
  a = which(sort(test)[m] == test)
  intersection = c(shape_df[a, ])
  ind = which(d[[a]] == test[a])
  a = line_df[ind, ]
  data_a = data.frame(x = a$x, y = a$y, long = intersection$long, lat = intersection$lat)
  return(data_a)
}

#Main function to get CSV for any hurricane now
#Takes several input: Name of Hurricane, Year of Hurricane, the Day to ideally analyze (for example day = 3 would be 3 days before impact), the maximum distance to consider the gridpoint (just set as 200 miles for default, id is a parameter for creating the csv itself onto the computer, dist and m parameters are for considering the "right" intersection point)
get_csv = function(name, year, day = 3, dist = 50, maxdist = 200, m = 1, id = 0) {
  df = cleanup_df(name, year); df$dist = distHaversine(df[, c("origin_long", "origin_lat")], df[, c("destin_long", "destin_lat")])/1609;
  u = unique(df$date); index = list()
  for (i in 1:length(u)) {
    j = which(df$date == unique(df$date)[i])
    a = which.max(df[j, ]$dist)
    index[[i]] = df[j, ][a, ]
  }
  i_df = ldply(index, data.frame); n = length(index); i_df$x = c()
  all_state = map_data("usa"); state_map = ggplot() + geom_polygon(data=all_state, aes(x=long, y=lat, group = group), colour="white", fill="grey80")
  vec = list(length = n); date = unique(i_df$date);
  i = n - day
  if (i < 0) {
    stop(print(n))
  }
  x_min = min(c(i_df[i, ]$origin_long, i_df[i, ]$destin_long)); x_max = max(c(i_df[i, ]$origin_long, i_df[i, ]$destin_long)); y_max = max(c(i_df[i, ]$origin_lat, i_df[i, ]$destin_lat)); y_min = min(c(i_df[i, ]$origin_lat, i_df[i, ]$destin_lat))
  x = c(i_df$origin_long[i_df$date == date[i]], i_df$destin_long[i_df$date == date[i]])
  y = c(i_df$origin_lat[i_df$date == date[i]], i_df$destin_lat[i_df$date == date[i]])
  in_df = data.frame(x = x, y = y)
  all_state$distance1 = distHaversine(in_df[1, ], all_state[, c("long", "lat")])/1609
  all_state$distance2 = distHaversine(in_df[2, ], all_state[, c("long", "lat")])/1609
  index_start = which.min(all_state$distance1); index_end = which.min(all_state$distance2)
  i_start = sort(c(index_start, index_end))[1]; 
  i_end = sort(c(index_start, index_end))[2]; 
  bt_df = best_track_line(name = name, year = year, idf = i_df[i, ])
  intersect = intersection_updated(bt_df, i_start = i_start, i_end = i_end, dist = dist, m = m)[1, ]
  grid_1 = grid
  grid_1$distance_track = distHaversine(grid, intersect[, c("long", "lat")])/1609
  new_grid = grid_1[grid_1$distance_track < maxdist, ]
  distance_watch = vector()
  watchzone_ref_long = vector()
  watchzone_ref_lat = vector()
  for (j in 1:nrow(new_grid)) {
    distance_watch[j] = watch_coord_distance_updated(watchlist_df = all_state[i_start:i_end, ], coords = new_grid[j, 1:2])$dist
    watchzone_ref_long[j] = watch_coord_distance_updated(watchlist_df = all_state[i_start:i_end, ], coords = new_grid[j, 1:2])$long
    watchzone_ref_lat[j] = watch_coord_distance_updated(watchlist_df = all_state[i_start:i_end, ], coords = new_grid[j, 1:2])$lat
  }
  new_grid$distance_watch = distance_watch
  num = nrow(new_grid)
  new_grid$origin_lat = rep(i_df[i, "origin_lat"], num)
  new_grid$origin_long = rep(i_df[i, "origin_long"], num)
  new_grid$origin_ref = rep(i_start, num)
  new_grid$destin_lat = rep(i_df[i, "destin_lat"], num)
  new_grid$destin_long = rep(i_df[i, "destin_long"], num)
  new_grid$destin_ref = rep(i_end, num)
  new_grid$date_analyzed = rep(i_df[i, 4], num)
  new_grid$max_category = rep(max(bt_df$category), num)
  new_grid$watchzone_ref_long = watchzone_ref_long
  new_grid$watchzone_ref_lat = watchzone_ref_lat
  new_grid$intersection_long = rep(intersect$long, num)
  new_grid$intersection_lat = rep(intersect$lat, num)
  new_grid$intersect_ref_long = rep(intersect$x, num)
  new_grid$intersect_ref_lat = rep(intersect$y, num)
  colnames(new_grid)[c(1,2)] = c("long", "lat")
  if (id != 0) {
    file_name = paste0("~/shared/dae_ham/Project/Hurricane/filedata/hurricane_csvs/csv_county", tolower(name), "_", year, "_", id,".csv")
  }
  else {
    file_name = paste0("~/shared/dae_ham/Project/Hurricane/filedata/hurricane_csvs/csv_county", tolower(name), "_", year, ".csv")
  }
  write.csv(new_grid, file = file_name)
}

#Writing csv files - each line of code writes a csv for each hurricane. The days and dist inputs are adjusted for each hurricane because each hurricane might not have gotten a 3 day warning or sometimes the intersection was off
#Becky
m = c(2, 1, 1, 1, 0, 1, 1, 0, 2, 1,1,2,2,1,1,1,1,1,1,3,2,2,1,1,1,2,3,2,3,2,2,2,1,1,1,1,1,2,0,4,2,3,1,3,1,3,2,3,3,2,1,3,2,1,4,1,1,2,0,2,2,1,3,3,1,2,2,1,2,5,2,3,3,2,1,3,1,3,1,3,2,2,3,1,1,3,1,3,2,1,2,3)

id = rep(0, times = length(m))
id[c(8, 9, 40, 41, 44, 45, 70, 71)] = rep(c(1,2), length.out = 8)

for (i in 1:length(m)) {
  get_csv(name = hurricane_names[i], year = dates[i], day = m[i], id = id[i])
}

csv_names = paste0(tolower(hurricane_names), "_", dates, ".csv")
csv_names = csv_names[-c(99:102)]
removed = c("fern", "anita", "barry", "gilbert", "andrew", "erin", "beryl", "michelle", "katrina", "eighteen", "nine", "gordon", "erika")
index_remove = vector(length = 0)
for (i in 1:length(removed)) {
  index_remove[i] = which(tolower(hurricane_names) == removed[i])
}

#Storing csv_names
csv_names = csv_names[- index_remove]
csv_names = csv_names[-(which(csv_names == "emily_2005.csv"))]
csv_names = c(csv_names, "fern_1971_1.csv", "fern_1971_2.csv", "andrew_1992_1.csv", "andrew_1992_2.csv", "erin_1995_1.csv", "erin_1995_2.csv", "katrina_2005_1.csv", "katrina_2005_2.csv")

#county shape file
county = map_data("county")

path = ""
get_updated_csv = function(csvs, county_df) {
  hurricane_dat = read.csv(paste0(path, csvs), header = TRUE)
  state_level = vector()
  county_level = vector()
  xy = hurricane_dat[, c("long", "lat")]
  for (i in 1:length(xy$long)) {
    vec = vector()
    vec = distHaversine(xy[i, ], county[, 1:2])
    index = which.min(vec)
    state_level = c(state_level, county[index, 5]); county_level = c(county_level, county[index, 6])
  }
  hurricane_dat$state = state_level
  hurricane_dat$county = paste0(county_level, "_",state_level)
  write.csv(hurricane_dat, paste0("~/shared/dae_ham/Project/Hurricane/filedata/hurricane_csvs/csv_county", csvs))
}

for (i in 1:length(csv_names)) {
  get_updated_csv(csvs = csv_names[i], county_df = county)
}