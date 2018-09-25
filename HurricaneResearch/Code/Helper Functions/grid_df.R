#Creates one master gridpoint data not panel yet 
grid_point_df = function(year = 1970:2016) {
  y = as.numeric(stri_extract_first_regex(csv_names, "[0-9]+"))
  index = vector()
  for (i in 1:length(year)) {
    index = c(index, which(y== year[i]))
  }
  df_grid = data.frame()
  for (i in index) {
    b = read.csv(file = paste0("~/Downloads/csv_county/", csv_names[i]), header = TRUE)
    b = b[, c("long", "lat", "max_category", "std_news", "county", "distance_track", "distance_watch")]
    b$hur_names = gsub("^(.*?)_.*", "\\1", csv_names[i])
    b$years = y[i]
    df_grid = rbind(df_grid, b)
  }
  df_grid$long_lat = paste0(df_grid$long, ",",df_grid$lat)
  hurricane = data.table(df_grid)
  df_grid_aggregate = data.frame(hurricane[ , list(std_news=mean(std_news)),by = long_lat])
  df_grid_aggregate$long = as.numeric(sapply(strsplit((df_grid_aggregate$long_lat), ","), "[", 1))
  df_grid_aggregate$lat = as.numeric(sapply(strsplit((df_grid_aggregate$long_lat), ","), "[", 2))
  grid_counts = table(df_grid$long_lat)
  grid_counts_long = as.character(sapply(strsplit(names(grid_counts), ","), "[", 1))
  grid_counts_lat = as.character(sapply(strsplit(names(grid_counts), ","), "[", 2))
  df_grid$max_category = df_grid$max_category + 1
  grid_counts_weighted = vector()
  for (i in 1:length(grid_counts)) {
    grid_counts_weighted = c(grid_counts_weighted, sum(df_grid$max_category[which(df_grid$long_lat == names(grid_counts)[i])]))
  }
  grid_counts_county= vector()
  for (i in 1:length(grid_counts)) {
    grid_counts_county = c(grid_counts_county, as.character(df_grid$county[which(df_grid$long_lat == names(grid_counts)[i])][1]))
  }
  categor = vector()
  hur_name = vector()
  dist_track = vector()
  dist_watch = vector()
  year_vec = vector()
  df_grid$max_category = df_grid$max_category - 1
  for (i in 1:length(grid_counts)) {
    index = which(df_grid$long_lat == names(grid_counts)[i])
    a = c(df_grid$max_category[index])
    a = paste(as.character(a), collapse=",")
    categor = c(categor, a)
    b = c(df_grid$hur_names[index])
    b = paste(as.character(b), collapse=",")
    hur_name = c(hur_name, b)
    d = c(df_grid$distance_track[index])
    d = paste(as.character(d), collapse=",")
    dist_track = c(dist_track, d)
    e = c(df_grid$distance_watch[index])
    e = paste(as.character(e), collapse=",")
    dist_watch = c(dist_watch, e)
    f = c(df_grid$years[index])
    f = paste(as.character(f), collapse=",")
    year_vec = c(year_vec, f)
  }
  grid_counts_df = data.frame(long = grid_counts_long, lat = grid_counts_lat, counts = as.vector(grid_counts), counts_weighted = as.vector(grid_counts_weighted), county = grid_counts_county, category = categor, hurricanes = hur_name, dist_track = dist_track, dist_watch = dist_watch, year = year_vec)
  grid_counts_df$long = as.numeric(as.character(grid_counts_df$long))
  grid_counts_df$lat = as.numeric(as.character(grid_counts_df$lat))
  grid_counts_df$counts = factor(grid_counts_df$counts)
  grid_counts_df$counts_weighted = as.numeric(grid_counts_df$counts_weighted)
  grid_counts = merge(grid_counts_df, df_grid_aggregate, by = c("long", "lat")) 
  return(grid_counts)
}

grid_df = grid_point_df(year = 1991:2012)

