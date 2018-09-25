#Creating Balanced Panel DF
grid_panel_df = grid_df

#need hurricanes first
names = gsub("^(.*?)_.*", "\\1", csv_names)
hurricane_list = strsplit(as.character(grid_panel_df$hurricanes), ",")
hurricane_vec = unlist(hurricane_list)

#Then track and watch distances
track_dist = strsplit(as.character(grid_panel_df$dist_track), ",")
track_dist = unlist(track_dist)

watch_dist = strsplit(as.character(grid_panel_df$dist_watch), ",")
watch_dist = unlist(watch_dist)

years = lapply(as.character(grid_panel_df$year), function(x) strsplit(x, ","))
year = unlist(years)

#Now because we need to aggregate the same years we need all this business
id = rep(grid_panel_df$long_lat, times = grid_panel_df$counts)

panel_df = data.frame(id = id, year = year)

panel_df = as.data.frame(table(panel_df))
panel_df = panel_df[order(panel_df$id), ]
panel_df = panel_df[ ! panel_df$Freq %in% 0, ]
panel_df$cumu = cumsum(panel_df$Freq)

#Then from the same year create the same above dataframe structure as above
get_comma_vec = function(input) {
  interested = input[1:panel_df$cumu[1]]
  a = c(interested)
  a = paste(as.character(a), collapse=",")
  track_vec = vector()
  track_vec = c(track_vec, a)
  for (i in 2:nrow(panel_df)) {
    index = panel_df$cumu[(i-1):i]
    index[1] = index[1] + 1
    interested = input[index[1]:index[2]]
    a = c(interested)
    a = paste(as.character(a), collapse=",")
    track_vec = c(track_vec, a)
  }
  return(track_vec)
}

dist_track = get_comma_vec(track_dist)
dist_watch = get_comma_vec(watch_dist)

panel_df$dist_track = dist_track
panel_df$dist_watch = dist_watch

#creating balanced panels
panel_years = sort(unique(panel_df$year))
panel_ids = unique(panel_df$id)
complete_df = data.frame()
for (i in 1:length(panel_ids)) {
  id_df = panel_df[which(panel_df$id == panel_ids[i]), -1]
  missing_years = setdiff(panel_years, id_df$year)
  n = length(missing_years)
  new_id_df = data.frame(id = rep(id_df$id, length.out = n), year = missing_years, Freq = rep(0, length.out = n), cumu = rep(0, length.out = n), dist_track = rep(-999, length.out = n), dist_watch = rep(-999, length.out = n), nightlights = rep(-999, length.out = n), avg_news = rep(-999, length.out = n), means = rep(-999, length.out = n))
  new_df = rbind(id_df, new_id_df)
  new_df = new_df[order(new_df$year), ]
  complete_df = rbind(complete_df, new_df)
}
panel_df = complete_df

#adding nightlights
nightlights_names = colnames(grid_panel_df_nightlights)[13:34]
nightlights_names = sort(nightlights_names)
nightlight_years = 1992:2013 

panel_df$nightlights = NA
panel_df = complete_df
for (i in 1:length(panel_df$id)) {
  index = which(as.character(grid_all_df$long_lat) == as.character(panel_df$id)[i])
  nightlights_index = which(nightlight_years == as.numeric(as.character(panel_df$year[i])))
  col_index = nightlights_names[nightlights_index + 1]
  nightlights = grid_all_df[index, col_index]
  panel_df$nightlights[i] = nightlights
}