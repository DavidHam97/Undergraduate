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

category = strsplit(as.character(grid_panel_df$category), ",")
category = unlist(category)

hurricane = strsplit(as.character(grid_panel_df$hurricanes), ",")
hurricane = unlist(hurricane)

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
hurricane = get_comma_vec(hurricane)
category = get_comma_vec(category)


panel_df$dist_track = dist_track
panel_df$dist_watch = dist_watch
panel_df$hurricane = hurricane
panel_df$category = category

#creating balanced panels
panel_years = sort(unique(panel_df$year))
panel_ids = unique(panel_df$id)
complete_df = data.frame()
for (i in 1:length(panel_ids)) {
  id_df = panel_df[which(panel_df$id == panel_ids[i]), ]
  missing_years = setdiff(panel_years, id_df$year)
  n = length(missing_years)
  new_id_df = data.frame(id = rep(id_df$id, length.out = n), year = missing_years, Freq = rep(0, length.out = n), cumu = rep(0, length.out = n), dist_track = rep(NA, length.out = n), dist_watch = rep(NA, length.out = n), category = rep(NA, length.out = n), hurricane = rep(NA, length.out = n))
  new_df = rbind(id_df, new_id_df)
  new_df = new_df[order(new_df$year), ]
  complete_df = rbind(complete_df, new_df)
}
panel_df = complete_df

#Adding nightlights, county
nightlights_names = colnames(full_df)[14:35]
nightlights_names = sort(nightlights_names)
nightlight_years = 1992:2013 
panel_df$nightlights = NA
panel_df$county = NA
for (i in 1:nrow(panel_df)) {
  index = which(as.character(full_df$long_lat) == as.character(panel_df$id)[i])
  nightlights_index = which(nightlight_years == as.numeric(as.character(panel_df$year[i])))
  if (length(nightlights_index) == 0) {
    nightlights = NA
  } else {
    col_index = nightlights_names[nightlights_index]
    nightlights = full_df[index, col_index]
  }
  panel_df$nightlights[i] = nightlights
  county = as.character(full_df$county[index])
  panel_df$county[i] = county
}
save(panel_df, file = "panel.RData")
