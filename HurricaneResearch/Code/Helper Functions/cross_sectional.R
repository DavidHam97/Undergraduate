#consider only until 1990 hurricanes
subset_data = function(year) {
  years = strsplit(as.character(full_df$year), ",")
  years = lapply(years, as.numeric)
  i_years = lapply(years, function(x) which(x <= year))
  empty_years = sapply(i_years, length)
  empty_index = which(empty_years == 0)
  i_years = i_years[-empty_index]
  counts = lapply(i_years, length)
  dist_track = strsplit(as.character(full_df$dist_track), ",")
  dist_watch = strsplit(as.character(full_df$dist_watch), ",")
  dist_track = lapply(dist_track, as.numeric)
  dist_watch = lapply(dist_watch, as.numeric)
  dist_track = dist_track[-empty_index]
  dist_watch = dist_watch[-empty_index]
  category = strsplit(as.character(full_df$category), ",")
  category = lapply(category, as.numeric)
  category = category[-empty_index]
  hurricanes = strsplit(as.character(full_df$category), ",")
  hurricanes = hurricanes[-empty_index]
  news = strsplit(as.character(full_df$news), ",")
  news = news[-empty_index]
  sub_df = full_df[-empty_index, ]
  sub_df$dist_track = NA
  sub_df$dist_watch = NA
  sub_df$category = NA
  sub_df$hurricanes = NA
  sub_df$year = NA
  sub_df$counts = unlist(counts)
  sub_df$news = NA
  for (i in 1:length(i_years)) {
    sub_df$year[i] = as.character(paste(years[[i]][i_years[[i]]], collapse = ","))
  }
  for (i in 1:length(i_years)) {
    sub_df$dist_track[i] = as.character(paste(dist_track[[i]][i_years[[i]]], collapse = ","))
  }
  i_dist_watch = list()
  for (i in 1:length(i_years)) {
    sub_df$dist_watch[i] = as.character(paste(dist_watch[[i]][i_years[[i]]], collapse = ","))
  }
  for (i in 1:length(i_years)) {
    sub_df$category[i] = as.character(paste(category[[i]][i_years[[i]]], collapse = ","))
  }
  for (i in 1:length(i_years)) {
    sub_df$hurricanes[i] = as.character(paste(hurricanes[[i]][i_years[[i]]], collapse = ","))
  }
  for (i in 1:length(i_years)) {
    sub_df$news[i] = as.character(paste(news[[i]][i_years[[i]]], collapse = ","))
  }
  return_df = sub_df[, -c(1, 6, 14)]
  return(return_df)
}

sub_1990_df = subset_data(year = 1990)

###Now add measures to our panel data
i_df = sub_1990_df
dist_track = strsplit(as.character(i_df$dist_track), ",")
dist_watch = strsplit(as.character(i_df$dist_watch), ",")
dist_track = lapply(dist_track, as.numeric)
dist_watch = lapply(dist_watch, as.numeric)


df_slopes = get_slopes(filter = 6, maxdist = 200)
i_df$slopes = NA; i_df$pvalues = NA
i_df$slopes[df_slopes$index] = df_slopes$slopes
i_df$pvalues[df_slopes$index] = df_slopes$p_values


df_average_news = get_avg_news(filter = 1, maxdist = 400)
i_df$avg_news = NA
i_df$avg_news[df_average_news$index] = df_average_news$avg_news

df_bools = get_bools(filter = 1, maxdist = 100, boolength = 30)
i_df$num_shocks = NA
i_df$num_shocks[df_bools$index] = df_bools$num_shocks

sub_1990_df = i_df
sub_1990_df$counts = factor(sub_1990_df$counts)
sub_1990_df$bool_shocks = factor(sub_1990_df$num_shocks)
write.csv(sub_1990_df, "1990_cross.csv")
