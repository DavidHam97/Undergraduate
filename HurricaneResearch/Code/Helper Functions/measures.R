###Getting all three measures

#Slope Measure
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

get_slopes = function(filter = 6, maxdist = 200) {
  dist_track_updated = list()
  dist_watch_updated = list()
  for (i in 1:length(dist_track)) {
    i_index = which(dist_track[[i]] < maxdist) 
    dist_track_updated[[i]] = dist_track[[i]][i_index]
    dist_watch_updated[[i]] = dist_watch[[i]][i_index]
  }
  news_list = mapply("-", dist_track_updated, dist_watch_updated, SIMPLIFY = FALSE)
  length_vec = sapply(news_list, length)
  interested_index = which(length_vec >= filter)
  lm_list = list()
  for (i in 1:length(interested_index)) {
    news = news_list[[interested_index[i]]]
    x = 1:length(news)
    lm_object = lm(news ~ x)
    lm_list[[i]] = lm_object
  }
  slopes = sapply(lm_list, coef)[2,]
  p_values = sapply(lm_list, lmp)
  values = data.frame(slopes = slopes, p_values = p_values, index = interested_index)
  return(values)
}

#Adding the slopes now to our data
updating_slopes = function(i_df) {
  df_slopes = get_slopes(filter = 6, maxdist = 200)
  i_df$slopes = NA; i_df$pvalues = NA
  i_df$slopes[df_slopes$index] = df_slopes$slopes
  i_df$pvalues[df_slopes$index] = df_slopes$p_values
}

#Average News Measure
get_avg_news = function(filter = 0, maxdist = 1000) {
  dist_track_updated = list()
  dist_watch_updated = list()
  for (i in 1:length(dist_track)) {
    i_index = which(dist_track[[i]] < maxdist) 
    dist_track_updated[[i]] = dist_track[[i]][i_index]
    dist_watch_updated[[i]] = dist_watch[[i]][i_index]
  }
  news_list = mapply("-", dist_track_updated, dist_watch_updated, SIMPLIFY = FALSE)
  length_vec = sapply(news_list, length)
  interested_index = which(length_vec >= filter)
  interested_news_list = news_list[interested_index]
  average_news = sapply(interested_news_list, mean)
  values = data.frame(avg_news = average_news, index = interested_index)
  return(values)
}

updating_avgnews = function(i_df) {
  df_average_news = get_avg_news(filter = 3, maxdist = 400)
  i_df$avg_news = NA
  i_df$avg_news[df_average_news$index] = df_average_news$avg_news
}

#Boolean measure
get_bools = function(filter = 1, maxdist = 1000, boolength = 30) {
  dist_track_updated = list()
  dist_watch_updated = list()
  for (i in 1:length(dist_track)) {
    i_index = which(dist_track[[i]] < maxdist) 
    dist_track_updated[[i]] = dist_track[[i]][i_index]
    dist_watch_updated[[i]] = dist_watch[[i]][i_index]
  }
  news_list = mapply("-", dist_track_updated, dist_watch_updated, SIMPLIFY = FALSE)
  length_vec = sapply(news_list, length)
  interested_index = which(length_vec >= filter)
  bool_list = lapply(news_list[interested_index], function(x) as.numeric(x < boolength)) #negative means distance to track is less than distance to watch i.e. you recieved bad forecast
  num_shocks = sapply(bool_list, sum)
  values = data.frame(num_shocks = num_shocks, index = interested_index)
  return(values)
}

###Now add to our cross sectional data
dist_track = strsplit(as.character(full_df$dist_track), ",")
dist_watch = strsplit(as.character(full_df$dist_watch), ",")
dist_track = lapply(dist_track, as.numeric)
dist_watch = lapply(dist_watch, as.numeric)


df_slopes = get_slopes(filter = 6, maxdist = 200)
full_df$slopes = NA; full_df$pvalues = NA
full_df$slopes[df_slopes$index] = df_slopes$slopes
full_df$pvalues[df_slopes$index] = df_slopes$p_values


df_average_news = get_avg_news(filter = 1, maxdist = 400)
full_df$avg_news = NA
full_df$avg_news[df_average_news$index] = df_average_news$avg_news

df_bools = get_bools(filter = 1, maxdist = 100, boolength = 30)
full_df$num_shocks = NA
full_df$num_shocks[df_bools$index] = df_bools$num_shocks

write.csv(full_df, "cross_sectional.csv")
###Now add measures to our panel data
panel_df$hurricane_bool = as.integer(!is.na(panel_df$hurricane))
interested_index = which(panel_df$hurricane_bool == 1)
sub_panel = panel_df[interested_index, ]

dist_track = strsplit(as.character(panel_df$dist_track), ",")
dist_watch = strsplit(as.character(panel_df$dist_watch), ",")
dist_track = lapply(dist_track, as.numeric)
dist_watch = lapply(dist_watch, as.numeric)

df_average_news = get_avg_news(filter = 1, maxdist = 400)
panel_df$avg_news = NA
panel_df$avg_news[df_average_news$index] = df_average_news$avg_news

df_bools = get_bools(filter = 1, maxdist = 100, boolength = 30)
panel_df$num_shocks = NA
panel_df$num_shocks[df_bools$index] = df_bools$num_shocks
write.csv(panel_df, "panel_df.csv")
