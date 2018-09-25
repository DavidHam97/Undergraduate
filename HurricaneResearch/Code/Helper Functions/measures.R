#Getting all three measures
dist_track = strsplit(as.character(panel_df$dist_track), ",")
dist_watch = strsplit(as.character(panel_df$dist_watch), ",")
dist_track = lapply(dist_track, as.numeric)
dist_watch = lapply(dist_watch, as.numeric)
length_vec = sapply(news_list, length)

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

df_slopes = get_slopes(filter = 6, maxdist = 200)
grid_df$slopes6 = NA
grid_df$pvalues6 = NA
grid_df$slopes6[df_slopes$index] = df_slopes$slopes
grid_df$pvalues6[df_slopes$index] = df_slopes$p_values

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

df_average_news = get_avg_news(filter = 3, maxdist = 400)
grid_df$avg_news= NA
grid_df$avg_news[df_average_news$index] = df_average_news$avg_news

#Boolean measure
get_bools = function(filter = 1, maxdist = 1000, boolength = 50) {
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
  bool_list = lapply(news_list[interested_index], function(x) as.numeric(x <boolength)) #negative means distance to track is less than distance to watch i.e. you recieved bad forecast
  means = sapply(bool_list, mean)
  values = data.frame(means = means, index = interested_index)
  return(values)
}


df_bools = get_bools(filter = 1, maxdist = 100, boolength = 30)
grid_df$means = NA
grid_df$means[df_bools$index] = df_bools$means

