path = ""

geocoded_df = read.csv(path, header = TRUE)

c_df$origin_lat = geocoded_df$origin_lat
c_df$origin_long = geocoded_df$origin_long

c_df$destin_lat = geocoded_df$destin_lat
c_df$destin_long = geocoded_df$destin_long