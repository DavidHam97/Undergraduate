###Add Nightlights
grid_all_df_slopes = grid_df
x = grid_all_df_slopes$long
y = grid_all_df_slopes$lat
sp = SpatialPoints(cbind (x,y), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
test_g = gBuffer(sp, byid = TRUE, width = 0.07, capStyle = "SQUARE")
test_d <- SpatialPolygonsDataFrame(test_g, data.frame(long = x, lat = y))

years = c(1992:2013)
all_df_nightlights = extractNightLights(directory = "night_lights", test_d,
                                        stats = c("mean"), years = years)

final_all_grid = merge(grid_all_df_slopes, all_df_nightlights, by = c("long", "lat"))
colnames(final_all_grid)[colnames(final_all_grid) == "long"] = "longtitude"

grid_df = final_all_grid

###Population and GDP
devtools::install_github("Nowosad/popgrids")
library(popgrids)

x = final_all_grid$longtitude
y = final_all_grid$lat
sp = SpatialPoints(cbind (x,y), proj4string=CRS("+proj=longlat +datum=WGS84"))
test_g = gBuffer(sp, byid = TRUE, width = 0.07, capStyle = "SQUARE")

pop_density_list <- extract(pop_grid, test_g)
pop_density_list = lapply(pop_density_list, as.data.frame)
pop1980 = sapply(pop_density_list, "[[", 1)
pop1990 = sapply(pop_density_list, "[[", 2)
pop2000 = sapply(pop_density_list, "[[", 3)
pop2010 = sapply(pop_density_list, "[[", 4)

final_all_grid$pop1980 = sapply(pop1980, mean)
final_all_grid$pop1990 = sapply(pop1990, mean)
final_all_grid$pop2000 = sapply(pop2000, mean)
final_all_grid$pop2010 = sapply(pop2010, mean)

gdp_density_list <- extract(gdp_grid, test_g)
gdp_density_list = lapply(gdp_density_list, as.data.frame)
gdp1980 = sapply(gdp_density_list, "[[", 1)
gdp1990 = sapply(gdp_density_list, "[[", 2)
gdp2000 = sapply(gdp_density_list, "[[", 3)
gdp2010 = sapply(gdp_density_list, "[[", 4)

final_all_grid$gdp1980 = sapply(gdp1980, mean)
final_all_grid$gdp1990 = sapply(gdp1990, mean)
final_all_grid$gdp2000 = sapply(gdp2000, mean)
final_all_grid$gdp2010 = sapply(gdp2010, mean)

grid_df = final_all_grid