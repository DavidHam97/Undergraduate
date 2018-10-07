path = "/Users/Ham/Desktop/Project/Hurricane/rawdata/outcome_variables/outcome_variables.csv"

outcome_df = read.csv(path, header = TRUE)

full_df = merge(grid_df, outcome_df, all.x = TRUE, by = "long_lat")
