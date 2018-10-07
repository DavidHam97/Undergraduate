###Parsing Data, Cleaning data### (includes all corrections to inconsistent formats)
main_path = "~/shared/dae_ham/Project/Hurricane/analysis/R_data_generation/helpers"
source(paste0(main_path, "cleaning_data.R"))

###Grid Point Generation and Event Level CSV generation### (Computationally Expensive)
source(paste0(main_path, "csv_generation.R"))
#Note: csv_names contains the vector for list of all csv_names one can loop over for analysis

###Aggregates Event Levels CSV into one big CSV for each observation### 
source(paste0(main_path, "grid_df.R"))

###Adds Nightlights/GDP/Population Outcome Variables### 
source(paste0(main_path, "outcome_variables.R"))
#By this point we have our "our full cross-sectional data" in full_df
#now the rest is make it into a balanced panel and then add the measures

###Changes Gridpoint Event to Balanced Panel Events### (Computationally Expensive)
source(paste0(main_path, "panel_df.R"))
#Main Data Frame Stored in panel_df

###Adds our three measure (slope, average news, boolean) into main data###
source(paste0(main_path, "measures.R"))

###Going to now perform analysis on a subset of our full data which we only want until 1990 to do our cross sectional regression###
source(paste0(main_path, "subset_generation.R"))
#subset data stored in sub_1990_df variable




