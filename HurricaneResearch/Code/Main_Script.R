###Parsing Data, Cleaning data### (includes all corrections to inconsistent formats)
main_path = "YOUR_PATH"
source(paste0(main_path, "cleaning_data.R"))

###Add Geocode from Raw Data### (includes all corrections for improper geocode)
source(paste0(main_path, "geocoding_data.R"))

###Grid Point Generation and Event Level CSV generation### (Computationally Expensive)
source(paste0(main_path, "csv_generation.R"))
#Note: csv_names contains the vector for list of all csv_names one can loop over for analysis

###Aggregates Event Levels CSV into one big CSV for each observation### (Computationally Expensive)
source(past0(main_path, "grid_df.R"))

###Adds Nightlights/GDP/Population Outcome Variables### (Very Computationally Expensive)
source(past0(main_path, "outcome_variables.R"))

###Changes Gridpoint Event to Balanced Panel Events### (Computationally Expensive)
source(past0(main_path, "panel_df.R"))
#Main Data Frame Stored in panel_df

###Adds our three measure (slope, average news, boolean) into main data###
source(past0(main_path, "measures.R"))



