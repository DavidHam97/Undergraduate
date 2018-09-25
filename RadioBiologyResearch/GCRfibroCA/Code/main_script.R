#This is free, open-source software under the (lenient) GNU GPLv3. It comes with no warranty. It concerns synergy analysis of mixtures whose component IDER (Individual Dose Effect Relations) are SNTE as defined in the NASA report "Synergy Analyses for Mixed Radiation Fields That Induce Non-Targeted Effects", "our NASA report" for short. This script was written by Dae Woong Ham between Sept. 2016 and May 2017. Quality control by Rainer K. Sachs May-June 2017.

##Main Script##
main_path = "Your_Path"

###Creating Data and Defining in Cacao's Model###
####Table 2, Table 3####
source(paste0(main_path, "data_model.R"))

###Introducing Own IDER###
####Table 4, Table 5####
source(paste0(main_path, "our_model.R"))

###Monte Carlo Simulation, Confidence Interval for Mixture Results###
####Figure 6, 7, 8####
source(paste0(main_path, "MC_CI.R"))

###Creating Figure 2, 3### 
####Section 4.1####
source(paste0(main_path, "figure_2_3.R"))



##Supplementary Calculations/Graphs##
supplement_path = "Your_Path"

###Model_Selection### 
####Table A2.3####
source(paste0(supplementary_path, "model_selection.R"))

###Graphs for A2### 
####Fig A2.3, A2.4####
source(paste0(supplementary_path, "fig_A2.R"))

###Graphs for A2.5### 
####Fig A2.5####
source(paste0(supplementary_path, "A2.5_fig.R"))

