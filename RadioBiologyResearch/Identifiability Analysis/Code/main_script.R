# This is free, open-source software under the (lenient) GNU GPLv3. It comes with no warranty. 
# Written by Dae Woong Ham between May. 2018 and August 2018. Some quality control by Rainer (Ray) K. Sachs (rks) May-August 2017.
# Script concerns synergy analysis and parsimonious modeling of WGE simple chromosome aberrations (CA) induced in 82-6 fibroblast cells by simulated GCR (Galactic Cosmic Radiation) mixed fields.
#The script uses various mixture component one-ion DERs (Dose Effect Relations), summarized in "16Cacao" = 
# = Cacao, Hada, Saganti, George and Cucinotta. "Relative Biological Effectiveness of HZE Particles for Chromosomal Exchanges and Other Surrogate Cancer Risk Endpoints." PLoS One 11(4): e0153998. (2016)].

##Main Script##
main_path = "Your_Path"

###Creating Data###
####Table 2.1.1.####
source(paste0(main_path, "CA_data.R"))

###Creating Figure 1###
####Section 2.3####
source(paste0(main_path, "figure_1.R"))

###Introducing Parsimonious Models and Analyzing Their Fits###
####Section 2.3, Table 2.3.1, Section 3 and all tables in Section 3####
source(paste0(main_path, "introducing_models.R"))

###Creating Figure 2, 3### 
####Section 4.1####
source(paste0(main_path, "figure_2_3.R"))

###Creating Figure 3### 
####Section 4.1####
source(paste0(main_path, "model_selection.R"))

##Supplementary Calculations/Graphs##
supplement_path = "Your_Path"

###Expansion Modeling###
####S2.2####
source(paste0(supplement_path, "expansion_model.R"))


