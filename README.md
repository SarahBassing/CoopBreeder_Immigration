# CoopBreeder_Immigration
A repository to hold code and input data used for analyses in "Immigration Does Not Offset Harvest Mortality in a Cooperatively Breeding Carnivore" manuscript by Bassing et al. Contains scripts for hypothesis testing of whether immigration into groups can compensate for harvest mortality in a cooperatively breeding species. 

Code:
Linear models that test whether the annual minimum density of wolves sampled is influenced by a) harvest mortality, b) differences in the East vs. West study areas in the Central Idaho region, and/or c) the amount of effort put into locating wolf rendezvous site surveys.
1. Idaho_MinDensity_Harvest.R
2. Idaho_MinDensity_Region.R
3. Idaho_MinDensity_SurveyEffort.R

Logistic models that test whether the probability a group contained long-distance immigrants is influenced by a) harvest mortality, b) differences in the East vs. West study areas in the Central Idaho region, and/or c) the amount of effort put into locating wolf rendezvous site surveys.
1. Idaho_LDI_Harvest.R
2. Idaho_LDI_Region.R
3. Idaho_LDI_SurveyEffort.R

Logistic models that test whether the probability a group contained short-distance immigrants is influenced by a) harvest mortality, b) differences in the East vs. West study areas in the Central Idaho region, and/or c) the amount of effort put into locating wolf rendezvous site surveys.
1. Idaho_SDI_Harvest.R
2. Idaho_SDI_Region.R
3. Idaho_SDI_SurveyEffort.R

Similar to the models above, these test whether the annual minimum density of wolves, and the probability a group contained long- and short-distance immigrants, respectively, differs between the two study regions at times when harvest was occuring. These models use a subset of the data only from years when harvest was occuring in the study regions (Central Idaho and Southwest Alberta).
1. IDvAB_MinDensity_Region.R
2. IDvAB_LDI_Region.R
3. IDvAB_SDI_Region.R
 
 

Data used by models:

Input data for models that evaluated the influence of harvest, East vs West study areas, and survey effort in Central Idaho only. 
  1. ID_Gdensity.RData: dataframe containing the study areas (East = 1, West = 2), the annual minimum density of wolves within that study area based on the number of individuals per group that were genetically sampled, the count of genetically samled wolves the minimum density is derived from, the year sampling took place within each study area, whether harvest occurred that year (no = 1, yes = 2), and the number of predicted rendezvous sites that were sampled each year within the study area. Each row represents a single observation of minimum density per study area per year.
  2. ID_imms.Rdata: dataframe containing the unique identification number for each indvidual wolf sampled, the study area it was sampled in (East = 1, West = 2), the putative group it was assigned to based on genetic analyses, the year(s) it was genetically detected, whether harvest occurred in Central Idaho that year, whether the individual was classified as a short-distance immigrant (SDI) or long-distance immigrant (LDI) each year it was sampled, whether it joined a group (i.e., was it a loner or not), and the number of predicted rendezvous sites that were sampled each year within the study area the individual was sampled in. Each row represents a single observation of a genetically sampled wolf per year.

Input data for models that evaluated the influence of harvest, Central Idaho vs Southwest Alberta study regions, and survey effort in years when harvest occurred.
  1. Harv_Gdensity.RData: dataframe containing the study areas (East Idaho = 1, West Idaho = 2, and Alberta = 3 study areas), the annual minimum density of wolves within that study region based on the number of individuals per group that were genetically sampled, the count of genetically samled wolves the minimum density is derived from, the year sampling took place within each study area, whether harvest occurred that year (no = 1, yes = 2), and the number of predicted rendezvous sites that were sampled each year within the study area. Each row represents a single observation of minimum density per study area per year.
  2. Harv_imms.Rdata: dataframe containing the unique identification number for each indvidual wolf sampled, the study area it was sampled in (East Idaho, West Idaho, and Alberta study areas), the putative group it was assigned to based on genetic analyses, the year(s) it was genetically detected, whether harvest occurred in Central Idaho that year, whether the individual was classified as a short-distance immigrant (SDI) or long-distance immigrant (LDI) each year it was sampled, whether it joined a group (i.e., was it a loner or not), and the number of predicted rendezvous sites that were sampled each year within the study area the individual was sampled in. Each row represents a single observation of a genetically sampled wolf per year.

