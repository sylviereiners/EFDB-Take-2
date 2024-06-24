EFDBbasedata <- read.csv("/Users/sylviereiners/Downloads/EFDB_output (2).csv")
CO2EFDB <- subset(EFDBbasedata, Gas == "CARBON DIOXIDE")
unique(CO2EFDB$Source.of.data)
library(dplyr)

## Primary Classification ##

##Tropical##
tropicalsubset <- CO2EFDB %>% 
  filter(apply(., 1, function(row) {
    any(grepl("tropical", row, ignore.case=TRUE)) & 
      !any(grepl("subtropical", row, ignore.case=TRUE))
  }))

##Subtropical##
subtropical_subset <- CO2EFDB %>% 
  filter(apply(., 1, function(row) any(grepl("subtropical", 
                                             row, ignore.case=TRUE))))

##Temperate##
temeperate_subset <- CO2EFDB %>% 
  filter(apply(., 1, function(row) any(grepl("temperate", 
                                             row, ignore.case=TRUE))))

##Boreal##
boreal_subset <- CO2EFDB %>% 
  filter(apply(., 1, function(row) any(grepl("boreal", 
                                             row, ignore.case=TRUE))))

##Polar##
polar_subset <- CO2EFDB %>% 
  filter(apply(., 1, function(row) any(grepl("polar", 
                                             row, ignore.case=TRUE))))
##Combined/remaining##
combinedsubsets <- bind_rows(tropicalsubset, 
                             subtropical_subset, 
                             temeperate_subset, 
                             boreal_subset, 
                             polar_subset)
combinedsubsets <- combinedsubsets %>% distinct()
remainingrows <- anti_join(CO2EFDB, combinedsubsets, by = names(CO2EFDB))
## 4375 rows were grabbed by the keywords, 2201 remain ##

    ##Allow me to fix my typos##
        tropical_subset <- tropicalsubset
        temperate_subset <- temeperate_subset

##Identifying overlap##
tropsubtropOL <- semi_join(tropical_subset, subtropical_subset)
troptempOL <- semi_join(tropical_subset, temperate_subset)
tropborealOL <- semi_join(tropical_subset, boreal_subset)
troppolarOL <- semi_join(tropical_subset, polar_subset)
subtroptempOL <- semi_join(subtropical_subset, temperate_subset)
subtropborealOL <- semi_join(subtropical_subset, boreal_subset)
subtroppolarOL <- semi_join(subtropborealOL, polar_subset)
tempborealOL <- semi_join(temperate_subset, boreal_subset)
temppolarOL <- semi_join(temperate_subset, polar_subset)
borealpolarOL <- semi_join(boreal_subset, polar_subset)
##        Results: trop/subtrop: 0
#                  trop/temp: 69
#                  trop/boreal: 36
#                  trop/polar: 0
#                  subtrop/temp: 2
#                  subtrop/boreal: 0
#                  subtrop/polar:0
#                  temp/boreal: 100
#                  temp/polar: 0
#                  boreal/polar: 3

## Subclassifying Tropical ##
tropical_dryforest <- tropical_subset %>%
  filter(apply(., 1, function(row) any(grepl("dry forest", 
                                             row, ignore.case=TRUE))))
tropical_desert <- tropical_subset %>%
  filter(apply(., 1, function(row) any(grepl("desert", 
                                             row, ignore.case=TRUE))))
tropical_moistforest <- tropical_subset %>%
  filter(apply(., 1, function(row) any(grepl("moist forest", 
                                             row, ignore.case=TRUE))))
tropical_rainforest <- tropical_subset %>%
  filter(apply(., 1, function(row) any(grepl("rain forest|rainforest", 
                                             row, ignore.case=TRUE))))
tropical_shrubland <- tropical_subset %>%
  filter(apply(., 1, function(row) any(grepl("shrubland", 
                                             row, ignore.case=TRUE))))
classifiedtropical <- bind_rows(tropical_dryforest, tropical_desert, 
                                tropical_moistforest, tropical_rainforest, tropical_shrubland)

## Subclassifying Subtropical ##
subtrop_desert <- subtropical_subset %>%
  filter(apply(., 1, function(row) any(grepl("desert",
                                             row, ignore.case=TRUE))))
subtrop_dryforest <- subtropical_subset %>%
  filter(apply(., 1, function(row) any(grepl("dry forest",
                                             row, ignore.case=TRUE))))
subtrop_humidforest <- subtropical_subset %>%
  filter(apply(., 1, function(row) any(grepl("humid forest",
                                             row, ignore.case=TRUE))))
subtrop_mntnsys <- subtropical_subset %>%
  filter(apply(., 1, function(row) any(grepl("mountain system",
                                             row, ignore.case=TRUE))))
subtrop_steppe <- subtropical_subset %>%
  filter(apply(., 1, function(row) any(grepl("steppe",
                                             row, ignore.case=TRUE))))
classifiedsubtrop <-  bind_rows(subtrop_desert, subtrop_dryforest, subtrop_humidforest, 
                                subtrop_mntnsys, subtrop_steppe)

## Subclassifying Temperate ##
temperate_desert <- temperate_subset %>%
  filter(apply(., 1, function(row) any(grepl("desert",
                                             row, ignore.case=TRUE))))
temperate_contforest <- temperate_subset %>%
  filter(apply(., 1, function(row) any(grepl("continental forest",
                                             row, ignore.case=TRUE))))
temperate_oceanicforest <- temperate_subset %>%
  filter(apply(., 1, function(row) any(grepl("oceanic forest",
                                             row, ignore.case=TRUE))))
temperate_mntnsys <- temperate_subset %>%
  filter(apply(., 1, function(row) any(grepl("mountain system",
                                             row, ignore.case=TRUE))))
temperate_steppe <- temperate_subset %>%
  filter(apply(., 1, function(row) any(grepl("steppe",
                                             row, ignore.case=TRUE))))
classifiedtemperate <- bind_rows(temperate_desert, temperate_contforest, 
                                 temperate_oceanicforest, temperate_mntnsys, temperate_steppe)

## Subclassifying Boreal ##
boreal_conifforest <- boreal_subset %>%
  filter(apply(., 1, function(row) any(grepl("coniferous forest",
                                             row, ignore.case=TRUE))))
boreal_mntnsys <- boreal_subset %>%
  filter(apply(., 1, function(row) any(grepl("mountain system",
                                             row, ignore.case=TRUE))))
boreal_tundrawoodland <- boreal_subset %>%
  filter(apply(., 1, function(row) any(grepl("tundra woodland",
                                             row, ignore.case=TRUE))))
classifiedboreal <- bind_rows(boreal_conifforest, boreal_mntnsys,
                              boreal_tundrawoodland)
    ## No subclass needed for polar ##
  # What missed the classifications? #
remainder_tropical <- anti_join(tropical_subset, classifiedtropical, 
                            by=names(tropical_subset))
remainder_subtrop <- anti_join(subtropical_subset, classifiedsubtrop, 
                               by=names(subtropical_subset))
remainder_temp <- anti_join(temperate_subset, classifiedtemperate, 
                            by=names(temperate_subset))
remainder_boreal <- anti_join(boreal_subset, classifiedboreal, 
                            by=names(boreal_subset))
#   Should I delete the overlapping? 
# Subtrop/temp OL should both by temp cont forest
# temp/boreal OL looks like actual overlap/double categorization
# trop/boreal OL (mainly) apply to temp, boreal, and tropical. not sure how.
# trop/temp needs to be sorted out (paper names getting in the way)

## Adding rows, sample "complete" dataframe ##
tropical_desert <- tropical_desert%>% 
  mutate('Global Ecological Zone' = "Tropical desert")
tropical_dryforest <- tropical_dryforest%>% 
  mutate('Global Ecological Zone' = "Tropical dry forest")
tropical_moistforest <- tropical_moistforest%>% 
  mutate('Global Ecological Zone' = "Tropical moist forest")
tropical_rainforest <- tropical_rainforest%>% 
  mutate('Global Ecological Zone' = "Tropical rain forest")
tropical_shrubland <- tropical_shrubland%>% 
  mutate('Global Ecological Zone' = "Tropical shrubland")

temperate_desert <- temperate_desert %>%
  mutate('Global Ecological Zone' = "Temperate desert")
temperate_contforest <- temperate_contforest %>%
  mutate('Global Ecological Zone' = "Temperate continental forest")
temperate_oceanicforest <- temperate_oceanicforest %>%
  mutate('Global Ecological Zone' = "Temperate oceanic forest")
temperate_mntnsys <- temperate_mntnsys %>%
  mutate('Global Ecological Zone' = "Temperate mountain system")
temperate_steppe <- temperate_steppe %>%
  mutate('Global Ecological Zone' = "Temperate steppe")

subtrop_desert <- subtrop_desert %>%
  mutate('Global Ecological Zone' = "Subtropical desert")
subtrop_dryforest <- subtrop_dryforest %>%
  mutate('Global Ecological Zone' = "Subtropical dry forest")
subtrop_humidforest <- subtrop_humidforest %>%
  mutate('Global Ecological Zone' = "Subtropical humid forest")
subtrop_mntnsys <- subtrop_mntnsys %>%
  mutate('Global Ecological Zone' = "Subtropical mountain system")
subtrop_steppe <- subtrop_steppe %>%
  mutate('Global Ecological Zone' = "Subtropical steppe")

boreal_conifforest <- boreal_conifforest %>%
  mutate('Global Ecological Zone' = "Boreal coniferous forest")
boreal_mntnsys <- boreal_mntnsys %>%
  mutate('Global Ecological Zone' = "Boreal mountain system")
boreal_tundrawoodland <- boreal_tundrawoodland %>%
  mutate('Global Ecological Zone' = "Boreal tundra woodland")

polar_subset <- polar_subset %>%
  mutate('Global Ecological Zone' = "Polar")

sample_bound <- bind_rows(tropical_desert, tropical_dryforest, tropical_moistforest, tropical_rainforest, tropical_shrubland, 
                          temperate_contforest, temperate_desert, temperate_oceanicforest, temperate_mntnsys, temperate_steppe, 
                          subtrop_desert, subtrop_dryforest, subtrop_humidforest, subtrop_mntnsys, subtrop_steppe,
                          boreal_conifforest, boreal_mntnsys, boreal_tundrawoodland, polar_subset)
unique(sample_bound)

    # Next steps: figure out what happened to the overlapping rows, ask Dave what to do with overlapping rows. #
unique_sources <- unique(sample_bound$Source.of.data)
source_matrix <- matrix(unique_sources)
