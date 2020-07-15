#Goal: to calculate species evenness of the garden using J=H'/lnS
# s=number of crops per year
library(ggplot2)
library(dplyr)

#get the data!
shannon_index_all_years
H.2011 <- 3.042144
H.2012 <- 3.084806
H.2013 <- 2.901443
#Isolate years
species_evenness <- shannon_index_all_years %>%
  mutate(crops = richness)

shannon_index_2011 <- species_evenness %>%
  filter(years == 2011)

shannon_index_2012 <- species_evenness %>%
  filter(years == 2012)

shannon_index_2013 <- species_evenness %>%
  filter(years == 2013)

str(shannon_index_2011)
shannon_index_2011 <- as.numeric(shannon_index_2011$crops)

# J = H'/lnS
#2011
j.2011 <- 3.042144 / log(44)
j.2012 <- H.2012 / log(44)
j.2013 <- H.2013 / log(39)
