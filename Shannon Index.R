#Goal: Calculate species evenness based on the weight amounts of each crop
#To calculate evenness, we will use J = H' / lnS, where H is the Shannon index
#and S is the number of categories of species in the garden.
#Shannon index <- H'= -sum(pi(ln(pi)) where p is the proportion of individuals
#found in the ith species

library(ggplot2)
library(dplyr)

#Upload data and join
garden.data.raw <- read.csv("~/Documents/Garden Data Analysis/Garden Data Full.csv") %>%
  mutate(weight_llbs = weight_oz * 0.0625)
crop.families <- read.csv("~/Documents/Garden Data Analysis/crop_key.csv")

garden.data.families <- full_join(garden.data.raw, crop.families, by = NULL,
                                  copy = FALSE, suffix = c(".garden.data.raw",
                                                           ".crop.families"))
garden.data.families$count <- as.numeric(garden.data.families$count)

#Isolate weights by crop and year

#2011
crops.2011 <- garden.data.families %>%
  filter(year == 2011)
factor.2011.crop <- crops.2011$crop
factor.2011.weight_llbs <- crops.2011$weight_llbs
crop.2011.weights <- data.frame(factor.2011.crop, factor.2011.weight_llbs)

total.2011.weights <- crop.2011.weights %>%
  group_by(factor.2011.crop) %>%
  summarize(total_llbs = sum(factor.2011.weight_llbs))
total.2011.weight.all.crop <- sum(factor.2011.weight_llbs)

#2012
crops.2012 <- garden.data.families %>%
  filter(year == 2012)
factor.2012.crop <- crops.2012$crop
factor.2012.weight_llbs <- crops.2012$weight_llbs
crop.2012.weights <- data.frame(factor.2012.crop, factor.2012.weight_llbs)

total.2012.weights <- crop.2012.weights %>%
  group_by(factor.2012.crop) %>%
  summarize(total_llbs = sum(factor.2012.weight_llbs))
total.2012.weight.all.crop <- sum(factor.2012.weight_llbs)

#2013
crops.2013 <- garden.data.families %>%
  filter(year == 2013)
factor.2013.crop <- crops.2013$crop
factor.2013.weight_llbs <- crops.2013$weight_llbs
crop.2013.weights <- data.frame(factor.2013.crop, factor.2013.weight_llbs)

total.2013.weights <- crop.2013.weights %>%
  group_by(factor.2013.crop) %>%
  summarize(total_llbs = sum(factor.2013.weight_llbs))
total.2013.weight.all.crop <- sum(factor.2013.weight_llbs)

#Calculate H
#2011
p.2011 <- total.2011.weights %>%
  mutate(p = total_llbs / total.2011.weight.all.crop) %>%
  mutate(ln = log(p)) %>%
  mutate(pln = ln * p)
H.2011 <- sum(p.2011$pln) * -1

#2012
p.2012 <- total.2012.weights %>%
  mutate(p = total_llbs / total.2012.weight.all.crop) %>%
  mutate(ln = log(p)) %>%
  mutate(pln = ln * p)
H.2012 <- sum(p.2012$pln) * -1

#2013
p.2013 <- total.2013.weights %>%
  mutate(p = total_llbs / total.2013.weight.all.crop) %>%
  mutate(ln = log(p)) %>%
  mutate(pln = ln * p)
H.2013 <- sum(p.2013$pln) * -1

#Graph Shannon index (H)
years <- c("2011", "2012", "2013")
shannon_index <- c(H.2011, H.2012, H.2013)
shannon_index_all_years <- data.frame(years, shannon_index)

ggplot(shannon_index_all_years, aes(x = years, y = shannon_index)) + geom_col()
