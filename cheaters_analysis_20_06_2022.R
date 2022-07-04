# AUTHOR METADATA----
# Code for data analysis
# Cheaters among Pollinators 
# (https://www.biorxiv.org/content/10.1101/2022.05.16.492032v2)
# Sailee Sakhalkar (sailee.sakha@gmail.com)
# ICEG (www.insect-communities.cz)

# ----
# This data is provisionally made available to reviewers during the 
# peer-review process. Please do not share publically. The data will be made 
# public after the manuscript is accepted for publication.


# Setting up workspace----

# Clear existing objects----
rm(list=ls())

# Sets the file path to whereever this script is placed
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Moving the directory up one step
setwd("..")


# Libraries----

# install.packages("pacman")
pacman::p_load(dplyr, tidyverse, bipartite, readr, car, ggplot2,
               ggpubr, viridis, corrplot, Hmisc, readxl,
               cluster, vegan, RColorBrewer, cowplot, svglite, ade4)

# select <- dplyr::select




# Importing and cleaning data---- 

## Data for visitation frequencies----
cheater_data <- read_excel("input/cheaters_visitation_and_trait_data.xlsx", 
                           sheet = "cheater_data")

# All of the character columns are factors
cheater_data %>% mutate(across(c(spcode:functional_group,behavior), factor))

# Filter out "landing" visits where nothing happens
cheater_data <- cheater_data %>% filter(!behavior %in% c("both","visiting"))

# Elevation is an ordered factor
cheater_data$elevation <-factor(cheater_data$elevation, ordered = T,
                           levels = c("650m", "1100m", "1450m", "2250m"))

## Data for plant traits----
plant_traits <- read_excel("input/plant_traits.xlsx", 
                           sheet = "plant_traits") %>% 
  distinct() %>% arrange(spcode)
  
# plant_traits <-read.csv("input/plant_traits.csv", na.strings="") %>% 
  
  # distinct() %>% arrange(spcode)

# ## Listing orders of interest----
# orders <- c(cheater_data$functional_group)
# names(orders) <- c(cheater_data$Visitor_order_common)
# orders <- orders[unique(names(orders))]

# Overview barplot ----

(basic_barplot <- cheater_data %>% group_by(behavior) %>% 
   
   ggplot(aes(x = elevation, y = freq_fm_per_species, fill = behavior))+
   
   geom_col()+ 
   
   facet_grid(.~season) +
   
   scale_fill_manual(values = c("cadetblue3", "goldenrod2", "ivory2", "black"),
                     
                     name = "Behaviour")+
   
   theme_classic() + 
   
   labs(
     x = "Elevation",
     y = "Visitation frequency/flower/min",
     title = "Frequencies of visitors across Seasons and Elevations",
     subtitle = "Relatively more robbers in the wet season and thieves in the dry season")+
   
   theme(strip.background = element_blank(),
         legend.position = "bottom"))

#This actually save the plot in a image
ggsave(file="barplot.png", plot=basic_barplot, width=10, height=8)

# Chi-square tests ----

## Creating tables to run tests----

# Overall
chi_test_all <- cheater_data %>% unite(site, c("elevation", "season")) %>%
  
  group_by(behavior, site) %>% 
  
  summarise(visits = n()) %>% 
  
  spread(site, visits, 0)

# Chi-square tests by elevation and season combinations

# Creating the tables for each combination

# Only elevations in the dry season
chi_test_DRY <- cheater_data %>% ungroup() %>% 
  
  filter(season == "DRY") %>% 
  
  group_by(behavior, elevation) %>% 
  
  summarise(visits = n()) %>% 
  
  spread(elevation, visits, 0)

# Only elevations in the wet season
chi_test_WET <-  cheater_data %>% ungroup() %>% 
  
  filter(season == "WET") %>% 
  
  group_by(behavior, elevation) %>%  
  
  summarise(visits = n()) %>% 
  
  spread(elevation, visits, 0)

# Only between seasons
chi_test_exp <-  cheater_data %>% ungroup() %>% 
  
  group_by(behavior, season) %>% 
  
  summarise(visits = n()) %>% 
  
  spread(season, visits, 0)

# Only across elevations
chi_test_elev <-  cheater_data %>% ungroup() %>% 
  
  group_by(behavior, elevation) %>% 
  
  summarise(visits = n()) %>% 
  
  spread(elevation, visits, 0)

## Running the tests----

(chi_result_all <- chisq.test(chi_test_all[-1]))

(chi_result_DRY <- chisq.test(chi_test_DRY[-1]))

(chi_result_WET <- chisq.test(chi_test_WET[-1]))

(chi_result_exp <- chisq.test(chi_test_exp[-1]))

(chi_result_elev <- chisq.test(chi_test_elev[-1]))

# RLQ Analyses----


# Creating matrix L (species per site)
(temp_matrix_L <- cheater_data %>% ungroup() %>%  
   
   select(elevation, season, spcode) %>% unique() %>% 
   
   unite(Site, c(elevation, season)) %>% mutate(presence=1) %>% 
   
   spread(Site, presence, 0))

# Creating matrix R, this is the behaviour per site----

(temp_matrix_R <- cheater_data %>% ungroup() %>% 
   
   group_by(elevation, season, behavior) %>% 
   
   summarise(freq=sum(freq_fm_per_species)) %>% 
   
   unite(Site, c(elevation, season), remove = FALSE) %>%
   
   ungroup() %>% select(-elevation) %>% 
   
   spread(behavior, freq, 0) %>% 
   
   mutate(elevation = c(650, 650, 1100, 1100, 1450, 1450, 2250, 2250)))

# Creating matrix Q, the traits per species matrix----
(temp_matrix_Q <- plant_traits %>% 
   
   select(-odour_strength, -brightness) %>%
   
   filter(spcode %in% temp_matrix_L$spcode) %>% 
   
   mutate_all(~replace(., is.na(.), 0)))

# Filtering matrix L for the species in Q

(temp_matrix_L <- temp_matrix_L %>% filter(spcode %in% temp_matrix_Q$spcode))

# Checking for matrix dimension matches----

setdiff(temp_matrix_Q$spcode, temp_matrix_L$spcode)


cheater_data %>% ungroup() %>% select(elevation, season,spcode) %>% unique() %>% 
  filter(!spcode %in% temp_matrix_L$spcode)


# Turning the tibbles to matrices----

matrix_L <- as.data.frame(temp_matrix_L[-1])
rownames(matrix_L) <- temp_matrix_L$spcode
(matrix_L <- t(matrix_L))


matrix_R <- as.data.frame(temp_matrix_R[-1])
rownames(matrix_R) <- temp_matrix_R$Site
matrix_R$season <- as.factor(matrix_R$season)
matrix_R$elevation <- as.integer(matrix_R$elevation)


matrix_Q <- as.data.frame(temp_matrix_Q[-1])
rownames(matrix_Q) <- temp_matrix_Q$spcode
matrix_Q <- matrix_Q %>%
  mutate_if(sapply(matrix_Q, is.character), as.factor)

# Running individual analyses----

# Site by species (CA)
afcL.aravo <- dudi.coa(matrix_L, scannf = FALSE)

# Site by behaviour(HSA weighted by CA)
acpR.aravo <- dudi.hillsmith(matrix_R, row.w = afcL.aravo$lw,scannf = FALSE)


# Species by traits (HSA weighted by CA)
acpQ.aravo <- dudi.hillsmith(matrix_Q, row.w = afcL.aravo$cw,
                             scannf = FALSE)

# Running RLQ----
rlq.aravo <- rlq(acpR.aravo, afcL.aravo, acpQ.aravo,scannf = FALSE)

# RLQ summary tables
summary(rlq.aravo)

# Monte-Carlo test for significance
randtest(rlq.aravo, modeltype = 6)


# Visualisations----
# par(mfrow = c(1, 3))

# s.arrow(rlq.aravo$l1,  grid = T, xlim=c(-1,1), ylim=c(-1,1), add.plot = T)

# (s.arrow(rlq.aravo$c1, boxes = FALSE, addaxes = T, cpoint = 1, edge = F, clabel   = 1, add.plot = F))
# (s.arrow(rlq.aravo$mR, boxes = FALSE, addaxes = T, cpoint = 1, edge = F, clabel   = 1, add.plot = F))
# (s.arrow(rlq.aravo$lR, boxes = FALSE,add.plot = F, addaxes = T, cpoint = 1, edge = F, clabel   = 1))
# s.arrow()

# s.label(rlq.aravo$lQ, boxes = FALSE, clab = 0, cpoint = 2, pch = 2)
 

# RDAs ----

# Source the triplot function
source("scripts/triplotfunction.R")


# RDA for robbers ----

## Formatting data----
# Creating a vector to hold orders of robbers
robber_orders <- cheater_data %>%
  
  filter(behavior == "robbed") %>% 
  
  select(functional_group) %>% unique()

# Subset of robbed plants

robbed_plants <- cheater_data %>% filter(behavior == "robbed") %>% 
  
  select(spcode) %>% unique()

# Traits for the subset of robbed plants
robbed_plant_traits <- plant_traits %>% 
  
  filter(spcode %in% robbed_plants$spcode) %>% 
  
  na.omit()

# All of the character columns are factors
robbed_plant_traits %>%
  
  mutate(across(c(shape:nectar_guides), factor))

rownames(robbed_plant_traits) <- robbed_plant_traits$spcode

# Creating a dataframe with the frequency of robbed by different F.G. per plant

robbing_rda <- cheater_data %>% group_by(spcode, functional_group) %>%
  
  filter(functional_group %in% robber_orders$functional_group) %>%
  
  mutate(robfreq = ifelse(behavior=="robbed", freq_fm_per_species, 0)) %>%
  
  summarise(robbingfrequency = sum(robfreq)) %>%
  
  spread(functional_group, robbingfrequency, fill = 0) %>% ungroup() %>%
  
  replace(is.na(.), 0) %>% filter(spcode %in% robbed_plant_traits$spcode)


# A dataframe with the traits of robbed plants
envrob <- robbed_plant_traits[,c(2:10)]
names(envrob)
rownames(envrob) <- c(1:nrow(envrob))

# Removing the first column
freqrob <- robbing_rda[,-1]
names(freqrob)

# Running the Hellinger distance algorithm 
rob.hellinger <- decostand(freqrob, "hellinger")

## Most parsimonious model ----

# After trying a few models, the parsimonious one was:
rob.rda.pars <- rda(rob.hellinger ~ tube_length +
                      shape + tube_width, envrob)

# Details about the model fit ----
# summary(rob.rda.pars)
# coef(rob.rda.pars)
# RsquareAdj(rob.rda.pars)
# anova(rob.rda.pars,permutations = how(nperm=999))
# anova(rob.rda.pars,permutations = how(nperm=999), by="axis")


## Plot ----

# Check the triplot.rda function for more details
triplot.rda(rob.rda.pars,
            site.sc = "lc", scaling=2,
            cex.char1 = 1, cex.char2 = 1,
            pos.env = 4, pos.spe = 3,
            mult.spe=0.9, mult.arrow = 0.92,
            plot.sites = F, label.sites=F,
            mar.percent = -1, optimum = T)

mtext("RDA for functional groups of robbers")



# RDA for thieves ----


## Formatting data----
# Creating a vector to hold orders of thieves
thief_orders <- cheater_data %>%
  
  filter(behavior == "thieved") %>% 
  
  select(functional_group) %>% unique()

# Subset of thieved plants

thieved_plants <- cheater_data %>% filter(behavior == "thieved") %>% 
  
  select(spcode) %>% unique()


# Traits for the subset of thieved plants
thieved_plant_traits <- plant_traits %>% 
  
  filter(spcode %in% thieved_plants$spcode) %>% 
  
 na.omit()

# All of the character columns are factors
thieved_plant_traits %>%
  
  mutate(across(c(shape:nectar_guides), factor))

rownames(thieved_plant_traits) <- thieved_plant_traits$spcode

# Creating a dataframe with the frequency of thieved by different F.G. per plant

thieving_rda <- cheater_data %>% group_by(spcode, functional_group) %>%
  
  filter(functional_group %in% thief_orders$functional_group) %>%
  
  mutate(thieffreq = ifelse(behavior=="thieved", freq_fm_per_species, 0)) %>%
  
  summarise(thievingfrequency = sum(thieffreq)) %>%
  
  spread(functional_group, thievingfrequency, fill = 0) %>% ungroup() %>%
  
  replace(is.na(.), 0) %>% filter(spcode %in% thieved_plant_traits$spcode)


# A dataframe with the traits of thievbed plants
envthief <- thieved_plant_traits[,c(2:10)]
names(envthief)
rownames(envthief) <- c(1:nrow(envthief))

# Removing the first column
freqthief <- thieving_rda[,-1]
names(freqthief)

# Running the Hellinger distance algorithm 
thief.hellinger <- decostand(freqthief, "hellinger")

# Most parsimonious model ----
(thief.rda.pars <- rda(thief.hellinger~ brightness + shape + tube_length, envthief))

# Details about the model fit ----
# summary(thief.rda.pars)
# coef(thief.rda.pars)
# RsquareAdj(thief.rda.pars)
# anova(thief.rda.pars,permutations = how(nperm=999))
# anova(thief.rda.pars,permutations = how(nperm=999), by="axis")


## Plot ----

# Check the triplot.rda function for more details
triplot.rda(thief.rda.pars,
            site.sc = "lc", scaling=2,
            cex.char1 = 1, cex.char2 = 1,
            pos.env = 4, pos.spe = 3,
            plot.sites = F, label.sites=F,
            mar.percent = -1, optimum = T)

mtext("RDA for functional groups of thieves")

