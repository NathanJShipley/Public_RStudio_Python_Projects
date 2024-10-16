#==============================================================================
#                             Election 2020 Maps                         #
#==============================================================================


#===========================================
# Load Required Libraries                  #
#===========================================
library(tidycensus)   # Access census API data
library(tidyverse)    # Data manipulation and visualization (dplyr, ggplot2, tidyr, readr)
library(sf)           # Simple features for spatial data
library(tmap)         # Thematic mapping
library(stringr)      # String manipulation functions
library(BAMMtools)    # Tools for Jenks classification
library(classInt)     # Class intervals for classification
library(psych)        # Psychological statistics functions
library(Hmisc)        # Functions for data analysis (e.g., correlation matrices)
library(svglite)      # Save plots as SVG vector graphics


#===========================================
# Load Census API Key                      #
#===========================================
# Uncomment and set your Census API key here (run only once)
# census_api_key("your_api_key_here", install = TRUE)


#===========================================
# Model Load variables of interest for spatial  #
#==========================================-

# Define variables of interest
vars <- c(D_Tot_Pop = "P1_001N")

# Retrieve decennial census data for counties
Decennial_dat <- tidycensus::get_decennial(
  geography = "county",
  variables = vars,
  year = 2020,
  geometry = TRUE,  # Includes geometry information
  output = "wide"
)

# Lets extract the state and county names

# Extract and clean state and county names
Decennial_dat$state <- str_trim(toupper(sapply(strsplit(Decennial_dat$NAME, ","), "[", 2)))
Decennial_dat$county_name <- toupper(sapply(strsplit(Decennial_dat$NAME, ","), "[", 1))
Decennial_dat$county_name <- str_trim(str_remove(Decennial_dat$county_name, "COUNTY"))
Decennial_dat$county_name <- str_trim(str_remove(Decennial_dat$county_name, "PARISH"))


#===========================================
# Load in the election data #
#==========================================-

Election_dat <- read.csv("Election_2020.csv", header = TRUE)

Election_2020_dat <- Election_dat %>%
  filter(year == '2020' & c(party == 'REPUBLICAN' | party == 'DEMOCRAT')) %>%
  select(c('state','state_po','county_name','party','candidatevotes','totalvotes','mode'))

# Okay, lets go ahead and pull out the states with TOTAL counts
Election_2020_dat_TOTAL <- Election_2020_dat %>%
  filter(mode == 'TOTAL')

# And make another data set for those whose totals we will have to calculate
Election_2020_dat_NOT_TOTAL <- Election_2020_dat %>%
  filter(!mode == 'TOTAL')

# Remove any state from the not total who has total
state_list <- unique(Election_2020_dat_TOTAL$state)

Election_2020_dat_NOT_TOTAL <- Election_2020_dat_NOT_TOTAL %>%
  filter(!state %in% state_list)

# Lets try to get total 
Not_Total_Output <- Election_2020_dat_NOT_TOTAL %>%
  group_by(state,county_name,party) %>% 
  summarise(candidatevotes = sum(candidatevotes),
            totalvotes = mean(totalvotes)) %>%
  ungroup()
  

# Join combine 
# First remove columns from dat total
Election_2020_dat_TOTAL <- Election_2020_dat_TOTAL %>%
  select(c('state','county_name','party','candidatevotes','totalvotes'))

Election_2020_dat_TOTAL <- rbind(Election_2020_dat_TOTAL, Not_Total_Output)


# Some how salt lake is missing..... SALT LAKE
Election_2020_dat_SALTLAKE <- Election_dat %>%
  filter(year == '2020' & county_name == 'SALT LAKE', c(party == 'REPUBLICAN' | party == 'DEMOCRAT')) %>%
  select(c('state','county_name','party','candidatevotes','totalvotes','mode'))

Election_2020_dat_SALTLAKE <- Election_2020_dat_SALTLAKE %>%
  group_by(state,county_name,party) %>% 
  summarise(candidatevotes = sum(candidatevotes),
            totalvotes = mean(totalvotes)) %>%
  ungroup()


Election_2020_dat_TOTAL <- rbind(Election_2020_dat_TOTAL, Election_2020_dat_SALTLAKE)

Election_2020_dat_TOTAL$vote_percentage <- round( Election_2020_dat_TOTAL$candidatevotes / Election_2020_dat_TOTAL$totalvotes, 3)


#drop total votes now and convert
Election_2020_dat_diff <- Election_2020_dat_TOTAL %>%
  reshape(idvar = c("state","county_name"), timevar = "party", direction = "wide") %>%
  select(!c('candidatevotes.DEMOCRAT','candidatevotes.REPUBLICAN','totalvotes.REPUBLICAN')) %>%
  rename(Democrat = vote_percentage.DEMOCRAT,
         Republican = vote_percentage.REPUBLICAN,
         TotalVotes = totalvotes.DEMOCRAT)

# Now get the largest 
Election_2020_dat_diff$diff <- pmax(Election_2020_dat_diff$Democrat, Election_2020_dat_diff$Republican)

# Code republican negative
Election_2020_dat_diff$diff <- ifelse(Election_2020_dat_diff$diff == Election_2020_dat_diff$Republican, -(Election_2020_dat_diff$diff), Election_2020_dat_diff$diff)

# Drop Alaska and Hawaii
Election_2020_dat_diff <- Election_2020_dat_diff %>%
  filter(!state == "ALASKA")

Election_2020_dat_diff <- Election_2020_dat_diff %>%
  filter(!state == "HAWAII")


# Make some custom changes

Election_2020_dat_diff$county_name <- ifelse(Election_2020_dat_diff$county_name == 'LA SALLE' & Election_2020_dat_diff$state == 'LOUISIANA', 'LASALLE', Election_2020_dat_diff$county_name)
Election_2020_dat_diff$county_name <- ifelse(Election_2020_dat_diff$county_name == 'DONA ANA', 'DO?A ANA', Election_2020_dat_diff$county_name)
Election_2020_dat_diff$county_name <- ifelse(Election_2020_dat_diff$county_name == 'ST MARY\'S', 'ST. MARY\'S', Election_2020_dat_diff$county_name)
Election_2020_dat_diff$county_name <- ifelse(Election_2020_dat_diff$county_name == 'SAINT LOUIS', 'ST. LOUIS', Election_2020_dat_diff$county_name)
Election_2020_dat_diff$county_name <- ifelse(Election_2020_dat_diff$county_name == 'ST. LOUIS COUNTY', 'ST. LOUIS', Election_2020_dat_diff$county_name)


#===========================================
# Lets join the election data to the spatial data
#==========================================-

# Right join the election data onto spatial
Joined_Election_Spatial_Data <- dplyr::inner_join(Decennial_dat, Election_2020_dat_diff, by = c("state","county_name"))

data_view <- as.data.frame(Joined_Election_Spatial_Data) %>%
  select(-geometry)

Decennial_dat_data_view <- as.data.frame(Decennial_dat) %>%
  select(-geometry)


#===========================================
# Lets Map this 
#==========================================-

# Okay, one last change, lets set the R to not be negative, but instead to be below 50%
Joined_Election_Spatial_Data$diff <- ifelse(Joined_Election_Spatial_Data$diff < 0, 1 + Joined_Election_Spatial_Data$diff, Joined_Election_Spatial_Data$diff)

# Looks pretty normal distribution
## hist(Joined_Election_Spatial_Data$diff)

# Set up custom color
MyPal <- c('#FF4A43','#E84D56','#D15069',
           '#BA547B','#A3578E','#8D5AA1',
           '#765DB4','#5F60C7','#4864D9',
           '#3167EC','#1A6AFF')

county_difference_map <- tm_shape(Joined_Election_Spatial_Data) + 
  tm_borders() + 
  tm_fill("diff", palette = MyPal, title = "Difference in Election per County",
          legend.is.portrait = T, alpha = .8, 
          breaks = c(0, .05, .15, .25,
                     .35, .45, .55, .65,
                     .75, .85, .95,  1),
          labels = c("95%+ Republican",  "85% to 95% Republican", "75% to 85% Republican",
                     "65% to 75% Republican", "55% to 65% Republican", "55% Republican to 55% Democrat",
                     "55% to 68% Democrat", "65% to 75% Democrat", "75% to 85% Democrat", "85% to 95% Democrat", 
                     "95%+ Democrat")) + 
  tm_layout(legend.outside = T, legend.outside.position = "right", frame = F,
            legend.title.size = 2, legend.text.size = 1)

county_difference_map

# Save the map as a PNG file
tmap_save(county_difference_map, filename = "county_difference_map.png", width = 10, height = 8, units = "in", dpi = 300)


# Okay, I would like to try and get the centroid of this
Joined_Election_Spatial_Data_CENTROID <- Joined_Election_Spatial_Data
Joined_Election_Spatial_Data_CENTROID$geometry <- st_centroid(st_geometry(Joined_Election_Spatial_Data_CENTROID))

county_difference_map_centroid <- tm_shape(Joined_Election_Spatial_Data_CENTROID) + 
  tm_dots("diff", palette = MyPal, title = "Difference in Election per County",
          legend.is.portrait = T, alpha = .8, size = "TotalVotes", size.max = 300000, legend.size.show = FALSE, 
          breaks = c(0, .05, .15, .25, .35, .45,
                     .55, .65, .75, .85, .95, 1),
          labels = c("95%+ Republican", "85% to 95% Republican", "75% to 85% Republican",
                     "65% to 75% Republican", "55% to 65% Republican"," 55% Republican to 55% Democrat",
                     "55% to 68% Democrat", "65% to 75% Democrat", "75% to 85% Democrat", 
                     "85% to 95% Democrat", "95%+ Democrat"),
          perceptual = TRUE,
          ) + 
  tm_layout(legend.outside = T, legend.outside.position = "right", frame = F,
            legend.title.size = 2, legend.text.size = 1)

county_difference_map_centroid

# Save the map as a PNG file
tmap_save(county_difference_map_centroid, filename = "county_difference_map_centroid.png", width = 10, height = 8, units = "in", dpi = 300)

