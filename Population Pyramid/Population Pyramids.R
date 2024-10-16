#==============================================================================
#                              Analysis Repository:                           #
#                                 US Census API                               #      
#==============================================================================

# Description

# This was a script I wrote to look at population pyrmids. I did nothing fancy here. 
# Simple script to pull from a census api and then build out the pyramid 

#===========================================
# Load Libraries                           #
#==========================================-
library(tidycensus) # Has all census api info
library(tidyverse) # dplyr, ggplot2, tidyr, readr
library(ggplot2)     # For plotting

#gc()

#===========================================
# Load Census API Key                      #
#==========================================-

# Only need to do once, can request https://api.census.gov/data/key_signup.html
# census_api_key("YourCensusKeyHere",install = T)

#===========================================
# Model Load variables of interest         #
#==========================================-

# To use the census api, you have to use this list of vars
vars <- c(Male_Under_5 = "B01001_003", 
          Male_5_to_9 = "B01001_004",
          Male_10_to_14 = "B01001_005",
          Male_15_to_17 = "B01001_006",
          Male_18_to_19 = "B01001_007",
          Male_20 = "B01001_008",
          Male_21 = "B01001_009",
          Male_22_to_24 = "B01001_010",
          Male_25_to_29 = "B01001_011",
          Male_30_to_34 = "B01001_012",
          Male_35_to_39 = "B01001_013",
          Male_40_to_44 = "B01001_014",
          Male_45_to_49 = "B01001_015",
          Male_50_to_54 = "B01001_016",
          Male_55_to_59 = "B01001_017",
          Male_60_to_61 = "B01001_018",
          Male_62_to_64 = "B01001_019",
          Male_65_to_66 = "B01001_020",
          Male_67_to_69 = "B01001_021",
          Male_70_to_74 = "B01001_022",
          Male_75_to_79 = "B01001_023",
          Male_80_to_84 = "B01001_024",
          Male_85_and_Up = "B01001_025",
          Female_Under_5 = "B01001_027",
          Female_5_to_9 = "B01001_028",
          Female_10_to_14 = "B01001_029",
          Female_15_to_17 = "B01001_030",
          Female_18_to_19 = "B01001_031",
          Female_20 = "B01001_032",
          Female_21 = "B01001_033",
          Female_22_to_24 = "B01001_034",
          Female_25_to_29 = "B01001_035",
          Female_30_to_34 = "B01001_036",
          Female_35_to_39 = "B01001_037",
          Female_40_to_44 = "B01001_038",
          Female_45_to_49 = "B01001_039",
          Female_50_to_54 = "B01001_040",
          Female_55_to_59 = "B01001_041",
          Female_60_to_61 = "B01001_042",
          Female_62_to_64 = "B01001_043",
          Female_65_to_66 = "B01001_044",
          Female_67_to_69 = "B01001_045",
          Female_70_to_74 = "B01001_046",
          Female_75_to_79 = "B01001_047",
          Female_80_to_84 = "B01001_048",
          Female_85_and_Up = "B01001_049")


#===========================================
# Pull from Census API         #
#==========================================-

# Pull from Census API
Dane.County.Census.CT.2020 <- tidycensus::get_acs(geography = "tract",
                                                  variables = vars,
                                                  state = "WI",
                                                  county = "Dane",
                                                  year = 2020,
                                                  survey = "acs5",
                                                  geometry = F, 
                                                  output = "wide")
              

#===========================================
# Now plot         #
#==========================================-

# First summarize the data into 5 year groups 
summary_data_5yr <- Dane.County.Census.CT.2020 %>%
  summarise(
    Male_Under_5 = sum(Male_Under_5E, na.rm = TRUE),
    Male_5_to_9 = sum(Male_5_to_9E, na.rm = TRUE),
    Male_10_to_14 = sum(Male_10_to_14E, na.rm = TRUE),
    Male_15_to_19 = sum(Male_15_to_17E, Male_18_to_19E, na.rm = TRUE),
    Male_20_to_24 = sum(Male_20E, Male_21E, Male_22_to_24E, na.rm = TRUE),
    Male_25_to_29 = sum(Male_25_to_29E, na.rm = TRUE),
    Male_30_to_34 = sum(Male_30_to_34E, na.rm = TRUE),
    Male_35_to_39 = sum(Male_35_to_39E, na.rm = TRUE),
    Male_40_to_44 = sum(Male_40_to_44E, na.rm = TRUE),
    Male_45_to_49 = sum(Male_45_to_49E, na.rm = TRUE),
    Male_50_to_54 = sum(Male_50_to_54E, na.rm = TRUE),
    Male_55_to_59 = sum(Male_55_to_59E, na.rm = TRUE),
    Male_60_to_64 = sum(Male_60_to_61E, Male_62_to_64E, na.rm = TRUE),
    Male_65_to_69 = sum(Male_65_to_66E, Male_67_to_69E, na.rm = TRUE),
    Male_70_to_74 = sum(Male_70_to_74E, na.rm = TRUE),
    Male_75_to_79 = sum(Male_75_to_79E, na.rm = TRUE),
    Male_80_to_84 = sum(Male_80_to_84E, na.rm = TRUE),
    Male_85_and_Up = sum(Male_85_and_UpE, na.rm = TRUE),
    Female_Under_5 = sum(Female_Under_5E, na.rm = TRUE),
    Female_5_to_9 = sum(Female_5_to_9E, na.rm = TRUE),
    Female_10_to_14 = sum(Female_10_to_14E, na.rm = TRUE),
    Female_15_to_19 = sum(Female_15_to_17E, Female_18_to_19E, na.rm = TRUE),
    Female_20_to_24 = sum(Female_20E, Female_21E, Female_22_to_24E, na.rm = TRUE),
    Female_25_to_29 = sum(Female_25_to_29E, na.rm = TRUE),
    Female_30_to_34 = sum(Female_30_to_34E, na.rm = TRUE),
    Female_35_to_39 = sum(Female_35_to_39E, na.rm = TRUE),
    Female_40_to_44 = sum(Female_40_to_44E, na.rm = TRUE),
    Female_45_to_49 = sum(Female_45_to_49E, na.rm = TRUE),
    Female_50_to_54 = sum(Female_50_to_54E, na.rm = TRUE),
    Female_55_to_59 = sum(Female_55_to_59E, na.rm = TRUE),
    Female_60_to_64 = sum(Female_60_to_61E, Female_62_to_64E, na.rm = TRUE),
    Female_65_to_69 = sum(Female_65_to_66E, Female_67_to_69E, na.rm = TRUE),
    Female_70_to_74 = sum(Female_70_to_74E, na.rm = TRUE),
    Female_75_to_79 = sum(Female_75_to_79E, na.rm = TRUE),
    Female_80_to_84 = sum(Female_80_to_84E, na.rm = TRUE),
    Female_85_and_Up = sum(Female_85_and_UpE, na.rm = TRUE)
  )


# Reshape data for pyramid chart
pyramid_data <- summary_data_5yr %>%
  pivot_longer(cols = everything(), 
               names_to = c("Gender", "Age_Group"), 
               names_sep = "_", 
               values_to = "Count") %>%
  mutate(Count = ifelse(Gender == "Male", -Count, Count))  # Negate male counts for left side of the pyramid

# Also do one more thing here to re-label things
pyramid_data_to_plot <- pyramid_data %>%
  mutate(
    Age_Group = recode(Age_Group,
                       Under = "Under 5",  # Keep "Under 5" label intact
                       `5` = "5 to 9",
                       `10` = "10 to 14",
                       `15` = "15 to 19",
                       `20` = "20 to 24",
                       `25` = "25 to 29",
                       `30` = "30 to 34",
                       `35` = "35 to 39",
                       `40` = "40 to 44",
                       `45` = "45 to 49",
                       `50` = "50 to 54",
                       `55` = "55 to 59",
                       `60` = "60 to 64",
                       `65` = "65 to 69",
                       `70` = "70 to 74",
                       `75` = "75 to 79",
                       `80` = "80 to 84",
                       `85` = "85 and Up")  # Explicitly define all age groups
  ) %>%
  mutate(Age_Group = factor(Age_Group, levels = c("Under 5", "5 to 9", "10 to 14", "15 to 19", 
                                                  "20 to 24", "25 to 29", "30 to 34", 
                                                  "35 to 39", "40 to 44", "45 to 49", 
                                                  "50 to 54", "55 to 59", "60 to 64", 
                                                  "65 to 69", "70 to 74", "75 to 79",
                                                  "80 to 84", "85 and Up")))
           

# Now you can create the pyramid chart as before
pyramid_plot <- ggplot(pyramid_data_to_plot, aes(x = Age_Group, y = Count, fill = Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axes
  scale_y_continuous(labels = abs) +  # Make y-axis absolute values
  labs(title = "Population Pyramid of Dane County Census (2020)",
       x = "Age Group",
       y = "Population Count",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),  # Y-axis label size
        axis.text.x = element_text(size = 10))  # X-axis label size

pyramid_plot

ggsave("population_pyramid_dane_county_2020.png", plot = pyramid_plot, width = 10, height = 6, dpi = 300, bg = "white")
