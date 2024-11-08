#==============================================================================
#                          MTG Power Creep Analysis                           #
#==============================================================================

#===========================================
# Load Required Libraries                 #
#===========================================

library(tidyverse)    # Contains dplyr and ggplot2 for data manipulation and visualization
library(ggplot2)

#===========================================
# LOAD DATA                                #
#===========================================

# First read in the card data
card_dat <- read.csv("../data/all_mtg_cards.csv", header = T)

# Next, load in the MTG set year
set_year <- read.csv("../data/Final_MTG_Set_Years.csv", header = T)


#===========================================
# Data processing                          #
#===========================================

# 1) Join year data to card data
# 2) Filter to just creatures, also remove un-sets, and only select normal cards
# 3) Select min year for unique card
# 4) select distinct rows / names

# Going to comment this all out below to reduce the amount of data stored in the public version of this repo
card_year_joined_dat <- card_dat %>%
                          dplyr::left_join(set_year, by = c("set" = "set_name")) %>%                    ## Join set year
                          filter(grepl('Creature',type)
                                 & !set_name %in% c ("Unglued","Unhinged","Unstable","Unfinity","Unsanctioned")) %>%    ## Filter to just creature types, also drop the un-sets
                          filter(!set %in% c ("UPLIST","CMB1","CMB2")) %>%                                              ## Also lets remove mystery booster test cards
                          filter(!grepl("^A-",name)) %>%                                                                ## Remove alchemy cards
                          filter(layout == "normal") %>%                                                                ## Filter just to normal cards, harder to compare non-normal across time
                          select(c(name,layout,cmc,power,toughness,release_year,color_identity,text)) %>%
                          group_by(name) %>%
                          filter(release_year == min(release_year)) %>%
                          ungroup() %>%
                          mutate(
                            text_num = nchar(text) ## Small way to try to capture complexity over time also, just counting characters lol, not very sophistocated
                          ) %>%
                          distinct() %>%
                          filter(!grepl("[*?]",power) &
                                   !grepl("[*?]",toughness)) %>% ## Remove if power or toughness is * or ?
                          mutate(
                            power = as.numeric(power),
                            toughness = as.numeric(toughness)
                          ) %>%
                          filter(release_year <= "2022") # looks like this data is really only valid through end of 2022

write.csv(card_year_joined_dat,"../data/mtg_card_year.csv")

#===========================================
# Build out the average data per years     #
#===========================================

mtg_analysis_year_dat <- card_year_joined_dat %>%
  group_by(release_year) %>%
  summarise(
    num_new_cards = n(),  # Count the number of new cards for the year
    avg_power = mean(power, na.rm = TRUE),  # Average power of cards
    avg_toughness = mean(toughness, na.rm = TRUE),  # Average toughness of cards
    avg_cmc = mean(cmc, na.rm = TRUE),  # Average converted mana cost (CMC)
    avg_text_len = mean(text_num, na.rm = TRUE),  # Average length of the card text
    p_cmc = sum(power, na.rm = TRUE) / sum(cmc, na.rm = TRUE),
    t_cmc = sum(toughness, na.rm = TRUE) / sum(cmc, na.rm = TRUE),
    pt_cmc = (sum(power, na.rm = TRUE) + sum(toughness, na.rm = TRUE)) / sum(cmc, na.rm = TRUE),  # Power/Toughness per CMC
    pt_text = (sum(power, na.rm = TRUE) + sum(toughness, na.rm = TRUE)) / sum(text_num, na.rm = TRUE)  # Power/Toughness per text length
  )


# Plot of Power and Toughness avg per card over release years                       
ggplot(data = mtg_analysis_year_dat, aes(x = release_year)) +
  geom_line(aes(y = avg_power, color = "Average Power"), linewidth = 1) +  # Line for average power
  geom_line(aes(y = avg_toughness, color = "Average Toughness"), linewidth = 1) +  # Line for average toughness
  geom_point(aes(y = avg_power, color = "Average Power"), size = 2) +  # Points for average power
  geom_point(aes(y = avg_toughness, color = "Average Toughness"), size = 2) +  # Points for average toughness
  labs(
    title = "Average Power and Toughness of MTG Cards Over Release Years",
    x = "Release Year",
    y = "Average Value",
    color = "Metric"
  ) +
  theme_minimal() +  # A clean theme for the plot
  theme(
    legend.position = "top",  # Position the legend at the top
    legend.title = element_blank()  # Remove the title of the legend
  )  

# Plot of Power and Toughness avg per card over release years 
ggplot(data = mtg_analysis_year_dat, aes(x = release_year)) +
  geom_line(aes(y = p_cmc, color = "CMC Weighted Average Power"), linewidth = 1) +  # Line for average power
  geom_line(aes(y = t_cmc, color = "CMC Weighted Average Toughness"), linewidth = 1) +  # Line for average toughness
  geom_point(aes(y = p_cmc, color = "CMC Weighted Average Power"), size = 2) +  # Points for average power
  geom_point(aes(y = t_cmc, color = "CMC Weighted Average Toughness"), size = 2) +  # Points for average toughness

  labs(
    title = "Average CMC Weighted Power and Toughness of MTG Cards Over Release Years",
    x = "Release Year",
    y = "Average Value",
    color = "Metric"
  ) +
  theme_minimal() +  # A clean theme for the plot
  theme(
    legend.position = "top",  # Position the legend at the top
    legend.title = element_blank()  # Remove the title of the legend
  ) 

# Combo of power and toughness over CMC
ggplot(data = mtg_analysis_year_dat, aes(x = release_year)) +
  geom_line(aes(y = pt_cmc, color = "Power + Toughness / CMC"), linewidth = 1) +  # Line for p+t/cmc
  geom_point(aes(y = pt_cmc, color = "Power + Toughness / CMC"), size = 2) +  # Points for p+t/cmc
  labs(
    title = "Power Creep (Power + Toughness / CMC) of MTG Cards Over Release Years",
    x = "Release Year",
    y = "Average Value",
    color = "Metric"
  ) +
  theme_minimal() +  # A clean theme for the plot
  theme(
    legend.position = "top",  # Position the legend at the top
    legend.title = element_blank()  # Remove the title of the legend
  ) 


# Card complexity 
ggplot(data = mtg_analysis_year_dat, aes(x = release_year)) +
  geom_line(aes(y = avg_text_len, color = "Average # of Characters Per Card"), linewidth = 1) +  # Line for p+t/cmc
  geom_point(aes(y = avg_text_len, color = "Average # of Characters Per Card"), size = 2) +  # Points for p+t/cmc
  labs(
    title = "Average Card Text (# Characters) of MTG Cards Over Release Years",
    x = "Release Year",
    y = "Average Value",
    color = "Metric"
  ) +
  theme_minimal() +  # A clean theme for the plot
  theme(
    legend.position = "top",  # Position the legend at the top
    legend.title = element_blank()  # Remove the title of the legend
  ) 



#===========================================
# Do the same visuals for colors     #
#===========================================

# Okay, lets generate some color identity 
# For this one, lets only focus on 2010 to now and just focus on the 5 main colors
card_year_joined_dat <- card_year_joined_dat %>%
  mutate(color_identity_category = case_when(
                                      color_identity == "" ~ "Colorless",
                                      color_identity == "['W']" ~ "White",
                                      color_identity == "['U']" ~ "Blue",
                                      color_identity == "['B']" ~ "Black",
                                      color_identity == "['R']" ~ "Red",
                                      color_identity == "['G']" ~ "Green",
                                      str_count(color_identity, ',') == 1 ~ "Multi Color",
                                      str_count(color_identity, ',') == 2 ~ "Multi Color",   
                                      str_count(color_identity, ',') == 3 ~ "Multi Color",   
                                      str_count(color_identity, ',') == 4 ~ "Multi Color"   
              )
           ) %>%
  filter(color_identity_category != "Colorless" & color_identity_category != "Multi Color") %>%
  filter(release_year >= "2010")


mtg_analysis_year_color_dat <- card_year_joined_dat %>%
  group_by(release_year, color_identity_category) %>%
  summarise(
    num_new_cards = n(),  # Count the number of new cards for the year
    avg_power = mean(power, na.rm = TRUE),  # Average power of cards
    avg_toughness = mean(toughness, na.rm = TRUE),  # Average toughness of cards
    avg_pt = sum(power, na.rm = TRUE) / sum(toughness, na.rm = TRUE),
    avg_cmc = mean(cmc, na.rm = TRUE),  # Average converted mana cost (CMC)
    avg_text_len = mean(text_num, na.rm = TRUE),  # Average length of the card text
    p_cmc = sum(power, na.rm = TRUE) / sum(cmc, na.rm = TRUE),
    t_cmc = sum(toughness, na.rm = TRUE) / sum(cmc, na.rm = TRUE),
    pt_cmc = (sum(power, na.rm = TRUE) + sum(toughness, na.rm = TRUE)) / sum(cmc, na.rm = TRUE),  # Power/Toughness per CMC
    pt_text = (sum(power, na.rm = TRUE) + sum(toughness, na.rm = TRUE)) / sum(text_num, na.rm = TRUE)  # Power/Toughness per text length
  )


  
##################### Now lets do some charts
# Power
plot_power <- ggplot(mtg_analysis_year_color_dat, aes(x = release_year, y = avg_power, color = color_identity_category)) +
  geom_line(size = 1.2) +                            # Thicker line for clarity
  geom_point(size = 3) +                             # Slightly larger points for emphasis
  labs(
    title = "Average Power per Card by Color Identity Category (2010 to 2022)",
    x = "Release Year",
    y = "Average Power",
    color = "Color Identity"
  ) +
  scale_color_manual(values = c("White" = "#E9E9E0", "Blue" = "#3498DB", "Black" = "#2C3E50", 
                                "Red" = "#E74C3C", "Green" = "#27AE60")) + # Custom colors for identity
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Center and bold title
    axis.title = element_text(size = 12),                            # Larger axis titles
    axis.text.x = element_text(angle = 45, hjust = 1),               # Rotate x-axis labels
    legend.position = "bottom",                                      # Position legend at bottom
    legend.title = element_text(face = "bold")                       # Bold legend title
  ) +
  scale_x_continuous(breaks = seq(2010, max(mtg_analysis_year_color_dat$release_year), by = 1)) # Yearly x-axis ticks

ggsave("../graphs/mtg_powercreep_power_analysis.jpeg", plot = plot_power, width = 10, height = 6, dpi = 300)


# Toughness
plot_toughness <- ggplot(mtg_analysis_year_color_dat, aes(x = release_year, y = avg_toughness, color = color_identity_category)) +
  geom_line(size = 1.2) +                            # Thicker line for clarity
  geom_point(size = 3) +                             # Slightly larger points for emphasis
  labs(
    title = "Average Toughness per Card by Color Identity Category (2010 to 2022)",
    x = "Release Year",
    y = "Average Toughness",
    color = "Color Identity"
  ) +
  scale_color_manual(values = c("White" = "#E9E9E0", "Blue" = "#3498DB", "Black" = "#2C3E50", 
                                "Red" = "#E74C3C", "Green" = "#27AE60")) + # Custom colors for identity
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Center and bold title
    axis.title = element_text(size = 12),                            # Larger axis titles
    axis.text.x = element_text(angle = 45, hjust = 1),               # Rotate x-axis labels
    legend.position = "bottom",                                      # Position legend at bottom
    legend.title = element_text(face = "bold")                       # Bold legend title
  ) +
  scale_x_continuous(breaks = seq(2010, max(mtg_analysis_year_color_dat$release_year), by = 1)) # Yearly x-axis ticks

ggsave("../graphs/mtg_powercreep_toughness_analysis.jpeg", plot = plot_toughness, width = 10, height = 6, dpi = 300)


# Power / Toughness
plot_power_and_toughness <- ggplot(mtg_analysis_year_color_dat, aes(x = release_year, y = avg_pt, color = color_identity_category)) +
  geom_line(size = 1.2) +                            # Thicker line for clarity
  geom_point(size = 3) +                             # Slightly larger points for emphasis
  labs(
    title = "Overall (Power:Toughness) Ratio by Color Identity Category (2010 to 2022)",
    x = "Release Year",
    y = "Average Toughness",
    color = "Color Identity"
  ) +
  scale_color_manual(values = c("White" = "#E9E9E0", "Blue" = "#3498DB", "Black" = "#2C3E50", 
                                "Red" = "#E74C3C", "Green" = "#27AE60")) + # Custom colors for identity
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Center and bold title
    axis.title = element_text(size = 12),                            # Larger axis titles
    axis.text.x = element_text(angle = 45, hjust = 1),               # Rotate x-axis labels
    legend.position = "bottom",                                      # Position legend at bottom
    legend.title = element_text(face = "bold")                       # Bold legend title
  ) +
  scale_x_continuous(breaks = seq(2010, max(mtg_analysis_year_color_dat$release_year), by = 1)) # Yearly x-axis ticks

ggsave("../graphs/mtg_powercreep_power_and_toughness_analysis.jpeg", plot = plot_power_and_toughness, width = 10, height = 6, dpi = 300)



# CMC Weighted Power / Toughness
cmc_weighted_plot_power_and_toughness <- ggplot(mtg_analysis_year_color_dat, aes(x = release_year, y = pt_cmc, color = color_identity_category)) +
  geom_line(size = 1.2) +                            # Thicker line for clarity
  geom_point(size = 3) +                             # Slightly larger points for emphasis
  labs(
    title = "CMC Weighted Power + Toughness by Color Identity Category (2010 to 2022)",
    x = "Release Year",
    y = "Average Toughness",
    color = "Color Identity"
  ) +
  scale_color_manual(values = c("White" = "#E9E9E0", "Blue" = "#3498DB", "Black" = "#2C3E50", 
                                "Red" = "#E74C3C", "Green" = "#27AE60")) + # Custom colors for identity
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Center and bold title
    axis.title = element_text(size = 12),                            # Larger axis titles
    axis.text.x = element_text(angle = 45, hjust = 1),               # Rotate x-axis labels
    legend.position = "bottom",                                      # Position legend at bottom
    legend.title = element_text(face = "bold")                       # Bold legend title
  ) +
  scale_x_continuous(breaks = seq(2010, max(mtg_analysis_year_color_dat$release_year), by = 1)) # Yearly x-axis ticks

ggsave("../graphs/mtg_powercreep_cmc_weighted_power_and_toughness_analysis.jpeg", plot = cmc_weighted_plot_power_and_toughness, width = 10, height = 6, dpi = 300)



