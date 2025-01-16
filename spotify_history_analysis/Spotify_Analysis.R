#===========================================
# Load Required Libraries                 #
#===========================================

library(purrr)
library(tidyverse)    # Contains dplyr and ggplot2 for data manipulation and visualization
library(jsonlite)
library(lubridate)
library(ggplot2)
library(tidytext)
library(randomcoloR)
library(RColorBrewer)
library(viridis)



#===========================================
# LOAD DATA                                #
#===========================================

# Read in the data
dat <- read.csv("spotify_data.csv")

dat <- dat %>%
  filter(
    year >= 2014,                                  # Keep data from 2014 onwards
    (artist != "Pink Noise" | is.na(artist)),      # Exclude "Pink Noise" or keep if artist is NA
    year <= year(Sys.Date()) - 1                   # Only include data up to the previous year
  ) %>%
  filter(ms_played != 0) %>%                       # Remove rows with zero playback time
  arrange(ts) %>%                                  # Arrange by timestamp
  distinct(ts, .keep_all = TRUE)                  # Remove duplicate timestamps
  

dat$ts <- lubridate::ymd_hms(dat$ts, tz = "UTC")

dat$day <- lubridate::yday(dat$ts)

#===========================================
# Visualize some data                      #
#===========================================

#=================================================================================================================== Total hours by year 
# Lets look at total hours by year
hours_year <- dat %>%
                  group_by(year,type) %>%
                  summarise(seconds = sum(seconds),
                            minutes = seconds/60,
                            hours = minutes/60,
                            days = hours/24
                            )

ggplot(hours_year, aes(x=year, y=days, color = type)) +
  geom_bar(stat='identity')
  

# Fancy Bar Chart
ggplot(hours_year, aes(x = year, y = days, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Dodge to separate bars
  scale_fill_brewer(palette = "Set2") +  # Custom color palette
  labs(title = "Total Hours by Year and Type",
       x = "Year",
       y = "Total Days",
       fill = "Activity Type") +  # Labels
  theme_minimal(base_size = 14) +  # Minimal theme with larger font
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


#=================================================================================================================== Total hours by year by artist
# I also want to look at it by artist, album, and song 

hours_artist_year <- dat %>%
  filter(type=="song") %>%
  group_by(year,artist) %>%
  summarise(seconds = sum(seconds),
            minutes = seconds/60,
            hours = minutes/60
  ) %>%
  group_by(year) %>%
  slice_max(order_by = hours, n = 5)

ggplot(hours_year, aes(x=year, y=days, color = type)) +
  geom_bar(stat='identity')



#=================================================================================================================== Top 10/20 Artists All Time

top_50_artists <- dat %>%
  filter(type=="song") %>%
  group_by(artist) %>%
  summarise(seconds = sum(seconds),
            minutes = seconds/60,
            hours = minutes/60,
            days = hours/24
  ) %>%
  slice_max(order_by = hours, n = 50)

# Create the plot with Viridis colors from most to least
ggplot(top_50_artists, aes(x = reorder(artist, hours), y = hours, fill = hours)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +  # Flip coordinates for better readability
  scale_fill_viridis_c(option = "viridis", direction = -1) +  # Use viridis with gradient from most to least
  labs(
    title = "Top 50 Artists by Total Listening Hours",
    x = "Artist",
    y = "Total Listening Hours"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",  # Hide legend if not needed
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

#=================================================================================================================== Top 10/20 Albums

top_50_albums <- dat %>%
  filter(type == "song") %>%
  group_by(album) %>%
  summarise(
    artist = first(artist),
    seconds = sum(seconds),
    minutes = seconds / 60,
    hours = minutes / 60,
    days = hours / 24,
    .groups = "drop"
  ) %>%
  slice_max(order_by = hours, n = 50) %>%
  mutate(label = paste(album, artist, sep = ", "))  # Combine album and artist for labels

# Create the bar chart with Viridis gradient
ggplot(top_50_albums, aes(x = reorder(label, hours), y = hours, fill = hours)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +  # Flip coordinates for horizontal bars
  scale_fill_viridis_c(option = "plasma", direction = -1) +  # Use a vibrant gradient color palette
  labs(
    title = "Top 50 Albums by Total Listening Hours",
    x = "Album, Artist",
    y = "Total Listening Hours"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",  # Hide legend since the color represents hours
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )


#=================================================================================================================== Top 10/20 Songs

top_50_songs <- dat %>%
  filter(type == "song") %>%
  group_by(track_name) %>%
  summarise(
    artist = first(artist),
    seconds = sum(seconds),
    minutes = seconds / 60,
    hours = minutes / 60,
    days = hours / 24,
    .groups = "drop"
  ) %>%
  slice_max(order_by = hours, n = 50) %>%
  mutate(label = paste(track_name, artist, sep = ", "))


# Create the bar chart with Viridis gradient for songs
ggplot(top_50_songs, aes(x = reorder(label, hours), y = hours, fill = hours)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +  # Flip coordinates for horizontal bars
  scale_fill_viridis_c(option = "plasma", direction = -1) +  # Use a vibrant gradient color palette
  labs(
    title = "Top 50 Songs by Total Listening Hours",
    x = "Track, Artist",
    y = "Total Listening Hours"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",  # Hide legend since the color represents hours
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )



#=================================================================================================================== Top 10 artists per year

# set top 10 per year
top_10_artists_by_year <- dat %>%
  filter(type == "song" & year >= 2018) %>%
  group_by(year, artist) %>%
  summarise(
    seconds = sum(seconds),
    minutes = seconds / 60,
    hours = minutes / 60,
    days = hours / 24,
    .groups = "drop" # Ensures clean ungrouped output after summarising
  ) %>%
  group_by(year) %>%
  slice_max(order_by = hours, n = 10) %>%
  ungroup() # Removes grouping for further operations


# Generate unique colors for each artist
artist_colors <- distinctColorPalette(length(unique(top_10_artists_by_year$artist)))


# PLot 
ggplot(top_10_artists_by_year, 
       aes(x = tidytext::reorder_within(artist, hours, year), y = hours, fill = artist)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  scale_fill_manual(values = artist_colors) +
  facet_wrap(~ year, scales = "free_y") +
  tidytext::scale_x_reordered() +
  labs(
    title = "Top 10 Artists by Total Listening Hours (by Year)",
    x = "Artist",
    y = "Total Listening Hours"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold")
  )


#=================================================================================================================== Top 10 albums per year


# Set top 10 albums per year
top_10_albums_by_year <- dat %>%
  filter(type == "song" & year >= 2018) %>%
  group_by(year, album, artist) %>%
  summarise(
    seconds = sum(seconds),
    minutes = seconds / 60,
    hours = minutes / 60,
    days = hours / 24,
    .groups = "drop" # Ensures clean ungrouped output after summarising
  ) %>%
  group_by(year) %>%
  slice_max(order_by = hours, n = 10) %>%
  ungroup() %>%
  mutate(label = paste(album, artist, sep = ", ")) # Add "album, artist" label

# Generate unique colors for each artist
artist_colors <- distinctColorPalette(length(unique(top_10_albums_by_year$artist)))

# Plot the top 10 albums
ggplot(top_10_albums_by_year, 
       aes(x = tidytext::reorder_within(label, hours, year), y = hours, fill = artist)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  scale_fill_manual(values = artist_colors) +
  facet_wrap(~ year, scales = "free_y") +
  tidytext::scale_x_reordered() +
  labs(
    title = "Top 10 Albums by Total Listening Hours (by Year)",
    x = "Album, Artist",
    y = "Total Listening Hours"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold")
  )

#=================================================================================================================== Top 10 songs per year

# Set top 10 songs per year
top_10_songs_by_year <- dat %>%
  filter(type == "song" & year >= 2018) %>%
  group_by(year, track_name, artist) %>%
  summarise(
    seconds = sum(seconds),
    minutes = seconds / 60,
    hours = minutes / 60,
    days = hours / 24,
    .groups = "drop" # Ensures clean ungrouped output after summarising
  ) %>%
  group_by(year) %>%
  slice_max(order_by = hours, n = 10) %>%
  ungroup() %>%
  mutate(label = paste(track_name, artist, sep = ", ")) # Add "track_name, artist" label

# Generate unique colors for each artist
artist_colors <- distinctColorPalette(length(unique(top_10_songs_by_year$artist)))

# Plot the top 10 songs
ggplot(top_10_songs_by_year, 
       aes(x = tidytext::reorder_within(label, hours, year), y = hours, fill = artist)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  scale_fill_manual(values = artist_colors) +
  facet_wrap(~ year, scales = "free_y") +
  tidytext::scale_x_reordered() +
  labs(
    title = "Top 10 Songs by Total Listening Hours (by Year)",
    x = "Song, Artist",
    y = "Total Listening Hours"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold")
  )



#=================================================================================================================== Distinct Artists Listened by year


# Count the number of distinct artists by year
distinct_artists_by_year <- dat %>%
  filter(type == "song" & year >= 2018) %>%
  group_by(year) %>%
  summarise(distinct_artists = n_distinct(artist), .groups = "drop")

# Plot the distinct count of artists by year
ggplot(distinct_artists_by_year, aes(x = year, y = distinct_artists)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +  # Bar chart with blue color
  labs(
    title = "Distinct Count of Artists by Year",
    x = "Year",
    y = "Distinct Artists"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Angle x-axis labels for readability
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )


#=================================================================================================================== Distinct Songs Listened by year


# Count the number of distinct artists by year
distinct_songs_by_year <- dat %>%
  filter(type == "song" & year >= 2018) %>%
  group_by(year) %>%
  summarise(distinct_songs = n_distinct(track_name), .groups = "drop")

# Plot the distinct count of artists by year
ggplot(distinct_songs_by_year, aes(x = year, y = distinct_songs)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +  # Bar chart with blue color
  labs(
    title = "Distinct Count of Songs by Year",
    x = "Year",
    y = "Distinct Songs"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Angle x-axis labels for readability
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )


#=================================================================================================================== Total Hours per Month by Year

# calculate hours per month per year 
total_hours_month_year <- dat %>%
  filter(type == "song" & year >= 2018) %>%
  group_by(year, month) %>%
  summarise(
    seconds = sum(seconds),
    minutes = seconds / 60,
    hours = minutes / 60
  ) %>%
  ungroup() # Removes grouping for further operations


# Plot the total hours per month, color-coded by year with similar colors
ggplot(total_hours_month_year, aes(x = month, y = hours, color = as.factor(year), group = year)) +
  geom_line(linewidth = 1.2) +  # Use 'linewidth' instead of 'size'
  labs(
    title = "Total Hours by Month, Color-coded by Year",
    x = "Month",
    y = "Total Hours",
    color = "Year"
  ) +
  scale_color_viridis(discrete = TRUE) +  # Use a similar, distinct color palette from viridis
  scale_x_continuous(breaks = 1:12, labels = month.name) +  # Set months as labels (Jan-Dec)
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Angle x-axis labels for readability
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )


#=================================================================================================================== Total Hours per Week by Year

# Calculate hours per week per year
total_hours_week_year <- dat %>%
  filter(type == "song" & year >= 2018) %>%
  group_by(year, week) %>%
  summarise(
    seconds = sum(seconds),
    minutes = seconds / 60,
    hours = minutes / 60,
    .groups = "drop"  # Removes grouping for further operations
  )

# Plot the total hours per week, with x-axis showing week numbers every 4 weeks
ggplot(total_hours_week_year, aes(x = week, y = hours, color = as.factor(year), group = year)) +
  geom_line(linewidth = 1.2) +  # Use 'linewidth' instead of 'size'
  labs(
    title = "Total Hours by Week, Show Week Number Every 4 Weeks",
    x = "Week of the Year",
    y = "Total Hours",
    color = "Year"
  ) +
  scale_color_viridis(discrete = TRUE) +  # Use a similar, distinct color palette from viridis
  scale_x_continuous(
    breaks = seq(1, 52, by = 3),  # Show week numbers every 4 weeks
    labels = seq(1, 52, by = 3)   # Show labels as the actual week numbers
  ) +  # Label x-axis with week numbers every 4 weeks
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Angle x-axis labels for readability
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  facet_wrap(~ year, scales = "free_y")  # Creates a separate plot for each year, free y-axis scales



#=================================================================================================================== Total Hours per Day by Year


# Calculate hours per day per year
total_hours_day_year <- dat %>%
  filter(type == "song" & year >= 2018) %>%
  group_by(year, day) %>%
  summarise(
    seconds = sum(seconds),
    minutes = seconds / 60,
    hours = minutes / 60,
    .groups = "drop"  # Removes grouping for further operations
  )

# Plot the total hours per day, with x-axis showing day numbers every 3 days
ggplot(total_hours_day_year, aes(x = day, y = hours, color = as.factor(year), group = year)) +
  geom_line(linewidth = 1.2) +  # Use 'linewidth' instead of 'size'
  labs(
    title = "Total Hours by Day, Show Day Number Every 3 Days",
    x = "Day of the Year",
    y = "Total Hours",
    color = "Year"
  ) +
  scale_color_viridis(discrete = TRUE) +  # Use a similar, distinct color palette from viridis
  scale_x_continuous(
    breaks = seq(1, 365, by = 21),  # Show day numbers every 3 days (adjust for non-leap years)
    labels = seq(1, 365, by = 21)    # Show labels as the actual day numbers
  ) +  # Label x-axis with day numbers every 3 days
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Angle x-axis labels for readability
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  facet_wrap(~ year, scales = "free_y")  # Creates a separate plot for each year, free y-axis scales



#=================================================================================================================== Total Hours per Day by Year


# Calculate hours per day per year
total_hours_day_year_2022 <- dat %>%
  filter(type == "song" & year == 2022) %>%
  group_by(year, day) %>%
  summarise(
    seconds = sum(seconds),
    minutes = seconds / 60,
    hours = minutes / 60,
    .groups = "drop"  # Removes grouping for further operations
  )

# Plot the total hours per day, with x-axis showing day numbers every 3 days
ggplot(total_hours_day_year_2022, aes(x = day, y = hours, color = as.factor(year), group = year)) +
  geom_line(linewidth = 1.2) +  # Use 'linewidth' instead of 'size'
  labs(
    title = "Total Hours by Day, Show Day Number Every 3 Days",
    x = "Day of the Year",
    y = "Total Hours",
    color = "Year"
  ) +
  scale_color_viridis(discrete = TRUE) +  # Use a similar, distinct color palette from viridis
  scale_x_continuous(
    breaks = seq(1, 365, by = 21),  # Show day numbers every 3 days (adjust for non-leap years)
    labels = seq(1, 365, by = 21)    # Show labels as the actual day numbers
  ) +  # Label x-axis with day numbers every 3 days
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Angle x-axis labels for readability
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  facet_wrap(~ year, scales = "free_y")  # Creates a separate plot for each year, free y-axis scales


#=================================================================================================================== Total Hours per Month by top 5 artist by year


# Step 1: Identify the top 5 artists by year
top_5_artists_per_year <- dat %>%
  filter(type == "song" & year >= 2018) %>%
  group_by(year, artist) %>%
  summarise(
    total_played_seconds = sum(seconds),
    total_played_hours = total_played_seconds / 3600,
    .groups = "drop"
  ) %>%
  arrange(year, desc(total_played_hours)) %>%
  group_by(year) %>%
  slice_max(order_by = total_played_hours, n = 5)

# Step 2: Filter the original data for only the top 5 artists by year
top_artists_data <- dat %>%
  filter(type == "song" & year >= 2018) %>%
  semi_join(top_5_artists_per_year, by = c("year", "artist"))

# Step 3: Calculate total hours by month for the top 5 artists
monthly_hours_top_artists <- top_artists_data %>%
  group_by(year, month, artist) %>%
  summarise(
    total_monthly_seconds = sum(seconds),
    total_monthly_hours = total_monthly_seconds / 3600,
    .groups = "drop"
  )

# Filter data for the year 2023
data_2023 <- monthly_hours_top_artists %>%
  filter(year == 2023)

# Create the plot for 2023 with each artist having a distinct color
ggplot(data_2023, aes(x = month, y = total_monthly_hours, color = artist, group = artist)) +
  geom_line(linewidth = 1.2) +  # Line width for better visibility
  labs(
    title = "Total Monthly Listening Hours for Top Artists in 2023",
    x = "Month of the Year",
    y = "Total Monthly Hours",
    color = "Artist"
  ) +
  scale_color_viridis_d(option = "plasma") +  # Use viridis color scale for distinct colors
  scale_x_continuous(
    breaks = 1:12,  # Show month numbers (1 to 12)
    labels = month.name  # Use month names as x-axis labels
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Angle x-axis labels for better readability
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom"  # Place the legend at the bottom for better layout
  )



# Filter data for the year 2024
data_2024 <- monthly_hours_top_artists %>%
  filter(year == 2024)

# Create the plot for 2023 with each artist having a distinct color
ggplot(data_2024, aes(x = month, y = total_monthly_hours, color = artist, group = artist)) +
  geom_line(linewidth = 1.2) +  # Line width for better visibility
  labs(
    title = "Total Monthly Listening Hours for Top Artists in 2024",
    x = "Month of the Year",
    y = "Total Monthly Hours",
    color = "Artist"
  ) +
  scale_color_viridis_d(option = "plasma") +  # Use viridis color scale for distinct colors
  scale_x_continuous(
    breaks = 1:12,  # Show month numbers (1 to 12)
    labels = month.name  # Use month names as x-axis labels
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Angle x-axis labels for better readability
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom"  # Place the legend at the bottom for better layout
  )

#=================================================================================================================== Total Hours per Week by top 5 artist by year


# Step 1: Identify the top 5 artists by year
top_5_artists_per_year <- dat %>%
  filter(type == "song" & year >= 2018) %>%
  group_by(year, artist) %>%
  summarise(
    total_played_seconds = sum(seconds),
    total_played_hours = total_played_seconds / 3600,
    .groups = "drop"
  ) %>%
  arrange(year, desc(total_played_hours)) %>%
  group_by(year) %>%
  slice_max(order_by = total_played_hours, n = 5)

# Step 2: Filter the original data for only the top 5 artists by year
top_artists_data <- dat %>%
  filter(type == "song" & year >= 2018) %>%
  semi_join(top_5_artists_per_year, by = c("year", "artist"))

# Step 3: Calculate total hours by week for the top 5 artists
weekly_hours_top_artists <- top_artists_data %>%
  group_by(year, week, artist) %>%
  summarise(
    total_weekly_seconds = sum(seconds),
    total_weekly_hours = total_weekly_seconds / 3600,
    .groups = "drop"
  )


# Filter data for the year 2023
data_2023 <- weekly_hours_top_artists %>%
  filter(year == 2023)

# Create the plot for 2023 with each artist having a distinct color
ggplot(data_2023, aes(x = week, y = total_weekly_hours, color = artist, group = artist)) +
  geom_line(linewidth = 1.2) +  # Line width for better visibility
  labs(
    title = "Total Weekly Listening Hours for Top Artists in 2023",
    x = "Week of the Year",
    y = "Total Weekly Hours",
    color = "Artist"
  ) +
  scale_color_viridis_d(option = "plasma") +  # Use viridis color scale for distinct colors
  scale_x_continuous(
    breaks = seq(1, 52, by = 4),  # Show week numbers every 4 weeks
    labels = seq(1, 52, by = 4)   # Label the x-axis accordingly
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Angle x-axis labels for better readability
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom"  # Place the legend at the bottom for better layout
  )


#=================================================================================================================== Most listened artist per month per year


# Step 1: Identify the top 5 artists by month per year
top_1_artists_per_month_year <- dat %>%
  filter(type == "song" & year >= 2018) %>%
  group_by(year, month, artist) %>%
  summarise(
    total_played_seconds = sum(seconds),
    total_played_hours = total_played_seconds / 3600,
    .groups = "drop"
  ) %>%
  arrange(year, month, desc(total_played_hours)) %>%  # Sort by year, month, and hours
  group_by(year, month) %>%  # Group by year and month
  slice_max(order_by = total_played_hours, n = 1)  # Get top 5 artists for each month


# Step 1: Identify the most-played artist per month and year
top_1_artists_per_month_year <- dat %>%
  filter(type == "song" & year >= 2018) %>%
  group_by(year, month, artist) %>%
  summarise(
    total_played_seconds = sum(seconds),
    total_played_hours = total_played_seconds / 3600,
    .groups = "drop"
  ) %>%
  arrange(year, month, desc(total_played_hours)) %>%  # Sort by year, month, and hours
  group_by(year, month) %>%  # Group by year and month
  slice_max(order_by = total_played_hours, n = 1)  # Get the most-played artist for each month



# Step 2: Plot the most-played artist per month and year
ggplot(top_1_artists_per_month_year, aes(x = factor(month), y = total_played_hours, fill = artist)) +
  geom_bar(stat = "identity", color = "black") +  # Bar chart with the most-played artist each month
  geom_text(aes(label = artist), size = 4, angle = 90, hjust = 0, vjust = 0.5, position = position_stack(vjust = 0)) +  # Rotate artist names 90 degrees to be vertical
  labs(
    title = "Most Played Artist per Month (2018 and Later)",
    x = "Month",
    y = "Total Listening Hours",
    fill = "Artist"
  ) +
  scale_x_discrete(
    breaks = 1:12,  # Show all months
    labels = month.name  # Label months by name
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"  # Remove legend for simplicity
  ) +
  facet_wrap(~ year, scales = "free_y")  # Separate plots for each year



