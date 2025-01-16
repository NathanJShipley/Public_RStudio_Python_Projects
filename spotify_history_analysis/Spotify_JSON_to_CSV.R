#===========================================
# Load Required Libraries                 #
#===========================================

library(purrr)
library(tidyverse)    # Contains dplyr and ggplot2 for data manipulation and visualization
library(jsonlite)
library(lubridate)

#===========================================
# LOAD DATA                                #
#===========================================

# Set up path and grab the file names 
json_path <- "JSON/"
files <- dir(json_path, pattern = "*.json")


# Now read in the files
dat <- files %>%
  map_df(~fromJSON(file.path(json_path, .), flatten = TRUE))

dat <- dat %>% 
          select(c("ts","ms_played","master_metadata_track_name","master_metadata_album_artist_name","master_metadata_album_album_name","reason_start","reason_end","shuffle","skipped","episode_show_name")) %>%
          rename("track_name" = "master_metadata_track_name",
                 "artist" = "master_metadata_album_artist_name",
                 "album" = "master_metadata_album_album_name",
                 "show/podcast/book" = "episode_show_name")

dat <- dat %>%
  filter(!if_all(c("track_name", "show/podcast/book"), is.na))

dat$type <- ifelse(is.na(dat$track_name), "show/podcast/book", "song")

dat$year <- lubridate::year(dat$ts)
dat$month <- lubridate::month(dat$ts)
dat$month_year <- lubridate::floor_date(as.Date(dat$ts), "month")
dat$week <- lubridate::week(dat$ts)

dat$seconds <- dat$ms_played / 1000
dat$mins <- dat$seconds / 60


# Okay, I think I have most of the data that I want
write.csv(dat,"spotify_data.csv")

