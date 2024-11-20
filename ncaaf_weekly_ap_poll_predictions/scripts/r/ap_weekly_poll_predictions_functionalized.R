#============================================================================== =
#                              AP Vote Prediction Analysis                   #      
#============================================================================== =

#=========================================== =
# Initialize environment                 #
#=========================================== =

# Define a function to load libraries, set a random seed, and clear memory
initialize_environment <- function(api_key = NULL, seed = 123) {
  # Load required packages
  library(tidyverse)    # Contains dplyr and ggplot2 for data manipulation and visualization
  library(cfbfastR)     # All good tools to interact with NCAA Football data
  library(jtools)       # My personal favorite package for working with linear models 
  library(earth)        # Package for working with multivariate adaptive regression splines (MARS)
  library(mgcv)         # Used for generalized additive models (GAM)
  library(psych)        # Correlation visualizations using pairs.panels
  library(FactoMineR)   # PCA
  library(caret)        # A bunch of stuff
  library(ranger)       # Random forest models 
  library(tictoc)       # Nice little package for timing models
  library(recipes)
  library(gt)           # Needed to create the final output chart
  library(webshot)      # Needed to create the final output chart
  
  # Clear up memory storage
  gc()
  
  # Set random seed for reproducibility
  set.seed(seed)
  
  # Set API key for CFB API if provided
  if (!is.null(api_key)) {
    Sys.setenv(CFBD_API_KEY = api_key)
  }
}

# Call the initalizaiton function with api key 
initialize_environment(api_key = "youkeyhere")

#=========================================== =
# Functions for Loading Data   #
#=========================================== =
###############################################################################

# Load play by play data! 
load_data <- function(start_year = 2014, end_year = cfbfastR:::most_recent_cfb_season()) {
  pbp <- cfbfastR::load_cfb_pbp(start_year:end_year)
  return(pbp)
}

# Lets get season data for rankings
# Fetch and combine rankings for all seasons
# Narrow data down to just the AP poll top 25 votes
# Set variable names to match

load_rankings <- function(seasons = 2014:cfbfastR:::most_recent_cfb_season()) {
  combined_rankings <- map_df(seasons, cfbfastR::cfbd_rankings)
  
  AP_Poll_Votes <- combined_rankings %>%
    filter(poll == 'AP Top 25') %>%
    select(season, week, school, points) %>%
    rename(pos_team = school, year = season)
  
  return(AP_Poll_Votes)
}

#=========================================== =
# Functions for data processing and feature extraction   #
#=========================================== =

# Not just some, actually doing ALOT of feature extraction 
# Going to set up a fairly large set of data for both teams when on offense and when on defense and join together

# 1.) Offense data features
# 2.) Defense data features, then join to offense
# 3.) Do a cross join for weeks, ensuring each team is represented each week in the data
# 4.) Calculate a ton of cumulative features
# 5.) Do the same, but for the opponent to then WEIGH different scores by opponent values
# 6.) Cumulative sums for the weighted scores
# 7.) Joining in the AP vote data
# 8.) Lastly weigh and sum up cumulative weighted scores by opponent AP

### NOTE FOR THIS ANALYSIS
# Playoff games that happen in the year seem to happen in week 1, so need to filter to the first game for each week

# 1.) Offense Data Functions 
extract_offense_features <- function(pbp) {
  
  offense_data <- pbp %>%
    group_by( across ( c( 'pos_team', 'week', 'year' ) ) ) %>%
    filter(game_id==min(game_id)) %>% 
    summarise(
      opponent = first(def_pos_team[game_play_number == max(game_play_number)]),
      conference = max(offense_conference[offense_play == pos_team]),
      
      # Total scores
      points_scored = first(pos_team_score[game_play_number == max(game_play_number)]),
      points_allowed = first(def_pos_team_score[game_play_number == max(game_play_number)]),
      point_dif = points_scored - points_allowed,
      win_flag = ifelse(points_scored > points_allowed, 1, 0),
      loss_flag = ifelse(points_scored > points_allowed, 0, 1),
      
      ## Offense
      # Yards
      total_passing_yards = sum(yards_gained[play_type == 'Pass Reception' | play_type == 'Passing Touchdown'], na.rm = TRUE),           
      total_rushing_yards = sum(yards_gained[play_type == 'Rush' | play_type ==  'Rushing Touchdown'], na.rm = TRUE), 
      total_yards = total_passing_yards + total_rushing_yards,
      
      # Pass Attempts + Touchdowns
      # For pass completes and incompleteness, looks like some have no, so need to set as 0 with some more logic
      total_pass_completes = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown'), 1, 0), na.rm = TRUE),
      total_pass_inccompletes = sum(ifelse(play_type %in% c('Pass Incompletion', 'Interception Return', 'Interception Return Touchdown'), 1, 0), na.rm = TRUE),
      pass_attempts = total_pass_completes + total_pass_inccompletes,
      pass_comp_attempts = total_pass_completes / pass_attempts,
      avg_passing_yards_per_attempt = total_passing_yards / pass_attempts,
      total_passing_touchdowns = sum(ifelse(play_type == 'Passing Touchdown', 1, 0), na.rm = TRUE),
      
      # Rushing + Touchdowns
      rush_attempts = sum(ifelse(play_type %in% c('Rush', 'Rushing Touchdown'), 1, 0), na.rm = TRUE),
      avg_rush_yards_per_attempt = total_rushing_yards / rush_attempts,
      total_rushing_touchdowns = sum(ifelse(play_type == 'Rushing Touchdown', 1, 0), na.rm = TRUE),
      
      # Misc Totals 
      total_plays = pass_attempts + rush_attempts,
      avg_yards_per_play = total_yards / total_plays,
      
      # Plays over Yards
      total_plays_over_5_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown', 'Rush', 'Rushing Touchdown') & yards_gained >= 5, 1, 0), na.rm = TRUE),
      total_passing_plays_over_5_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown') & yards_gained >= 5, 1, 0), na.rm = TRUE),
      total_rushing_plays_over_5_yards = sum(ifelse(play_type %in% c('Rush', 'Rushing Touchdown') & yards_gained >= 5, 1, 0), na.rm = TRUE),
      total_plays_over_10_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown', 'Rush', 'Rushing Touchdown') & yards_gained >= 10, 1, 0), na.rm = TRUE),
      total_passing_plays_over_10_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown') & yards_gained >= 10, 1, 0), na.rm = TRUE),
      total_rushing_plays_over_10_yards = sum(ifelse(play_type %in% c('Rush', 'Rushing Touchdown') & yards_gained >= 10, 1, 0), na.rm = TRUE),
      total_plays_over_15_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown', 'Rush', 'Rushing Touchdown') & yards_gained >= 15, 1, 0), na.rm = TRUE),
      total_passing_plays_over_15_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown') & yards_gained >= 15, 1, 0), na.rm = TRUE),
      total_rushing_plays_over_15_yards = sum(ifelse(play_type %in% c('Rush', 'Rushing Touchdown') & yards_gained >= 15, 1, 0), na.rm = TRUE),
      total_plays_over_20_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown', 'Rush', 'Rushing Touchdown') & yards_gained >= 20, 1, 0), na.rm = TRUE),
      total_passing_plays_over_20_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown') & yards_gained >= 20, 1, 0), na.rm = TRUE),
      total_rushing_plays_over_20_yards = sum(ifelse(play_type %in% c('Rush', 'Rushing Touchdown') & yards_gained >= 20, 1, 0), na.rm = TRUE),
      total_plays_over_25_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown', 'Rush', 'Rushing Touchdown') & yards_gained >= 25, 1, 0), na.rm = TRUE),
      total_passing_plays_over_25_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown') & yards_gained >= 25, 1, 0), na.rm = TRUE),
      total_rushing_plays_over_25_yards = sum(ifelse(play_type %in% c('Rush', 'Rushing Touchdown') & yards_gained >= 25, 1, 0), na.rm = TRUE),
      
      # Possessions
      total_possessions = sum( drive_numbers == 1 ),
      total_time_possession_mins = ( ( sum ( drive_time_minutes_elapsed[drive_numbers == 1] ) * 60) + sum ( drive_time_seconds_elapsed[drive_numbers == 1] ) ) / 60 ,
      avg_time_possession = total_time_possession_mins / total_possessions,
      avg_time_per_play = total_time_possession_mins / total_plays,
      
      
      # Down efficicney
      total_first_downs = sum(ifelse(down == 1 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      total_first_down_converted = sum(ifelse(down == 1 & success == 1 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      first_down_efficiency = total_first_down_converted / total_first_downs,
      total_second_downs = sum(ifelse(down == 2 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      total_second_down_converted = sum(ifelse(down == 2 & success == 1 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      second_down_efficiency = total_second_down_converted / total_second_downs,
      total_third_downs = sum(ifelse(down == 3 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      total_third_down_converted = sum(ifelse(down == 3 & success == 1 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      third_down_efficiency = total_third_down_converted / total_third_downs,
      total_fourth_downs = sum(ifelse(down == 4 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      total_fourth_down_converted = sum(ifelse(down == 4 & success == 1 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      fourth_down_efficiency = total_fourth_down_converted / total_fourth_downs,
      
      
      # RedZone Efficiency
      total_drives_redzone = n_distinct(drive_number[yards_to_goal <= 20]),
      total_drives_redzone_touchdown = n_distinct(drive_number[yards_to_goal <= 20 & ( play_type == 'Passing Touchdown' | play_type == 'Rushing Touchdown')]),
      total_drives_redzone_fg = n_distinct(drive_number[yards_to_goal <= 20 & play_type == 'Field Goal Good']),
      total_drives_redzone_scoring = total_drives_redzone_touchdown + total_drives_redzone_fg,
      total_redzone_touchdown_efficiency = total_drives_redzone_touchdown / total_drives_redzone,
      total_redzone_fg_efficiency = total_drives_redzone_fg / total_drives_redzone,
      total_redzone_scoring_efficiency = total_drives_redzone_scoring / total_drives_redzone,
      
      # Penalties
      total_penalties_for = sum(ifelse(play_type == 'Penalty' & yards_gained > 0, 1, 0), na.rm = TRUE),
      total_penalties_against = sum(ifelse(play_type == 'Penalty' & yards_gained < 0, 1, 0), na.rm = TRUE),
      
      
      total_penalties_for_yards = ifelse( sum(yards_gained[play_type == 'Penalty' & yards_gained > 0], na.rm = TRUE) > 0, sum(yards_gained[play_type == 'Penalty' & yards_gained > 0], na.rm = TRUE), 0),
      total_penalites_against_yards = ifelse( sum(yards_gained[play_type == 'Penalty' & yards_gained < 0], na.rm = TRUE) > 0, sum(yards_gained[play_type == 'Penalty' & yards_gained < 0], na.rm = TRUE), 0),
      
      # Others while on Offense
      times_sacked = sum(ifelse(play_type == 'Sack', 1, 0), na.rm = TRUE),
      
      ## Turn Overs
      total_interceptions = sum(ifelse(play_type %in% c('Interception Return', 'Interception Return Touchdown'), 1, 0), na.rm = TRUE),
      total_fumbles = sum(ifelse(play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown'), 1, 0), na.rm = TRUE),
      total_turnovers = total_interceptions + total_fumbles,
      
      # Punts
      total_punts_blocked = sum(ifelse(play_type %in% c('Blocked Punt', 'Blocked Punt Touchdown'), 1, 0), na.rm = TRUE),
      total_punts = sum(ifelse(play_type %in% c('Blocked Punt', 'Blocked Punt Touchdown', 'Punt', 'Punt Return Touchdown'), 1, 0), na.rm = TRUE),
      
      # Field Goals
      total_fg_blocked = sum(ifelse(play_type %in% c('Blocked Field Goal', 'Blocked Field Goal Touchdown'), 1, 0), na.rm = TRUE),
      total_fg_attempts = sum(ifelse(play_type %in% c('Blocked Field Goal', 'Blocked Field Goal Touchdown', 'Field Goal Good', 'Field Goal Missed', 'Missed Field Goal Return', 'Missed Field Goal Return Touchdown'), 1, 0), na.rm = TRUE),
      total_fg_good = sum(ifelse(play_type == 'Field Goal Good', 1, 0), na.rm = TRUE),
      fg_comp_attempts = total_fg_good / total_fg_attempts,
      
      # Final misc
      total_scoring_plays = total_fg_good + total_passing_touchdowns + total_rushing_touchdowns
      
      # Other considerations
      # Home vs away 
      # QB ratings
      # Conference? 
      
    ) %>% ungroup()
  
  return(offense_data)
  
}

# 2.) Defense data function 
extract_defense_features <- function(pbp) {
  
  defense_data <- pbp %>%
    ##filter( year == '2024'
    ##        & week == '1' 
    ##        & ( pos_team == "Clemson" | def_pos_team == "Clemson" ) ) %>%
    group_by( across ( c( 'def_pos_team', 'week', 'year' ) ) ) %>%
    filter(game_id==min(game_id)) %>%
    summarise(
      ## Offense
      # Yards
      allowed_total_passing_yards = sum(yards_gained[play_type == 'Pass Reception' | play_type == 'Passing Touchdown'], na.rm = TRUE),           
      allowed_total_rushing_yards = sum(yards_gained[play_type == 'Rush' | play_type ==  'Rushing Touchdown'], na.rm = TRUE), 
      allowed_total_yards = allowed_total_passing_yards + allowed_total_rushing_yards,
      
      allowed_total_pass_completes = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown'), 1, 0), na.rm = TRUE),
      allowed_total_pass_inccompletes = sum(ifelse(play_type %in% c('Pass Incompletion', 'Interception Return', 'Interception Return Touchdown'), 1, 0), na.rm = TRUE),
      allowed_pass_attempts = allowed_total_pass_completes + allowed_total_pass_inccompletes,
      allowed_pass_comp_attempts = allowed_total_pass_completes / allowed_pass_attempts,
      allowed_avg_passing_yards_per_attempt = allowed_total_passing_yards / allowed_pass_attempts,
      allowed_total_passing_touchdowns = sum(ifelse(play_type == 'Passing Touchdown', 1, 0), na.rm = TRUE),
      allowed_rush_attempts = sum(ifelse(play_type %in% c('Rush', 'Rushing Touchdown'), 1, 0), na.rm = TRUE),
      allowed_avg_rush_yards_per_attempt = allowed_total_rushing_yards / allowed_rush_attempts,
      allowed_total_rushing_touchdowns = sum(ifelse(play_type == 'Rushing Touchdown', 1, 0), na.rm = TRUE),
      allowed_total_plays = allowed_pass_attempts + allowed_rush_attempts,
      allowed_avg_yards_per_play = allowed_total_yards / allowed_total_plays,
      
      
      # Plays over Yards
      allowed_total_plays_over_5_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown', 'Rush', 'Rushing Touchdown') & yards_gained >= 5, 1, 0), na.rm = TRUE),
      allowed_total_passing_plays_over_5_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown') & yards_gained >= 5, 1, 0), na.rm = TRUE),
      allowed_total_rushing_plays_over_5_yards = sum(ifelse(play_type %in% c('Rush', 'Rushing Touchdown') & yards_gained >= 5, 1, 0), na.rm = TRUE),
      allowed_total_plays_over_10_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown', 'Rush', 'Rushing Touchdown') & yards_gained >= 10, 1, 0), na.rm = TRUE),
      allowed_total_passing_plays_over_10_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown') & yards_gained >= 10, 1, 0), na.rm = TRUE),
      allowed_total_rushing_plays_over_10_yards = sum(ifelse(play_type %in% c('Rush', 'Rushing Touchdown') & yards_gained >= 10, 1, 0), na.rm = TRUE),
      allowed_total_plays_over_15_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown', 'Rush', 'Rushing Touchdown') & yards_gained >= 15, 1, 0), na.rm = TRUE),
      allowed_total_passing_plays_over_15_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown') & yards_gained >= 15, 1, 0), na.rm = TRUE),     
      allowed_total_rushing_plays_over_15_yards = sum(ifelse(play_type %in% c('Rush', 'Rushing Touchdown') & yards_gained >= 15, 1, 0), na.rm = TRUE),     
      allowed_total_plays_over_20_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown', 'Rush', 'Rushing Touchdown') & yards_gained >= 20, 1, 0), na.rm = TRUE),
      allowed_total_passing_plays_over_20_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown') & yards_gained >= 20, 1, 0), na.rm = TRUE),  
      allowed_total_rushing_plays_over_20_yards = sum(ifelse(play_type %in% c('Rush', 'Rushing Touchdown') & yards_gained >= 20, 1, 0), na.rm = TRUE),   
      allowed_total_plays_over_25_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown', 'Rush', 'Rushing Touchdown') & yards_gained >= 25, 1, 0), na.rm = TRUE),
      allowed_total_passing_plays_over_25_yards = sum(ifelse(play_type %in% c('Pass Reception', 'Passing Touchdown') & yards_gained >= 25, 1, 0), na.rm = TRUE),       
      allowed_total_rushing_plays_over_25_yards = sum(ifelse(play_type %in% c('Rush', 'Rushing Touchdown') & yards_gained >= 25, 1, 0), na.rm = TRUE),   
      
      # Down efficicney
      allowed_total_first_downs = sum(ifelse(down == 1 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      allowed_total_first_down_converted = sum(ifelse(down == 1 & success == 1 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      allowed_first_down_efficiency = allowed_total_first_down_converted / allowed_total_first_downs,
      allowed_total_second_downs = sum(ifelse(down == 2 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      allowed_total_second_down_converted = sum(ifelse(down == 2 & success == 1 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      allowed_second_down_efficiency = allowed_total_second_down_converted / allowed_total_second_downs,      
      allowed_total_third_downs = sum(ifelse(down == 3 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      allowed_total_third_down_converted = sum(ifelse(down == 3 & success == 1 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      allowed_third_down_efficiency = allowed_total_third_down_converted / allowed_total_third_downs,        
      allowed_total_forth_downs = sum(ifelse(down == 4 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      allowed_total_forth_down_converted = sum(ifelse(down == 4 & success == 1 & play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown', 'Fumble Recovery (Own)', 'Fumble Return Touchdown', 'Interception Return', 'Interception Return Touchdown', 'Safety', 'Rushing Touchdown', 'Sack', 'Rush', 'Passing Touchdown', 'Pass Reception', 'Pass Incompletion'), 1, 0), na.rm = TRUE),
      allowed_forth_down_efficiency = allowed_total_forth_down_converted / allowed_total_forth_downs,   
      
      # Others while on Offense
      forced_times_sacked = sum(ifelse(play_type == 'Sack', 1, 0), na.rm = TRUE),
      
      
      ## Turn Overs
      forced_total_interceptions = sum(ifelse(play_type %in% c('Interception Return', 'Interception Return Touchdown'), 1, 0), na.rm = TRUE),
      forced_total_fumbles = sum(ifelse(play_type %in% c('Fumble Recovery (Opponent)', 'Fumble Recovery (Opponent) Touchdown'), 1, 0), na.rm = TRUE),
      forced_total_turnovers = forced_total_interceptions + forced_total_fumbles,
      
    ) %>%
    rename(pos_team = def_pos_team) %>% ungroup()
  
  
  return(defense_data)
  
}

# 3.) Join data 
join_offense_defense <- function(offense_data, defense_data) {
  combined_data <- offense_data %>%
    left_join(defense_data,  by = c("pos_team", "week", "year"))
  
  return(combined_data)
}

# 4.) Cross Join Data
cross_joined_join <- function(combined_data){
  # Set up cross join
  bye_cross_join_dat <- combined_data %>% select(c(pos_team,year,conference)) %>% distinct()
  
  # get weeks
  week_dat <- expand.grid(week = 1:15)
  
  # Do the cross join
  bye_cross_join_dat_joined <- bye_cross_join_dat %>%
    cross_join(week_dat)  %>%
    arrange(pos_team, year, week)
  
  # NOW, lets rejoin the joined data back to this cross joined data
  joined_data_cross_joined <- bye_cross_join_dat_joined %>% dplyr::left_join(combined_data, by = c("pos_team","year","week","conference"))
  
  # Adding new step here to add 0 if na to win and loss flag to reduce the number of NA values, as these are VERY USEFUL in the model 
  joined_data_cross_joined$win_flag <- ifelse(is.na(joined_data_cross_joined$win_flag),0,joined_data_cross_joined$win_flag)
  joined_data_cross_joined$loss_flag <- ifelse(is.na(joined_data_cross_joined$loss_flag),0,joined_data_cross_joined$loss_flag)
  
  return(joined_data_cross_joined)
}

# 5.) Calculate Cumulative Sums
join_data_cumulative <- function(joined_data_cross_joined) {
  
  joined_data_with_cumulative_data <- joined_data_cross_joined %>%
    arrange(pos_team,year,week) %>%
    group_by(pos_team,year) %>%
    mutate(
      # Some basic stats cumulative
      game_played_week = ifelse(!is.na(opponent), 1, 0),
      cumulative_games_played = cumsum(ifelse(is.na(game_played_week), 0, game_played_week)),
      cumulative_games_won = cumsum(ifelse(is.na(win_flag), 0, win_flag)),
      cumulative_games_lost = cumsum(ifelse(is.na(loss_flag), 0, loss_flag)),
      cumulative_game_win_efficiency = cumulative_games_won / cumulative_games_played,
      cumulative_point_dif = cumsum(ifelse(is.na(point_dif), 0, point_dif)),
      cumulative_point_dif_avg_per_game = cumulative_point_dif / cumulative_games_played,
      
      # cumulative Points
      cumulative_points_scored = cumsum(ifelse(is.na(points_scored), 0, points_scored)),
      cumulative_points_allowed = cumsum(ifelse(is.na(points_allowed), 0, points_allowed)),
      cumulative_avg_points_per_game = cumulative_points_scored / cumulative_games_played,
      cumulative_avg_points_allowed_per_game = cumulative_points_allowed / cumulative_games_played,
      
      # cumulative Yards
      cumulative_total_yards = cumsum(ifelse(is.na(total_yards), 0, total_yards)),
      cumulative_total_passing_yards = cumsum(ifelse(is.na(total_passing_yards), 0, total_passing_yards)),
      cumulative_total_rushing_yards = cumsum(ifelse(is.na(total_rushing_yards), 0, total_rushing_yards)),
      cumulative_avg_yards_per_game = cumulative_total_yards / cumulative_games_played,
      cumulative_avg_passing_yards_per_game = cumulative_total_passing_yards / cumulative_games_played,                        
      cumulative_avg_rushing_yards_per_game = cumulative_total_rushing_yards / cumulative_games_played,                        
      
      # cumulative Pass Attempts + Touchdowns
      cumulative_total_pass_attempts = cumsum(ifelse(is.na(pass_attempts), 0, pass_attempts)),
      cumulative_total_complete_pass = cumsum(ifelse(is.na(total_pass_completes), 0, total_pass_completes)),
      cumulative_total_incomplete_pass = cumsum(ifelse(is.na(total_pass_inccompletes), 0, total_pass_inccompletes)),
      cumulative_avg_pass_comp_attempts = cumulative_total_complete_pass / cumulative_total_pass_attempts,
      cumulative_avg_yards_per_pass_attempt = cumulative_total_passing_yards / cumulative_total_pass_attempts,
      cumulative_avg_yards_per_complete_pass = cumulative_total_passing_yards / cumulative_total_complete_pass,        
      cumulative_total_passing_touchdowns = cumsum(ifelse(is.na(total_passing_touchdowns), 0, total_passing_touchdowns)),
      cumulative_avg_pass_attempts_per_game = cumulative_total_pass_attempts / cumulative_games_played,
      cumulative_avg_complete_passes_per_game = cumulative_total_complete_pass / cumulative_games_played,
      cumulative_avg_passing_touchdowns_per_game =  cumulative_total_passing_touchdowns / cumulative_games_played,
      cumulative_avg_yards_per_pass_per_game = cumulative_avg_yards_per_complete_pass / cumulative_games_played,
      
      # cumulative Rush Attempts + Touchdowns
      cumulative_total_rushing_attempts = cumsum(ifelse(is.na(rush_attempts), 0, rush_attempts)),   
      cumulative_avg_yards_per_rush = cumulative_total_rushing_yards / cumulative_total_rushing_attempts,
      cumulative_total_rushing_touchdowns = cumsum(ifelse(is.na(total_rushing_touchdowns), 0, total_rushing_touchdowns)),
      cumulative_avg_rush_attempts_per_game = cumulative_total_rushing_attempts / cumulative_games_played,
      cumulative_avg_rushing_touchdowns_per_game = cumulative_total_rushing_touchdowns / cumulative_games_played,
      
      # cumulative Misc Plays
      cumulative_total_plays = cumsum(ifelse(is.na(total_plays), 0, total_plays)),
      cumulative_avg_yards_per_play = cumulative_total_yards / cumulative_total_plays,
      cumulative_total_scoring_plays = cumsum(ifelse(is.na(total_scoring_plays), 0, total_scoring_plays)),
      cumulative_avg_plays_per_game = cumulative_total_plays / cumulative_games_played,
      cumulative_avg_yards_per_play_per_game = cumulative_avg_yards_per_play / cumulative_games_played,
      cumulative_avg_scoring_plays_per_game = cumulative_total_scoring_plays / cumulative_games_played,
      
      
      # cumulative Total Threshold Plays
      cumulative_total_plays_over_5_yards = cumsum(ifelse(is.na(total_plays_over_5_yards), 0, total_plays_over_5_yards)),
      cumulative_total_passing_plays_over_5_yards = cumsum(ifelse(is.na(total_passing_plays_over_5_yards), 0, total_passing_plays_over_5_yards)),   
      cumulative_total_rushing_plays_over_5_yards = cumsum(ifelse(is.na(total_rushing_plays_over_5_yards), 0, total_rushing_plays_over_5_yards)),
      cumulative_total_plays_over_10_yards = cumsum(ifelse(is.na(total_plays_over_10_yards), 0, total_plays_over_10_yards)),
      cumulative_total_passing_plays_over_10_yards = cumsum(ifelse(is.na(total_passing_plays_over_10_yards), 0, total_passing_plays_over_10_yards)),   
      cumulative_total_rushing_plays_over_10_yards = cumsum(ifelse(is.na(total_rushing_plays_over_10_yards), 0, total_rushing_plays_over_10_yards)),
      cumulative_total_plays_over_15_yards = cumsum(ifelse(is.na(total_plays_over_15_yards), 0, total_plays_over_15_yards)),
      cumulative_total_passing_plays_over_15_yards = cumsum(ifelse(is.na(total_passing_plays_over_15_yards), 0, total_passing_plays_over_15_yards)),
      cumulative_total_rushing_plays_over_15_yards = cumsum(ifelse(is.na(total_rushing_plays_over_15_yards), 0, total_rushing_plays_over_15_yards)),
      cumulative_total_plays_over_20_yards = cumsum(ifelse(is.na(total_plays_over_20_yards), 0, total_plays_over_20_yards)),
      cumulative_total_passing_plays_over_20_yards = cumsum(ifelse(is.na(total_passing_plays_over_20_yards), 0, total_passing_plays_over_20_yards)),
      cumulative_total_rushing_plays_over_20_yards = cumsum(ifelse(is.na(total_rushing_plays_over_20_yards), 0, total_rushing_plays_over_20_yards)),
      cumulative_total_plays_over_25_yards = cumsum(ifelse(is.na(total_plays_over_25_yards), 0, total_plays_over_25_yards)),
      cumulative_total_passing_plays_over_25_yards = cumsum(ifelse(is.na(total_passing_plays_over_25_yards), 0, total_passing_plays_over_25_yards)),   
      cumulative_total_rushing_plays_over_25_yards = cumsum(ifelse(is.na(total_rushing_plays_over_25_yards), 0, total_rushing_plays_over_25_yards)),
      cumulative_avg_per_game_total_plays_over_5_yards = cumulative_total_plays_over_5_yards / cumulative_games_played,
      cumulative_avg_per_game_total_passing_plays_over_5_yards = cumulative_total_passing_plays_over_5_yards / cumulative_games_played,   
      cumulative_avg_per_game_total_rushing_plays_over_5_yards = cumulative_total_rushing_plays_over_5_yards / cumulative_games_played,
      cumulative_avg_per_game_total_plays_over_10_yards = cumulative_total_plays_over_10_yards / cumulative_games_played,
      cumulative_avg_per_game_total_passing_plays_over_10_yards = cumulative_total_passing_plays_over_10_yards / cumulative_games_played,    
      cumulative_avg_per_game_total_rushing_plays_over_10_yards = cumulative_total_rushing_plays_over_10_yards / cumulative_games_played,
      cumulative_avg_per_game_total_plays_over_15_yards = cumulative_total_plays_over_15_yards / cumulative_games_played,
      cumulative_avg_per_game_total_passing_plays_over_15_yards = cumulative_total_passing_plays_over_15_yards / cumulative_games_played,
      cumulative_avg_per_game_total_rushing_plays_over_15_yards = cumulative_total_rushing_plays_over_15_yards / cumulative_games_played,
      cumulative_avg_per_game_total_plays_over_20_yards = cumulative_total_plays_over_20_yards / cumulative_games_played,
      cumulative_avg_per_game_total_passing_plays_over_20_yards = cumulative_total_passing_plays_over_20_yards / cumulative_games_played,
      cumulative_avg_per_game_total_rushing_plays_over_20_yards = cumulative_total_rushing_plays_over_20_yards / cumulative_games_played,
      cumulative_avg_per_game_total_plays_over_25_yards = cumulative_total_plays_over_25_yards / cumulative_games_played,
      cumulative_avg_per_game_total_passing_plays_over_25_yards = cumulative_total_passing_plays_over_25_yards / cumulative_games_played, 
      cumulative_avg_per_game_total_rushing_plays_over_25_yards = cumulative_total_rushing_plays_over_25_yards / cumulative_games_played,
      
      
      # cumulative Total Possessions
      cumulative_total_possessions = cumsum(ifelse(is.na(total_possessions), 0, total_possessions)),
      cumulative_total_time_possession_mins = cumsum(ifelse(is.na(total_time_possession_mins), 0, total_time_possession_mins)),
      cumulative_avg_time_possession = cumulative_total_time_possession_mins / cumulative_total_possessions,
      cumulative_avg_time_per_play = cumulative_total_time_possession_mins / cumulative_total_plays,
      cumulative_avg_possessions_per_game = cumulative_total_possessions / cumulative_games_played,
      cumulative_avg_possession_time_per_game = cumulative_total_time_possession_mins / cumulative_games_played,
      cumulative_avg_time_per_game = cumulative_avg_time_possession / cumulative_games_played,
      
      # cumulative down efficiency
      cumulative_total_first_downs = cumsum(ifelse(is.na(total_first_downs), 0, total_first_downs)),
      cumulative_total_first_down_converted = cumsum(ifelse(is.na(total_first_down_converted), 0, total_first_down_converted)),
      cumulative_total_second_downs = cumsum(ifelse(is.na(total_second_downs), 0, total_second_downs)),
      cumulative_total_second_down_converted = cumsum(ifelse(is.na(total_second_down_converted), 0, total_second_down_converted)),
      cumulative_total_third_downs = cumsum(ifelse(is.na(total_third_downs), 0, total_third_downs)),
      cumulative_total_third_down_converted = cumsum(ifelse(is.na(total_third_down_converted), 0, total_third_down_converted)),
      cumulative_total_fourth_downs = cumsum(ifelse(is.na(total_fourth_downs), 0, total_fourth_downs)),
      cumulative_total_fourth_down_converted = cumsum(ifelse(is.na(total_fourth_down_converted), 0, total_fourth_down_converted)),
      cumulative_first_down_efficicency = cumulative_total_first_down_converted / cumulative_total_first_downs,
      cumulative_second_down_efficicency = cumulative_total_second_down_converted / cumulative_total_second_downs,
      cumulative_third_down_efficicency = cumulative_total_third_down_converted / cumulative_total_third_downs,
      cumulative_fourth_down_efficicency = cumulative_total_fourth_down_converted / cumulative_total_fourth_downs,
      cumulative_avg_per_game_total_first_downs = cumulative_total_first_downs / cumulative_games_played,
      cumulative_avg_per_game_total_first_down_converted = cumulative_total_first_down_converted / cumulative_games_played,
      cumulative_avg_per_game_total_second_downs = cumulative_total_second_downs / cumulative_games_played,
      cumulative_avg_per_game_total_second_down_converted = cumulative_total_second_down_converted / cumulative_games_played,
      cumulative_avg_per_game_total_third_downs = cumulative_total_third_downs / cumulative_games_played,
      cumulative_avg_per_game_total_third_down_converted = cumulative_total_third_down_converted / cumulative_games_played,
      cumulative_avg_per_game_total_fourth_downs = cumulative_total_fourth_downs / cumulative_games_played,
      cumulative_avg_per_game_total_fourth_down_converted = cumulative_total_fourth_down_converted / cumulative_games_played,
      cumulative_avg_per_game_first_down_efficicency = cumulative_first_down_efficicency / cumulative_games_played,
      cumulative_avg_per_game_second_down_efficicency = cumulative_second_down_efficicency / cumulative_games_played,
      cumulative_avg_per_game_third_down_efficicency = cumulative_third_down_efficicency / cumulative_games_played,
      cumulative_avg_per_game_fourth_down_efficicency = cumulative_fourth_down_efficicency / cumulative_games_played,                
      
      
      # cumulative redzone efficiency 
      cumulative_total_drives_redzone = cumsum(ifelse(is.na(total_drives_redzone), 0, total_drives_redzone)),
      cumulative_total_drives_redzone_touchdown = cumsum(ifelse(is.na(total_drives_redzone_touchdown), 0, total_drives_redzone_touchdown)),
      cumulative_total_drives_redzone_fg = cumsum(ifelse(is.na(total_drives_redzone_fg), 0, total_drives_redzone_fg)),
      cumulative_total_drives_redzone_scoring = cumsum(ifelse(is.na(total_drives_redzone_scoring), 0, total_drives_redzone_scoring)),
      cumulative_redzone_touchdown_efficiency = cumulative_total_drives_redzone_touchdown / cumulative_total_drives_redzone, 
      cumulative_redzone_fg_efficiency = cumulative_total_drives_redzone_fg / cumulative_total_drives_redzone,
      cumulative_redzone_scoring_efficiency = cumulative_total_drives_redzone_scoring / cumulative_total_drives_redzone,
      cumulative_avg_per_game_total_drives_redzone = cumulative_total_drives_redzone / cumulative_games_played,
      cumulative_avg_per_game_total_drives_redzone_touchdown = cumulative_total_drives_redzone_touchdown / cumulative_games_played,
      cumulative_avg_per_game_total_drives_redzone_fg = cumulative_total_drives_redzone_fg / cumulative_games_played,
      cumulative_avg_per_game_total_drives_redzone_scoring = cumulative_total_drives_redzone_scoring / cumulative_games_played, 
      cumulative_avg_per_game_redzone_touchdown_efficiency = cumulative_redzone_touchdown_efficiency / cumulative_games_played,
      cumulative_avg_per_game_redzone_fg_efficiency = cumulative_redzone_fg_efficiency / cumulative_games_played,
      cumulative_avg_per_game_redzone_scoring_efficiency = cumulative_redzone_scoring_efficiency / cumulative_games_played,
      
      
      # Penalties
      cumulative_total_penalties_for = cumsum(ifelse(is.na(total_penalties_for), 0, total_penalties_for)),
      cumulative_total_penalties_against = cumsum(ifelse(is.na(total_penalties_against), 0, total_penalties_against)),
      cumulative_total_penalties_for_yards = cumsum(ifelse(is.na(total_penalties_for_yards), 0, total_penalties_for_yards)),
      cumulative_total_penalites_against_yards = cumsum(ifelse(is.na(total_penalites_against_yards), 0, total_penalites_against_yards)),
      cumulative_avg_per_game_total_penalties_for = cumulative_total_penalties_for / cumulative_games_played,
      cumulative_avg_per_game_total_penalties_against = cumulative_total_penalties_against / cumulative_games_played,
      cumulative_avg_per_game_total_penalties_for_yards = cumulative_total_penalties_for_yards / cumulative_games_played,
      cumulative_avg_per_game_total_penalites_against_yards = cumulative_total_penalites_against_yards / cumulative_games_played, 
      
      
      # cumulative Others
      cumulative_times_sacked = cumsum(ifelse(is.na(times_sacked), 0, times_sacked)),
      cumulative_avg_per_game_times_sacked = cumulative_times_sacked / cumulative_games_played,
      
      
      # cumulative Turn Overs
      cumulative_total_interceptions = cumsum(ifelse(is.na(total_interceptions), 0, total_interceptions)),
      cumulative_total_fumbles = cumsum(ifelse(is.na(total_fumbles), 0, total_fumbles)),
      cumulative_total_turnovers = cumsum(ifelse(is.na(total_turnovers), 0, total_turnovers)),
      cumulative_avg_per_game_total_interceptions = cumulative_total_interceptions / cumulative_games_played,
      cumulative_avg_per_game_total_fumbles = cumulative_total_fumbles / cumulative_games_played,
      cumulative_avg_per_game_total_turnovers = cumulative_total_turnovers / cumulative_games_played,                    
      
      
      # cumulative Punts
      cumulative_total_punts_blocked = cumsum(ifelse(is.na(total_punts_blocked), 0, total_punts_blocked)),
      cumulative_total_punts = cumsum(ifelse(is.na(total_punts), 0, total_punts)),
      cumulative_avg_per_game_total_punts_blocked = cumulative_total_punts_blocked / cumulative_games_played,
      cumulative_avg_per_game_total_punts = cumulative_total_punts / cumulative_games_played,                 
      
      
      # cumulative Field Goals
      cumulative_total_fg_attempts = cumsum(ifelse(is.na(total_fg_attempts), 0, total_fg_attempts)),
      cumulative_total_fg_good = cumsum(ifelse(is.na(total_fg_good), 0, total_fg_good)),
      cumulative_total_fg_blocked = cumsum(ifelse(is.na(total_fg_blocked), 0, total_fg_blocked)),
      cumulative_fg_comp_attempts = cumulative_total_fg_good / cumulative_total_fg_attempts,
      cumulative_avg_per_game_total_fg_attempts = cumulative_total_fg_attempts / cumulative_games_played,
      cumulative_avg_per_game_total_fg_good = cumulative_total_fg_good / cumulative_games_played,
      cumulative_avg_per_game_total_fg_blocked = cumulative_total_fg_blocked / cumulative_games_played,
      cumulative_avg_per_game_fg_comp_attempts = cumulative_fg_comp_attempts / cumulative_games_played,                  
      
      
      # cumulative yards against and scores against
      cumulative_allowed_total_passing_yards = cumsum(ifelse(is.na(allowed_total_passing_yards), 0, allowed_total_passing_yards)),        
      cumulative_allowed_total_rushing_yards = cumsum(ifelse(is.na(allowed_total_rushing_yards), 0, allowed_total_rushing_yards)),    
      cumulative_allowed_total_yards = cumsum(ifelse(is.na(allowed_total_yards), 0, allowed_total_yards)),    
      cumulative_allowed_total_pass_completes = cumsum(ifelse(is.na(allowed_total_pass_completes), 0, allowed_total_pass_completes)),    
      cumulative_allowed_total_pass_inccompletes = cumsum(ifelse(is.na(allowed_total_pass_inccompletes), 0, allowed_total_pass_inccompletes)),   
      cumulative_allowed_pass_attempts = cumsum(ifelse(is.na(allowed_pass_attempts), 0, allowed_pass_attempts)),    
      cumulative_allowed_total_passing_touchdowns = cumsum(ifelse(is.na(allowed_total_passing_touchdowns), 0, allowed_total_passing_touchdowns)),   
      cumulative_allowed_rush_attempts = cumsum(ifelse(is.na(allowed_rush_attempts), 0, allowed_rush_attempts)),    
      cumulative_allowed_total_rushing_touchdowns = cumsum(ifelse(is.na(allowed_total_rushing_touchdowns), 0, allowed_total_rushing_touchdowns)),   
      cumulative_allowed_total_plays = cumsum(ifelse(is.na(allowed_total_plays), 0, allowed_total_plays)),   
      cumulative_allowed_pass_comp_attempts = cumulative_allowed_total_pass_completes / cumulative_allowed_pass_attempts,
      cumulative_allowed_avg_passing_yards_per_attempt = cumulative_allowed_total_passing_yards / cumulative_allowed_pass_attempts,
      cumulative_allowed_avg_yards_per_play = cumulative_allowed_total_yards / cumulative_allowed_total_plays,
      cumulative_avg_per_game_allowed_total_passing_yards = cumulative_allowed_total_passing_yards / cumulative_games_played,       
      cumulative_avg_per_game_allowed_total_rushing_yards = cumulative_allowed_total_rushing_yards / cumulative_games_played,  
      cumulative_avg_per_game_allowed_total_yards = cumulative_allowed_total_yards / cumulative_games_played, 
      cumulative_avg_per_game_allowed_total_pass_completes = cumulative_allowed_total_pass_completes / cumulative_games_played,  
      cumulative_avg_per_game_allowed_total_pass_inccompletes = cumulative_allowed_total_pass_inccompletes / cumulative_games_played,   
      cumulative_avg_per_game_allowed_pass_attempts = cumulative_allowed_pass_attempts / cumulative_games_played,  
      cumulative_avg_per_game_allowed_total_passing_touchdowns = cumulative_allowed_total_passing_touchdowns / cumulative_games_played,  
      cumulative_avg_per_game_allowed_rush_attempts = cumulative_allowed_rush_attempts / cumulative_games_played, 
      cumulative_avg_per_game_allowed_total_rushing_touchdowns = cumulative_allowed_total_rushing_touchdowns / cumulative_games_played,    
      cumulative_avg_per_game_allowed_total_plays = cumulative_allowed_total_plays / cumulative_games_played,    
      cumulative_avg_per_game_allowed_pass_comp_attempts = cumulative_allowed_pass_comp_attempts / cumulative_games_played,
      cumulative_avg_per_game_allowed_avg_passing_yards_per_attempt = cumulative_allowed_avg_passing_yards_per_attempt / cumulative_games_played,
      cumulative_avg_per_game_allowed_avg_yards_per_play = cumulative_allowed_avg_yards_per_play / cumulative_games_played,             
      
      
      # cumulative plays over yardage against 
      cumulative_allowed_total_plays_over_5_yards = cumsum(ifelse(is.na(allowed_total_plays_over_5_yards), 0, allowed_total_plays_over_5_yards)),
      cumulative_allowed_total_passing_plays_over_5_yards = cumsum(ifelse(is.na(allowed_total_passing_plays_over_5_yards), 0, allowed_total_passing_plays_over_5_yards)), 
      cumulative_allowed_total_rushing_plays_over_5_yards = cumsum(ifelse(is.na(allowed_total_rushing_plays_over_5_yards), 0, allowed_total_rushing_plays_over_5_yards)),
      cumulative_allowed_total_plays_over_10_yards = cumsum(ifelse(is.na(allowed_total_plays_over_10_yards), 0, allowed_total_plays_over_10_yards)),
      cumulative_allowed_total_passing_plays_over_10_yards = cumsum(ifelse(is.na(allowed_total_passing_plays_over_10_yards), 0, allowed_total_passing_plays_over_10_yards)),      
      cumulative_allowed_total_rushing_plays_over_10_yards = cumsum(ifelse(is.na(allowed_total_rushing_plays_over_10_yards), 0, allowed_total_rushing_plays_over_10_yards)),
      cumulative_allowed_total_plays_over_15_yards = cumsum(ifelse(is.na(allowed_total_plays_over_15_yards), 0, allowed_total_plays_over_15_yards)),
      cumulative_allowed_total_passing_plays_over_15_yards = cumsum(ifelse(is.na(allowed_total_passing_plays_over_15_yards), 0, allowed_total_passing_plays_over_15_yards)),    
      cumulative_allowed_total_rushing_plays_over_15_yards = cumsum(ifelse(is.na(allowed_total_rushing_plays_over_15_yards), 0, allowed_total_rushing_plays_over_15_yards)),
      cumulative_allowed_total_plays_over_20_yards = cumsum(ifelse(is.na(allowed_total_plays_over_20_yards), 0, allowed_total_plays_over_20_yards)),
      cumulative_allowed_total_passing_plays_over_20_yards = cumsum(ifelse(is.na(allowed_total_passing_plays_over_20_yards), 0, allowed_total_passing_plays_over_20_yards)),   
      cumulative_allowed_total_rushing_plays_over_20_yards = cumsum(ifelse(is.na(allowed_total_rushing_plays_over_20_yards), 0, allowed_total_rushing_plays_over_20_yards)),
      cumulative_allowed_total_plays_over_25_yards = cumsum(ifelse(is.na(allowed_total_plays_over_25_yards), 0, allowed_total_plays_over_25_yards)),
      cumulative_allowed_total_passing_plays_over_25_yards = cumsum(ifelse(is.na(allowed_total_passing_plays_over_25_yards), 0, allowed_total_passing_plays_over_25_yards)),    
      cumulative_allowed_total_rushing_plays_over_25_yards = cumsum(ifelse(is.na(allowed_total_rushing_plays_over_25_yards), 0, allowed_total_rushing_plays_over_25_yards)),
      cumulative_avg_per_game_allowed_total_plays_over_5_yards = cumulative_allowed_total_plays_over_5_yards / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_passing_plays_over_5_yards = cumulative_allowed_total_passing_plays_over_5_yards / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_rushing_plays_over_5_yards = cumulative_allowed_total_rushing_plays_over_5_yards / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_plays_over_10_yards = cumulative_allowed_total_plays_over_10_yards / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_passing_plays_over_10_yards = cumulative_allowed_total_passing_plays_over_10_yards / cumulative_games_played,     
      cumulative_avg_per_game_allowed_total_rushing_plays_over_10_yards = cumulative_allowed_total_rushing_plays_over_10_yards / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_plays_over_15_yards = cumulative_allowed_total_plays_over_15_yards / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_passing_plays_over_15_yards = cumulative_allowed_total_passing_plays_over_15_yards / cumulative_games_played,   
      cumulative_avg_per_game_allowed_total_rushing_plays_over_15_yards = cumulative_allowed_total_rushing_plays_over_15_yards / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_plays_over_20_yards = cumulative_allowed_total_plays_over_20_yards / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_passing_plays_over_20_yards = cumulative_allowed_total_passing_plays_over_20_yards / cumulative_games_played,  
      cumulative_avg_per_game_allowed_total_rushing_plays_over_20_yards = cumulative_allowed_total_rushing_plays_over_20_yards / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_plays_over_25_yards = cumulative_allowed_total_plays_over_25_yards / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_passing_plays_over_25_yards = cumulative_allowed_total_passing_plays_over_25_yards / cumulative_games_played,    
      cumulative_avg_per_game_allowed_total_rushing_plays_over_25_yards = cumulative_allowed_total_rushing_plays_over_25_yards / cumulative_games_played,             
      
      
      # cumulative down efficicency 
      cumulative_allowed_total_first_downs = cumsum(ifelse(is.na(allowed_total_first_downs), 0, allowed_total_first_downs)),
      cumulative_allowed_total_first_down_converted = cumsum(ifelse(is.na(allowed_total_first_down_converted), 0, allowed_total_first_down_converted)),
      cumulative_allowed_total_second_downs = cumsum(ifelse(is.na(allowed_total_second_downs), 0, allowed_total_second_downs)),
      cumulative_allowed_total_second_down_converted = cumsum(ifelse(is.na(allowed_total_second_down_converted), 0, allowed_total_second_down_converted)), 
      cumulative_allowed_total_third_downs = cumsum(ifelse(is.na(allowed_total_third_downs), 0, allowed_total_third_downs)),
      cumulative_allowed_total_third_down_converted = cumsum(ifelse(is.na(allowed_total_third_down_converted), 0, allowed_total_third_down_converted)),   
      cumulative_allowed_total_forth_downs = cumsum(ifelse(is.na(allowed_total_forth_downs), 0, allowed_total_forth_downs)),
      cumulative_allowed_total_forth_down_converted = cumsum(ifelse(is.na(allowed_total_forth_down_converted), 0, allowed_total_forth_down_converted)),   	
      cumulative_allowed_first_down_efficiency = cumulative_allowed_total_first_down_converted / cumulative_allowed_total_first_downs,
      cumulative_allowed_second_down_efficiency = cumulative_allowed_total_second_down_converted / cumulative_allowed_total_second_downs,      
      cumulative_allowed_third_down_efficiency = cumulative_allowed_total_third_down_converted / cumulative_allowed_total_third_downs,        
      cumulative_allowed_forth_down_efficiency = cumulative_allowed_total_forth_down_converted / cumulative_allowed_total_forth_downs,
      cumulative_avg_per_game_allowed_total_first_downs = cumulative_allowed_total_first_downs / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_first_down_converted = cumulative_allowed_total_first_down_converted / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_second_downs = cumulative_allowed_total_second_downs / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_second_down_converted = cumulative_allowed_total_second_down_converted / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_third_downs = cumulative_allowed_total_third_downs / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_third_down_converted = cumulative_allowed_total_third_down_converted / cumulative_games_played,    
      cumulative_avg_per_game_allowed_total_forth_downs = cumulative_allowed_total_forth_downs / cumulative_games_played,
      cumulative_avg_per_game_allowed_total_forth_down_converted = cumulative_allowed_total_forth_down_converted / cumulative_games_played,  	
      cumulative_avg_per_game_allowed_first_down_efficiency = cumulative_allowed_first_down_efficiency / cumulative_games_played,
      cumulative_avg_per_game_allowed_second_down_efficiency = cumulative_allowed_second_down_efficiency/ cumulative_games_played,   
      cumulative_avg_per_game_allowed_third_down_efficiency = cumulative_allowed_third_down_efficiency / cumulative_games_played,      
      cumulative_avg_per_game_allowed_forth_down_efficiency = cumulative_allowed_forth_down_efficiency / cumulative_games_played,                         
      
      
      
      ## cumulative Turn Overs and Sacks
      cumulative_forced_total_interceptions = cumsum(ifelse(is.na(forced_total_interceptions), 0, forced_total_interceptions)),
      cumulative_forced_total_fumbles = cumsum(ifelse(is.na(forced_total_fumbles), 0, forced_total_fumbles)),
      cumulative_forced_total_turnovers = cumsum(ifelse(is.na(forced_total_turnovers), 0, forced_total_turnovers)),
      cumulative_forced_times_sacked = cumsum(ifelse(is.na(forced_times_sacked), 0, forced_times_sacked)),
      cumulative_avg_per_game_forced_total_interceptions = cumulative_forced_total_interceptions / cumulative_games_played,
      cumulative_avg_per_game_forced_total_fumbles = cumulative_forced_total_fumbles / cumulative_games_played, 
      cumulative_avg_per_game_forced_total_turnovers = cumulative_forced_total_turnovers / cumulative_games_played,
      cumulative_avg_per_game_forced_times_sacked =  cumulative_forced_times_sacked / cumulative_games_played,                 
      
      
      
    ) %>%
    ungroup()
  
  return(joined_data_with_cumulative_data)
  
}

# 6.) Calculate further cumulative data
cumulative_fully_joined <- function(joined_data_with_cumulative_data) {
  
  joined_data_with_cumulative_data_opponent_dat <- joined_data_with_cumulative_data %>%
    dplyr::left_join(joined_data_with_cumulative_data,
                     by = c("opponent" = "pos_team", "week" = "week", "year" = "year"),
                     suffix = c("","_opponent"))
  
  joined_data_with_cumulative_data_opponent_dat <- joined_data_with_cumulative_data_opponent_dat %>%
    mutate(
      # Create a new slate of metrics by weighing the metric by cumulative_game_win_efficiency_opponent
      weighted_cumulative_game_win_efficiency = cumulative_game_win_efficiency * cumulative_game_win_efficiency_opponent,
      weighted2_cumulative_game_win_efficiency = ( cumulative_games_won - weighted_cumulative_game_win_efficiency) / cumulative_games_played,
      
      # Possibly more important vars
      weighted_cumulative_avg_points_per_game = cumulative_avg_points_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_games_won = cumulative_games_won * cumulative_game_win_efficiency_opponent,    
      weighted_cumulative_avg_yards_per_play = cumulative_avg_yards_per_play * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_yards_per_game = cumulative_avg_yards_per_game * cumulative_game_win_efficiency_opponent, 
      weighted_win_flag = win_flag * cumulative_game_win_efficiency_opponent,
      weighted_loss_flag = loss_flag * cumulative_game_win_efficiency_opponent,
      weighted_points_scored = points_scored * cumulative_game_win_efficiency_opponent,    
      weighted_total_scoring_plays = total_scoring_plays * cumulative_game_win_efficiency_opponent,
      weighted_avg_yards_per_play = avg_yards_per_play * cumulative_game_win_efficiency_opponent,
      weighted_total_yards = total_yards * cumulative_game_win_efficiency_opponent,
      weighted_win_flag_opponent = win_flag_opponent * cumulative_game_win_efficiency_opponent, 
      weighted_points_allowed = points_allowed * cumulative_game_win_efficiency_opponent, 
      weighted_points_scored_opponent = points_scored_opponent * cumulative_game_win_efficiency_opponent, 
      weighted_total_scoring_plays_opponent = total_scoring_plays_opponent * cumulative_game_win_efficiency_opponent, 
      weighted_allowed_total_yards = allowed_total_yards * cumulative_game_win_efficiency_opponent, 
      weighted_total_yards_opponent = total_yards_opponent * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_point_dif = cumulative_point_dif * cumulative_point_dif_opponent,
      
      # Get some more variables taking team offense and defense vs opponent offence and defense per game 
      # OF
      avg_weighted_points_scored = points_scored * cumulative_avg_points_allowed_per_game_opponent,
      avg_weighted_total_yards = total_yards * cumulative_avg_per_game_allowed_total_yards_opponent,
      avg_weighted_total_passing_yards = total_passing_yards * cumulative_avg_per_game_allowed_total_passing_yards_opponent,
      avg_weighted_total_rushing_yards = total_rushing_yards * cumulative_avg_per_game_allowed_total_rushing_yards_opponent,
      avg_weighted_pass_attempts = pass_attempts * cumulative_avg_per_game_allowed_total_pass_completes_opponent,
      avg_weighted_total_pass_completes = total_pass_completes * cumulative_avg_per_game_allowed_total_pass_completes_opponent,
      avg_weighted_total_passing_touchdowns = total_passing_touchdowns * cumulative_avg_per_game_allowed_total_passing_touchdowns_opponent,
      avg_weighted_rush_attempts = rush_attempts * cumulative_avg_per_game_allowed_rush_attempts_opponent,
      avg_weighted_total_rushing_touchdowns = total_rushing_touchdowns * cumulative_avg_per_game_allowed_total_rushing_touchdowns_opponent,
      avg_weighted_first_down_efficiency = first_down_efficiency * cumulative_avg_per_game_allowed_first_down_efficiency_opponent,
      avg_weighted_second_down_efficiency = second_down_efficiency * cumulative_avg_per_game_allowed_second_down_efficiency_opponent,
      avg_weighted_third_down_efficiency = third_down_efficiency * cumulative_avg_per_game_allowed_third_down_efficiency_opponent,
      
      # DEF
      avg_weighted_points_allowed = points_allowed * cumulative_avg_points_per_game_opponent,
      avg_weighted_allowed_total_passing_yards = allowed_total_passing_yards * cumulative_avg_passing_yards_per_game_opponent,
      avg_weighted_allowed_total_rushing_yards = allowed_total_rushing_yards * cumulative_avg_rushing_yards_per_game_opponent,
      avg_weighted_allowed_total_yards = allowed_total_yards * cumulative_avg_yards_per_game_opponent,
      avg_weighted_allowed_total_pass_completes = allowed_total_pass_completes * cumulative_avg_pass_comp_attempts_opponent,
      avg_weighted_allowed_pass_attempts = allowed_pass_attempts * cumulative_avg_pass_attempts_per_game_opponent,
      avg_weighted_allowed_total_passing_touchdowns = allowed_total_passing_touchdowns * cumulative_avg_passing_touchdowns_per_game_opponent,
      avg_weighted_allowed_rush_attempts = allowed_rush_attempts * cumulative_avg_rush_attempts_per_game_opponent,
      avg_weighted_allowed_total_rushing_touchdowns = allowed_total_rushing_touchdowns * cumulative_avg_rushing_touchdowns_per_game_opponent,
      avg_weighted_allowed_total_plays = allowed_total_plays * cumulative_total_scoring_plays,
      
      
      # Actually just going to calculate a ton of these weighted variables that seem to have high correlations and multiply by cumulative opponent game win %
      weighted_cumulative_avg_scoring_plays_per_game = cumulative_avg_scoring_plays_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_drives_redzone_scoring = cumulative_avg_per_game_total_drives_redzone_scoring * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_plays_over_15_yards = cumulative_avg_per_game_total_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_drives_redzone_touchdown = cumulative_avg_per_game_total_drives_redzone_touchdown * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_first_down_converted = cumulative_avg_per_game_total_first_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_first_down_efficicency = cumulative_first_down_efficicency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_plays_over_10_yards = cumulative_avg_per_game_total_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_second_down_efficicency = cumulative_second_down_efficicency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_plays_over_20_yards = cumulative_avg_per_game_total_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_pass_comp_attempts = cumulative_avg_pass_comp_attempts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_yards_per_pass_attempt = cumulative_avg_yards_per_pass_attempt * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_plays_over_5_yards = cumulative_avg_per_game_total_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_passing_touchdowns_per_game = cumulative_avg_passing_touchdowns_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_third_down_efficicency = cumulative_third_down_efficicency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_rushing_touchdowns_per_game = cumulative_avg_rushing_touchdowns_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_plays_over_25_yards = cumulative_avg_per_game_total_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_drives_redzone = cumulative_avg_per_game_total_drives_redzone * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_points_scored = cumulative_points_scored * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_second_down_converted = cumulative_avg_per_game_total_second_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_drives_redzone_touchdown = cumulative_total_drives_redzone_touchdown * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_passing_plays_over_15_yards = cumulative_avg_per_game_total_passing_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_passing_plays_over_20_yards = cumulative_avg_per_game_total_passing_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_rushing_touchdowns = cumulative_total_rushing_touchdowns * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_scoring_plays = cumulative_total_scoring_plays * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_yards_per_rush = cumulative_avg_yards_per_rush * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_drives_redzone_scoring = cumulative_total_drives_redzone_scoring * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_forced_times_sacked = cumulative_avg_per_game_forced_times_sacked * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_passing_touchdowns = cumulative_total_passing_touchdowns * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_redzone_touchdown_efficiency = cumulative_redzone_touchdown_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_redzone_scoring_efficiency = cumulative_redzone_scoring_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_passing_plays_over_25_yards = cumulative_avg_per_game_total_passing_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_passing_yards_per_game = cumulative_avg_passing_yards_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_first_downs = cumulative_avg_per_game_total_first_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_passing_plays_over_10_yards = cumulative_avg_per_game_total_passing_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_points_allowed_opponent = points_allowed_opponent * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_rushing_plays_over_10_yards = cumulative_avg_per_game_total_rushing_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_points_allowed_per_game = cumulative_avg_points_allowed_per_game * cumulative_game_win_efficiency_opponent,
      
      # More 
      weighted_allowed_avg_passing_yards_per_attempt = allowed_avg_passing_yards_per_attempt * cumulative_game_win_efficiency_opponent,
      weighted_allowed_avg_rush_yards_per_attempt = allowed_avg_rush_yards_per_attempt * cumulative_game_win_efficiency_opponent,
      weighted_allowed_avg_yards_per_play = allowed_avg_yards_per_play * cumulative_game_win_efficiency_opponent,
      weighted_allowed_first_down_efficiency = allowed_first_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_allowed_forth_down_efficiency = allowed_forth_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_allowed_pass_attempts = allowed_pass_attempts * cumulative_game_win_efficiency_opponent,
      weighted_allowed_pass_comp_attempts = allowed_pass_comp_attempts * cumulative_game_win_efficiency_opponent,
      weighted_allowed_rush_attempts = allowed_rush_attempts * cumulative_game_win_efficiency_opponent,
      weighted_allowed_second_down_efficiency = allowed_second_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_allowed_third_down_efficiency = allowed_third_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_first_down_converted = allowed_total_first_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_first_downs = allowed_total_first_downs * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_forth_down_converted = allowed_total_forth_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_forth_downs = allowed_total_forth_downs * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_pass_completes = allowed_total_pass_completes * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_pass_inccompletes = allowed_total_pass_inccompletes * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_passing_plays_over_10_yards = allowed_total_passing_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_passing_plays_over_15_yards = allowed_total_passing_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_passing_plays_over_20_yards = allowed_total_passing_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_passing_plays_over_25_yards = allowed_total_passing_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_passing_plays_over_5_yards = allowed_total_passing_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_passing_touchdowns = allowed_total_passing_touchdowns * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_passing_yards = allowed_total_passing_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_plays = allowed_total_plays * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_plays_over_10_yards = allowed_total_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_plays_over_15_yards = allowed_total_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_plays_over_20_yards = allowed_total_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_plays_over_25_yards = allowed_total_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_plays_over_5_yards = allowed_total_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_rushing_plays_over_10_yards = allowed_total_rushing_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_rushing_plays_over_15_yards = allowed_total_rushing_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_rushing_plays_over_20_yards = allowed_total_rushing_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_rushing_plays_over_25_yards = allowed_total_rushing_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_rushing_plays_over_5_yards = allowed_total_rushing_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_rushing_touchdowns = allowed_total_rushing_touchdowns * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_rushing_yards = allowed_total_rushing_yards * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_second_down_converted = allowed_total_second_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_second_downs = allowed_total_second_downs * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_third_down_converted = allowed_total_third_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_allowed_total_third_downs = allowed_total_third_downs * cumulative_game_win_efficiency_opponent,
      weighted_avg_passing_yards_per_attempt = avg_passing_yards_per_attempt * cumulative_game_win_efficiency_opponent,
      weighted_avg_rush_yards_per_attempt = avg_rush_yards_per_attempt * cumulative_game_win_efficiency_opponent,
      weighted_avg_time_per_play = avg_time_per_play * cumulative_game_win_efficiency_opponent,
      weighted_avg_time_possession = avg_time_possession * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_avg_passing_yards_per_attempt = cumulative_allowed_avg_passing_yards_per_attempt * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_avg_yards_per_play = cumulative_allowed_avg_yards_per_play * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_first_down_efficiency = cumulative_allowed_first_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_forth_down_efficiency = cumulative_allowed_forth_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_pass_attempts = cumulative_allowed_pass_attempts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_pass_comp_attempts = cumulative_allowed_pass_comp_attempts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_rush_attempts = cumulative_allowed_rush_attempts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_second_down_efficiency = cumulative_allowed_second_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_third_down_efficiency = cumulative_allowed_third_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_first_down_converted = cumulative_allowed_total_first_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_first_downs = cumulative_allowed_total_first_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_forth_down_converted = cumulative_allowed_total_forth_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_forth_downs = cumulative_allowed_total_forth_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_pass_completes = cumulative_allowed_total_pass_completes * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_pass_inccompletes = cumulative_allowed_total_pass_inccompletes * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_passing_plays_over_10_yards = cumulative_allowed_total_passing_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_passing_plays_over_15_yards = cumulative_allowed_total_passing_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_passing_plays_over_20_yards = cumulative_allowed_total_passing_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_passing_plays_over_25_yards = cumulative_allowed_total_passing_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_passing_plays_over_5_yards = cumulative_allowed_total_passing_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_passing_touchdowns = cumulative_allowed_total_passing_touchdowns * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_passing_yards = cumulative_allowed_total_passing_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_plays = cumulative_allowed_total_plays * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_plays_over_10_yards = cumulative_allowed_total_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_plays_over_15_yards = cumulative_allowed_total_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_plays_over_20_yards = cumulative_allowed_total_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_plays_over_25_yards = cumulative_allowed_total_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_plays_over_5_yards = cumulative_allowed_total_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_rushing_plays_over_10_yards = cumulative_allowed_total_rushing_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_rushing_plays_over_15_yards = cumulative_allowed_total_rushing_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_rushing_plays_over_20_yards = cumulative_allowed_total_rushing_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_rushing_plays_over_25_yards = cumulative_allowed_total_rushing_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_rushing_plays_over_5_yards = cumulative_allowed_total_rushing_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_rushing_touchdowns = cumulative_allowed_total_rushing_touchdowns * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_rushing_yards = cumulative_allowed_total_rushing_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_second_down_converted = cumulative_allowed_total_second_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_second_downs = cumulative_allowed_total_second_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_third_down_converted = cumulative_allowed_total_third_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_third_downs = cumulative_allowed_total_third_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_allowed_total_yards = cumulative_allowed_total_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_complete_passes_per_game = cumulative_avg_complete_passes_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_pass_attempts_per_game = cumulative_avg_pass_attempts_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_avg_passing_yards_per_attempt = cumulative_avg_per_game_allowed_avg_passing_yards_per_attempt * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_avg_yards_per_play = cumulative_avg_per_game_allowed_avg_yards_per_play * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_first_down_efficiency = cumulative_avg_per_game_allowed_first_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_forth_down_efficiency = cumulative_avg_per_game_allowed_forth_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_pass_attempts = cumulative_avg_per_game_allowed_pass_attempts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_pass_comp_attempts = cumulative_avg_per_game_allowed_pass_comp_attempts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_rush_attempts = cumulative_avg_per_game_allowed_rush_attempts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_second_down_efficiency = cumulative_avg_per_game_allowed_second_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_third_down_efficiency = cumulative_avg_per_game_allowed_third_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_first_down_converted = cumulative_avg_per_game_allowed_total_first_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_first_downs = cumulative_avg_per_game_allowed_total_first_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_forth_down_converted = cumulative_avg_per_game_allowed_total_forth_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_forth_downs = cumulative_avg_per_game_allowed_total_forth_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_pass_completes = cumulative_avg_per_game_allowed_total_pass_completes * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_pass_inccompletes = cumulative_avg_per_game_allowed_total_pass_inccompletes * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_passing_plays_over_10_yards = cumulative_avg_per_game_allowed_total_passing_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_passing_plays_over_15_yards = cumulative_avg_per_game_allowed_total_passing_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_passing_plays_over_20_yards = cumulative_avg_per_game_allowed_total_passing_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_passing_plays_over_25_yards = cumulative_avg_per_game_allowed_total_passing_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_passing_plays_over_5_yards = cumulative_avg_per_game_allowed_total_passing_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_passing_touchdowns = cumulative_avg_per_game_allowed_total_passing_touchdowns * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_passing_yards = cumulative_avg_per_game_allowed_total_passing_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_plays = cumulative_avg_per_game_allowed_total_plays * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_plays_over_10_yards = cumulative_avg_per_game_allowed_total_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_plays_over_15_yards = cumulative_avg_per_game_allowed_total_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_plays_over_20_yards = cumulative_avg_per_game_allowed_total_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_plays_over_25_yards = cumulative_avg_per_game_allowed_total_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_plays_over_5_yards = cumulative_avg_per_game_allowed_total_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_rushing_plays_over_10_yards = cumulative_avg_per_game_allowed_total_rushing_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_rushing_plays_over_15_yards = cumulative_avg_per_game_allowed_total_rushing_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_rushing_plays_over_20_yards = cumulative_avg_per_game_allowed_total_rushing_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_rushing_plays_over_25_yards = cumulative_avg_per_game_allowed_total_rushing_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_rushing_plays_over_5_yards = cumulative_avg_per_game_allowed_total_rushing_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_rushing_touchdowns = cumulative_avg_per_game_allowed_total_rushing_touchdowns * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_rushing_yards = cumulative_avg_per_game_allowed_total_rushing_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_second_down_converted = cumulative_avg_per_game_allowed_total_second_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_second_downs = cumulative_avg_per_game_allowed_total_second_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_third_down_converted = cumulative_avg_per_game_allowed_total_third_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_third_downs = cumulative_avg_per_game_allowed_total_third_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_allowed_total_yards = cumulative_avg_per_game_allowed_total_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_fg_comp_attempts = cumulative_avg_per_game_fg_comp_attempts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_first_down_efficicency = cumulative_avg_per_game_first_down_efficicency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_forced_total_fumbles = cumulative_avg_per_game_forced_total_fumbles * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_forced_total_interceptions = cumulative_avg_per_game_forced_total_interceptions * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_forced_total_turnovers = cumulative_avg_per_game_forced_total_turnovers * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_fourth_down_efficicency = cumulative_avg_per_game_fourth_down_efficicency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_redzone_fg_efficiency = cumulative_avg_per_game_redzone_fg_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_redzone_scoring_efficiency = cumulative_avg_per_game_redzone_scoring_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_redzone_touchdown_efficiency = cumulative_avg_per_game_redzone_touchdown_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_second_down_efficicency = cumulative_avg_per_game_second_down_efficicency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_third_down_efficicency = cumulative_avg_per_game_third_down_efficicency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_times_sacked = cumulative_avg_per_game_times_sacked * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_drives_redzone_fg = cumulative_avg_per_game_total_drives_redzone_fg * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_fg_attempts = cumulative_avg_per_game_total_fg_attempts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_fg_blocked = cumulative_avg_per_game_total_fg_blocked * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_fg_good = cumulative_avg_per_game_total_fg_good * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_fourth_down_converted = cumulative_avg_per_game_total_fourth_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_fourth_downs = cumulative_avg_per_game_total_fourth_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_fumbles = cumulative_avg_per_game_total_fumbles * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_interceptions = cumulative_avg_per_game_total_interceptions * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_passing_plays_over_5_yards = cumulative_avg_per_game_total_passing_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_penalites_against_yards = cumulative_avg_per_game_total_penalites_against_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_penalties_against = cumulative_avg_per_game_total_penalties_against * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_penalties_for = cumulative_avg_per_game_total_penalties_for * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_penalties_for_yards = cumulative_avg_per_game_total_penalties_for_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_punts = cumulative_avg_per_game_total_punts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_punts_blocked = cumulative_avg_per_game_total_punts_blocked * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_rushing_plays_over_15_yards = cumulative_avg_per_game_total_rushing_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_rushing_plays_over_20_yards = cumulative_avg_per_game_total_rushing_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_rushing_plays_over_25_yards = cumulative_avg_per_game_total_rushing_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_rushing_plays_over_5_yards = cumulative_avg_per_game_total_rushing_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_second_downs = cumulative_avg_per_game_total_second_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_third_down_converted = cumulative_avg_per_game_total_third_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_third_downs = cumulative_avg_per_game_total_third_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_per_game_total_turnovers = cumulative_avg_per_game_total_turnovers * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_plays_per_game = cumulative_avg_plays_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_possession_time_per_game = cumulative_avg_possession_time_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_possessions_per_game = cumulative_avg_possessions_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_rush_attempts_per_game = cumulative_avg_rush_attempts_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_rushing_yards_per_game = cumulative_avg_rushing_yards_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_time_per_game = cumulative_avg_time_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_time_per_play = cumulative_avg_time_per_play * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_time_possession = cumulative_avg_time_possession * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_yards_per_complete_pass = cumulative_avg_yards_per_complete_pass * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_yards_per_pass_per_game = cumulative_avg_yards_per_pass_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_avg_yards_per_play_per_game = cumulative_avg_yards_per_play_per_game * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_fg_comp_attempts = cumulative_fg_comp_attempts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_forced_times_sacked = cumulative_forced_times_sacked * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_forced_total_fumbles = cumulative_forced_total_fumbles * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_forced_total_interceptions = cumulative_forced_total_interceptions * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_forced_total_turnovers = cumulative_forced_total_turnovers * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_fourth_down_efficicency = cumulative_fourth_down_efficicency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_games_played = cumulative_games_played * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_points_allowed = cumulative_points_allowed * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_redzone_fg_efficiency = cumulative_redzone_fg_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_times_sacked = cumulative_times_sacked * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_complete_pass = cumulative_total_complete_pass * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_drives_redzone = cumulative_total_drives_redzone * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_drives_redzone_fg = cumulative_total_drives_redzone_fg * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_fg_attempts = cumulative_total_fg_attempts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_fg_blocked = cumulative_total_fg_blocked * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_fg_good = cumulative_total_fg_good * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_first_down_converted = cumulative_total_first_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_first_downs = cumulative_total_first_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_fourth_down_converted = cumulative_total_fourth_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_fourth_downs = cumulative_total_fourth_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_fumbles = cumulative_total_fumbles * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_incomplete_pass = cumulative_total_incomplete_pass * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_interceptions = cumulative_total_interceptions * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_pass_attempts = cumulative_total_pass_attempts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_passing_plays_over_10_yards = cumulative_total_passing_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_passing_plays_over_15_yards = cumulative_total_passing_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_passing_plays_over_20_yards = cumulative_total_passing_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_passing_plays_over_25_yards = cumulative_total_passing_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_passing_plays_over_5_yards = cumulative_total_passing_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_passing_yards = cumulative_total_passing_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_penalites_against_yards = cumulative_total_penalites_against_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_penalties_against = cumulative_total_penalties_against * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_penalties_for = cumulative_total_penalties_for * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_penalties_for_yards = cumulative_total_penalties_for_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_plays = cumulative_total_plays * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_plays_over_10_yards = cumulative_total_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_plays_over_15_yards = cumulative_total_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_plays_over_20_yards = cumulative_total_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_plays_over_25_yards = cumulative_total_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_plays_over_5_yards = cumulative_total_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_possessions = cumulative_total_possessions * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_punts = cumulative_total_punts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_punts_blocked = cumulative_total_punts_blocked * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_rushing_attempts = cumulative_total_rushing_attempts * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_rushing_plays_over_10_yards = cumulative_total_rushing_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_rushing_plays_over_15_yards = cumulative_total_rushing_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_rushing_plays_over_20_yards = cumulative_total_rushing_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_rushing_plays_over_25_yards = cumulative_total_rushing_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_rushing_plays_over_5_yards = cumulative_total_rushing_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_rushing_yards = cumulative_total_rushing_yards * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_second_down_converted = cumulative_total_second_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_second_downs = cumulative_total_second_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_third_down_converted = cumulative_total_third_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_third_downs = cumulative_total_third_downs * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_time_possession_mins = cumulative_total_time_possession_mins * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_turnovers = cumulative_total_turnovers * cumulative_game_win_efficiency_opponent,
      weighted_cumulative_total_yards = cumulative_total_yards * cumulative_game_win_efficiency_opponent,
      weighted_fg_comp_attempts = fg_comp_attempts * cumulative_game_win_efficiency_opponent,
      weighted_first_down_efficiency = first_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_forced_times_sacked = forced_times_sacked * cumulative_game_win_efficiency_opponent,
      weighted_forced_total_fumbles = forced_total_fumbles * cumulative_game_win_efficiency_opponent,
      weighted_forced_total_interceptions = forced_total_interceptions * cumulative_game_win_efficiency_opponent,
      weighted_forced_total_turnovers = forced_total_turnovers * cumulative_game_win_efficiency_opponent,
      weighted_fourth_down_efficiency = fourth_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_pass_attempts = pass_attempts * cumulative_game_win_efficiency_opponent,
      weighted_pass_comp_attempts = pass_comp_attempts * cumulative_game_win_efficiency_opponent,
      weighted_rush_attempts = rush_attempts * cumulative_game_win_efficiency_opponent,
      weighted_second_down_efficiency = second_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_third_down_efficiency = third_down_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_times_sacked = times_sacked * cumulative_game_win_efficiency_opponent,
      weighted_total_drives_redzone = total_drives_redzone * cumulative_game_win_efficiency_opponent,
      weighted_total_drives_redzone_fg = total_drives_redzone_fg * cumulative_game_win_efficiency_opponent,
      weighted_total_drives_redzone_scoring = total_drives_redzone_scoring * cumulative_game_win_efficiency_opponent,
      weighted_total_drives_redzone_touchdown = total_drives_redzone_touchdown * cumulative_game_win_efficiency_opponent,
      weighted_total_fg_attempts = total_fg_attempts * cumulative_game_win_efficiency_opponent,
      weighted_total_fg_blocked = total_fg_blocked * cumulative_game_win_efficiency_opponent,
      weighted_total_fg_good = total_fg_good * cumulative_game_win_efficiency_opponent,
      weighted_total_first_down_converted = total_first_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_total_first_downs = total_first_downs * cumulative_game_win_efficiency_opponent,
      weighted_total_fourth_down_converted = total_fourth_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_total_fourth_downs = total_fourth_downs * cumulative_game_win_efficiency_opponent,
      weighted_total_fumbles = total_fumbles * cumulative_game_win_efficiency_opponent,
      weighted_total_interceptions = total_interceptions * cumulative_game_win_efficiency_opponent,
      weighted_total_pass_completes = total_pass_completes * cumulative_game_win_efficiency_opponent,
      weighted_total_pass_inccompletes = total_pass_inccompletes * cumulative_game_win_efficiency_opponent,
      weighted_total_passing_plays_over_10_yards = total_passing_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_passing_plays_over_15_yards = total_passing_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_passing_plays_over_20_yards = total_passing_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_passing_plays_over_25_yards = total_passing_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_passing_plays_over_5_yards = total_passing_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_passing_touchdowns = total_passing_touchdowns * cumulative_game_win_efficiency_opponent,
      weighted_total_passing_yards = total_passing_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_penalites_against_yards = total_penalites_against_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_penalties_against = total_penalties_against * cumulative_game_win_efficiency_opponent,
      weighted_total_penalties_for = total_penalties_for * cumulative_game_win_efficiency_opponent,
      weighted_total_penalties_for_yards = total_penalties_for_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_plays = total_plays * cumulative_game_win_efficiency_opponent,
      weighted_total_plays_over_10_yards = total_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_plays_over_15_yards = total_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_plays_over_20_yards = total_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_plays_over_25_yards = total_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_plays_over_5_yards = total_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_possessions = total_possessions * cumulative_game_win_efficiency_opponent,
      weighted_total_punts = total_punts * cumulative_game_win_efficiency_opponent,
      weighted_total_punts_blocked = total_punts_blocked * cumulative_game_win_efficiency_opponent,
      weighted_total_redzone_fg_efficiency = total_redzone_fg_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_total_redzone_scoring_efficiency = total_redzone_scoring_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_total_redzone_touchdown_efficiency = total_redzone_touchdown_efficiency * cumulative_game_win_efficiency_opponent,
      weighted_total_rushing_plays_over_10_yards = total_rushing_plays_over_10_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_rushing_plays_over_15_yards = total_rushing_plays_over_15_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_rushing_plays_over_20_yards = total_rushing_plays_over_20_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_rushing_plays_over_25_yards = total_rushing_plays_over_25_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_rushing_plays_over_5_yards = total_rushing_plays_over_5_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_rushing_touchdowns = total_rushing_touchdowns * cumulative_game_win_efficiency_opponent,
      weighted_total_rushing_yards = total_rushing_yards * cumulative_game_win_efficiency_opponent,
      weighted_total_second_down_converted = total_second_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_total_second_downs = total_second_downs * cumulative_game_win_efficiency_opponent,
      weighted_total_third_down_converted = total_third_down_converted * cumulative_game_win_efficiency_opponent,
      weighted_total_third_downs = total_third_downs * cumulative_game_win_efficiency_opponent,
      weighted_total_time_possession_mins = total_time_possession_mins * cumulative_game_win_efficiency_opponent,
      weighted_total_turnovers = total_turnovers * cumulative_game_win_efficiency_opponent,
      
    )
  
  # Add this step too since win and loss matter so much
  joined_data_with_cumulative_data_opponent_dat$weighted_win_flag <- ifelse(is.na(joined_data_with_cumulative_data_opponent_dat$weighted_win_flag),0,joined_data_with_cumulative_data_opponent_dat$weighted_win_flag)
  joined_data_with_cumulative_data_opponent_dat$weighted_loss_flag <- ifelse(is.na(joined_data_with_cumulative_data_opponent_dat$weighted_loss_flag),0,joined_data_with_cumulative_data_opponent_dat$weighted_loss_flag)
  
  
  joined_data_with_cumulative_data_opponent_datcumavg <- joined_data_with_cumulative_data_opponent_dat %>%
    arrange(pos_team,year,week) %>%
    group_by(pos_team,year) %>%
    mutate(
      # Some basic stats cumulative
      w_weighted_cumulative_games_won = cumsum(ifelse(is.na(weighted_win_flag), 0, weighted_win_flag)),
      w_weighted_cumulative_games_lost = cumsum(ifelse(is.na(weighted_loss_flag), 0, weighted_loss_flag)),
      w_weighted_cumulative_game_win_efficiency = w_weighted_cumulative_games_won / cumulative_games_played,
      
      
      ### cumulative Points and weighted per game
      # points per game 
      w_weighted_cumulative_points_scored = cumsum(ifelse(is.na(weighted_points_scored), 0, weighted_points_scored)),
      w_weighted_cumulative_points_allowed = cumsum(ifelse(is.na(weighted_points_allowed), 0, weighted_points_allowed)),
      w_weighted_cumulative_avg_points_per_game = w_weighted_cumulative_points_scored / cumulative_games_played,
      w_weighted_cumulative_avg_points_allowed_per_game = w_weighted_cumulative_points_allowed / cumulative_games_played,
      
      # cumulative yards
      w_weighted_avg_weighted_points_scored = cumsum(ifelse(is.na(avg_weighted_points_scored), 0, avg_weighted_points_scored)),   
      w_weighted_weighted_total_yards = cumsum(ifelse(is.na(weighted_total_yards), 0, weighted_total_yards)),
      w_weighted_avg_weighted_total_yards = cumsum(ifelse(is.na(avg_weighted_total_yards), 0, avg_weighted_total_yards)),
      w_weighted_avg_weighted_total_passing_yards = cumsum(ifelse(is.na(avg_weighted_total_passing_yards), 0, avg_weighted_total_passing_yards)),
      w_weighted_avg_weighted_total_rushing_yards = cumsum(ifelse(is.na(avg_weighted_total_rushing_yards), 0, avg_weighted_total_rushing_yards)), 
      w_weighted_weighted_total_scoring_plays = cumsum(ifelse(is.na(weighted_total_scoring_plays), 0, weighted_total_scoring_plays)),
      w_weighted_weighted_avg_yards_per_play = cumsum(ifelse(is.na(weighted_avg_yards_per_play), 0, weighted_avg_yards_per_play)),
      w_weighted_weighted_win_flag_opponent = cumsum(ifelse(is.na(weighted_win_flag_opponent), 0, weighted_win_flag_opponent)),
      w_weighted_weighted_points_scored_opponent = cumsum(ifelse(is.na(weighted_points_scored_opponent), 0, weighted_points_scored_opponent)),
      w_weighted_weighted_total_scoring_plays_opponent = cumsum(ifelse(is.na(weighted_total_scoring_plays_opponent), 0, weighted_total_scoring_plays_opponent)),
      w_weighted_weighted_allowed_total_yards = cumsum(ifelse(is.na(weighted_allowed_total_yards), 0, weighted_allowed_total_yards)),
      w_weighted_weighted_total_yards_opponent = cumsum(ifelse(is.na(weighted_total_yards_opponent), 0, weighted_total_yards_opponent)),
      w_weighted_avg_weighted_pass_attempts = cumsum(ifelse(is.na(avg_weighted_pass_attempts), 0, avg_weighted_pass_attempts)),
      w_weighted_avg_weighted_total_pass_completes = cumsum(ifelse(is.na(avg_weighted_total_pass_completes), 0, avg_weighted_total_pass_completes)),
      w_weighted_avg_weighted_total_passing_touchdowns = cumsum(ifelse(is.na(avg_weighted_total_passing_touchdowns), 0, avg_weighted_total_passing_touchdowns)),
      w_weighted_avg_weighted_rush_attempts = cumsum(ifelse(is.na(avg_weighted_rush_attempts), 0, avg_weighted_rush_attempts)),
      w_weighted_avg_weighted_total_rushing_touchdowns = cumsum(ifelse(is.na(avg_weighted_total_rushing_touchdowns), 0, avg_weighted_total_rushing_touchdowns)),
      w_weighted_avg_weighted_first_down_efficiency = cumsum(ifelse(is.na(avg_weighted_first_down_efficiency), 0, avg_weighted_first_down_efficiency)),
      w_weighted_avg_weighted_second_down_efficiency = cumsum(ifelse(is.na(avg_weighted_second_down_efficiency), 0, avg_weighted_second_down_efficiency)),
      w_weighted_avg_weighted_third_down_efficiency = cumsum(ifelse(is.na(avg_weighted_third_down_efficiency), 0, avg_weighted_third_down_efficiency)),
      w_weighted_avg_weighted_points_allowed = cumsum(ifelse(is.na(avg_weighted_points_allowed), 0, avg_weighted_points_allowed)),
      w_weighted_avg_weighted_allowed_total_passing_yards = cumsum(ifelse(is.na(avg_weighted_allowed_total_passing_yards), 0, avg_weighted_allowed_total_passing_yards)),
      w_weighted_avg_weighted_allowed_total_rushing_yards = cumsum(ifelse(is.na(avg_weighted_allowed_total_rushing_yards), 0, avg_weighted_allowed_total_rushing_yards)),
      w_weighted_avg_weighted_allowed_total_yards = cumsum(ifelse(is.na(avg_weighted_allowed_total_yards), 0, avg_weighted_allowed_total_yards)),
      w_weighted_avg_weighted_allowed_total_pass_completes = cumsum(ifelse(is.na(avg_weighted_allowed_total_pass_completes), 0, avg_weighted_allowed_total_pass_completes)),
      w_weighted_avg_weighted_allowed_pass_attempts = cumsum(ifelse(is.na(avg_weighted_allowed_pass_attempts), 0, avg_weighted_allowed_pass_attempts)),
      w_weighted_avg_weighted_allowed_total_passing_touchdowns = cumsum(ifelse(is.na(avg_weighted_allowed_total_passing_touchdowns), 0, avg_weighted_allowed_total_passing_touchdowns)),
      w_weighted_avg_weighted_allowed_rush_attempts = cumsum(ifelse(is.na(avg_weighted_allowed_rush_attempts), 0, avg_weighted_allowed_rush_attempts)),
      w_weighted_avg_weighted_allowed_total_rushing_touchdowns = cumsum(ifelse(is.na(avg_weighted_allowed_total_rushing_touchdowns), 0, avg_weighted_allowed_total_rushing_touchdowns)),
      w_weighted_avg_weighted_allowed_total_plays = cumsum(ifelse(is.na(avg_weighted_allowed_total_plays), 0, avg_weighted_allowed_total_plays)),
      
      
      cumulative_w_weighted_avg_weighted_points_scored = w_weighted_avg_weighted_points_scored/ cumulative_games_played,
      cumulative_w_weighted_weighted_total_yards = w_weighted_weighted_total_yards/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_total_yards = w_weighted_avg_weighted_total_yards/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_total_passing_yards = w_weighted_avg_weighted_total_passing_yards/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_total_rushing_yards = w_weighted_avg_weighted_total_rushing_yards/ cumulative_games_played,
      cumulative_w_weighted_weighted_total_scoring_plays = w_weighted_weighted_total_scoring_plays/ cumulative_games_played,
      cumulative_w_weighted_weighted_avg_yards_per_play = w_weighted_weighted_avg_yards_per_play/ cumulative_games_played,
      cumulative_w_weighted_weighted_win_flag_opponent = w_weighted_weighted_win_flag_opponent/ cumulative_games_played,
      cumulative_w_weighted_weighted_points_scored_opponent = w_weighted_weighted_points_scored_opponent/ cumulative_games_played,
      cumulative_w_weighted_weighted_total_scoring_plays_opponent = w_weighted_weighted_total_scoring_plays_opponent/ cumulative_games_played,
      cumulative_w_weighted_weighted_allowed_total_yards = w_weighted_weighted_allowed_total_yards/ cumulative_games_played,
      cumulative_w_weighted_weighted_total_yards_opponent = w_weighted_weighted_total_yards_opponent/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_pass_attempts = w_weighted_avg_weighted_pass_attempts/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_total_pass_completes = w_weighted_avg_weighted_total_pass_completes/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_total_passing_touchdowns = w_weighted_avg_weighted_total_passing_touchdowns/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_rush_attempts = w_weighted_avg_weighted_rush_attempts/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_total_rushing_touchdowns = w_weighted_avg_weighted_total_rushing_touchdowns/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_first_down_efficiency = w_weighted_avg_weighted_first_down_efficiency/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_second_down_efficiency = w_weighted_avg_weighted_second_down_efficiency/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_third_down_efficiency = w_weighted_avg_weighted_third_down_efficiency/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_points_allowed = w_weighted_avg_weighted_points_allowed/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_allowed_total_passing_yards = w_weighted_avg_weighted_allowed_total_passing_yards/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_allowed_total_rushing_yards = w_weighted_avg_weighted_allowed_total_rushing_yards/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_allowed_total_yards = w_weighted_avg_weighted_allowed_total_yards/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_allowed_total_pass_completes = w_weighted_avg_weighted_allowed_total_pass_completes/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_allowed_pass_attempts = w_weighted_avg_weighted_allowed_pass_attempts/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_allowed_total_passing_touchdowns = w_weighted_avg_weighted_allowed_total_passing_touchdowns/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_allowed_rush_attempts = w_weighted_avg_weighted_allowed_rush_attempts/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_allowed_total_rushing_touchdowns = w_weighted_avg_weighted_allowed_total_rushing_touchdowns/ cumulative_games_played,
      cumulative_w_weighted_avg_weighted_allowed_total_plays = w_weighted_avg_weighted_allowed_total_plays/ cumulative_games_played,
      
    ) %>%
    ungroup()

  return(joined_data_with_cumulative_data_opponent_datcumavg)
  
}

# 7.) Join the AP poll data
joined_ap_poll <- function(joined_data_with_cumulative_data_opponent_datcumavg) {
  AP_Poll_Votes <- load_rankings()
  
  # Join the data
  joined_data_poll <- joined_data_with_cumulative_data_opponent_datcumavg %>%
    left_join(AP_Poll_Votes, by = c("pos_team", "week", "year"))
  
  # Lets do a few quick adjustments to the vote data. If is NA, really should be 0
  # ALSO going to create a POINTS for analytical purposes by setting 0 to be .1, but keep the 0 for real points for filtering FBS conferences below
  joined_data_poll$points[is.na(joined_data_poll$points)] <- 0
  joined_data_poll$analytical_points <- ifelse(joined_data_poll$points == 0, .1, joined_data_poll$points)
  
  ########## FILTER CONFERENCES BASED ON VOTING POINTS HISTORY!!!!! 
  # Also noticing that the model is bad, so lets just focus on Power Conferences
  conference_include_list <- joined_data_poll %>% group_by(conference) %>% summarise( total_points = sum (points) ) %>% filter(total_points > .1) %>% pull(conference)
  
  joined_data_poll_FBS_filter <- joined_data_poll %>%
    filter(conference %in% conference_include_list)
  
  # First lets try to narrow down the data set to those of most influence on the DV and strongest correlations 
  # First get the log of the analytically points for count data
  joined_data_poll_FBS_filter$log_votes <- log(joined_data_poll_FBS_filter$analytical_points)
  
  return(joined_data_poll_FBS_filter)
  
}

# 8.) Final lag calculations 
final_lag <- function(joined_data_poll_FBS_filter) {
  
  laggedjoined_data_poll_FBS_filter <- joined_data_poll_FBS_filter %>% 
    arrange(pos_team,year,week) %>%
    group_by(pos_team,year) %>%
    mutate(lagged_points = dplyr::lag(points),
           lagged_points = ifelse(is.na(lagged_points), 0, lagged_points),
           
           lagged_analytical_points = dplyr::lag(analytical_points),
           lagged_analytical_points = ifelse(is.na(lagged_analytical_points), 0.1, lagged_analytical_points),
           
           lagged_log_votes = dplyr::lag(log_votes),
           lagged_log_votes = ifelse(is.na(lagged_log_votes), log(0.1), lagged_log_votes)
    ) %>%
    ungroup()
  
  cross_join_ap_votes <- laggedjoined_data_poll_FBS_filter %>%
    dplyr::left_join(laggedjoined_data_poll_FBS_filter,
                     by = c("opponent" = "pos_team", "week" = "week", "year" = "year"),
                     suffix = c("","_opponent"))
  
  lagged_opponent_ap_votes_dat <- cross_join_ap_votes %>% select(c(pos_team, week, year, lagged_log_votes_opponent ))
  
  laggedjoined_data_poll_FBS_filter_joined_lagged <- laggedjoined_data_poll_FBS_filter %>%
    dplyr::left_join(lagged_opponent_ap_votes_dat, by = c("pos_team","week","year"))
  
  
  
  # Calculate some basic stats weighted by AP votes, use the LOG votes lagged
  ap_weighted_dat <- laggedjoined_data_poll_FBS_filter_joined_lagged %>% 
    mutate(
      ap_weighted_points_scored = points_scored * lagged_log_votes_opponent,
      ap_weighted_points_allowed = points_allowed * lagged_log_votes_opponent,
      ap_weighted_point_dif = point_dif * lagged_log_votes_opponent,
      ap_weighted_win_flag = win_flag * lagged_log_votes_opponent,
      ap_weighted_loss_flag = loss_flag * lagged_log_votes_opponent,
      ap_weighted_total_passing_yards = total_passing_yards * lagged_log_votes_opponent,
      ap_weighted_total_rushing_yards = total_rushing_yards * lagged_log_votes_opponent,
      ap_weighted_total_yards = total_yards * lagged_log_votes_opponent,
      ap_weighted_total_pass_completes = total_pass_completes * lagged_log_votes_opponent,
      ap_weighted_avg_passing_yards_per_attempt = avg_passing_yards_per_attempt * lagged_log_votes_opponent,
      ap_weighted_total_passing_touchdowns = total_passing_touchdowns * lagged_log_votes_opponent,
      ap_weighted_avg_rush_yards_per_attempt = avg_rush_yards_per_attempt * lagged_log_votes_opponent,
      ap_weighted_total_rushing_touchdowns = total_rushing_touchdowns * lagged_log_votes_opponent,
      ap_weighted_total_plays = total_plays * lagged_log_votes_opponent,
      ap_weighted_avg_yards_per_play = avg_yards_per_play * lagged_log_votes_opponent,
      ap_weighted_total_redzone_scoring_efficiency = total_redzone_scoring_efficiency * lagged_log_votes_opponent,
      ap_weighted_fourth_down_efficiency = fourth_down_efficiency * lagged_log_votes_opponent,
      
      ap_weighted_allowed_total_passing_yards = allowed_total_passing_yards * lagged_log_votes_opponent,
      ap_weighted_allowed_total_rushing_yards =  allowed_total_rushing_yards * lagged_log_votes_opponent,
      ap_weighted_allowed_total_yards = allowed_total_yards * lagged_log_votes_opponent,
      ap_weighted_allowed_total_pass_completes = allowed_total_pass_completes * lagged_log_votes_opponent,
      ap_weighted_allowed_total_passing_touchdowns = allowed_total_passing_touchdowns * lagged_log_votes_opponent,
      ap_weighted_allowed_rush_attempts = allowed_rush_attempts * lagged_log_votes_opponent,
      ap_weighted_allowed_total_rushing_touchdowns = allowed_total_rushing_touchdowns * lagged_log_votes_opponent,
      ap_weighted_allowed_forth_down_efficiency = allowed_forth_down_efficiency * lagged_log_votes_opponent
      
    )
  
  # New section for this
  ap_weighted_dat$ap_weighted_win_flag <- ifelse(is.na(ap_weighted_dat$ap_weighted_win_flag),0,ap_weighted_dat$ap_weighted_win_flag)
  ap_weighted_dat$ap_weighted_loss_flag <- ifelse(is.na(ap_weighted_dat$ap_weighted_loss_flag),0,ap_weighted_dat$ap_weighted_loss_flag)
  
  cumulative_ap_weighted_dat <- ap_weighted_dat %>%
    arrange(pos_team,year,week) %>%
    group_by(pos_team,year) %>%
    mutate(
      # Some basic stats cumulative
      cumulative_ap_weighted_points_scored = cumsum(ifelse(is.na( ap_weighted_points_scored ), 0, ap_weighted_points_scored)),
      cumulative_ap_weighted_points_scored_per_game_avg = cumulative_ap_weighted_points_scored / cumulative_games_played,
      cumulative_ap_weighted_points_allowed = cumsum(ifelse(is.na( ap_weighted_points_allowed ), 0, ap_weighted_points_allowed)),
      cumulative_ap_weighted_points_allowed_per_game_avg = cumulative_ap_weighted_points_allowed / cumulative_games_played,
      cumulative_ap_weighted_point_dif = cumsum(ifelse(is.na( ap_weighted_point_dif ), 0, ap_weighted_point_dif)),
      cumulative_ap_weighted_point_dif_per_game_avg = cumulative_ap_weighted_point_dif / cumulative_games_played,
      cumulative_ap_weighted_win_flag = cumsum(ifelse(is.na( ap_weighted_win_flag ), 0, ap_weighted_win_flag)),
      cumulative_ap_weighted_win_flag_per_game_avg = cumulative_ap_weighted_win_flag / cumulative_games_played,
      cumulative_ap_weighted_loss_flag = cumsum(ifelse(is.na( ap_weighted_loss_flag ), 0, ap_weighted_loss_flag)),
      cumulative_ap_weighted_loss_flag_per_game_avg = cumulative_ap_weighted_loss_flag / cumulative_games_played,
      cumulative_ap_weighted_total_passing_yards = cumsum(ifelse(is.na( ap_weighted_total_passing_yards ), 0, ap_weighted_total_passing_yards)),
      cumulative_ap_weighted_total_passing_yards_per_game_avg = cumulative_ap_weighted_total_passing_yards / cumulative_games_played,
      cumulative_ap_weighted_total_rushing_yards = cumsum(ifelse(is.na( ap_weighted_total_rushing_yards ), 0, ap_weighted_total_rushing_yards)),
      cumulative_ap_weighted_total_rushing_yards_per_game_avg = cumulative_ap_weighted_total_rushing_yards / cumulative_games_played,
      cumulative_ap_weighted_total_yards = cumsum(ifelse(is.na( ap_weighted_total_yards ), 0, ap_weighted_total_yards)),
      cumulative_ap_weighted_total_yards_per_game_avg = cumulative_ap_weighted_total_yards / cumulative_games_played,
      cumulative_ap_weighted_total_pass_completes = cumsum(ifelse(is.na( ap_weighted_total_pass_completes ), 0, ap_weighted_total_pass_completes)),
      cumulative_ap_weighted_total_pass_completes_per_game_avg = cumulative_ap_weighted_total_pass_completes / cumulative_games_played,
      cumulative_ap_weighted_avg_passing_yards_per_attempt = cumsum(ifelse(is.na( ap_weighted_avg_passing_yards_per_attempt ), 0, ap_weighted_avg_passing_yards_per_attempt)),
      cumulative_ap_weighted_avg_passing_yards_per_attempt_per_game_avg = cumulative_ap_weighted_avg_passing_yards_per_attempt / cumulative_games_played,
      cumulative_ap_weighted_total_passing_touchdowns = cumsum(ifelse(is.na( ap_weighted_total_passing_touchdowns ), 0, ap_weighted_total_passing_touchdowns)),
      cumulative_ap_weighted_total_passing_touchdowns_per_game_avg = cumulative_ap_weighted_total_passing_touchdowns / cumulative_games_played,
      cumulative_ap_weighted_avg_rush_yards_per_attempt = cumsum(ifelse(is.na( ap_weighted_avg_rush_yards_per_attempt ), 0, ap_weighted_avg_rush_yards_per_attempt)),
      cumulative_ap_weighted_avg_rush_yards_per_attempt_per_game_avg = cumulative_ap_weighted_avg_rush_yards_per_attempt / cumulative_games_played,
      cumulative_ap_weighted_total_rushing_touchdowns = cumsum(ifelse(is.na( ap_weighted_total_rushing_touchdowns ), 0, ap_weighted_total_rushing_touchdowns)),
      cumulative_ap_weighted_total_rushing_touchdowns_per_game_avg = cumulative_ap_weighted_total_rushing_touchdowns / cumulative_games_played,
      cumulative_ap_weighted_total_plays = cumsum(ifelse(is.na( ap_weighted_total_plays ), 0, ap_weighted_total_plays)),
      cumulative_ap_weighted_total_plays_per_game_avg = cumulative_ap_weighted_total_plays / cumulative_games_played,
      cumulative_ap_weighted_avg_yards_per_play = cumsum(ifelse(is.na( ap_weighted_avg_yards_per_play ), 0, ap_weighted_avg_yards_per_play)),
      cumulative_ap_weighted_avg_yards_per_play_per_game_avg = cumulative_ap_weighted_avg_yards_per_play / cumulative_games_played,
      cumulative_ap_weighted_total_redzone_scoring_efficiency = cumsum(ifelse(is.na( ap_weighted_total_redzone_scoring_efficiency ), 0, ap_weighted_total_redzone_scoring_efficiency)),
      cumulative_ap_weighted_total_redzone_scoring_efficiency_per_game_avg = cumulative_ap_weighted_total_redzone_scoring_efficiency / cumulative_games_played,
      cumulative_ap_weighted_fourth_down_efficiency = cumsum(ifelse(is.na( ap_weighted_fourth_down_efficiency ), 0, ap_weighted_fourth_down_efficiency)),
      cumulative_ap_weighted_fourth_down_efficiency_per_game_avg = cumulative_ap_weighted_fourth_down_efficiency / cumulative_games_played,
      cumulative_ap_weighted_allowed_total_passing_yards = cumsum(ifelse(is.na( ap_weighted_allowed_total_passing_yards ), 0, ap_weighted_allowed_total_passing_yards)),
      cumulative_ap_weighted_allowed_total_passing_yards_per_game_avg = cumulative_ap_weighted_allowed_total_passing_yards / cumulative_games_played,
      cumulative_ap_weighted_allowed_total_rushing_yards = cumsum(ifelse(is.na( ap_weighted_allowed_total_rushing_yards ), 0, ap_weighted_allowed_total_rushing_yards)),
      cumulative_ap_weighted_allowed_total_rushing_yards_per_game_avg = cumulative_ap_weighted_allowed_total_rushing_yards / cumulative_games_played,
      cumulative_ap_weighted_allowed_total_yards = cumsum(ifelse(is.na( ap_weighted_allowed_total_yards ), 0, ap_weighted_allowed_total_yards)),
      cumulative_ap_weighted_allowed_total_yards_per_game_avg = cumulative_ap_weighted_allowed_total_yards / cumulative_games_played,
      cumulative_ap_weighted_allowed_total_pass_completes = cumsum(ifelse(is.na( ap_weighted_allowed_total_pass_completes ), 0, ap_weighted_allowed_total_pass_completes)),
      cumulative_ap_weighted_allowed_total_pass_completes_per_game_avg = cumulative_ap_weighted_allowed_total_pass_completes / cumulative_games_played,
      cumulative_ap_weighted_allowed_total_passing_touchdowns = cumsum(ifelse(is.na( ap_weighted_allowed_total_passing_touchdowns ), 0, ap_weighted_allowed_total_passing_touchdowns)),
      cumulative_ap_weighted_allowed_total_passing_touchdowns_per_game_avg = cumulative_ap_weighted_allowed_total_passing_touchdowns / cumulative_games_played,
      cumulative_ap_weighted_allowed_rush_attempts = cumsum(ifelse(is.na( ap_weighted_allowed_rush_attempts ), 0, ap_weighted_allowed_rush_attempts)),
      cumulative_ap_weighted_allowed_rush_attempts_per_game_avg = cumulative_ap_weighted_allowed_rush_attempts / cumulative_games_played,
      cumulative_ap_weighted_allowed_total_rushing_touchdowns = cumsum(ifelse(is.na( ap_weighted_allowed_total_rushing_touchdowns ), 0, ap_weighted_allowed_total_rushing_touchdowns)),
      cumulative_ap_weighted_allowed_total_rushing_touchdowns_per_game_avg = cumulative_ap_weighted_allowed_total_rushing_touchdowns / cumulative_games_played,
      cumulative_ap_weighted_allowed_forth_down_efficiency = cumsum(ifelse(is.na( ap_weighted_allowed_forth_down_efficiency ), 0, ap_weighted_allowed_forth_down_efficiency)),
      cumulative_ap_weighted_allowed_forth_down_efficiency_per_game_avg = cumulative_ap_weighted_allowed_forth_down_efficiency / cumulative_games_played
      
    ) %>%
    ungroup()
  
  final_data_for_analyze <- cumulative_ap_weighted_dat %>% 
    arrange(pos_team,year,week) %>%
    group_by(pos_team,year) %>%
    mutate(lagged_win_flag = dplyr::lag(win_flag),
           lagged_points = ifelse(is.na(lagged_points), 0, lagged_points),
           
           lagged_loss_flag = dplyr::lag(loss_flag),
           lagged_loss_flag = ifelse(is.na(lagged_loss_flag), 0, lagged_loss_flag),
           
           lagged_weighted_win_flag = dplyr::lag(weighted_win_flag),
           lagged_weighted_win_flag = ifelse(is.na(lagged_weighted_win_flag), 0, lagged_weighted_win_flag),
           
           lagged_weighted_loss_flag = dplyr::lag(weighted_loss_flag),     
           lagged_weighted_loss_flag = ifelse(is.na(lagged_weighted_loss_flag), 0, lagged_weighted_loss_flag),
           
           lagged_ap_weighted_win_flag = dplyr::lag(ap_weighted_win_flag),    
           lagged_ap_weighted_win_flag = ifelse(is.na(lagged_ap_weighted_win_flag), 0, lagged_ap_weighted_win_flag),
           
           lagged_ap_weighted_loss_flag = dplyr::lag(ap_weighted_loss_flag),         
           lagged_ap_weighted_loss_flag = ifelse(is.na(lagged_ap_weighted_loss_flag), 0, lagged_ap_weighted_loss_flag),
           
    ) %>%
    ungroup()
  
  
  
  return(final_data_for_analyze)
  
}
  
# Final
# Final Function Pull All Together Data Processing + Feature Extraction 
prepare_season_data <- function(pbp) {
  # 1.) 
  offense_data <- extract_offense_features(pbp)
  # 2.)
  defense_data <- extract_defense_features(pbp)
  # 3.)
  combined_data <- join_offense_defense(offense_data, defense_data)
  # 4.)
  joined_data_cross_joined <- cross_joined_join(combined_data)
  # 5.)
  joined_cumulative_data <- join_data_cumulative(joined_data_cross_joined)
  # 6.)
  joined_cumulative_full_data <- cumulative_fully_joined(joined_cumulative_data)
  # 7.)
  joined_data_ap_poll <- joined_ap_poll(joined_cumulative_full_data)
  # 8.) 
  cumulative_weighted_data <- final_lag(joined_data_ap_poll)
  
  return(cumulative_weighted_data)
}

#=========================================== =
# Do final data PREP functions         #
#=========================================== =

# Remove missing Data
remove_missing_data <- function(analyze_dat) {
  na_percent_filter <- colMeans(is.na(analyze_dat)) * 100 
  analyze_dat_na_filtered <- analyze_dat %>% select(which(na_percent_filter < 20))
  
  # And now lets drop all rows with missing data, very few and not very relevant either way 
  cleaned_analyze_dat <- analyze_dat_na_filtered %>% na.omit()
  
  cleaned_analyze_dat <- cleaned_analyze_dat %>% select(-c(cumulative_total_penalites_against_yards, cumulative_avg_per_game_total_penalites_against_yards))
  
  return(cleaned_analyze_dat)
}

# Do PCA
run_pca_scores <- function(cleaned_analyze_dat) {
  
  # Drop data that should not be factored
  dim_red_dat <- cleaned_analyze_dat %>% select(-c(pos_team, week, year, conference, points, analytical_points, log_votes, lagged_points, lagged_analytical_points, lagged_log_votes))
  
  # Perform PCA
  # Setting to be 6, which is what seemed to be best last
  pca_facto <- PCA(dim_red_dat, scale.unit = TRUE, ncp = 6, graph = FALSE)
  
  ### Save the first 6 component scores 
  component_scores <- as.data.frame(pca_facto$ind$coord[, 1:6])
  colnames(component_scores) <- paste0("PC", 1:6)
  
  return(component_scores)
  
}
  
# Select key variables, collate, join PCA
final_collate <- function(cleaned_analyze_dat, component_scores) {
  
  selected_data <- cleaned_analyze_dat %>% select(c(pos_team, week, year, log_votes, analytical_points, lagged_log_votes, cumulative_games_won,cumulative_games_lost
                                                    ,cumulative_w_weighted_avg_weighted_allowed_total_pass_completes,cumulative_allowed_pass_comp_attempts
                                                    ,cumulative_allowed_total_pass_inccompletes,cumulative_avg_per_game_allowed_avg_passing_yards_per_attempt
                                                    ,cumulative_w_weighted_avg_weighted_allowed_total_passing_yards,cumulative_avg_per_game_allowed_total_passing_touchdowns
                                                    ,cumulative_allowed_avg_yards_per_play,cumulative_avg_per_game_fourth_down_efficicency
                                                    ,w_weighted_cumulative_game_win_efficiency, w_weighted_cumulative_games_won, w_weighted_avg_weighted_points_scored, w_weighted_cumulative_games_lost,
                                                    ,w_weighted_weighted_win_flag_opponent, w_weighted_cumulative_avg_points_allowed_per_game,cumulative_redzone_scoring_efficiency
                                                    ,cumulative_w_weighted_avg_weighted_allowed_total_pass_completes,cumulative_game_win_efficiency, cumulative_ap_weighted_allowed_total_yards
                                                    ,cumulative_ap_weighted_loss_flag,cumulative_ap_weighted_total_yards, cumulative_ap_weighted_point_dif_per_game_avg, 
                                                    
                                                    # New variables since last tuning of hyperparms
                                                    cumulative_point_dif, cumulative_point_dif_avg_per_game, cumulative_ap_weighted_point_dif, cumulative_ap_weighted_loss_flag_per_game_avg
                                                    ,w_weighted_cumulative_games_lost, 
                                                    
                                                    # These need to be lagged
                                                    #win_flag, loss_flag, weighted_win_flag, weighted_loss_flag, ap_weighted_win_flag, ap_weighted_loss_flag
                                                    
                                                    lagged_win_flag, lagged_loss_flag, lagged_weighted_win_flag, lagged_weighted_loss_flag, lagged_ap_weighted_win_flag,lagged_ap_weighted_loss_flag                                            
  ))
  
  final_dat <- bind_cols(selected_data, component_scores)
  
  return(final_dat)
  
}

# Run it all together for pipeline development
preprocesss_data <- function(analyze_dat){
  cleaned_analyze_dat <- remove_missing_data(analyze_dat)
  component_scores <- run_pca_scores(cleaned_analyze_dat)
  final_dat <- final_collate(cleaned_analyze_dat,component_scores)
  
}


#=========================================== =
# Split into test and train functions        #
#=========================================== =

# FIRST get max year and month of the most recent AP poll
# Function to get the most recent AP Poll year and week
get_most_recent_AP_poll <- function(pbp) {
  most_recent_year <- pbp %>%
    summarise(most_recent_year = max(year, na.rm = TRUE)) %>%
    pull(most_recent_year)
  
  most_recent_week <- pbp %>%
    filter(year == most_recent_year) %>%
    summarise(most_recent_week = max(week, na.rm = TRUE)) %>%
    pull(most_recent_week) + 1 # Adjust week due to lag in rankings
  
  return(list(year = most_recent_year, week = most_recent_week))
}

# And function to use most recent AP poll
split_train_test <- function(processed_data,ap_dates) {

  
  train_dat <- processed_data %>% filter((year != ap_dates$year) | (year == ap_dates$year & week < ap_dates$week))
  test_dat <- processed_data %>% filter(year == ap_dates$year & week == ap_dates$week)
  
  return(list(train = train_dat, test = test_dat))
}


#=========================================== =
# Build out the final model functions        #
#=========================================== =

ensamble_final_model_function <- function(train_test_dat,ap_dates) {
  
  # Going to set up the caret hyper parameters and tune grid
    final_model_train_control <- trainControl(method = "none")
    
    tune_grid_xgb_final_model <- expand.grid(
      nrounds = 50,
      max_depth = 4,
      eta = 0.15,
      gamma = 0,
      colsample_bytree = 0.75,
      min_child_weight = 1,
      subsample = 0.9
    )
    
    tune_grid_svm_final_model <- expand.grid(
      sigma = 0.00325,
      C = 3
    )
    
    tune_grid_gbm_final_model <- expand.grid(
      n.trees = 250,
      interaction.depth = 5,
      shrinkage = 0.04,
      n.minobsinnode = 4
    )
    
    tune_grid_ranger_final_model <- expand.grid(
      mtry = 10,
      splitrule = "extratrees",
      min.node.size = 10
    )
    
    tune_grid_glmnet_final_model <- expand.grid(
      alpha = 0.75,
      lambda = 0.0025
    )
  
  # Clear out cache, set up parallel
    gc()
    cl <- parallel::makePSOCKcluster(10)
    doParallel::registerDoParallel(cl)
    
    # Run inital models 
      xgb.fit <- caret::train(log_votes ~ . - analytical_points, data = train_test_dat$train, method = "xgbTree", trControl = final_model_train_control, tuneGrid = tune_grid_xgb_final_model)
      svm.fit <- caret::train(log_votes ~ . - analytical_points, data = train_test_dat$train, method = "svmRadial", trControl = final_model_train_control, tuneGrid = tune_grid_svm_final_model)
      gbm.fit <- caret::train(log_votes ~ . - analytical_points, data = train_test_dat$train, method = "gbm", trControl = final_model_train_control, tuneGrid = tune_grid_gbm_final_model, verbose = FALSE)
      ranger.fit <- caret::train(log_votes ~ . - analytical_points, data = train_test_dat$train, method = "ranger", trControl = final_model_train_control, tuneGrid = tune_grid_ranger_final_model)
      glmnetinter.fit <- caret::train(log_votes ~ . + pos_team * year * week * lagged_log_votes - analytical_points, data = train_test_dat$train, method = "glmnet", trControl = final_model_train_control, tuneGrid = tune_grid_glmnet_final_model)

      # Now save off predicted scores to ensamle data
      ensamble_train_df <- train_test_dat$train
      ensamble_train_df$xgb_pred <- predict(xgb.fit, newdata = train_test_dat$train)
      ensamble_train_df$svm_pred <- predict(svm.fit, newdata = train_test_dat$train)
      ensamble_train_df$gbm_pred <- predict(gbm.fit, newdata = train_test_dat$train)
      ensamble_train_df$ranger_pred <- predict(ranger.fit, newdata = train_test_dat$train)
      ensamble_train_df$glmnet_pred <- predict(glmnetinter.fit, newdata = train_test_dat$train) 
      
      # Now set up the final ensamle model
      tune_grid_ranger_ensamble <- expand.grid(
        mtry = 100,
        splitrule = "variance",
        min.node.size = 10
      )
      
      ranger.fit.final.ensamble <- caret::train(log_votes ~ . - analytical_points, data = ensamble_train_df, method = "ranger", trControl = final_model_train_control, tuneGrid = tune_grid_ranger_ensamble )
      
      # Now save off the sub models then predict using the final model
      ensamble_test_df <- train_test_dat$test
      ensamble_test_df$xgb_pred <- predict(xgb.fit, newdata = train_test_dat$test)
      ensamble_test_df$svm_pred <- predict(svm.fit, newdata = train_test_dat$test)
      ensamble_test_df$gbm_pred <- predict(gbm.fit, newdata = train_test_dat$test)
      ensamble_test_df$ranger_pred <- predict(ranger.fit, newdata = train_test_dat$test)
      ensamble_test_df$glmnet_pred <- predict(glmnetinter.fit, newdata = train_test_dat$test)
      
      ensamble_test_df$pred_ranger_final <- as.numeric( format( exp( predict(ranger.fit.final.ensamble, newdata = ensamble_test_df) ),scientific = FALSE ) )
      
      # Now take the final predictions for the most recent year and week
      final_predictions <- ensamble_test_df %>% filter(year == ap_dates$year) %>% select(pos_team,week,analytical_points, pred_ranger_final)
      
    parallel::stopCluster(cl)
    
    rm(cl)
    gc()
    
    return(final_predictions)
    
}

#=========================================== =
# Save output functions         #
#=========================================== =

print_output <- function(final_output_df,ap_dates) {
  
  #save final table in a form that is usable in the table save
  final_table_save <- final_output_df %>% select(pos_team,week,analytical_points, pred_ranger_final) %>%
    mutate(
      Predicted_Rank = rank(-pred_ranger_final),
      Actual_Rank = rank(-analytical_points),
      Rank_Difference = Actual_Rank - Predicted_Rank
    ) %>%
    filter(Predicted_Rank <= 25) %>%
    select(-c(week,analytical_points, pred_ranger_final)) %>%
    arrange(Predicted_Rank)
  
  rank_table <- final_table_save %>%
    gt() %>%
    cols_label(
      pos_team = "Team",
      Predicted_Rank = "Predicted Rank",
      Actual_Rank = "Actual Rank",
      Rank_Difference = "Rank Difference"
    ) %>%
    tab_header(
      title = paste("AP Poll Rankings Comparison -", ap_dates$year, "Week", ap_dates$week),
      subtitle = paste("Comparison of Predicted vs. Actual AP Poll Ranks for Week", ap_dates$week, "in", ap_dates$year)
    ) %>%
    fmt_number(
      columns = c(Predicted_Rank, Actual_Rank, Rank_Difference),
      decimals = 0
    ) %>%
    tab_style(
      style = cell_fill(color = "red", alpha = 0.5),
      locations = cells_body(
        columns = Rank_Difference,
        rows = Rank_Difference < 0
      )
    ) %>%
    tab_style(
      style = cell_fill(color = "green", alpha = 0.5),
      locations = cells_body(
        columns = Rank_Difference,
        rows = Rank_Difference > 0
      )
    ) %>%
    tab_options(
      table.width = pct(80),  # Set to 100% to fit the entire table
      heading.align = "center",
      column_labels.font.size = 18,  # Larger column headers font size
      table.font.size = 16,  # Larger body font size for table content
      row.striping.include_table_body = TRUE,
      column_labels.padding = px(2)  # Increase space between columns for readability
    )
  
  # return final
  return(rank_table)
  
}
  
#print(rank_table) # for review









#=========================================== =
# Final Declare all Functions         #
#=========================================== =

# Load and process data
pbp <- load_data()
prepared_data <- prepare_season_data(pbp)
processed_data <- preprocesss_data(prepared_data)
ap_dates <- get_most_recent_AP_poll(pbp) # This function grabs dates that are used for train and testing, plus model development
train_test_dat <- split_train_test(processed_data,ap_dates)


# Run Model
model_output <- ensamble_final_model_function(train_test_dat,ap_dates)


# Get and print/save output
rank_table <- print_output(model_output, ap_dates)
print(rank_table)

file_name <- paste0("team_rankings_comparison_", most_recent_AP_vote_year, "_week_", most_recent_AP_vote_week, ".html")
gtsave(data = rank_table, filename = file_name)

webshot::webshot(
  file_name,
  paste0("team_rankings_comparison_", most_recent_AP_vote_year, "_week_", most_recent_AP_vote_week, ".png"),
  vheight = 1000,  # Increase vertical height for more resolution
  vwidth = 1000,   # Increase vertical width for better resolution
  delay = 1,       # Add a short delay if needed to ensure proper rendering
  zoom = 2         # Increase zoom level to enhance resolution (higher zoom = better quality)
)


