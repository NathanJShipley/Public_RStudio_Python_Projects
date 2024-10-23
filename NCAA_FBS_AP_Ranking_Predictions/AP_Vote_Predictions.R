#==============================================================================
#                              AP Vote Prediction Analysis                   #      
#==============================================================================

#===========================================
# Load Required Libraries                 #
#===========================================

library(tidyverse)    # Contains dplyr and ggplot2 for data manipulation and visualization
library(cfbfastR)     # All good tools to interact with NCAA Football data
library(jtools)       # My personal favorite package for working with linear models 
library(earth)
#library(caret)


Sys.setenv(CFBD_API_KEY = "jMH8x4oFxOwTVlqYDq6Wgqdz2V8N1G6Z2rMJ2FuzQ5c5nRgQ2Rw2Xx0HPwwmZ2T3")



#===========================================
# LOAD DATA                                #
#===========================================

# Set up empty container for play by play data
pbp <- data.frame()

# Get data for all seasons where play by play data is available
seasons <- 2014:cfbfastR:::most_recent_cfb_season()

# Now load that data in! 
progressr::with_progress({
  
  pbp <- cfbfastR::load_cfb_pbp(seasons)
})


### Okay, need to also get rankings using another data
rankings_list <- list()

for (year in seasons) {
  rankings <- cfbfastR::cfbd_rankings(year)  # Fetch the rankings for the current year
  rankings_list[[as.character(year)]] <- rankings  # Store the data frame in the list
}

combined_rankings <- bind_rows(rankings_list)

# Narrow data down to just the AP poll top 25 votes
AP_Poll_Votes <- combined_rankings %>%
  filter(poll == 'AP Top 25') %>%
  select(c('season','week','school','points'))




#===========================================
# Now Filter and do some feature extraction   #
#===========================================

# Step 1 : Generate a bunch of data for the offense side of the ball
# Step 2 : Generate a bunch of data for the defense side of the ball
# Step 3 : Join the data into one giant form
# Step 4 : Calculate cumulative season statistics


# Going to set up a fairly large set of data for both teams when on offense and when on defense and join together

########################################### OFENSE 
# Set up the offense side of the data
pos_team_data <- pbp %>%
  ##filter( year == '2024'
  ##       & week == '1' 
  ##       & ( pos_team == "Clemson" | def_pos_team == "Clemson" ) ) %>%
  group_by( across ( c( 'pos_team', 'week', 'year' ) ) ) %>%
  summarise(
        conference = max(offense_conference[offense_play == pos_team]),
    
        # Total scores
        points_scored = max(pos_team_score),
        points_allowed = max(def_pos_team_score),
        win_flag = ifelse(points_scored > points_allowed, 1, 0),
        
        ## Offense
        # Yards
        total_passing_yards = sum(yards_gained[play_type == 'Pass Reception' | play_type == 'Passing Touchdown'], na.rm = TRUE),           
        total_rushing_yards = sum(yards_gained[play_type == 'Rush' | play_type ==  'Rushing Touchdown'], na.rm = TRUE), 
        total_yards = total_passing_yards + total_rushing_yards,
        total_pass_completes = sum(play_type == 'Pass Reception' | play_type == 'Passing Touchdown'),
        total_pass_inccompletes = sum(play_type == 'Pass Incompletion' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown'),
        pass_attempts = total_pass_completes + total_pass_inccompletes,
        pass_comp_attempts = total_pass_completes / pass_attempts,
        avg_passing_yards_per_attempt = total_passing_yards / pass_attempts,
        total_passing_touchdowns = sum(play_type == 'Passing Touchdown'),
        rush_attempts = sum(play_type == 'Rush' | play_type ==  'Rushing Touchdown'),
        avg_rush_yards_per_attempt = total_rushing_yards / rush_attempts,
        total_rushing_touchdowns = sum(play_type ==  'Rushing Touchdown'),
        total_plays = pass_attempts + rush_attempts,
        avg_yards_per_play = total_yards / total_plays,
        
        
        # Plays over Yards
        total_plays_over_5_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' | play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 5),
        total_passing_plays_over_5_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' ) & yards_gained >= 5),       
        total_rushing_plays_over_5_yards = sum( ( play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 5),       
        total_plays_over_10_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' | play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 10),
        total_passing_plays_over_10_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' ) & yards_gained >= 10),       
        total_rushing_plays_over_10_yards = sum( ( play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 10),
        total_plays_over_15_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' | play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 15),
        total_passing_plays_over_15_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' ) & yards_gained >= 15),       
        total_rushing_plays_over_15_yards = sum( ( play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 15),       
        total_plays_over_20_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' | play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 20),
        total_passing_plays_over_20_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' ) & yards_gained >= 20),       
        total_rushing_plays_over_20_yards = sum( ( play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 20),       
        total_plays_over_25_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' | play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 25),
        total_passing_plays_over_25_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' ) & yards_gained >= 25),       
        total_rushing_plays_over_25_yards = sum( ( play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 25),   
        
        
        # Possessions
        total_possessions = sum( drive_numbers == 1 ),
        total_time_possession_mins = ( ( sum ( drive_time_minutes_elapsed[drive_numbers == 1] ) * 60) + sum ( drive_time_seconds_elapsed[drive_numbers == 1] ) ) / 60 ,
        avg_time_possession = total_time_possession_mins / total_possessions,
        avg_time_per_play = total_time_possession_mins / total_plays,
        
        
        # Down efficicney
        total_first_downs = sum( down == 1 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
        total_first_down_converted = sum( down == 1 & success == 1 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
        first_down_efficiency = total_first_down_converted / total_first_downs,
        total_second_downs = sum( down == 2 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
        total_second_down_converted = sum( down == 2 & success == 1 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
        second_down_efficiency = total_second_down_converted / total_second_downs,      
        total_third_downs = sum( down == 3 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
        total_third_down_converted = sum( down == 3 & success == 1 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
        third_down_efficiency = total_third_down_converted / total_third_downs,        
        total_forth_downs = sum( down == 4 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
        total_forth_down_converted = sum( down == 4 & success == 1 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
        forth_down_efficiency = total_forth_down_converted / total_forth_downs,   
        
        
        # total number and effifcy % coverts
        # red zone efficicency
        
        # Penalties
        
      
        # Others while on Offense
        times_sacked = sum(play_type == 'Sack'),
        
        
        ## Turn Overs
        total_interceptions = sum(play_type == 'Interception Return' | play_type == 'Interception Return Touchdown'),
        total_fumbles = sum(play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown'),
        total_turnovers = total_interceptions + total_fumbles,
        
        
        # Punts
        total_punts_blocked = sum(play_type == 'Blocked Punt' | play_type == 'Blocked Punt Touchdown'),
        total_punts = sum(play_type == 'Blocked Punt' | play_type == 'Blocked Punt Touchdown' | play_type == 'Punt' | play_type == 'Punt Return Touchdown'),
        
        
        # Field Goals
        total_fg_blocked = sum(play_type == 'Blocked Field Goal' | play_type == 'Blocked Punt Touchdown'),
        total_fg_attempts = sum(play_type == 'Blocked Field Goal' | play_type == 'Blocked Punt Touchdown' | play_type == 'Field Goal Good' | play_type == 'Field Goal Missed', play_type == 'Missed Field Goal Return'),
        total_fg_good = sum(play_type == 'Field Goal Good'),
        fg_comp_attempts = total_fg_good / total_fg_attempts,

        # Final misc
        total_scoring_plays = total_fg_good + total_passing_touchdowns + total_rushing_touchdowns
        
        # Other considerations
        # Home vs away 
        # QB ratings
        # Conference? 
        
  ) %>% ungroup()

############## DEFENSE
# Now set up the defense side of the data
def_team_data <- pbp %>%
  ##filter( year == '2024'
  ##        & week == '1' 
  ##        & ( pos_team == "Clemson" | def_pos_team == "Clemson" ) ) %>%
  group_by( across ( c( 'def_pos_team', 'week', 'year' ) ) ) %>%
  summarise(
    ## Offense
    # Yards
    allowed_total_passing_yards = sum(yards_gained[play_type == 'Pass Reception' | play_type == 'Passing Touchdown'], na.rm = TRUE),           
    allowed_total_rushing_yards = sum(yards_gained[play_type == 'Rush' | play_type ==  'Rushing Touchdown'], na.rm = TRUE), 
    allowed_total_yards = allowed_total_passing_yards + allowed_total_rushing_yards,
    allowed_total_pass_completes = sum(play_type == 'Pass Reception' | play_type == 'Passing Touchdown'),
    allowed_total_pass_inccompletes = sum(play_type == 'Pass Incompletion' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown'),
    allowed_pass_attempts = allowed_total_pass_completes + allowed_total_pass_inccompletes,
    allowed_pass_comp_attempts = allowed_total_pass_completes / allowed_pass_attempts,
    allowed_avg_passing_yards_per_attempt = allowed_total_passing_yards / allowed_pass_attempts,
    allowed_total_passing_touchdowns = sum(play_type == 'Passing Touchdown'),
    allowed_rush_attempts = sum(play_type == 'Rush' | play_type ==  'Rushing Touchdown'),
    allowed_avg_rush_yards_per_attempt = allowed_total_rushing_yards / allowed_rush_attempts,
    allowed_total_rushing_touchdowns = sum(play_type ==  'Rushing Touchdown'),
    allowed_total_plays = allowed_pass_attempts + allowed_rush_attempts,
    allowed_avg_yards_per_play = allowed_total_yards / allowed_total_plays,
    
    
    # Plays over Yards
    allowed_total_plays_over_5_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' | play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 5),
    allowed_total_passing_plays_over_5_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' ) & yards_gained >= 5),       
    allowed_total_rushing_plays_over_5_yards = sum( ( play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 5),       
    allowed_total_plays_over_10_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' | play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 10),
    allowed_total_passing_plays_over_10_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' ) & yards_gained >= 10),       
    allowed_total_rushing_plays_over_10_yards = sum( ( play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 10),
    allowed_total_plays_over_15_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' | play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 15),
    allowed_total_passing_plays_over_15_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' ) & yards_gained >= 15),       
    allowed_total_rushing_plays_over_15_yards = sum( ( play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 15),       
    allowed_total_plays_over_20_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' | play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 20),
    allowed_total_passing_plays_over_20_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' ) & yards_gained >= 20),       
    allowed_total_rushing_plays_over_20_yards = sum( ( play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 20),       
    allowed_total_plays_over_25_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' | play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 25),
    allowed_total_passing_plays_over_25_yards = sum( ( play_type == 'Pass Reception' | play_type == 'Passing Touchdown' ) & yards_gained >= 25),       
    allowed_total_rushing_plays_over_25_yards = sum( ( play_type == 'Rush' | play_type ==  'Rushing Touchdown' ) & yards_gained >= 25),   
    
    # Down efficicney
    allowed_total_first_downs = sum( down == 1 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
    allowed_total_first_down_converted = sum( down == 1 & success == 1 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
    allowed_first_down_efficiency = allowed_total_first_down_converted / allowed_total_first_downs,
    allowed_total_second_downs = sum( down == 2 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
    allowed_total_second_down_converted = sum( down == 2 & success == 1 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
    allowed_second_down_efficiency = allowed_total_second_down_converted / allowed_total_second_downs,      
    allowed_total_third_downs = sum( down == 3 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
    allowed_total_third_down_converted = sum( down == 3 & success == 1 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
    allowed_third_down_efficiency = allowed_total_third_down_converted / allowed_total_third_downs,        
    allowed_total_forth_downs = sum( down == 4 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
    allowed_total_forth_down_converted = sum( down == 4 & success == 1 & ( play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown' | play_type == 'Fumble Recovery (Own)' | play_type == 'Fumble Return Touchdown' | play_type == 'Interception Return' | play_type == 'Interception Return Touchdown' | play_type == 'Safety' | play_type == 'Rushing Touchdown' | play_type == 'Sack' | play_type == 'Rush' | play_type == 'Passing Touchdown' | play_type == 'Pass Reception' | play_type == 'Pass Incompletion' ) ),
    allowed_forth_down_efficiency = allowed_total_forth_down_converted / allowed_total_forth_downs,   

    # Others while on Offense
    forced_times_sacked = sum(play_type == 'Sack'),
   
    
    ## Turn Overs
    forced_total_interceptions = sum(play_type == 'Interception Return' | play_type == 'Interception Return Touchdown'),
    forced_total_fumbles = sum(play_type == 'Fumble Recovery (Opponent)' | play_type == 'Fumble Recovery (Opponent) Touchdown'),
    forced_total_turnovers = forced_total_interceptions + forced_total_fumbles,

) %>%
  rename(pos_team = def_pos_team) %>% ungroup()


############## JOIN DATA
# Now combine! 
joined_data <- pos_team_data %>%
                left_join(def_team_data, by = c("pos_team", "week", "year"))


########################## LASTLY, make sure to do some cumulative sum calculations 



#===========================================
# Join the AP data to Scores         #
#===========================================

# Set variable names to match
AP_Poll_Votes <- AP_Poll_Votes %>% rename(pos_team = school, year = season)

# Join the data
joined_data_poll <- joined_data %>%
                      left_join(AP_Poll_Votes, by = c("pos_team", "week", "year"))


# Lets do a few quick adjustments to the vote data. If is NA, really should be 0
joined_data_poll$points[is.na(joined_data_poll$points)] <- 0


# Also noticing that the model is bad, so lets just focus on Power Conferences
conference_include_list <- joined_data_poll %>% group_by(conference) %>% summarise( total_points = sum (points) ) %>% filter(total_points > 0) %>% pull(conference)

joined_data_poll_FBS_filter <- joined_data_poll %>%
                                  filter(conference %in% conference_include_list)




#===========================================
# Now lets start building some modeling!!!   #
#===========================================

# Lets start very simple with a good ole' linear regression
# Seems like the school variable is really messing things up, maybe make the model agnostic to it? Or just do P5 and G5?
filtered_prediction_data <- joined_data_poll %>% select(-pos_team)

ap_poll_pred_fit <- lm(points ~ ., data = joined_data_poll_FBS_filter)

jtools::summ(ap_poll_pred_fit)

joined_data_poll_FBS_filter$pred_points <- predict(ap_poll_pred_fit, newdata = joined_data_poll_FBS_filter)



data_2024_projections <- joined_data_poll_FBS_filter %>% filter(year == 2024 & points > 0) %>% select(c('pos_team','year','week','points','pred_points'))
data_2024_projections_week8 <- joined_data_poll_FBS_filter %>% filter(year == 2024 & points > 0 & week == 8) %>% select(c('pos_team','year','week','points','pred_points'))








# So some current issues
# Preason / early season rankings are really hard, might just say wont do them?
# how to handle negative points, not possible lol
# Set up season long stats / cumulative
# finish other data points
# Also, consider just looking at P5 and G5












