# Purpose: Clean the play-by-play data, add useful variables from other season data provided by NHL 
library(tidyverse)
library(rvest)
library(stringi)

# Filtering raw data to desired segment -----------------------------------
games_details <- read.csv('game.csv') # administrative info

# Extract the most recent season (no playoff games)
reg_1819_szn <- games_details %>% 
  filter(season == 20182019 & type == 'R') %>% 
  arrange(game_id)

game_ids <- as.vector(unique(reg_1819_szn$game_id))

# Load play-by-play
all_events <- read.csv('game_plays.csv')

# Filter pbp to 18_19 season
szn_1819_events <- all_events %>%
  filter(game_id %in% game_ids)

# Save this dataframe - Commented for now because CHI will not be using it
# write.csv(szn_1819_events, 'reg_szn_events_1819.csv')


# Team names and Home Win ------------------------------------------------------

teams_info <- read.csv('team_info.csv')

teams <- teams_info %>% select(team_id, abbreviation) %>% arrange(team_id)

# Remove PHX and ATL
teams <- teams[-c(27,11), ]

# Add the Home and Away names to game info df
szn_games_info_1819 <- reg_1819_szn %>% 
  mutate(H_team = case_when(
    home_team_id == 1 ~ 'NJD'
    ,home_team_id == 2 ~ 'NYI'
    ,home_team_id == 3 ~ 'NYR'
    ,home_team_id == 4 ~ 'PHI'
    ,home_team_id == 5 ~ 'PIT'
    ,home_team_id == 6 ~ 'BOS'
    ,home_team_id == 7 ~ 'BUF'
    ,home_team_id == 8 ~ 'MTL'
    ,home_team_id == 9 ~ 'OTT'
    ,home_team_id == 10 ~ 'TOR'
    ,home_team_id == 11 ~ 'ATL'
    ,home_team_id == 12 ~ 'CAR'
    ,home_team_id == 13 ~ 'FLA'
    ,home_team_id == 14 ~ 'TBL'
    ,home_team_id == 15 ~ 'WSH'
    ,home_team_id == 16 ~ 'CHI'
    ,home_team_id == 17 ~ 'DET'
    ,home_team_id == 18 ~ 'NSH'
    ,home_team_id == 19 ~ 'STL'
    ,home_team_id == 20 ~ 'CGY'
    ,home_team_id == 21 ~ 'COL'
    ,home_team_id == 22 ~ 'EDM'
    ,home_team_id == 23 ~ 'VAN'
    ,home_team_id == 24 ~ 'ANA'
    ,home_team_id == 25 ~ 'DAL',
    home_team_id == 26 ~ 'LAK',
    home_team_id == 28 ~ 'SJS',
    home_team_id == 29 ~ 'CBJ',
    home_team_id == 30 ~ 'MIN',
    home_team_id == 52 ~ 'WPG',
    home_team_id == 53 ~ 'ARI',
    home_team_id == 54 ~ 'VGK',
    TRUE ~ '0')) %>% 
  mutate(A_team = case_when(
    away_team_id == 1 ~ 'NJD'
    ,away_team_id == 2 ~ 'NYI'
    ,away_team_id == 3 ~ 'NYR'
    ,away_team_id == 4 ~ 'PHI'
    ,away_team_id == 5 ~ 'PIT'
    ,away_team_id == 6 ~ 'BOS'
    ,away_team_id == 7 ~ 'BUF'
    ,away_team_id == 8 ~ 'MTL'
    ,away_team_id == 9 ~ 'OTT'
    ,away_team_id == 10 ~ 'TOR'
    ,away_team_id == 11 ~ 'ATL'
    ,away_team_id == 12 ~ 'CAR'
    ,away_team_id == 13 ~ 'FLA'
    ,away_team_id == 14 ~ 'TBL'
    ,away_team_id == 15 ~ 'WSH'
    ,away_team_id == 16 ~ 'CHI'
    ,away_team_id == 17 ~ 'DET'
    ,away_team_id == 18 ~ 'NSH'
    ,away_team_id == 19 ~ 'STL'
    ,away_team_id == 20 ~ 'CGY'
    ,away_team_id == 21 ~ 'COL'
    ,away_team_id == 22 ~ 'EDM'
    ,away_team_id == 23 ~ 'VAN'
    ,away_team_id == 24 ~ 'ANA'
    ,away_team_id == 25 ~ 'DAL',
    away_team_id == 26 ~ 'LAK',
    away_team_id == 28 ~ 'SJS',
    away_team_id == 29 ~ 'CBJ',
    away_team_id == 30 ~ 'MIN',
    away_team_id == 52 ~ 'WPG',
    away_team_id == 53 ~ 'ARI'
    ,away_team_id == 54 ~ 'VGK'
    ,TRUE ~ '0'))

# Add column for Home team Win
szn_games_info_1819 <- szn_games_info_1819 %>% 
  mutate(H_Win = case_when(
    (outcome == 'home win OT') | (outcome == 'home win REG') ~ 1,
    TRUE ~ 0))

# Remove duplicates
szn_games_info_1819 <- szn_games_info_1819 %>% distinct(game_id, .keep_all = TRUE)


  # Check to see if Home Win makes sense
  table(szn_games_info_1819$outcome)
  #  Away wins = 144+445 = 589
  #  Home wins = 127+555 = 682

# Only need game_id, H_Team, A_Team, and H_Win for each game
szn_game_summaries <- szn_games_info_1819[,c(1,16,17,18)]


# Putting Together the Play by Play and game info ----------------------

# Add the team names, H_Win to the pbp
all_data <- inner_join(szn_1819_events, szn_game_summaries, by = "game_id") 

# Extract the play number (makes it easier to order the plays)
all_data$play_num <- as.numeric(sub(".*_", "", all_data$play_id))

# Group by game, change time to a time datatype, order chronologically, remove duplicates
all_data <- all_data %>%  
  group_by(game_id) %>%
  mutate(real_time = as.POSIXct(dateTime, format = "%Y-%m-%d %H:%M:%S")) %>% 
  arrange(play_num, .by_group = TRUE) %>% 
  distinct(play_id, .keep_all = TRUE) 
  # Ordering by play number rather than real time because in the first game play 67 has
  # out of order events if you do it by real timestamp


# Making sure each game has a "Game End" event ----------------

all_games_have_game_end <- all_data %>%
  group_by(game_id) %>%
  summarise(has_game_end = any(event == "Game End"))

table(all_games_have_game_end$has_game_end)

all_games_have_game_end %>% filter(has_game_end == FALSE)
# Game 2018020063 has no row with event == 'Game End'


# Adding a Game End row to game 2018020063-------

# Subset to specific game
troubling_game <- all_data %>% filter(game_id == 2018020063)

# Select the last row of the game as a template
new_row_pt1 <- troubling_game[nrow(troubling_game),] 

# Make all the column values like the other 'Game End' rows in the dataset
  # change the time to be one second after last event
new_row <- new_row_pt1 %>% mutate(play_id = '2018020063_314', team_id_for = NA,
                                  team_id_against = NA, event = 'Game End', 
                                  secondaryType = NA, x = NA, y = NA, periodType = 'REGULAR', 
                                  periodTime = 1200,
                                  periodTimeRemaining = 0, description = 'Game End',
                                  st_x = NA, st_y = NA, 
                                  real_time = as.POSIXct('2018-10-14 02:42:58', format = "%Y-%m-%d %H:%M:%S"))


# Add this new row into the end of the game
fixed_game <- all_data %>% 
  filter(game_id == 2018020063) %>% 
  bind_rows(new_row)

# Remove the incorrect version of that game and put the replacement game in
all_data <- all_data %>%
  filter(game_id != 2018020063) %>%
  bind_rows(fixed_game)


# Check to make sure there are 1271 game end rows in the season
df <- all_data %>% filter(event == 'Game End') 
length(unique(df$game_id))
nrow(df) # two extra 'Game End's, so two games must have multiple Game End rows

# Find which game_ids have multiple rows in the Game End df
duplicated_ids <- df$game_id[duplicated(df$game_id)]

# Print the duplicated game IDs
print(duplicated_ids) #2018020189 2018020238

# Checking out if they are just duplicated rows or if the rows are unique
df %>% filter(game_id %in% c(2018020189, 2018020238)) %>% View()

# Remove these extra rows 
df <- df %>% filter(!duplicated(game_id)) # subset the Game End df to only the non-duplicates
nrow(df)


all_data <- all_data %>%
  filter(event != 'Game End') %>% # get all pbp rows that are not Game End
  bind_rows(df) # add in the updated (without-duplicates) Game End df

# Now reorder all_data by game and time again because the `Game End`s got 
# added to the bottom of all_data
all_data <- all_data %>% group_by(game_id) %>% arrange(real_time, .by_group = TRUE)


# Game clock, Home/Away team IDs -----------
# Drop the old dateTime column that was a character
all_data <- all_data[, -which(names(all_data) == 'dateTime')]


# DATA IS NOW IN ORDER BY GAME AND TIME WITHIN GAME
# - THERE IS AN ISSUE WITH RECORDING OF DATA - 
# OCCASIONALLY THERE IS SHOT AFTER STOPPAGE 
# AND BEFORE A FACEOFF (DUE TO DATETIME BEING RECORDED INCORRECTLY)
# IGNORE IT FOR NOW, ORDER BY play_id INSTEAD LATER


# Create gametime variable (counting up in seconds)
all_data$total_game_time <- NA
for(i in 1:nrow(all_data)){
  if(all_data[i, 'period'] == 1){
    all_data[i, 'total_game_time'] <- all_data[i, 'periodTime']
  }
  if(all_data[i, 'period'] == 2){
    all_data[i, 'total_game_time'] <- all_data[i, 'periodTime'] + 1200
  }
  if(all_data[i, 'period'] == 3){
    all_data[i, 'total_game_time'] <- all_data[i, 'periodTime'] + 2400
  }
  if(all_data[i, 'period'] == 4){
    all_data[i, 'total_game_time'] <- all_data[i, 'periodTime'] + 3600
  }
  if(all_data[i, 'period'] == 5){ # Shootout
    all_data[i, 'total_game_time'] <- NA
  }
  if(i %% 1000 == 0){
    print(i)
  }
}


# Creating a Variable for the Home and Away team IDs
all_data <- all_data %>% mutate(H_team_id = case_when(
  H_team == 'NJD' ~ 1
  ,H_team == 'NYI' ~ 2
  ,H_team == 'NYR' ~ 3
  ,H_team == 'PHI' ~ 4
  ,H_team == 'PIT' ~ 5
  ,H_team == 'BOS' ~ 6
  ,H_team == 'BUF' ~ 7
  ,H_team == 'MTL' ~ 8
  ,H_team =='OTT' ~ 9
  ,H_team =='TOR' ~ 10
  ,H_team =='ATL' ~ 11
  ,H_team == 'CAR' ~ 12
  ,H_team =='FLA' ~ 13
  ,H_team == 'TBL' ~ 14
  ,H_team == 'WSH' ~ 15
  ,H_team == 'CHI' ~ 16
  ,H_team == 'DET' ~ 17
  ,H_team == 'NSH' ~ 18
  ,H_team =='STL' ~ 19
  ,H_team =='CGY' ~ 20
  ,H_team =='COL' ~ 21
  ,H_team =='EDM' ~ 22
  ,H_team =='VAN' ~ 23
  ,H_team =='ANA' ~ 24
  ,H_team == 'DAL' ~ 25
  ,H_team == 'LAK' ~ 26
  ,H_team == 'SJS' ~ 28
  ,H_team == 'CBJ' ~ 29
  ,H_team == 'MIN' ~ 30
  ,H_team == 'WPG' ~ 52
  ,H_team == 'ARI' ~ 53
  ,H_team == 'VGK' ~ 54)) %>% mutate(A_team_id = case_when(
    A_team == 'NJD' ~ 1
    ,A_team == 'NYI' ~ 2
    ,A_team == 'NYR' ~ 3
    ,A_team == 'PHI' ~ 4
    ,A_team == 'PIT' ~ 5
    ,A_team == 'BOS' ~ 6
    ,A_team == 'BUF' ~ 7
    ,A_team == 'MTL' ~ 8
    ,A_team =='OTT' ~ 9
    ,A_team =='TOR' ~ 10
    ,A_team =='ATL' ~ 11
    ,A_team == 'CAR' ~ 12
    ,A_team =='FLA' ~ 13
    ,A_team == 'TBL' ~ 14
    ,A_team == 'WSH' ~ 15
    ,A_team == 'CHI' ~ 16
    ,A_team == 'DET' ~ 17
    ,A_team == 'NSH' ~ 18
    ,A_team =='STL' ~ 19
    ,A_team =='CGY' ~ 20
    ,A_team =='COL' ~ 21
    ,A_team =='EDM' ~ 22
    ,A_team =='VAN' ~ 23
    ,A_team =='ANA' ~ 24
    ,A_team == 'DAL' ~ 25
    ,A_team == 'LAK' ~ 26
    ,A_team == 'SJS' ~ 28
    ,A_team == 'CBJ' ~ 29
    ,A_team == 'MIN' ~ 30
    ,A_team == 'WPG' ~ 52
    ,A_team == 'ARI' ~ 53
    ,A_team == 'VGK' ~ 54))



# Home/Away Faceoff Wins ----------------------------------
# Specify the URL of the webpage containing the players table
  # Note: this page is NOT listed as restricted in robots.txt
url <- "https://www.hockey-reference.com/leagues/NHL_2019_skaters.html"
  
# Read the HTML content of the webpage
webpage <- read_html(url)

# Extract the table containing team statistics

table <- webpage %>%
  html_node(xpath = '//*[@id="all_player_stats"]') %>%
  html_table(header = TRUE)

names(table) <- c("Index", "Player", "Pos", "Team")

# Cleaning roster table
for(i in nrow(table):1) {
  if (table[i, 1] == 'Rk') { # removing the row separators
    table <- table[-i, ]
  }
  if(table[i,4] == 'VEG'){
    table[i, 4] <- 'VGK' # aligning the team names of the roster and the game data
  }
  table[i, 'Player'] <- stri_trans_general(table[i, 'Player'], "Latin-ASCII")
  # taking care of accents and other marks in player names to align them with event data
}

all_data$H_FO_Win <- 0

fo_rows <- all_data %>% filter(event == 'Faceoff')

# Finding the current team of the player winning the faceoff
for(i in 1:nrow(fo_rows)){
  # Who won the faceoff?
  fname <- strsplit(as.character(fo_rows[i,'description']), " ")[[1]][1]
  lname <- strsplit(as.character(fo_rows[i,'description']), " ")[[1]][2]
  pl_name <- paste0(fname, " ", lname)
  
  # Which is the home team in this game?
  h_team <- fo_rows[i, 'H_team']
  
  # Is the faceoff winner on the home team's roster?
  for(j in 1:nrow(table)){
    if((table[j, 'Player']== pl_name) & (h_team == table[j, 'Team'])){
      fo_rows[i, 'H_FO_Win'] <- 1
    }
  }
  # Print progress
  if(i %% 1000 == 0){
    print(i)
  }
}

# Get rid of all FO rows in the old all_data, replace with new ones:
# Save non-faceoff rows from all_data
non_fo <- all_data %>% filter(event != 'Faceoff')

# Make sure column count is same
ncol(non_fo) 
ncol(fo_rows)

# Put new faceoff rows in all_data
new_all_data <- rbind(non_fo, fo_rows) 

# Order the new_all_data by time so faceoff rows are in correct spot
new_all_data2 <- new_all_data %>% group_by(game_id) %>% arrange(real_time, .by_group = TRUE)

# Add in the Away FO Wins
new_all_data2 <- new_all_data2 %>% 
  mutate(A_FO_Win = 
           case_when((event == 'Faceoff' & H_FO_Win != 1) ~ 1, TRUE ~ 0))


all_data <- new_all_data2


# Make sure every faceoff has a winner:
sum(all_data$H_FO_Win + all_data$A_FO_Win)
# should equal
nrow(all_data %>% filter(event == 'Faceoff'))

# Penalty Info ------

pen_info <- read.csv('game_penalties.csv')

play_ids_1819 <- all_data$play_id

pen_info$szn_1819 <- 0

# Finding 2018-2019 season penalties
for(i in 1:nrow(pen_info)){
  if(pen_info[i,'play_id'] %in% play_ids_1819){
    pen_info[i,'szn_1819'] <- 1
  }
}

all_pen_plays <- all_data %>% filter(event == 'Penalty') # all the penalty plays 2018-2019
non_pen_plays <- all_data %>% filter(event != 'Penalty')
new_pen_info <- pen_info %>% filter(szn_1819 == 1)

# take out duplicates in new_pen_info
new_pen_info_2 <- new_pen_info %>% distinct(play_id, .keep_all = TRUE)

# select only penalty minutes and play id
new_pen_info_3 <- new_pen_info_2 %>% select(play_id, penaltyMinutes)

# make the rest of all_data have a 0 penalty minutes column
non_pen_plays$penaltyMinutes <- 0

# join on play_id to get all penalty events and minutes in the penalty dataset
all_pen_rows <- left_join(all_pen_plays, new_pen_info_3, by = 'play_id')

# Checking no data was lost
nrow(rbind(all_pen_rows, non_pen_plays))
# should be equal to
nrow(all_data)

# combine the non penalty rows with the penalty rows
all_data_w_pm <- rbind(all_pen_rows, non_pen_plays)


# Scoring Chances ------------------
  # Based on whether shot was taken inside house region / 'home plate' / trapezoid
all_data_w_pm <- all_data_w_pm %>% mutate(house_s = ifelse(
  (event %in% c('Shot', 'Goal', 'Missed Shot')) & 
    st_y <= 22 & st_y >= -22 & 
    st_x >= 54 & st_x <= 89 &
    st_y <= (-.95*st_x + 87.55) &
    st_y >= (.95*st_x - 87.55),
  1, 
  0
))

# Save the data as new file before adding covariates ------------
write.csv(all_data_w_pm, 'reg_szn_events_1819.csv')

# Real-time stat updates, tracking covariates---------
covar_play_by_play <- all_data_w_pm %>% group_by(game_id) %>% arrange(real_time, .by_group = TRUE) %>%
  # Penalty Minutes Identifier
  mutate(H_Penalty_Min = case_when((event == 'Penalty' & team_id_for == H_team_id) ~ penaltyMinutes)) %>%
  mutate(A_Penalty_Min = case_when((event == 'Penalty' & team_id_for == A_team_id) ~ penaltyMinutes)) %>%
  mutate(H_Penalty_Min = case_when(is.na(H_Penalty_Min) ~ 0, TRUE ~ as.numeric(H_Penalty_Min))) %>%
  mutate(A_Penalty_Min = case_when(is.na(A_Penalty_Min) ~ 0, TRUE ~ as.numeric(A_Penalty_Min))) %>% 
  # Running Total PIM
  mutate(H_Tot_PIM = cumsum(H_Penalty_Min)) %>%
  mutate(A_Tot_PIM = cumsum(A_Penalty_Min)) %>%
  # PIM Per Minute of Gameplay 
    #(this scale lets us compare OT length games with regulation length)
  mutate(H_PIM_PM = H_Tot_PIM *60 / total_game_time) %>%
  mutate(A_PIM_PM = A_Tot_PIM *60/ total_game_time) %>%
  
  # Shot Attempts Identifier (not including Blocked Shots for now)
  mutate(H_Shot_Att = case_when(((event == 'Shot' | event == 'Missed Shot' | event == 'Goal') 
                                 & team_id_for == H_team_id) ~ 1, TRUE ~ as.numeric(0))) %>%
  
  mutate(A_Shot_Att = case_when(((event == 'Shot' | event == 'Missed Shot' | event == 'Goal') 
                                 & team_id_for == A_team_id) ~ 1, TRUE ~ as.numeric(0))) %>%
  # Running Total Shot Attempts
  mutate(H_Tot_SAtt = cumsum(H_Shot_Att)) %>%
  mutate(A_Tot_SAtt = cumsum(A_Shot_Att)) %>%
  # SAtt Per Minute of Gameplay
  mutate(H_SAtt_PM = H_Tot_SAtt *60 / total_game_time) %>%
  mutate(A_SAtt_PM = A_Tot_SAtt *60/ total_game_time) %>%
  
  # Shots (on goal) Identifier
  mutate(H_Shot = case_when(((event == 'Shot' | event == 'Goal') 
                             & team_id_for == H_team_id) ~ 1, TRUE ~ as.numeric(0))) %>%
  mutate(A_Shot = case_when(((event == 'Shot' | event == 'Goal') 
                             & team_id_for == A_team_id) ~ 1, TRUE ~ as.numeric(0))) %>%
  # Running Total Shots
  mutate(H_Tot_S = cumsum(H_Shot)) %>%
  mutate(A_Tot_S = cumsum(A_Shot)) %>%
  # Shots Per Minute of Gameplay
  mutate(H_S_PM = H_Tot_S *60 / total_game_time) %>%
  mutate(A_S_PM = A_Tot_S *60 / total_game_time) %>%
  
  # Takeaway Identifier
  mutate(H_TA = case_when((event == 'Takeaway' & team_id_for == H_team_id) ~ 1, TRUE ~ as.numeric(0))) %>%
  mutate(A_TA = case_when((event == 'Takeaway' & team_id_for == A_team_id) ~ 1, TRUE ~ as.numeric(0))) %>%
  # Running Total Takeaways
  mutate(H_Tot_TA = cumsum(H_TA)) %>%
  mutate(A_Tot_TA = cumsum(A_TA)) %>%
  # Takeaways Per Minute of Gameplay
  mutate(H_TA_PM = H_Tot_TA *60 / total_game_time) %>%
  mutate(A_TA_PM = A_Tot_TA *60 / total_game_time) %>%
  
  # Giveaway Identifier
  mutate(H_GvA = case_when((event == 'Giveaway' & team_id_for == H_team_id) ~ 1, TRUE ~ as.numeric(0))) %>% 
  mutate(A_GvA = case_when((event == 'Giveaway' & team_id_for == A_team_id) ~ 1, TRUE ~ as.numeric(0))) %>% 
  # Running Total Giveaways
  mutate(H_Tot_GvA = cumsum(H_GvA)) %>% 
  mutate(A_Tot_GvA = cumsum(A_GvA)) %>% 
  # Giveaways Per Minute of Gameplay
  mutate(H_GvA_PM = H_Tot_GvA *60 / total_game_time) %>%
  mutate(A_GvA_PM = H_Tot_GvA *60 / total_game_time) %>%
  
  # Blocked Shots Identifier (the team who blocked the shot)
  mutate(H_BS = case_when((event == 'Blocked Shot' & team_id_for == H_team_id) ~ 1, TRUE ~ as.numeric(0))) %>% 
  mutate(A_BS = case_when((event == 'Blocked Shot' & team_id_for == A_team_id) ~ 1, TRUE ~ as.numeric(0))) %>% 
  # Running Total Blocked Shots
  mutate(H_Tot_BS = cumsum(H_BS)) %>% 
  mutate(A_Tot_BS = cumsum(A_BS)) %>% 
  # Blocked Shots Per Minute of Gameplay
  mutate(H_BS_PM = H_Tot_BS *60 / total_game_time) %>% 
  mutate(A_BS_PM = A_Tot_BS *60 / total_game_time) %>%
  
  # Hit Identifier
  mutate(H_Hit = case_when((event == 'Hit' & team_id_for == H_team_id) ~ 1, TRUE ~ as.numeric(0))) %>%
  mutate(A_Hit = case_when((event == 'Hit' & team_id_for == A_team_id) ~ 1, TRUE ~ as.numeric(0))) %>%
  # Running Total Hits
  mutate(H_Tot_Hit = cumsum(H_Hit)) %>%
  mutate(A_Tot_Hit = cumsum(A_Hit)) %>%
  # Hits Per Minute of Gameplay
  mutate(H_Hit_PM = H_Tot_Hit *60 / total_game_time) %>%
  mutate(A_Hit_PM = A_Tot_Hit *60 / total_game_time) %>%
  
  # Saves Identifier
  mutate(H_SV = case_when((event == 'Shot' & team_id_for == A_team_id) ~ 1, TRUE ~ as.numeric(0))) %>%
  mutate(A_SV = case_when((event == 'Shot' & team_id_for == H_team_id) ~ 1, TRUE ~ as.numeric(0))) %>%
  # Running Total Saves
  mutate(H_Tot_SV = cumsum(H_SV)) %>%
  mutate(A_Tot_SV = cumsum(A_SV)) %>%
  # Saves Per Minute of Gameplay
  mutate(H_SV_PM = H_Tot_SV *60 / total_game_time) %>%
  mutate(A_SV_PM = A_Tot_SV *60 / total_game_time) %>%
  

  # Running Total Faceoffs Won
  mutate(H_Tot_FO = cumsum(H_FO_Win)) %>%
  mutate(A_Tot_FO = cumsum(A_FO_Win)) %>%
  # Faceoffs Won Per Minute of Gampelay
  mutate(H_FO_PM = H_Tot_FO *60 / total_game_time) %>%
  mutate(A_FO_PM = A_Tot_FO *60 / total_game_time) %>%

  # Scoring Chances Identifier
  mutate(H_SC = case_when(((event == 'Shot' | event == 'Missed Shot' |
                              event == 'Goal') &
                             house_s == 1 &
                             team_id_for == H_team_id)
                          ~ 1,
                          TRUE ~ as.numeric(0))) %>%
  mutate(A_SC = case_when(((event == 'Shot' | event == 'Missed Shot' |
                              event == 'Goal') &
                             house_s == 1 &
                             team_id_for == A_team_id)
                          ~ 1,
                          TRUE ~ as.numeric(0))) %>%
  # Running Total Scoring Chances
  mutate(H_Tot_SC = cumsum(H_SC)) %>%
  mutate(A_Tot_SC = cumsum(A_SC)) %>%
  # SC Per Minute of Gameplay
  mutate(H_SC_PM = H_Tot_SC *60 / total_game_time) %>%
  mutate(A_SC_PM = A_Tot_SC *60/ total_game_time)






# Checking if all the game_id s are present and in order ----------
df <- covar_play_by_play %>% filter(event == "Game Start")
covar_play_by_play <- covar_play_by_play[order(all_data$game_id), ]

# Get the minimum and maximum values in the column
min_value <- min(covar_play_by_play$game_id)
max_value <- max(covar_play_by_play$game_id)

# Create a sequence of expected values from the minimum to the maximum game_id
expected_values <- seq(min_value, max_value)

# Check for missing values
missing_values <- setdiff(expected_values, covar_play_by_play$game_id)

if (length(missing_values) == 0) {
  print("No missing games")
} else {
  print(paste("Missing games:", paste(missing_values, collapse = ", ")))
}

# Write out the data as new file WITH covariates ------------
write.csv(covar_play_by_play, 'pbp_w_covar.csv')


