project_location <- paste0("/AnalysisProjects/DraftKings/")
working_directory <- paste0(file.path(Sys.getenv("USERPROFILE"),"Documents"),project_location)
setwd(working_directory)

library(reticulate)
library(RPostgreSQL)
library(tidyverse)

driver  <- RPostgreSQL::PostgreSQL()
nfl_con <- RPostgreSQL::dbConnect(driver,host = 'localhost',port='5432',dbname='nfldb',user='nfldb',password='December69!')

RPostgreSQL::dbExistsTable(nfl_con, "play_player")
RPostgreSQL::dbListTables(nfl_con)

### Update Schedule / Players / DB ###
######## Activate python27 
### Update nfl game schedule ###  cd "C:\ProgramData\Anaconda3\envs\python27\Scripts\"
######## python27\Scripts>python update_sched.py --2018
### update nfl players ### cd "C:\ProgramData\Anaconda3\envs\python27\Scripts"
######## python27\Scripts>python nflgame-update-players2.py
### Update the database ###
######## python27\Scripts> python nfldb-update.py


#Connect to python source
reticulate::use_python('C:/ProgramData/Anaconda3/envs/python27/python')
reticulate::use_condaenv(condaenv = "python27",conda = ,required = T)

#load modules
nfldb <- import("nfldb")
nflgame <- import("nflgame")

#How to create python session in R
reticulate::repl_python() # You can run code below like it was in a python ide after calling repl_python
#import nfldb
#import nflgame
#nflgame.games(2018)
#exit



RPostgreSQL::dbListTables(nfl_con)




game_data_2018 <- RPostgreSQL::dbGetQuery(conn = nfl_con,statement = "select * from game 
                                                                      where season_year = '2018' and season_type = 'Regular'
                                                                      order by week")

#Gets team data ID | City | Name
team_data <- RPostgreSQL::dbGetQuery(conn = nfl_con, statement = "select * from team")
write_csv(team_data,'./Data/nfl-stats/team_data_10-17.csv')

#Gets meta data which is last updated and Year and Week
meta_data <- RPostgreSQL::dbGetQuery(conn = nfl_con, statement = "select * from meta")
write_csv(meta_data,'./Data/nfl-stats/last_updated_10-17.csv')

#Get a players individual plays and what the result of that play was 
every_play_player <- RPostgreSQL::dbGetQuery(conn = nfl_con, statement = 'select * from play_player')
write_csv(every_play_granular_data,'./Data/nfl-stats/every_play_player_data_10-17.csv')

#Get a players individual plays and what the result of that play was 
every_player <- RPostgreSQL::dbGetQuery(conn = nfl_con, statement = 'select * from player')
write_csv(every_player,'./Data/nfl-stats/every_player_data_10-17.csv')

#Get granular level play data | yards to go | yardline | down | first_down | rushing_first_down | etc
every_play_data <- RPostgreSQL::dbGetQuery(conn = nfl_con, statement = 'select * from play')
write_csv(every_play_data,'./Data/nfl-stats/every_play_granular_data_10-17.csv')

#Get aggregated play data | defense | fumbles | kicking | passing | punt | rushing | recieving
agg_play_data <- RPostgreSQL::dbGetQuery(conn = nfl_con, statement = 'select * from agg_play')
write_csv(agg_play_data,'./Data/nfl-stats/every_play_data_10-17.csv')

#Get individual game data | Day of Week | Year | Game Type | Quarter Score Breakdown | Turnovers | Home/Away
every_team_drive <- RPostgreSQL::dbGetQuery(conn = nfl_con, statement = "select * from drive")
write_csv(every_team_drive,'./Data/nfl-stats/team_drive_data_10-17.csv')

#Get individual game data | Day of Week | Year | Game Type | Quarter Score Breakdown | Turnovers | Home/Away
game_data_all <- RPostgreSQL::dbGetQuery(conn = nfl_con, statement = "select * from game")
write_csv(game_data_all,'./Data/nfl-stats/game_data_10-17.csv')


game_data_all %>% filter(season_year=='2018',week=='6') ->check

# team => team_data
# meta => meta_data
# play_player => every_play_player
# player => every_player
# play => every_play
# agg_play => agg_play_data
# drive => every_team_drive
# game => game_data_all




print(unique(every_player$position))
positions_to_keep <- c("TE","WR","RB","QB","K")
every_offensive_player <- every_player %>% filter(position %in% positions_to_keep)
write_csv(every_offensive_player,'./Data/nfl-stats/every_offensive_player_data_10-17.csv')






