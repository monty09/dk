rm(list=ls())
functions <- list()
project_location <- paste0("/AnalysisProjects/DraftKings/")
working_directory <- paste0(file.path(Sys.getenv("USERPROFILE"),"Documents"),project_location)
setwd(working_directory)
print(getwd())

functions$packages <- function(){
  Sys.setenv(TZ = "America/New_York")
  #Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_181')
  Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_181')
  
  
  
  package_names <- c('caret','mlbench','glmnet','h2o','modelr','tidyverse','rJava','xlsx',"readxl","devtools", "futile.logger", "DBI", "RMySQL", "uuid", "readr", "caret", "gbm", "rpart", "randomForest", "stringdist", "tm", "curl", "stringr", "jsonlite", "purrr", "dplyr", "tidyr", "fastmatch")
  ok = sapply(package_names,
              FUN = function (package_name) {
                suppressMessages(require(package_name, character.only=TRUE, quietly = T, warn.conflicts = F))
              })
  
}
functions$packages()

source('./Rscripts/4 - Utils/FF_Utils.R')
# every_player_2014 <- RPostgreSQL::dbGetQuery(conn = nfl_con, statement = "select * from play_player Where left(gsis_id,4) = '2014'")
# print_functions_in_file()
# how_to_update_database()
# write_database_to_csv('10-17',6)
update_draftkings_data(2018)
write_database_to_csv(date = '10-26',Week = 7)


RPostgreSQL::dbListTables(nfl_con)

date <- '10-26'
date_directory <- paste0('./Data/nfl-stats/',date,'/')
every_offensive_player <- read_csv(paste0(date_directory,'every_player_data_',date,'.csv'),col_types = cols(.default = "c"))
every_play_player <- read_csv(paste0(date_directory,'every_play_player_data_',date,'.csv'),col_types = cols(.default = "c"))


player_ids_gid <- read_csv('./Data/Players/player_ids.csv',col_types = cols(.default = "c"))
team_lookup <- read_csv('./Data/Players/team_lookup.csv',col_types = cols(.default = "c"))

dk_year_files <- list.files(path = "./Data/dk_csv/ByYear/",full.names = T)

dk.all <- lapply(dk_year_files,function(file){
  read_csv(file,col_types = cols(.default = "c"))
}) %>% bind_rows()


dk.all <- 
  dk.all %>%
  distinct(Name,GID,Pos,Team,Year) %>% 
  tidyr::separate(col = Name,sep = ", ",into = c('last_name','first_name'),remove=F) %>%
  distinct(last_name,first_name,GID,Pos,Team,Year) %>%
  filter(!is.na(first_name)) %>%
  dplyr::left_join(team_lookup,by=c('Team'='Team1'))

db_players2 <-
  every_offensive_player %>%
  distinct(player_id,first_name,last_name,full_name,position) %>%
  filter(!is.na(full_name)) %>%
  select(-full_name)

db_players <- 
  every_play_player %>%
  distinct(player_id,team) %>%
  dplyr::left_join(db_players2,by =c('player_id'='player_id'))

unique(db_players$position)
unique(dk.all$Pos)


player_crossdb <-
  dk.all %>% 
  left_join(db_players,by = c('first_name'='first_name','last_name'='last_name','Team2'='team','Pos'='position'))

player_cross_keep <- player_crossdb %>% filter(!is.na(player_id))
player_cross2 <- 
  player_crossdb %>% 
  filter(is.na(player_id)) %>% select(-player_id) %>%
  left_join(db_players,by = c('first_name'='first_name','last_name'='last_name','Team2'='team')) %>% select(-position)

player_cross_keep <- rbind(player_cross_keep,player_cross2 %>%filter(!is.na(player_id)))

player_cross3 <-
  player_cross2 %>%
  filter(is.na(player_id)) %>% select(-player_id) %>%
  left_join(db_players,by = c('first_name'='first_name','last_name'='last_name')) %>% select(-position,-team)

player_cross_keep <- rbind(player_cross_keep,player_cross3 %>%filter(!is.na(player_id)))
                           
player_ids_gid <- read_csv('./Data/Players/player_ids.csv',col_types = cols(.default = "c"))
player_cross4 <- 
  player_cross3 %>%
  filter(is.na(player_id)) %>% 
  select(-player_id) %>%
  left_join(player_ids_gid,by=c('GID'='GID'))

player_cross_keep <- rbind(player_cross_keep,player_cross4 %>%filter(!is.na(player_id)))
player_cross_keep2 <- player_cross_keep %>% distinct(GID,player_id)

write_csv(player_cross_keep2,"./Data/Players/player_lookup.csv")

rm(player_cross,player_cross_na,player_crossdb,player_crossdb_na,player_ids_gid,manual_cross,dk.14,dk.15,dk.16,dk.17,dk.18)


compile_combine_stats <- function(){
  combine_files <- list.files('./Data/Combine/',full.names = T)
  combine.all <- lapply(combine_files,function(file){
    read_csv(file,col_types = cols(.default = "c"))
  }) %>% bind_rows()
  
  unique(combine.all$POS)
  combine <-
    combine.all %>%
    filter(POS %in% c('QB','RB','WR','TE')) %>%
    select(-X1,-College,-`60Yd Shuttle`,-Wonderlic,-`Arm Length (in)`) %>%
    tidyr::separate(col = Name,sep = " ",into = c('first_name','last_name'),remove=F)
  write_csv(combine,"./Data/Players/combine_lookup.csv")
}


combine_lookup <- read_csv("./Data/Players/combine_lookup.csv",col_types = cols(.default = "c")) %>% select(-Year,-Name)

df <- 
  player_cross_keep %>%
  dplyr::left_join(combine_lookup,by=c('first_name'='first_name','last_name'='last_name','Pos'='POS')) %>%
  distinct(GID,player_id,.keep_all = T) %>%
  select(-Team2)

write_csv(df,"./Data/Players/combine_player_id.csv")







