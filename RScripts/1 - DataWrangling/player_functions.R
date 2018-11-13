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

#Give the year and get back the DraftKings Results
functions$dk_df <- function(year){
  dk.df <- read_csv(file = paste0('./Data/dk_csv/ByYear/dk_',year,'.csv')
                    ,col_types = cols(.default = "c"))
  
  dk.df <- dk.df %>% mutate_at(.vars = vars(Week,DK.points,DK.salary),.funs = funs(as.numeric)) 
  
  #Get rid of non defense 0 point players
  dk.df <- rbind(dk.df %>% filter(DK.points==0,Pos=='Def'),dk.df %>% filter(DK.points!=0))
  #Keep highest salary, then highest point qb of the week
  dk.df.qb <- dk.df %>% filter(Pos=='QB') %>% group_by(Team,Week,Pos) %>% mutate(max_qb = max(DK.salary)) %>% mutate(max_qb_p = max(DK.points)) %>% ungroup() %>% filter(DK.salary == max_qb) %>% filter(DK.points == max_qb_p)
  dk.df <- rbind(dk.df %>% filter(Pos!='QB'),dk.df.qb %>% select(-max_qb,-max_qb_p))
  rm(dk.df.qb)
  return(dk.df)
} #MAIN FUNCTION # 1

###For a given week, return a team's points and salary by position
functions$player_week_points_salary_pos <- function(weekly_dataframe,week_num,player_stats_by_week){
  pts_week_name <- paste0("Week.",week_num,".Pts")
  salary_week_name <- paste0("Week.",week_num,".Salary")
  pts_sal_week_name <- paste0("Week.",week_num,".SPP")
  rank_week_name <- paste0("Week.",week_num,".PTRank")
  oppt_week_name <- paste0("Week.",week_num,".Oppt")
  
  keep_defense <- c('defense_frec','defense_fgblk','defense_frec_tds','defense_int','defense_int_tds','defense_misc_tds','defense_safe'
                    ,'defense_sk','kickret_tds','puntret_tds')
  keep_passing <- c('passing_tds','passing_att','passing_cmp','passing_cmp_air_yds','passing_int','passing_sk','passing_tds','passing_yds')
  keep_rushing <- c('rushing_att','rushing_tds','rushing_yds')
  keep_recieving <- c('receiving_rec','receiving_tar','receiving_tds','receiving_yac_yds','receiving_yds')
  keep <- c(keep_defense,keep_passing,keep_recieving,keep_rushing)
  
  psbw <- 
    as.data.frame(player_stats_by_week) %>%
    mutate_at(.vars = vars(keep),.funs=funs(as.numeric)) %>%
    group_by(player_id,team) %>%
    summarise_at(.vars = vars(keep),.funs = funs(sum)) %>%
    ungroup() 
    
  colnames(psbw) <- colnames(psbw) %>% map_chr(function(x){
                  ret <- x
                  if(x!='team' && x!= 'player_id'){
                    ret <- paste0("Week.",week_num,".",x)
                  }
                  ret
    })
  
  team_lookup <- read_csv('./Data/Players/team_lookup.csv',col_types = cols(.default = "c"))
  defense_stats <- 
    psbw %>%
    group_by(team) %>%
    summarise_at(.vars = vars(colnames(psbw)[3:12]),.funs = funs(sum)) %>%
    ungroup() %>%
    dplyr::left_join(team_lookup,by=c('team'='Team2'))

  Non_Defense <- weekly_dataframe %>% dplyr::left_join(psbw,by=c('player_id'='player_id')) %>% filter(Pos!='Def',!is.na(team))
  return_nondef <-
    Non_Defense %>% 
    filter(Week == week_num) %>% 
    mutate(!!oppt_week_name := Oppt) %>%
    mutate(!!pts_week_name := DK.points) %>%
    mutate(!!salary_week_name := DK.salary) %>%
    mutate(!!pts_sal_week_name := ifelse(DK.points != 0,DK.salary/DK.points,0)) %>%
    group_by(Pos) %>%
    mutate(!!rank_week_name := dense_rank(desc(DK.points))) %>%
    ungroup() %>%
    select(-DK.points,-DK.salary,-h.a,-Oppt,-team,-Week)
  
  Defense <- weekly_dataframe %>% filter(Pos=='Def') %>% dplyr::left_join(defense_stats,by=c('Team'='Team1'))
  return_def <-
    Defense %>% 
    filter(Week == week_num) %>% 
    mutate(!!pts_week_name := DK.points) %>%
    mutate(!!salary_week_name := DK.salary) %>%
    mutate(!!pts_sal_week_name := ifelse(DK.points != 0,DK.salary/DK.points,0)) %>%
    group_by(Pos) %>%
    mutate(!!rank_week_name := dense_rank(desc(DK.points))) %>%
    ungroup() %>%
    select(-DK.points,-DK.salary,-h.a,-Oppt,-team,-Week)
  
  colnames_missing <- setdiff(colnames(return_nondef),colnames(return_def))
  return_def[,colnames_missing] <- NA
  
  return_df <- rbind(return_nondef,return_def)
  
  
  return(return_df)
}

###Add in home and away averages to dataframes
functions$player_home_away_averages <- function(weekly_dataframe){
  player_home_away_avgs <-
    weekly_dataframe %>%
    group_by(Team,Pos,Week,GID,Name) %>%
    mutate(sum_pts = sum(DK.points)) %>%
    mutate(sum_sals = sum(DK.salary)) %>%
    ungroup() %>%
    group_by(GID,Name,Team,Pos,h.a) %>%
    mutate(avg_points = round(mean(sum_pts),2)) %>%
    mutate(avg_salary = round(mean(sum_sals),2)) %>%
    distinct(GID,Name,Team,Pos,h.a,avg_points,avg_salary) %>%
    tidyr::unite(all_avg,avg_points,avg_salary) %>%
    spread(h.a,all_avg) %>%
    separate(col = "a",into = c("away_avg_pts","away_avg_salary"),sep = "_") %>%
    separate(col = "h",into = c("home_avg_pts","home_avg_salary"),sep = "_")
  return(player_home_away_avgs)
}

###Add in database id data to player
functions$player_database_id <- function(weekly_dataframe){
  player_lookup <- read_csv(file = "./Data/Players/player_lookup.csv",col_types = cols(.default = "c")) # Player Convert GID to player_id
  weekly_dataframe <- 
    weekly_dataframe %>%
    dplyr::left_join(player_lookup %>% select(GID,player_id),by=c('GID'='GID')) %>%
    select(GID,player_id,everything())
  return(weekly_dataframe)
}

###Add in database data basics height,weight,age,years_pro
functions$player_database_basics <- function(weekly_dataframe,date,year){
  date_directory <- paste0('./Data/nfl-stats/',date,'/')
  every_offensive_player <- read_csv(paste0(date_directory,'every_player_data_',date,'.csv'),col_types = cols(.default = "c"))
  
  weekly_dataframe <- 
    weekly_dataframe %>%
    dplyr::left_join(every_offensive_player %>% select(player_id,birthdate,years_pro),by=c('player_id'='player_id')) %>%
    mutate(age = year - as.numeric(str_sub(birthdate,nchar(birthdate)-3,nchar(birthdate)))) %>%
    select(-birthdate) %>%
    select(GID,player_id,everything())
}

###Add in database data for year, split up by week
functions$player_database_in_depth <- function(date,year){
  date_directory <- paste0('./Data/nfl-stats/',date,'/')
  every_play_player <- read_csv(paste0(date_directory,'every_play_player_data2_',date,'.csv'),col_types = cols(.default = "c"))
  game_data_all <- read_csv(paste0('./Data/nfl-stats/',date,'/game_data_',date,'.csv'),col_types = cols(.default = "c"))
  
  #colnames(every_play_player)
  every_play_player <- every_play_player %>% filter(str_sub(gsis_id,end = 4)==year)
  keep_basic <- c('gsis_id','player_id','team')
  keep_defense <- c('defense_frec','defense_fgblk','defense_frec_tds','defense_int','defense_int_tds','defense_misc_tds','defense_safe'
                    ,'defense_sk','kickret_tds','puntret_tds')
  keep_passing <- c('passing_tds','passing_att','passing_cmp','passing_cmp_air_yds','passing_int','passing_sk','passing_tds','passing_yds')
  keep_rushing <- c('rushing_att','rushing_tds','rushing_yds')
  keep_recieving <- c('receiving_rec','receiving_tar','receiving_tds','receiving_yac_yds','receiving_yds')
  every_play_player <- every_play_player %>% select(keep_basic,keep_passing,keep_rushing,keep_recieving,keep_defense)
  
  game_data_keep <- c('gsis_id','week','day_of_week','home_team','away_team','season_type','gamekey')
  every_play_player <- 
    every_play_player %>% 
    dplyr::left_join(game_data_all %>% select(game_data_keep) ,by=c('gsis_id'='gsis_id')) %>%
    filter(season_type == 'Regular') %>%
    select(-season_type)

 return(every_play_player)

}

###Add in combine data
functions$player_combine_stats <- function(weekly_dataframe){
  combine_data <- read_csv("./Data/Players/combine_player_id.csv",col_types = cols(.default = "c"))
  
  setdiff(colnames(combine_data),colnames(weekly_dataframe))
  weekly_dataframe2 <-
    weekly_dataframe %>%
    dplyr::left_join(combine_data,by=c('player_id'='player_id','GID'='GID'))
}

###This will transform into the Player's points and salary by week
functions$player_df <- function(weekly_dataframe,date,year){
  # weekly_dataframe <- dk.14
  # date <- '10-26'
  # year <- 2014
  # week_num <- 1
  weekly_dataframe <- functions$player_database_id(weekly_dataframe)
  weekly_dataframe <- functions$player_database_basics(weekly_dataframe,date,year)
  player_stats_by_week <- functions$player_database_in_depth(date,year)
  
  dataframes <- lapply(sort(unique(weekly_dataframe$Week)),function(week_num){
    player_stats_by_week <- player_stats_by_week %>% filter(week==week_num)
    weekly_dataframe <- weekly_dataframe %>% filter(Week == week_num)
    print(week_num)
    functions$player_week_points_salary_pos(weekly_dataframe,week_num,player_stats_by_week)
  })
  
  rdf <- plyr::join_all(dfs = dataframes,by = c('Name','GID','Pos','Team'),type='full')
  player_home_away_avgs <- functions$player_home_away_averages(weekly_dataframe)
  
  weekly_dataframe <- 
    weekly_dataframe %>%
    distinct(Name,GID,Pos,Team) %>%
    dplyr::left_join(player_home_away_avgs,by=c('Name'='Name','GID'='GID','Pos'='Pos','Team'='Team')) %>%
    dplyr::left_join(rdf,by=c('Name'='Name','GID'='GID','Pos'='Pos','Team'='Team'))

  combine_stats <- functions$player_combine_stats(weekly_dataframe) 
  combine_stats <- 
    combine_stats %>% 
    select(GID,player_id,Height,Weight,`Hand Size (in)`,`40 Yard`,`Bench Press`,`Vert Leap (in)`,`Broad Jump (in)`,Shuttle,`3Cone`)
  
  all_players <-
    weekly_dataframe %>%
    dplyr::left_join(combine_stats,by=c('GID'='GID','player_id'='player_id')) %>%
    dplyr::distinct(GID,.keep_all = T)

  return(all_players)
}    #MAIN FUNCTION # 2

#Split up each dataframe into indiviudal positions
functions$player_position_split <- function(player_dataframe){
  positions <- list()
  positions$QB <- player_dataframe %>% filter(Pos=='QB') %>% select(-contains("defense"),-contains("kick"),-contains("punt"),-contains("receiving"))
  positions$RB <- player_dataframe %>% filter(Pos=='RB') %>% select(-contains("defense"),-contains("passing"))
  positions$WR <- player_dataframe %>% filter(Pos=='WR') %>% select(-contains("defense"),-contains("passing"))
  positions$TE <- player_dataframe %>% filter(Pos=='TE') %>% select(-contains("defense"),-contains("passing"))
  positions$Def <- player_dataframe %>% filter(Pos=='Def') %>% select(contains("defense"),contains("kick"),contains("punt"))
  
  return(positions)
}


#Split Up Players By Week Segmentation 
functions$player_week_split_up <- function(player_dataframe, week_start = 1, num_weeks = 3){
  keep_cols <- c("GID","Name","Pos","Team","away_avg_pts","away_avg_salary","home_avg_pts","home_avg_salary","player_id","Year","years_pro","age","Height","Weight","Vert Leap (in)","Hand Size (in)","Broad Jump (in)","40 Yard","Shuttle","Bench Press","3Cone")
  x <- c()
  for(i in week_start:(week_start+num_weeks-1)){x[i] = paste0('Week.',i,'.')}
  vars <- lapply(x,function(week){
    cols <- colnames(player_dataframe %>% select(contains(week)))
  }) %>% unlist() %>% as.vector()
  
  player_dataframe <-
    player_dataframe %>% 
    select(keep_cols,vars)
  
  avg_pts_name <- paste0("Avg_pts_",num_weeks)
  avg_sal_name <- paste0("Avg_sal_",num_weeks)
  avg_pt_sal <- paste0("Avg_SPP_",num_weeks)
  
  pd <- 
    player_dataframe %>% 
    mutate(!!avg_pts_name := rowMeans(select(.,contains(".Pts")), na.rm = TRUE)) %>%
    mutate(!!avg_sal_name := rowMeans(select(.,contains(".Sal")), na.rm = TRUE)) %>%
    mutate(!!avg_pt_sal := (rowMeans(select(.,contains(".Sal")), na.rm = TRUE) / rowMeans(select(.,contains(".Pts")), na.rm = TRUE)))
}









