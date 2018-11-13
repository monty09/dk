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

dk.14 <- read_csv(file = './Data/dk_csv/ByYear/dk_2014.csv'
                  ,col_types = cols(.default = "c"))

dk.14 <- dk.14 %>% mutate_at(.vars = vars(Week,DK.points,DK.salary),.funs = funs(as.numeric)) 

#Get rid of non defense 0 point players
dk.14 <- rbind(dk.14 %>% filter(DK.points==0,Pos=='Def'),dk.14 %>% filter(DK.points!=0))
#Keep highest salary, then highest point qb of the week
dk.14.qb <- dk.14 %>% filter(Pos=='QB') %>% group_by(Team,Week,Pos) %>% mutate(max_qb = max(DK.salary)) %>% mutate(max_qb_p = max(DK.points)) %>% ungroup() %>% filter(DK.salary == max_qb) %>% filter(DK.points == max_qb_p)
dk.14 <- rbind(dk.14 %>% filter(Pos!='QB'),dk.14.qb %>% select(-max_qb,-max_qb_p))
rm(dk.14.qb)

###For a given week, return a team's points and salary by position
functions$player_week_points_salary_pos <- function(weekly_dataframe,week_num){
  pts_week_name <- paste0("Week.",week_num,".Pts")
  salary_week_name <- paste0("Week.",week_num,".Salary")
  pts_sal_week_name <- paste0("Week.",week_num,".SPP")
  rank_week_name <- paste0("Week.",week_num,".PTRank")

  return_df <-
    weekly_dataframe %>% 
    filter(Week == week_num) %>% 
    select(Name,GID,Pos,Team,DK.points,DK.salary) %>%
    mutate(!!pts_week_name := DK.points) %>%
    mutate(!!salary_week_name := DK.salary) %>%
    mutate(!!pts_sal_week_name := ifelse(DK.points != 0,DK.salary/DK.points,0)) %>%
    group_by(Pos) %>%
    mutate(!!rank_week_name := dense_rank(desc(DK.points))) %>%
    distinct_('Name','GID','Pos','Team',pts_week_name,salary_week_name,pts_sal_week_name,rank_week_name) %>%
    ungroup()
  
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

###This will transform into the Player's points and salary by week
functions$player_salary_points <- function(weekly_dataframe){
  
  dataframes <- lapply(sort(unique(weekly_dataframe$Week)),function(week){
    functions$player_week_points_salary_pos(weekly_dataframe,week)
  })
  
  rdf <- plyr::join_all(dfs = dataframes,by = c('Name','GID','Pos','Team'),type='full')
  player_home_away_avgs <- functions$player_home_away_averages(weekly_dataframe)
  
  all_players <- 
    weekly_dataframe %>%
    distinct(Name,GID,Pos,Team) %>%
    dplyr::left_join(player_home_away_avgs,by=c('Name'='Name','GID'='GID','Pos'='Pos','Team'='Team')) %>%
    dplyr::left_join(rdf,by=c('Name'='Name','GID'='GID','Pos'='Pos','Team'='Team'))
    
  
  return(all_players)
}

player_byweek_dataframe_2014 <- functions$player_salary_points(dk.14)     #Each Individual Player 2014 DK Season Results



str_split(dk.14$Name[100],pattern = ",")[[1]][1]
###For a given week, return a team's points and salary by position
functions$team_week_points_salary_pos <- function(weekly_dataframe,week_num){
  max_pts_week_name <- paste0("Max.Week.",week_num,".Pts")
  max_salary_week_name <- paste0("Max.Week.",week_num,".Salary")
  total_pts_week_name <- paste0("Total.Week.",week_num,".Pts")
  total_salary_week_name <- paste0("Total.Week.",week_num,".Salary")
  pos_count_week_name <- paste0("Week.",week_num,".Pos.Count")

  return_df <-
    weekly_dataframe %>% 
    filter(Week == week_num) %>% 
    select(Team,Pos,DK.points,DK.salary) %>%
    group_by(Team,Pos) %>% 
    mutate(!!max_pts_week_name := max(DK.points)) %>% 
    mutate(!!max_salary_week_name := max(DK.salary)) %>% 
    mutate(!!total_pts_week_name := sum(DK.points)) %>% 
    mutate(!!total_salary_week_name := sum(DK.salary)) %>% 
    mutate(!!pos_count_week_name := n()) %>% 
    distinct_('Team','Pos',max_pts_week_name,max_salary_week_name,pos_count_week_name,total_pts_week_name,total_salary_week_name) %>%
    arrange(Team,Pos) %>%
    ungroup()
  
  return(return_df)
    
}

###Add in home and away averages to dataframes
functions$team_home_away_averages <- function(weekly_dataframe){
  team_home_away_avgs <-
    weekly_dataframe %>%
    group_by(Team,Pos,Week) %>%
    mutate(sum_pts = sum(DK.points)) %>%
    mutate(sum_sals = sum(DK.salary)) %>%
    mutate(max_pts = max(DK.points)) %>%
    mutate(max_sals = max(DK.salary)) %>%
    ungroup() %>%
    group_by(Team,Pos,h.a) %>%
    mutate(avg_points = round(mean(DK.points),2)) %>%
    mutate(avg_salary = round(mean(DK.salary),2)) %>%
    mutate(max_avg_points = round(mean(max_pts),2)) %>%
    mutate(max_avg_salary = round(mean(max_sals),2)) %>%
    distinct(Team,Pos,h.a,avg_points,avg_salary,max_avg_points,max_avg_salary) %>%
    tidyr::unite(all_avg,avg_points,avg_salary,max_avg_points,max_avg_salary) %>%
    spread(h.a,all_avg) %>%
    separate(col = "a",into = c("away_avg_pts","away_avg_salary","away_max_avg_points","away_max_avg_salary"),sep = "_") %>%
    separate(col = "h",into = c("home_avg_pts","home_avg_salary","home_max_avg_points","home_max_avg_salary"),sep = "_")
  return(team_home_away_avgs)
}

###Add in teams points against
functions$team_oppt_pos_averages <- function(weekly_dataframe){
  oppt_numbers <-
    weekly_dataframe %>%
    select(Week,Pos,Oppt,h.a,DK.points,DK.salary) %>%
    mutate(h.a = ifelse(h.a == 'h','a','h')) %>%
    group_by(Pos,Oppt,Week) %>%
    mutate(max_pts = max(DK.points)) %>%
    mutate(max_sals = max(DK.salary)) %>%
    ungroup() %>%
    group_by(Pos,Oppt,h.a) %>% 
    mutate(avg_points = round(mean(DK.points),2)) %>%
    mutate(avg_salary = round(mean(DK.salary),2)) %>%
    mutate(max_avg_points = round(mean(max_pts),2)) %>%
    mutate(max_avg_salary = round(mean(max_sals),2)) %>%
    ungroup() %>%
    distinct(Pos,Oppt,h.a,avg_points,avg_salary,max_avg_points,max_avg_salary) %>%
    tidyr::unite(points_salary,avg_points,avg_salary,max_avg_points,max_avg_salary) %>%
    tidyr::spread(h.a,points_salary) %>%
    tidyr::separate(col = "a",into = c("oppt_away_avg_pts","oppt_away_avg_salary","oppt_away_max_avg_points","oppt_away_max_avg_salary"),sep = "_") %>%
    tidyr::separate(col = "h",into = c("oppt_home_avg_pts","oppt_home_avg_salary","oppt_home_max_avg_points","oppt_home_max_avg_salary"),sep = "_") %>%
    filter(Oppt != "-")
  return(oppt_numbers)
    
}

###This will transform into the Team's points and salary by position by week
functions$team_salary_points <- function(weekly_dataframe){

  dataframes <- lapply(sort(unique(weekly_dataframe$Week)),function(week){
    functions$team_week_points_salary_pos(weekly_dataframe,week)
  })
  
  rdf <- plyr::join_all(dfs = dataframes,by = c('Team','Pos'),type='full')
  
  team_home_away_avgs <- functions$team_home_away_averages(weekly_dataframe)
  oppt_home_away_avgs <- functions$team_oppt_pos_averages(weekly_dataframe)
  
  all_teams <- 
    weekly_dataframe %>%
    distinct(Team,Pos) %>%
    dplyr::left_join(team_home_away_avgs,by=c('Pos'='Pos','Team'='Team')) %>%
    dplyr::left_join(oppt_home_away_avgs, by = c('Pos'='Pos','Team'='Oppt')) %>%
    dplyr::left_join(rdf,by=c('Pos'='Pos','Team'='Team'))
    
  return(all_teams)
}


team_byweek_dataframe_2014 <- functions$team_salary_points(dk.14)         #Each Team 2014 DK Pos Results

player_lookup <- read_csv(file = "./Data/Players/player_lookup.csv",col_types = cols(.default = "c"))



functions$trend_players <- function(weekly_dataframe){
  
}

trend_value <- function(date,value,games_back=3){
  #date <- emp_max %>% arrange(desc(date_of_record)) %>% pull(date_of_record) %>% as.numeric()
  trend <- 0
  date <- date %>% as.numeric()
  value <- value %>% as.numeric()
  #print(length(date))
  if(length(date) > 11){
    dates <- date[1]
    vals <- value[1]
    #print(dates)
    for(i in 1:back){
      if(length(date) > i*12){
        dates <- append(dates,date[i*12 + 1])
        vals <- append(vals,value[i*12 + 1])
      }
      else{break}
    }
    if(length(dates) > 1){
      dates <- rev(dates)
      vals <- rev(vals)
      trend <- round(coef(lm(vals~dates))[2],8)
      #print(dates)
      #print(vals)
      
      if(trend > 0){trend = 1}
      if(trend < 0){trend = -1}
      print(paste0("The trend is: ",trend))
    }
    
  }
  
  return(trend)
}

delta_value <- function(date,value,back=2){
  date <- date %>% as.numeric()
  value <- value %>% as.numeric()
  
  delta <- 0
  if(length(date) > 11){
    dates <- date[1]
    vals <- value[1]
    for(i in 1:back){
      if(length(date) > i*12){
        vals <- append(vals,value[i*12 + 1])
      }
    }
    delta <- `if`(length(unique(vals))>1,length(unique(vals)),0)
    print(paste0("Delta is: ",delta))
  }
  
  return(delta)
}




