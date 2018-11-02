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
  
  return_df <-
    weekly_dataframe %>% 
    filter(Week == week_num) %>% 
    select(Name,GID,Pos,Team,DK.points,DK.salary) %>%
    mutate(!!pts_week_name := DK.points) %>%
    mutate(!!salary_week_name := DK.salary) %>%
    distinct_('Name','GID','Pos','Team',pts_week_name,salary_week_name) %>%
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


player_byweek_dataframe_2014 <- functions$player_salary_points(dk.14) 

week1_player_df <- 
  player_byweek_dataframe_2014 %>%
  select(GID,Name,Pos,Team,Week.1.Pts,Week.1.Salary) %>%
  filter(!is.na(Week.1.Salary),!is.na(Week.1.Pts))


library(lpSolve)
find_teams <- function(train, cap = 50000, constraint = "none", league = "DraftKings", setplayers = NULL,removeplayers = NULL, removeteams = NULL) {
  
  colnames(train) <- c("GID", "Name", "Pos", "Team", "Pts", "Salary")
  
  ## set constraints to use
  
  qb <- ifelse(train$Pos == "QB", 1, 0)
  wr <- ifelse(train$Pos == "WR", 1, 0)
  te <- ifelse(train$Pos == "TE", 1, 0)
  rb <- ifelse(train$Pos == "RB", 1, 0)
  def <- ifelse(train$Pos == "Def", 1, 0)
  
  ## number of decision variables is equal to the number of fantasy players/teams
  lpfantasy <- make.lp(0, nrow(train))
  
  ## Set objective function with the expected number of points
  set.objfn(lpfantasy, train$Pts)
  
  ## Make sure the decision variables are binary
  set.type(lpfantasy, seq(1, nrow(train), by=1), type = c("binary"))
  
  ## Add Position Constraints

  dk_total <- qb + wr + rb + te + def
  add.constraint(lpfantasy, qb, "=", 1)
  add.constraint(lpfantasy, wr, "<=", 4)
  add.constraint(lpfantasy, rb, "<=", 3)
  add.constraint(lpfantasy, te, "<=", 2)
  add.constraint(lpfantasy, def, "=", 1)
  add.constraint(lpfantasy, dk_total, "=", 9)
  
  
  ## Add monetary constraint, max salary for the team
  add.constraint(lpfantasy, train$Salary, "<=", cap)
  
  ## Set objective direction
  lp.control(lpfantasy, sense='max')
  
  team_names <- levels(factor(train$Team))
  constraint <- match.arg(constraint)
  
  if(!is.null(setplayers)) {
    ## Set constraints that each player here must be in lineup
    for(k in 1:nrow(setplayers)) {
      add.constraint(lpfantasy, ifelse(setplayers$GID[k] == train$GID, 1, 0), "=", 1)
    }
  }
  
  if(!is.null(removeplayers)) {
    ## Set constraints that each player here must be in lineup
    for(k in 1:nrow(removeplayers)) {
      add.constraint(lpfantasy, ifelse(removeplayers$GID[k] == train$GID, 1, 0), "=", 0)
    }
  }
  
  if(!is.null(removeteams)) {
    if(nrow(removeteams) != nrow(train))
      stop("Your team restrictions do not match the number of players included in the 'train' file")
    for(m in 1:ncol(removeteams)) {
      add.constraint(lpfantasy, removeteams[, m], "<=", 8)
    }
  }


  team <- data.frame(matrix(0, 1, ncol(train) + 2))
  colnames(team) <- c(colnames(train), "TeamSalary", "TotalPoints")
  
  ## Solve the model, if this returns 0 an optimal solution is found
  solve(lpfantasy)
  if(solve(lpfantasy) != 0)
    stop("Optimization failed at some step")
  
  ## Get the players on the team
  team_select <- subset(data.frame(train, get.variables(lpfantasy)), get.variables.lpfantasy. == 1)
  team_select$get.variables.lpfantasy. <- NULL
  team_select$TeamSalary <- sum(team_select$Salary)
  team_select$TotalPoints <- sum(team_select$Pts)
  team <- rbind(team, team_select)
  team <- team[-1,]
  rownames(team) <- NULL
  team
}

train <- week1_player_df

removeplayers = NULL
teams <- vector("list")
for(i in 1:5){
  print(i)
  team <- paste0("Team",i)
  result <- find_teams(train,removeplayers = removeplayers)
  removeplayers <- rbind(removeplayers,train %>% filter(GID %in% result$GID))
  result$team_num <- i
  teams[[team]] <- result
  
}

all_teams <- teams %>% bind_rows()





library(lpSolveAPI)
library(lpSolve)
library(FLSSS)
library(adagio)
library(ompr)
library(ompr.roi)
library(ROI)
library(ROI.plugin.glpk)







