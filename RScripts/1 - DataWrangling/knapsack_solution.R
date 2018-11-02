

library(lpSolveAPI)
library(lpSolve)
library(FLSSS)
library(adagio)
library(ompr)
library(ompr.roi)
library(ROI)
library(ROI.plugin.glpk)

library(lpSolve)
find_teams <- function(train, cap = 50000, constraint = "none", league = "DraftKings", setplayers = NULL,removeplayers = NULL, removeteams = NULL) {

  colnames(train) <- c("GID", "Name", "Pos", "Team","Week", "Pts", "Salary")
  
  ## set constraints to use
  train$Pts <- as.numeric(train$Pts)
  train$Salary <- as.numeric(train$Salary)
  
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
  add.constraint(lpfantasy, rb, "<=", 3)
  add.constraint(lpfantasy, rb, ">=", 2)
  add.constraint(lpfantasy, wr, "<=", 4)
  add.constraint(lpfantasy, wr, ">=", 3)
  add.constraint(lpfantasy, te, "<=", 2)
  add.constraint(lpfantasy, te, ">=", 1)
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

top_teams <- function(train, num_top = 5) {
  result <- find_teams(train)
  restrict <- as.matrix(rep(0, nrow(train)))
  restrict[match(result$GID, train$GID), 1] <- 1
  j <- 1
  
  while(j < num_top) {
    resultnew <- find_teams(train, removeteams = restrict)
    restrict <- cbind(restrict, rep(0, nrow(restrict)))
    restrict[match(resultnew$GID, train$GID), j] <- 1
    result <- rbind(result, resultnew)
    j <- j + 1
  }
  
  TeamNumber <- rep(1:num_top, each = 9)
  result <- cbind.data.frame(result, TeamNumber)
  result
} 

independent_teams <- function(train){
  removeplayers = NULL
  teams <- vector("list")
  for(i in 1:5){
    #print(i)
    team <- paste0("Team",i)
    result <- find_teams(train,removeplayers = removeplayers)
    removeplayers <- rbind(removeplayers,train %>% filter(GID %in% result$GID))
    result$TeamNumber <- i
    teams[[team]] <- result
  }
  all_teams <- teams %>% bind_rows()
  return(all_teams)
}




dk_files <- list.files(path = 'C:/Users/MonticT/Documents/AnalysisProjects/DraftKings/Data/dk_csv/ByYear/',full.names = T)
dk.14 <- read_csv(file = dk_files[1],col_types = cols(.default = "c"))
dk.15 <- read_csv(file = dk_files[2],col_types = cols(.default = "c"))
dk.16 <- read_csv(file = dk_files[3],col_types = cols(.default = "c"))
dk.17 <- read_csv(file = dk_files[4],col_types = cols(.default = "c"))

colnames(train) <- c("GID", "Name", "Pos", "Team","Week", "Pts", "Salary")
dk.14 <- dk.14 %>% select(GID,Name,Pos,Team,Week,DK.points,DK.salary) %>% rename(Pts="DK.points",Salary="DK.salary")
dk.15 <- dk.15 %>% select(GID,Name,Pos,Team,Week,DK.points,DK.salary) %>% rename(Pts="DK.points",Salary="DK.salary")
dk.16 <- dk.16 %>% select(GID,Name,Pos,Team,Week,DK.points,DK.salary) %>% rename(Pts="DK.points",Salary="DK.salary")
dk.17 <- dk.17 %>% select(GID,Name,Pos,Team,Week,DK.points,DK.salary) %>% rename(Pts="DK.points",Salary="DK.salary")

train <- dk.14 %>% filter(Week==1)
top_teams(train,5)
independent_teams(train)


best14 <- lapply(unique(dk.14$Week),function(week){
  y <- dk.14 %>% filter(Week==week,!is.na(Pts),Salary > 0)
  x <- top_teams(train=y,5)
  x$year <- 2014
  x
}) %>% bind_rows()

best15 <- lapply(unique(dk.15$Week),function(week){
  y <- dk.15 %>% filter(Week==week,!is.na(Pts),Salary > 0)
  x <- top_teams(train=y,5)
  x$year <- 2015
  x
}) %>% bind_rows()

best16 <- lapply(unique(dk.16$Week),function(week){
  y <- dk.16 %>% filter(Week==week,!is.na(Pts),Salary > 0)
  x <- top_teams(train=y,5)
  x$year <- 2016
  x
}) %>% bind_rows()

best17 <- lapply(unique(dk.17$Week),function(week){
  y <- dk.17 %>% filter(Week==week,!is.na(Pts),Salary > 0)
  x <- top_teams(train=y,5)
  x$year <- 2017
  x
}) %>% bind_rows()

best_all <- rbind(best14,best15,best16,best17)


position_breakdown <- function(winner_dataframe,year,with_team_number = F){
  if(with_team_number){
    best_how <- 
      winner_dataframe %>% 
      select(Pos,Week,year,TeamNumber) %>%
      group_by(Week,year,TeamNumber) %>%
      add_count(Pos) %>%
      ungroup() %>%
      distinct(Pos,Week,year,TeamNumber,n) %>%
      filter(Pos %in% c('RB','WR','TE'))
    
    num_weeks <- best_how %>% distinct(Week,year,TeamNumber) %>% nrow()
  }
  
  if(!with_team_number){
    best_how <- 
      winner_dataframe %>% 
      select(Pos,Week,year,TeamNumber) %>%
      group_by(Week,year,TeamNumber) %>%
      add_count(Pos) %>%
      ungroup() %>%
      distinct(Pos,Week,year,TeamNumber,n) %>%
      filter(Pos %in% c('RB','WR','TE'))
    
    num_weeks <- best_how %>% distinct(Week,year) %>% nrow()  
  }
  
  best_rb_count <- best_how %>% filter(Pos=='RB',n==3) %>% nrow()
  best_wr_count <- best_how %>% filter(Pos=='WR',n==4) %>% nrow()
  best_te_count <- best_how %>% filter(Pos=='TE',n==2) %>% nrow()
  
  rb_flex_percent <- best_rb_count / num_weeks * 100
  wr_flex_percent <- best_wr_count / num_weeks * 100
  te_flex_percent <- best_te_count / num_weeks * 100
  
  print(paste0("For Year(s): ",year," 'Winning Lineups have Flex position of' "," RB Flex is: ",round(rb_flex_percent,2),"% WR Flex is: ",round(wr_flex_percent,2),"% TE Flex is: ",round(te_flex_percent,2),"%"))

}

all_best5 <- function(){
  print("DraftKings Winning Lineup Quick Breakdown")
  print("Using 5 Best Teams Per Week: ")
  position_breakdown(best_all,'2014 to 2017',T)
  position_breakdown(best17,'2017',T)
  position_breakdown(best16,'2016',T)
  position_breakdown(best15,'2015',T)
  position_breakdown(best14,'2014',T)
}
all_best5()





best14 <- lapply(unique(dk.14$Week),function(week){
  y <- dk.14 %>% filter(Week==week,!is.na(Pts),Salary > 0)
  x <- independent_teams(train=y)
  x$year <- 2014
  x
}) %>% bind_rows()

best15 <- lapply(unique(dk.15$Week),function(week){
  y <- dk.15 %>% filter(Week==week,!is.na(Pts),Salary > 0)
  x <- independent_teams(train=y)
  x$year <- 2015
  x
}) %>% bind_rows()

best16 <- lapply(unique(dk.16$Week),function(week){
  y <- dk.16 %>% filter(Week==week,!is.na(Pts),Salary > 0)
  x <- independent_teams(train=y)
  x$year <- 2016
  x
}) %>% bind_rows()

best17 <- lapply(unique(dk.17$Week),function(week){
  y <- dk.17 %>% filter(Week==week,!is.na(Pts),Salary > 0)
  x <- independent_teams(train=y)
  x$year <- 2017
  x
}) %>% bind_rows()

best_all <- rbind(best14,best15,best16,best17)

all_best_ind5 <- function(){
  print("DraftKings Winning Lineup Quick Breakdown")
  print("Using 5 Best Independent Teams Per Week: ")
  position_breakdown(best_all,'2014 to 2017',T)
  position_breakdown(best17,'2017',T)
  position_breakdown(best16,'2016',T)
  position_breakdown(best15,'2015',T)
  position_breakdown(best14,'2014',T)
}
all_best_ind5()












