rm(list=ls())
functions <- list()
setwd('C:/Users/MonticT/Documents/AnalysisProjects/Fantasy/')

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

all_files <- list.files(path = 'C:/Users/MonticT/Documents/AnalysisProjects/Fantasy/dk_csv/ByWeek/',full.names = T)

full <- lapply(all_files,function(file){
  read_csv(file = file,col_types = cols(.default = "c"))
}) %>% bind_rows()


dk.17 <- 
  full %>% 
  filter(Year==2017) %>% 
  mutate_at(.funs = funs(as.numeric),.vars = vars(Week,Year,DK.points,DK.salary)) %>%
  filter(DK.points != 0) %>%
  arrange(Week)


#Lets Get Team Analysis For The Year
#opponent Breakdown by Position
#Avg Position Against
dk.17 <- as.data.frame(dk.17)



opp_position_sum <-
  dk.17 %>%
  group_by(Week,Oppt,Pos) %>%
  summarise(opp_sum_pts = sum(DK.points)) %>%
  ungroup() 

opp_position_avg_year <-
  dk.17 %>%
  left_join(opp_position_sum,by =c("Week"="Week","Oppt"="Oppt","Pos"="Pos")) %>%
  select(Week,Oppt,Pos,DK.points,opp_sum_pts) %>%
  add_count(Week,Oppt,Pos)  %>%
  distinct(Oppt,Pos,opp_sum_pts,n) %>%
  mutate(n = ifelse(Pos=="QB",1,n)) %>%
  group_by(Oppt,Pos) %>%
  mutate(avg_oppt_pos_pt_17 = sum(opp_sum_pts)/sum(n)) %>%
  distinct(Oppt,Pos,avg_oppt_pos_pt_17)


opp_position_avg_year %>% filter(Pos=="QB",Oppt=="nor") %>% summarise(x = mean(opp_sum_pts))
  summarise(avg_oppt_pos_pt_17 = mean(opp_sum_pts)/n)
  group_by(Oppt,Pos) %>%
  mutate(opp_avg_points_year = mean(opp_sum_pts)/n) %>%
  filter(Oppt=="nwe",Pos=="QB")


dk.17.sum <-
  dk.17 %>% 
  dplyr::left_join(opp_position_sum,by =c("Week"="Week","Oppt"="Oppt","Pos"="Pos")) %>%
  dplyr::left_join(opp_position_avg_year,by = c("Oppt"="Oppt","Pos"="Pos"))




opp_position_avg <-
  opp_position_sum %>%
  group_by(Oppt,Pos) %>%
  summarise(avg_pts_against_17 = mean(sum_pts)
            ,max_pts_against_17 = max(sum_pts)
            ,min_pts_against_17 = min(sum_pts)) %>%
  ungroup() %>%
  
  summarise(max_pts_against_17 = max(sum_pts)) %>%
  summarise(min_pts_against_17 = min(sum_pts)) %>%
  ungroup()
  
  


unique(dk.17$Oppt)


unique(dk.17$Week)




