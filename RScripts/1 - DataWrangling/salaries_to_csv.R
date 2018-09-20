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


#Convert text file 
dk_txt_files <- list.files(path = "./DK-Salaries/",full.names = T)
lapply(dk_txt_files,function(file){
  x <- read.csv(file = file,sep = ";")
  write_csv(x = x,path = file %>% str_replace("DK-Salaries","dk_csv") %>% str_replace(".txt",".csv"))
})


fd_txt_files <- list.files(path = "./Fanduel-Salaries/",full.names = T)
lapply(fd_txt_files,function(file){
  x <- read.csv(file = file,sep = ";")
  write_csv(x = x,path = file %>% str_replace("Fanduel-Salaries","fd_csv") %>% str_replace(".txt",".csv"))
})



fd_csv_files <- list.files('C:/Users/MonticT/Documents/AnalysisProjects/Fantasy/fd_csv/',full.names = T)
fd_full <- lapply(fd_csv_files,function(file){
  read_csv(file,col_types = cols(.default = "c"))
}) %>% bind_rows()

write_csv(x = fd_full,path = 'C:/Users/MonticT/Documents/AnalysisProjects/Fantasy/fd_csv/fd_full.csv')

dk_csv_files <- list.files('C:/Users/MonticT/Documents/AnalysisProjects/Fantasy/dk_csv/ByWeek/',full.names = T)
dk_full <- lapply(dk_csv_files,function(file){
  read_csv(file,col_types = cols(.default = "c"))
}) %>% bind_rows()

write_csv(x = dk_full,path = 'C:/Users/MonticT/Documents/AnalysisProjects/Fantasy/dk_csv/dk_full.csv')


#Break up full file to create by year csv
lapply(unique(dk_full$Year),function(year){
  write_csv(x = dk_full %>% filter(Year==year),path = paste0('C:/Users/MonticT/Documents/AnalysisProjects/Fantasy/dk_csv/ByYear/dk_',year,'.csv'))
})
























