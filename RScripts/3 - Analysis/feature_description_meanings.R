rm(list=ls())
functions <- list()
setwd('C:/Users/MonticT/Documents/AnalysisProjects/Vol_Terms/Yoh_Terms_Only/')

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


features_with_segments <- readr::read_csv(file = "./Models/ModelExplaination/figure_out_features.csv"
                                  ,col_types = cols(.default = "c")
                                  ,progress = T)


for(i in 1:rows){
  print(paste0("Feature Desc: ",features_with_segments$Feature_desc[i]," item set: ", features_with_segments$item_sets[i]))
}
feature_1 <- features_with_segments$Feature[1]
desc_1 <- features_with_segments$Feature_desc[1]
place_one <- stringr::str_locate(string = desc_1,pattern = feature_1)

get_item_set <- function(feature,desc){
  #feature <- features_with_segments$Feature[25]
  #desc <- features_with_segments$Feature_desc[25]
  item_set <- 0
  place <- stringr::str_locate(string = desc,pattern = feature)
  
  #desc_1 emp_age <= 2 Need to get 0,1,2
  if(place[1] == 1){
    pos <- stringr::str_locate(string = desc,pattern = "<=")[2]
    items <- as.numeric(stringr::str_sub(string = desc,start = pos+2,end = nchar(desc)))
    item_set <- seq(0,items)
  }
  
  #desc_2 and desc_3
  if(place[1] > 1){
    greater_than <- stringr::str_locate_all(string = desc,pattern = "<") %>% as.data.frame()
    
    #desc_2 3 < emp_age <= 5 #Need to get items 4,5
    if(nrow(greater_than) > 1){
      pos_one <- stringr::str_locate(string = desc,pattern = "<")[1]
      items_one <- as.numeric(stringr::str_sub(string = desc,start = 1,end = pos_one-2)) + 1
      
      pos_two <- stringr::str_locate(string = desc,pattern = "<=")[1]
      items_two <- as.numeric(stringr::str_sub(string = desc,start = pos_two+2,end = nchar(desc)))
      
      item_set <- seq(items_one,items_two)
    }
    
    #desc_4 5 < emp_age  Need to get 6 and up
    if(nrow(greater_than) < 2){
      pos_three <- stringr::str_locate(string = desc,pattern = "<")[1]
      items_three <- as.numeric(stringr::str_sub(string = desc,start = 1,end = pos_three-2))+1
      
      item_set <- seq(items_three,54)
    }
    
  }
  print(item_set)
  return(item_set %>% as.list())
  
}

rows <- nrow(features_with_segments)

#get_item_set(features_with_segments$Feature[10],features_with_segments$Feature_desc[10])
#features_with_segments$Feature_desc[10]
#2 < emp_annual_salary_percent_increase <= 4
#3,4
y <- vector("list",rows)
for(i in 1:rows){
  print(i)
  y[[i]] <- get_item_set(features_with_segments$Feature[i],features_with_segments$Feature_desc[i])
  print(y[[i]])
}
features_with_segments$item_sets <- y



features_with_segments$group_range <- NULL
for(i in 1:rows){
  item_set = features_with_segments$item_sets[i] %>% unlist()
  xz <- colnames(features_with_segments) %in% item_set
  fs <- features_with_segments[,xz]
  range <- paste(fs[i,],collapse = ",")
  features_with_segments$group_range[i] <- range
}


features_with_segments <- 
  features_with_segments %>%
  mutate(group_range2 = str_replace_all(string = group_range ,pattern = ",NA",replacement = ""))


final_range_numeric <- function(group_range){
  #group_range <- features_with_segments$group_range2[1]
  pattern <- group_range %>% str_locate(pattern = "\\\\,")
  final_range <- group_range
  
  first_group <- str_sub(string = group_range,start = 1,end=pattern[1]-1)
  
  if(suppressWarnings(all(!is.na(as.numeric(as.character(first_group)))))){
    first_num <- str_sub(string = group_range,start = 1,end=pattern[1]-1)
    
    patterns <- group_range %>% str_locate_all(pattern = "\\\\,") %>% as.data.frame()
    end_num <- patterns[nrow(patterns),'end']
    if(suppressWarnings(all(!is.na(as.numeric(as.character(end_num)))))){
      second_num <- str_sub(string = group_range,start = end_num+1,end = nchar(group_range))
      final_range <- paste0(first_num," - ",second_num)
    }
  }
  return(final_range)
}

#x <- final_range_numeric(features_with_segments$group_range2[1])


for(i in 1:rows){
  features_with_segments$final_range[i] <- final_range_numeric(features_with_segments$group_range2[i])
}

write_csv(features_with_segments %>% select(-item_sets),path = "./Models/ModelExplaination/figure_out_features_grouped_better.csv")





















