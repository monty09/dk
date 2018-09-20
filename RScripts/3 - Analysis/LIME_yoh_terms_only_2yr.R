#LIME EXPLAINATION OF NEURAL NETWORK MODEL
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

yoh_terms_21 <- read_csv(file = "./Data/Yoh_Terms_Only_Two_Year.csv",col_types = cols(.default = "c"))
#Create training and test dataframes
set.seed(123)
data_split2 <- resample_partition(yoh_terms_21, c(test = 0.3, train = 0.7))

colnames(yoh_terms_21)
columns_to_remove <- c('emp_how_long_since_first_hired','emp_how_long_since_last_hired','max_record','term','emp_months_till_term','emp_action_type_massn','emp_action_type_massg','termyes','term_under_12'
                       ,'row_id','emp_id','date_of_record','emp_quit_date','emp_voluntary_term_next_6_months','emp_date_first_hired','emp_org1','emp_how_long_in_same_position'
                       ,'emp_annual_salary_percent_increase','emp_review_rating','emp_difference_between_most_recent_and_prior_review_rating','emp_sick_time_used_past_year'
                       ,'emp_has_invol_term_in_past','emp_prior_review_rating','emp_vac_time_used_past_year','emp_floting_holiday_used_in_last_year')


target <- 'term_under_12'
trend_cols <- colnames(yoh_terms_21)[str_detect(pattern = "trend",string = colnames(yoh_terms_21))]
delta_cols <- colnames(yoh_terms_21)[str_detect(pattern = "delta",string = colnames(yoh_terms_21))]
all_vars <- colnames(yoh_terms_21)[!(colnames(yoh_terms_21) %in% c(trend_cols,delta_cols,columns_to_remove))]
formula_all <- as.formula(paste(target,"~",paste(all_vars,collapse="+"),sep = ""))

#Generate Neural Networks with threshold to beat non information rate
source('C:/Users/MonticT/Documents/AnalysisProjects/Utils/utils_classification_models.R')

df_split <- classify$get_test_train_df(data_split2)
testdf <- df_split$testdf %>% select(all_vars,target) %>% mutate_at(.funs = funs(as.integer),.vars = vars(everything())) %>% as.data.frame()
traindf <- df_split$traindf %>% select(all_vars,target) %>% mutate_at(.funs = funs(as.integer),.vars = vars(everything())) %>% as.data.frame()
totaldf <- rbind(traindf,testdf)

load("C:/Users/MonticT/Documents/AnalysisProjects/Vol_Terms/Yoh_Terms_Only/Models/SavedRF/rf_quick_208.Rdata")
randomForest::varImpPlot(rf.Model)
varImp(rf.Model)

pred <-predict(rf.Model, testdf)
cm <-caret::confusionMatrix(pred, testdf %>% pull(target) %>% as.factor())
print(cm)


#####################################################
############## RandomForest::RandomForest ###########
library(doParallel)
library(parallel)
library(multidplyr)

model_type.randomForest <- function(x, ...){"classification"}
predict_model.randomForest <- function (x, newdata, type, ...) {
  #res <- predict(object = rf.Model,testdf,type = 'prob') %>% as.data.frame()

  res_raw <- predict(object = x,newdata = newdata,type = 'response',...) %>% as.data.frame()
  res_prob <- predict(object = x,newdata = newdata,type = 'prob',...) %>% as.data.frame()
  
  switch(
    type,
    raw = data.frame(Response = res_raw$., stringsAsFactors = FALSE),
    prob = res_prob
  )
}

model_type.randomForest.formula <- function(x, ...){"classification"}
predict_model.randomForest.formula <- function (x, newdata, type, ...) {
  #res <- predict(object = rf.Model,testdf,type = 'prob') %>% as.data.frame()
  
  res_raw <- predict(object = x,newdata = newdata,type = 'response',...) %>% as.data.frame()
  res_prob <- predict(object = x,newdata = newdata,type = 'prob',...) %>% as.data.frame()
  
  switch(
    type,
    raw = data.frame(Response = res_raw$., stringsAsFactors = FALSE),
    prob = res_prob
  )
}


explainer <- lime::lime(model = rf.Model,x = traindf)
totaldf <- rbind(traindf,testdf)

#Get explanation for each person in totaldf in parallel
cl <- makePSOCKcluster(parallel::detectCores()-1)
cluster_copy(cluster = cl, totaldf)
cluster_copy(cluster=cl,explainer)
cluster_copy(cluster=cl,model_type.randomForest)
cluster_copy(cluster=cl,predict_model.randomForest)
cluster_copy(cluster=cl,model_type.randomForest.formula)
cluster_copy(cluster=cl,predict_model.randomForest.formula)
registerDoParallel(cl)


exps <- foreach::foreach(i=1:nrow(totaldf),.combine=rbind,.packages = c("lime","dplyr",'randomForest')) %dopar%{
  df <- as.data.frame(totaldf %>% select(-term_under_12)%>%slice(i))
  explanation <- NULL
  explanation <- lime::explain(x = df,explainer = explainer,n_labels = 2,n_features = 38)
  explanation <- explanation %>% select(-data)
  explanation$employee <- i
  explanation
}

stopCluster(cl)


save(exps,file="./Models/ModelExplaination/Yoh_Terms_Only_RF_results.Rdata")
load(file = "./Models/ModelExplaination/NeuralNetworkModel1_results.Rdata")

library(data.table)
prediction_probabilities <- rbindlist(lapply(exps$prediction, as.data.frame))

e2 <- cbind(exps,prediction_probabilities)
e2 <-
  e2 %>%
  select(-prediction,-model_type) %>%
  rename("stay_prob"=X0,"leave_prob"=X1)


#colnames(e2)
#table(e2$feature)
#unique(e2$feature_desc)
var_imp <- varImp(rf.Model)
var_imp
write_csv(var_imp %>% tibble::rownames_to_column(),'./Models/ModelExplaination/variable_importance.csv')
write_csv(totaldf,'./Models/ModelExplaination/yoh_terms_twoyr_totaldf.csv')
write_csv(e2,"./Models/ModelExplaination/yoh_terms_only_results.csv")

feature <-
  e2 %>%
  group_by(label,feature_desc) %>%
  mutate(avg_weight = base::mean(as.numeric(feature_weight))) %>%
  mutate(max_weight = max(as.numeric(feature_weight))) %>%
  mutate(min_weight = min(as.numeric(feature_weight))) %>%
  mutate(avg_stay_prob = base::mean(as.numeric(stay_prob))) %>%
  mutate(avg_leave_prob = base::mean(as.numeric(leave_prob))) %>%
  ungroup() %>%
  select(label,feature,feature_desc,avg_stay_prob,avg_leave_prob,avg_weight,max_weight,min_weight) %>%
  distinct()

write_csv(testdf,'C:/Users/MonticT/Documents/AnalysisProjects/Vol_Terms/Models/ModelExplanation/testdf.csv')
write_csv(feature,path = 'C:/Users/MonticT/Documents/AnalysisProjects/Vol_Terms/Models/ModelExplanation/feature_weights.csv')

































############################################# 
############# CARET RF MODEL ################


load("C:/Users/MonticT/Documents/AnalysisProjects/Vol_Terms/Yoh_Terms_Only/Models/SavedRF/rf_tuned_1.Rdata")
varImp(rf_random)

res <- caret::predict.train(rf_random,testdf)


explainer <- lime::lime(model = rf_random,x = traindf)
#Get explanation for each person in testdf in parallel
library(doParallel)
library(parallel)
library(multidplyr)
cl <- makePSOCKcluster(parallel::detectCores()-1)
cluster_copy(cluster = cl, totaldf)
cluster_copy(cluster=cl,explainer)
registerDoParallel(cl)


exps <- foreach::foreach(i=1:nrow(totaldf),.combine=rbind,.packages = c("lime","dplyr")) %dopar%{
  df <- as.data.frame(totaldf %>% select(-term_under_12)%>%slice(i))
  explanation <- NULL
  explanation <- lime::explain(x = df,explainer = explainer,n_labels = 2,n_features = 66)
  explanation <- explanation %>% select(-data)
  explanation$employee <- i
  explanation
}

stopCluster(cl)

save(exps,file="./Models/ModelExplaination/TermsOnly_RF_results.Rdata")
load(file = "./Models/ModelExplaination/NeuralNetworkModel1_results.Rdata")













predict_model.nn <- function (x, newdata, type, ...) {
  res <- neuralnet::compute(x,newdata)
  r2 <- res$net.result %>% as.data.frame()
  r2$pred <- round(res$net.result[,1],0)
  r3 <-
    r2 %>%
    mutate(stay= ifelse(pred == 0,1-V1,1-V1)) %>%
    mutate(leave= ifelse(pred == 0,V1,V1)) %>%
    select(stay,leave)
  
  switch(
    type,
    raw = data.frame(Response = round(res$net.result,0), stringsAsFactors = FALSE),
    prob = r3
  )
}

