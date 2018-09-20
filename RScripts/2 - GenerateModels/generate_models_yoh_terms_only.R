rm(list=ls())
Sys.setenv(TZ = "America/New_York")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_181')
setwd('C:/Users/MonticT/Documents/AnalysisProjects/Vol_Terms/Yoh_Terms_Only/')

packages <- function(){
  package_names <- c('caret','mlbench','glmnet','h2o','modelr','tidyverse','rJava','xlsx',"readxl","devtools", "futile.logger", "DBI", "RMySQL", "uuid", "readr", "caret", "gbm", "rpart", "randomForest", "stringdist", "tm", "curl", "stringr", "jsonlite", "purrr", "dplyr", "tidyr", "fastmatch")
  ok = sapply(package_names,
              FUN = function (package_name) {
                suppressMessages(require(package_name, character.only=TRUE, quietly = T, warn.conflicts = F))
              })
  
}
packages()

yoh_terms_21 <- read_csv(file = "./Data/Yoh_Terms_Only_Two_Year.csv",col_types = cols(.default = "c"))
yoh_terms_31 <- read_csv(file = "./Data/Yoh_Terms_Only_Three_Year.csv",col_types = cols(.default = "c"))
yoh_terms_41 <- read_csv(file = "./Data/Yoh_Terms_Only_Four_Year.csv",col_types = cols(.default = "c"))


#Create training and test dataframes
set.seed(123)
data_split2 <- resample_partition(yoh_terms_21, c(test = 0.3, train = 0.7))
data_split3 <- resample_partition(yoh_terms_31, c(test = 0.3, train = 0.7))
data_split4 <- resample_partition(yoh_terms_41, c(test = 0.3, train = 0.7))


#Get Complete Variable List
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


colnames(traindf)

classify$run_neural_net( formula = formula_all, num_models_to_create = 10
                        ,traindf = traindf, testdf = testdf
                        ,formula_target = target, rand_layers = F
                        ,hidden_layer1_range = list(5,14), percent_higher_than_no_information_rate = .1
                        ,num_reps = 20,num_hiddenlayers = 1
                        ,directory_to_save_model = 'C:/Users/MonticT/Documents/AnalysisProjects/Vol_Terms/Yoh_Terms_Only/Models/SavedNN/')





classify$run_random_forest_quick(testdf = testdf,traindf = traindf,variables_to_keep = all_vars
                                 ,target_variable = target,percent_higher_than_no_information_rate = .29
                                 ,num_models_to_create = 500, formula = formula_all
                                 ,directory_to_save_model = 'C:/Users/MonticT/Documents/AnalysisProjects/Vol_Terms/Yoh_Terms_Only/Models/SavedRF/')




start <- Sys.time()

classify$run_random_forest_tuned(testdf = testdf,traindf = traindf,variables_to_keep = all_vars
                                 ,target_variable = target,percent_higher_than_no_information_rate = .23
                                 ,directory_to_save_model = 'C:/Users/MonticT/Documents/AnalysisProjects/Vol_Terms/Yoh_Terms_Only/Models/SavedRF/'
                                 ,num_models_to_create = 1,grid_search = T
                                 ,tuneLength = 10,tunegrid = expand.grid(.mtry=c(1:15))
                                 ,num_repeats = 5,num_folds = 5,formula = formula_all)

end <- Sys.time()

print(end- start)

#Time difference of 2.274818083 mins


classify$run_NaiveBayes_quick(testdf = testdf,traindf = traindf,formula = formula_all
                              ,variables_to_keep = all_vars,target_variable = target
                              ,percent_higher_than_no_information_rate = .05
                              ,directory_to_save_model = 'C:/Users/MonticT/Documents/AnalysisProjects/Vol_Terms/Yoh_Terms_Only/Models/SavedNB/'
                              ,num_models_to_create = 1)



classify$run_multi_layer_perceptron(testdf = testdf,traindf = traindf
                                    ,variables_to_keep = all_vars,target_variable = target
                                    ,percent_higher_than_no_information_rate = .05 
                                    ,num_units_hidden_layer = list(50,55), maximum_iterations = c(100,5000)
                                    ,directory_to_save_model = 'C:/Users/MonticT/Documents/AnalysisProjects/Vol_Terms/Yoh_Terms_Only/Models/SavedMLP/'
                                    ,num_models_to_create = 10)



classify$run_svm_quick(testdf = testdf,traindf = traindf,formula = formula_all
                       ,variables_to_keep = all_vars,target_variable = target
                       ,percent_higher_than_no_information_rate = .05
                       ,directory_to_save_model = 'C:/Users/MonticT/Documents/AnalysisProjects/Vol_Terms/Yoh_Terms_Only/Models/SavedSVM/'
                       ,num_models_to_create = 1)





classify$run_svm_tuned(testdf = testdf, traindf = traindf
                      ,variables_to_keep = all_vars, target_variable = target
                      ,percent_higher_than_no_information_rate = 0, formula = formula_all
                      ,tuneLength = 10, maxit = 1000,tuneGrid = expand.grid(sigma = c(.01,.05,.1), C = c(0.75, 0.9, 1, 1.1, 1.25))
                      ,control_fold_repeats = 5,control_number_folds = 10,num_models_to_create = 1
                      ,directory_to_save_model = 'C:/Users/MonticT/Documents/AnalysisProjects/Vol_Terms/Yoh_Terms_Only/Models/SavedSVM/')













