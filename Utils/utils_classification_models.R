Sys.setenv(TZ = "America/New_York")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_181')

packages <- function(){
  package_names <- c('parallel','doParallel','nnet','neuralnet','e1071','caret','tidyverse', "readr", "caret", "gbm"
                     , "rpart", "randomForest", "stringdist", "tm", "stringr",  "purrr", "dplyr", "tidyr", "fastmatch"
                     ,"RSNNS")
  ok = sapply(package_names,
              FUN = function (package_name) {
                suppressMessages(require(package_name, character.only=TRUE, quietly = T, warn.conflicts = F))
              })
  
}
packages()



classify <- list()

classify$run_neural_net <- function(formula,num_models_to_create = 1000, traindf,testdf,formula_target
                                    ,num_hiddenlayers = 2,hidden_layer1_range = list(10,30), rand_layers = F
                                    ,hidden_layer2_range = list(5,15),hidden_layer3_range = list(1,10)
                                    ,percent_higher_than_no_information_rate = .05, num_reps = 10
                                    ,directory_to_save_model,algorithms = c('rprop+','rprop-','sag','slr')){
  
  #setwd(directory_to_save_model)
  testdf  <- testdf   %>% dplyr::mutate_at(.funs = funs(as.integer),.vars = vars(everything())) %>% as.data.frame()
  traindf <- traindf  %>% dplyr::mutate_at(.funs = funs(as.integer),.vars = vars(everything())) %>% as.data.frame()
  
  for(i in 1:num_models_to_create){
    
    if(num_hiddenlayers > 3){
      print('3 hidden layers max, making the switch to 3 now')
      num_hiddenlayers = 3 }
    
    
    layer1 <- sample(hidden_layer1_range[[1]]:hidden_layer1_range[[2]],1)
    layer2 <- sample(hidden_layer2_range[[1]]:hidden_layer2_range[[2]],1)
    layer3 <- sample(hidden_layer3_range[[1]]:hidden_layer3_range[[2]],1)
    layers <- c(layer1,layer2,layer3)
    hidden_layers <- layers[1:num_hiddenlayers]
    
    if(rand_layers){
      hidden_layers <- layers[1:sample(1:length(layers),1)]
    }
    
    algorithms <- c('rprop+','rprop-','sag','slr')
    algo_num <- sample(1:length(algorithms),1)
    algorithm <- algorithms[algo_num]
    #print(paste0(directory_to_save_model,"nn_",paste(hidden_layers,collapse = "_")," algo ",algorithm,".Rdata"))
    
    run <- function(hidden_layers,formula,traindf,testdf,formula_target,num_reps = 10){
      #formula_target = 'term'
      model <- neuralnet::neuralnet(formula = formula,data = traindf, threshold = .01, algorithm = algorithm
                                    ,hidden = hidden_layers,rep = num_reps,err.fct = "ce"
                                    ,act.fct = "logistic",linear.output = F,lifesign = "minimal")
      
      pred <- compute(model, testdf %>% select(-formula_target))
      NNPredictions <- round(pred$net.result,0) %>% as.factor()
      cmNN <- caret::confusionMatrix(NNPredictions, testdf %>% pull(formula_target) %>% as.factor())
  
      accuracy <- cmNN$overall %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Accuracy") %>% pull(.)
      no_information_rate <- cmNN$byClass %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Prevalence") %>% pull(.)
      print(paste0("The accuracy of the model is: ",round(accuracy,2)," the no information rate is: "
                   ,round(no_information_rate,2)," better than by: ",round(accuracy - no_information_rate,2)))
      
      if(accuracy > no_information_rate + percent_higher_than_no_information_rate){
        save(model,file = paste0(directory_to_save_model,"nn_",paste(hidden_layers,collapse = "_"),"algo ",algorithm,".Rdata"))
      }
    }
    tryCatch(run(hidden_layers,formula,traindf,testdf,formula_target,num_reps),error=function(err){print(err)})
  }
}



classify$pretty_stats <- function(testdf,predictions){
  
  testdf$pred <- predictions
  actual_stay <- nrow(testdf %>% filter(term==0))
  actual_leave <- nrow(testdf %>% filter(term==1))
  
  total_test_rows <- nrow(testdf)
  correctly_predicted <- nrow(testdf %>% filter(term==0,pred==0)) + nrow(testdf %>% filter(term==1,pred==1))
  correctly_predicted_percent <- round(correctly_predicted / nrow(testdf)*100,1)
  
  actual_leave_predicted_leave = nrow(testdf %>% filter(pred==1,term==1))
  actual_leave_predicted_stay =  nrow(testdf %>% filter(pred==0,term==1))
  actual_stay_predicted_stay =  nrow(testdf %>% filter(pred==0,term==0))
  actual_stay_predicted_leave =  nrow(testdf %>% filter(pred==1,term==0))
  
  stay_correct_percent <- round(actual_stay_predicted_stay / nrow(testdf %>% filter(term==0)) *100,1)  
  leave_correct_percent <- round(actual_leave_predicted_leave / nrow(testdf %>% filter(term==1)) *100,1) 
  incorrect_stay_percent <- round(actual_leave_predicted_stay / nrow(testdf %>% filter(term==1))*100,1)        
  incorrect_leave_percent <- round(actual_stay_predicted_leave / nrow(testdf %>% filter(term==0))*100,1)        
  
  print(paste0("Predicted ",nrow(testdf %>% filter(pred==0))," to stay. ",nrow(testdf %>% filter(term==0))," stayed.  Correctly predicted: ",stay_correct_percent,"%"))
  print(paste0("Predicted ",nrow(testdf %>% filter(pred==1))," to leave. ",nrow(testdf %>% filter(term==1))," left.  Correctly predicted: ",leave_correct_percent,"%"))
  
  #print(paste0("Variables used were: ",colnames(testdf %>% select(-term))))
  return_list <- list(actual_stay = actual_stay
                      ,actual_leave = actual_leave
                      ,actual_leave_predicted_leave = actual_leave_predicted_leave
                      ,actual_leave_predicted_stay = actual_leave_predicted_stay
                      ,actual_stay_predicted_stay = actual_stay_predicted_stay
                      ,actual_stay_predicted_leave = actual_stay_predicted_leave
                      ,total_test_rows = total_test_rows
                      ,correctly_predicted = correctly_predicted
                      ,correctly_predicted_percent = correctly_predicted_percent 
                      ,stay_correct_percent = stay_correct_percent
                      ,leave_correct_percent = leave_correct_percent
                      ,incorrect_stay_percent = incorrect_stay_percent
                      ,incorrect_leave_percent = incorrect_leave_percent)
  return(return_list)
  
}





classify$get_test_train_df <- function(data_split){
  traindf <-  data_split$train$data[data_split$train$idx,]
  testdf  <-  data_split$test$data[data_split$test$idx,]
  return(list(traindf=traindf,testdf=testdf))
}






classify$run_random_forest_quick <- function(testdf,traindf,variables_to_keep,target_variable
                                       ,percent_higher_than_no_information_rate = .05
                                       ,directory_to_save_model,num_models_to_create = 1000){
  
  #setwd(directory_to_save_model)
  testdf  <- testdf  %>% dplyr::select(variables_to_keep,target_variable) %>% mutate_at(.funs = funs(as.numeric),.vars = vars(-target_variable)) %>% mutate_at(.funs = funs(as.factor),.vars = vars(target_variable)) %>% as.data.frame()
  traindf <- traindf %>% dplyr::select(variables_to_keep,target_variable) %>% mutate_at(.funs = funs(as.numeric),.vars = vars(-target_variable)) %>% mutate_at(.funs = funs(as.factor),.vars = vars(target_variable)) %>% as.data.frame()
  traindf_with_variables <- traindf %>% dplyr::select(variables_to_keep)
  
  for(i in 1:num_models_to_create){
    rf.Model <- randomForest::randomForest(formula,data = traindf)
    #rf.Model <- randomForest::tuneRF(x = traindf %>% select(-target_variable),y = traindf %>% pull(target_variable))
    
    #plot(rf.Model)
    pred <-predict(rf.Model, testdf)
    cm <-caret::confusionMatrix(pred, testdf %>% pull(target_variable))
    accuracy <- cm$overall %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Accuracy") %>% pull(.)
    no_information_rate <- cm$byClass %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Prevalence") %>% pull(.)
    print(paste0("The accuracy of the model is: ",round(accuracy,2)," the no information rate is: "
                 ,round(no_information_rate,2)," better than by: ",round(accuracy - no_information_rate,2)))
    
    if(accuracy > no_information_rate + percent_higher_than_no_information_rate){
      save(rf.Model,file = paste0(directory_to_save_model,"rf_quick_",i,".Rdata"))
    }
  }

}





classify$run_random_forest_tuned <- function(testdf,traindf,variables_to_keep,target_variable
                                             ,percent_higher_than_no_information_rate = .05
                                             ,directory_to_save_model,num_models_to_create = 1000
                                             ,grid_search = F,tuneLength = 10,tunegrid = expand.grid(.mtry=c(1:15))
                                             ,num_folds = 5, num_repeats = 5){
  
  #setwd(directory_to_save_model)
  testdf  <- testdf  %>% dplyr::select(variables_to_keep,target_variable) %>% mutate_at(.funs = funs(as.numeric),.vars = vars(-target_variable)) %>% mutate_at(.funs = funs(as.factor),.vars = vars(target_variable)) %>% as.data.frame()
  traindf <- traindf %>% dplyr::select(variables_to_keep,target_variable) %>% mutate_at(.funs = funs(as.numeric),.vars = vars(-target_variable)) %>% mutate_at(.funs = funs(as.factor),.vars = vars(target_variable)) %>% as.data.frame()
  traindf_with_variables <- traindf %>% dplyr::select(variables_to_keep)
  
  for(i in 1:num_models_to_create){
    
    metric <- "Accuracy"
    
    if(!grid_search){
      library(doParallel)
      cl <- makePSOCKcluster(parallel::detectCores()-1)
      registerDoParallel(cl)
      control <- trainControl(method="repeatedcv",number = num_folds,repeats = num_repeats, search="random",allowParallel = T)
      rf_random <- train(formula, data=traindf, method="rf", metric=metric,tuneLength = tuneLength, trControl=control)
      stopCluster(cl)
    }
    
    if(grid_search){
      library(doParallel)
      cl <- makePSOCKcluster(parallel::detectCores()-1)
      registerDoParallel(cl)
      control <- trainControl(method="repeatedcv",number = num_folds,repeats = num_repeats, search="grid",allowParallel = T)
      rf_random <- train(formula, data=traindf, method="rf", metric=metric, tuneLength = tuneLength, trControl=control,tuneGrid = tunegrid)
      stopCluster(cl)    
    }
    
    
    pred <-predict(rf_random, testdf)
    cm <-caret::confusionMatrix(pred, testdf$term)
    accuracy <- cm$overall %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Accuracy") %>% pull(.)
    no_information_rate <- cm$byClass %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Prevalence") %>% pull(.)
    print(paste0("The accuracy of the model is: ",round(accuracy,2)," the no information rate is: "
                 ,round(no_information_rate,2)," better than by: ",round(accuracy - no_information_rate,2)))
    
    if(accuracy > no_information_rate + percent_higher_than_no_information_rate){
      save(rf.Model,file = paste0(directory_to_save_model,"rf_tuned_",i,".Rdata"))
    }
  }
  
  
  
}




classify$run_NaiveBayes_quick <- function(testdf,traindf,formula,variables_to_keep,target_variable
                                          ,percent_higher_than_no_information_rate = .05
                                          ,directory_to_save_model,num_models_to_create = 1000){
  
  #setwd(directory_to_save_model)
  testdf  <- testdf  %>% dplyr::select(variables_to_keep,target_variable) %>% mutate_at(.funs = funs(as.numeric),.vars = vars(-target_variable)) %>% mutate_at(.funs = funs(as.factor),.vars = vars(target_variable)) %>% as.data.frame()
  traindf <- traindf %>% dplyr::select(variables_to_keep,target_variable) %>% mutate_at(.funs = funs(as.numeric),.vars = vars(-target_variable)) %>% mutate_at(.funs = funs(as.factor),.vars = vars(target_variable)) %>% as.data.frame()
  
  for(i in 1:num_models_to_create){
    library(e1071)
    Naive_Bayes_Model= e1071::naiveBayes(formula, data=traindf)
    pred <-predict(Naive_Bayes_Model, testdf)
    
    cm <-confusionMatrix(pred, testdf %>% pull(target_variable))
    accuracy <- cm$overall %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Accuracy") %>% pull(.)
    no_information_rate <- cm$byClass %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Prevalence") %>% pull(.)
    print(paste0("The accuracy of the model is: ",round(accuracy,2)," the no information rate is: "
                 ,round(no_information_rate,2)," better than by: ",round(accuracy - no_information_rate,2)))
    
    if(accuracy > no_information_rate + percent_higher_than_no_information_rate){
      save(rf.Model,file = paste0(directory_to_save_model,"naivebayes_quick_",i,".Rdata"))
    }
  }
}





classify$run_NaiveBayes_tuned <- function(testdf,traindf,formula,variables_to_keep,target_variable
                                          ,percent_higher_than_no_information_rate = .05
                                          ,directory_to_save_model,num_models_to_create = 1000){
  
  df2 <- get_test_train_df(data_split2)
  variables_to_keep <- all_vars
  
  testdf <- df2$testdf %>% mutate(term = replace(term, term == 0, 'stay')) %>% mutate(term = replace(term, term == 1, 'leave'))
  traindf <- df2$traindf %>% mutate(term = replace(term, term == 0, 'stay')) %>% mutate(term = replace(term, term == 1, 'leave'))
  
  testdf  <- testdf  %>% dplyr::select(variables_to_keep,target_variable) %>% mutate_at(.funs = funs(as.numeric),.vars = vars(-target_variable)) %>% mutate_at(.funs = funs(as.factor),.vars = vars(target_variable)) %>% as.data.frame()
  traindf <- traindf %>% dplyr::select(variables_to_keep,target_variable) %>% mutate_at(.funs = funs(as.numeric),.vars = vars(-target_variable)) %>% mutate_at(.funs = funs(as.factor),.vars = vars(target_variable)) %>% as.data.frame()
  traindf_with_variables <- traindf %>% dplyr::select(variables_to_keep)
  
  unique(testdf$term)

  
  for(i in 1:num_models_to_create){
    
    library(doParallel)
    library(parallel)
    cl <- makePSOCKcluster(parallel::detectCores()-1)
    registerDoParallel(cl)
    
    train_control <- trainControl(method = "boot", number = 10000,allowParallel = T)
    search_grid <- expand.grid(usekernel = c(TRUE, FALSE),fL = 0:5,adjust = seq(0, 5, by = 1))
    
    nb.Model <- train(traindf_with_variables, traindf %>% pull(target_variable)
                      ,method = "nb",trControl = train_control
                      ,tuneGrid = search_grid, preProc = c("BoxCox", "center", "scale", "pca")
    )
    stopCluster(cl)
    
    plot(nb.Model)
    pred <-predict(nb.Model, testdf)
    

    cm <-confusionMatrix(pred, testdf %>% pull(target_variable))
    accuracy <- cm$overall %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Accuracy") %>% pull(.)
    no_information_rate <- cm$byClass %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Prevalence") %>% pull(.)
    print(paste0("The accuracy of the model is: ",round(accuracy,2)," the no information rate is: "
                 ,round(no_information_rate,2)," better than by: ",round(accuracy - no_information_rate,2)))
    
    if(accuracy > no_information_rate + percent_higher_than_no_information_rate){
      save(rf.Model,file = paste0(directory_to_save_model,"naivebayes_tuned_",i,".Rdata"))
    }
  }
}





classify$run_multi_layer_perceptron <- function(testdf,traindf,variables_to_keep,target_variable
                                          ,percent_higher_than_no_information_rate = .05 
                                          ,num_units_hidden_layer = list(1,60), maximum_iterations = c(100,5000)
                                          ,directory_to_save_model,num_models_to_create = 1000){
  

  testdf  <- testdf  %>% dplyr::select(variables_to_keep,target_variable) %>% mutate_at(.funs = funs(as.numeric),.vars = vars(-target_variable)) %>% mutate_at(.funs = funs(as.factor),.vars = vars(target_variable)) %>% as.data.frame()
  traindf <- traindf %>% dplyr::select(variables_to_keep,target_variable) %>% mutate_at(.funs = funs(as.numeric),.vars = vars(-target_variable)) %>% mutate_at(.funs = funs(as.factor),.vars = vars(target_variable)) %>% as.data.frame()
  traindf_with_variables <- traindf %>% dplyr::select(variables_to_keep)
  testdf_with_variables  <- testdf  %>% dplyr::select(variables_to_keep)
  
  for(i in 1:num_models_to_create){
    
    iterations <- sample(maximum_iterations[[1]]:maximum_iterations[[2]],1)
    hidden_units <- sample(num_units_hidden_layer[[1]]:num_units_hidden_layer[[2]],1)

    
    mlp.model <- RSNNS::mlp(x = traindf_with_variables,y = traindf %>% pull(target_variable)
                            ,maxit = iterations,size = hidden_units,learnFuncParams = c(0,1)
                            ,initFunc = "Randomize_Weights",learnFunc = "Rprop",hiddenActFunc = "Act_Logistic"
                            ,shufflePatterns = F, outputActFunc ="Act_Logistic")
    
    pred <-predict(mlp.model, testdf_with_variables)
    z <- testdf %>% pull(target_variable) %>% as.factor()
    zy <- round(pred,0) %>% as.factor()
    cm <-confusionMatrix(zy,z)
    
    accuracy <- cm$overall %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Accuracy") %>% pull(.)
    no_information_rate <- cm$byClass %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Prevalence") %>% pull(.)
    print(paste0("The accuracy of the model is: ",round(accuracy,2)," the no information rate is: "
                 ,round(no_information_rate,2)," better than by: ",round(accuracy - no_information_rate,2)))
   
    if(accuracy > no_information_rate + percent_higher_than_no_information_rate){
      save(rf.Model,file = paste0(directory_to_save_model,"mlp_",i,".Rdata"))
    }
  }
}






classify$run_svm_quick <- function(testdf,traindf,variables_to_keep,target_variable,formula
                                   ,percent_higher_than_no_information_rate = 0 
                                   ,directory_to_save_model,num_models_to_create = 1){
  
  testdf  <- testdf  %>% dplyr::select(variables_to_keep,target_variable) %>% mutate_at(.funs = funs(as.numeric),.vars = vars(-target_variable)) %>% mutate_at(.funs = funs(as.factor),.vars = vars(target_variable)) %>% as.data.frame()
  traindf <- traindf %>% dplyr::select(variables_to_keep,target_variable) %>% mutate_at(.funs = funs(as.numeric),.vars = vars(-target_variable)) %>% mutate_at(.funs = funs(as.factor),.vars = vars(target_variable)) %>% as.data.frame()
  
  for(i in 1:num_models_to_create){
    
    svm.model <- e1071::svm(formula,data=traindf,type="C-classification")
    
    pred <-predict(svm.model, testdf)
    cm <-confusionMatrix(pred,testdf %>% pull(target_variable))
    
    accuracy <- cm$overall %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Accuracy") %>% pull(.)
    no_information_rate <- cm$byClass %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Prevalence") %>% pull(.)
    print(paste0("The accuracy of the model is: ",round(accuracy,2)," the no information rate is: "
                 ,round(no_information_rate,2)," better than by: ",round(accuracy - no_information_rate,2)))
    
    if(accuracy > no_information_rate + percent_higher_than_no_information_rate){
      save(rf.Model,file = paste0(directory_to_save_model,"svm_quick_",i,".Rdata"))
    }
  }
}






classify$run_svm_tuned <- function(testdf,traindf,variables_to_keep,target_variable,formula
                                   ,percent_higher_than_no_information_rate = .05
                                   ,tuneLength = 10, maxit = 1000,tuneGrid = expand.grid(sigma = c(0.0577), C = c(2.21049))
                                   ,control_fold_repeats = 5,control_number_folds = 10
                                   ,directory_to_save_model,num_models_to_create = 1){
  
  testdf  <- testdf  %>% dplyr::select(variables_to_keep,target_variable) %>% mutate_at(.funs = funs(as.numeric),.vars = vars(-target_variable)) %>% mutate_at(.funs = funs(as.factor),.vars = vars(target_variable)) %>% as.data.frame()
  traindf <- traindf %>% dplyr::select(variables_to_keep,target_variable) %>% mutate_at(.funs = funs(as.numeric),.vars = vars(-target_variable)) %>% mutate_at(.funs = funs(as.factor),.vars = vars(target_variable)) %>% as.data.frame()
  
  for(i in 1:num_models_to_create){
    
    library(doParallel)
    library(parallel)
    cl <- makePSOCKcluster(parallel::detectCores()-1)
    registerDoParallel(cl)

    TrainingParameters <- trainControl(method = "repeatedcv", number = control_number_folds, repeats = control_fold_repeats, allowParallel = T)
    svm.Model <- suppressWarnings(train(formula, data=traindf,
                                         method = "svmRadial",
                                         trControl= TrainingParameters,
                                         preProcess = c("YeoJohnson","scale"),
                                         na.action = na.omit,
                                         tuneLength = tuneLength,
                                         tuneGrid = tuneGrid,
                                         maxit = maxit))
    
    ## When you are done:
    stopCluster(cl)
    table(pred)
    pred <-predict(svm.model, testdf)
    cm <-confusionMatrix(pred,testdf %>% pull(target_variable))
    
    accuracy <- cm$overall %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Accuracy") %>% pull(.)
    no_information_rate <- cm$byClass %>% as.data.frame() %>% tibble::rownames_to_column() %>% filter(rowname=="Prevalence") %>% pull(.)
    print(paste0("The accuracy of the model is: ",round(accuracy,2)," the no information rate is: "
                 ,round(no_information_rate,2)," better than by: ",round(accuracy - no_information_rate,2)))
    
    if(accuracy > no_information_rate + percent_higher_than_no_information_rate){
      save(rf.Model,file = paste0(directory_to_save_model,"svm_tuned_",i,".Rdata"))
    }
  }
}










