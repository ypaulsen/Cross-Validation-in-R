################################################################################
############# Load libraries and data ##########################################
################################################################################


library(ranger)
library(dplyr)
library(yardstick)
library(visdat)

##Read in the data from github user ypaulsen

dat12_14 <- read.csv("https://github.com/ypaulsen/Cross-Validation-in-R/raw/main/dat12_14a.csv") 
dat12_14$loan_status <- as.factor(dat12_14$loan_status) # Change outcome to factor  



################################################################################
#############  Set values ######################################################
################################################################################


CV <- 10  # Number of subsets
dat <- dat12_14  # Set dataset for crossvalidation
test_l <- nrow(dat)%/%CV   
seq <- seq(from=0, to=10*test_l, by = test_l)


################################################################################
#############  Begin nested for loop ###########################################
################################################################################


mtry <- c(3, 4)
numt <- c(300, 200)
mean_kappa <- c()
mean_AUC <- c()
mean_Acc <- c()
output <- list()
for(j in 1:length(mtry)){
  test <- list()
  train <- list()
  pred_cv <- list()
  results <- list()
  metrics <- list()
  kappa <- c()
  Acc <- c()
  AUC <- c()
  output$mtry[j] <- mtry[j]
  output$numt[j] <- numt[j]
  for(i in 1:CV){
    test_rows <- seq[i]:seq[i+1]
    test[[i]] <- dat[test_rows,]
    train[[i]] <- dat[-test_rows,]
    ranger_lending_cv <- ranger(loan_status ~ ., data = train[[i]], 
                                num.threads = 3, mtry = mtry[j], num.trees = numt[j])
    pred_cv[[i]] <- predict(ranger_lending_cv, data = test[[i]])$predictions
    results[[i]] <- bind_cols(truth = test[[i]]$loan_status, 
                              estimate=(as.numeric(pred_cv[[i]])-2)^2, 
                              f_estimate = pred_cv[[i]])
    
    metrics[[i]] <- results[[i]] %>% 
      metrics(truth = truth, estimate = f_estimate)
    kappa[i] <- metrics[[i]]$.estimate[2] 
    Acc[i] <- metrics[[i]]$.estimate[1]
    
    auc <- results[[i]] %>%
      roc_auc(truth = truth, estimate)
    AUC[i] <- auc$.estimate
  }
  mean_kappa[j] <- mean(kappa)
  mean_AUC[j] <- mean(AUC)
  mean_Acc[j] <- mean(Acc)
}  
output$mean_Acc <- mean_Acc
output$mean_AUC <- mean_AUC
output$mean_kappa <- mean_kappa


################################################################################
#############  Evaluate ########################################################
################################################################################


output