#Alex Zhou
#Economics 50
#Harvard University
#Send suggestions and corrections to gbruich@fas.harvard.edu
sink(file = "project3.txt", split = TRUE)

rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

#Set seed for cross validation and random forests
set.seed(2110)

# Install packages (if necessary) and load required libraries
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(randomForest)) install.packages("randomForest"); library(randomForest)
if (!require(rpart)) install.packages("rpart"); library(rpart)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)

#-------------------------------------------------------------------------------
# Data set up
#-------------------------------------------------------------------------------

#Open stata data set
dat <- read_dta("birthweight.dta")
head(dat)

#Generate a low birth weight variable
dat$lbw <- 0
dat$lbw[dat$birthweight<2500] <- 1
summary(dat$lbw)

#Store predictor variables which all start with P_*
all_predictors <- colnames(dat[,grep("^[P_]", names(dat))])
all_predictors

#Store predictor variables which all start with P_*, but EXCLUDE race
race <- c("P_mom_race_black", "P_mom_race_native", "P_mom_race_asian", "P_mom_race_white", "P_mom_hispanic")
exclude_race <- setdiff(all_predictors,race)
exclude_race

#Data frame with training data
training <- subset(dat, training==1)
summary(training)

#Data frame with test data
test <- subset(dat, training==0)
summary(test)

#-------------------------------------------------------------------------------
# Model 1: Multivariate regression with a small number of handpicked predictors
#-------------------------------------------------------------------------------

# my initial handpicked predictors: age of mother / substance use
mod1 <- lm(lbw ~ P_mom_age + P_mom_use_tobacco + P_mom_use_alcohol, data = training)
summary(mod1)

### generate predictions for all observations in test and training samples
y_test_predictions_mod1 <- predict(mod1, newdata=test)
y_train_predictions_mod1 <- predict(mod1, newdata=training)


#-------------------------------------------------------------------------------
# Model 2: Multivariable regression with the full set of predictors, 
# EXCLUDING mother's race and ethnicity
#-------------------------------------------------------------------------------

#Reformulate allows us to write lbw ~ followed by a list all the variables without writing them out
mod2 <- with(training, lm(reformulate(exclude_race, "lbw")))
summary(mod2)

### generate predictions for all observations in test and training samples
y_train_predictions_mod2 <- predict(mod2, newdata=training)
y_test_predictions_mod2 <- predict(mod2, newdata=test)

#-------------------------------------------------------------------------------
# Model 3: Decision tree with a small number of handpicked predictors and 
# handpicked tuning parameters
#-------------------------------------------------------------------------------

# My handpicked predictors for this model were: 
# hypertension (P_mom_gest_hyeprtension), incomp. cervix (P_mom_incomp_cerv)
# Based on significance levels of model 2, these were some of the most statistically significant predictors that
# I felt could plausibly have a causal relationship w/ low birthweight

mod3 <- rpart(lbw ~ P_mom_gest_hypertension + P_mom_prenatal_visits + P_mom_incomp_cerv,
                data=training, 
                maxdepth = 3, 
                cp=0) 

#Visualize tree
plot(mod3) # plot tree
text(mod3) # add labels to tree

### generate predictions for all observations in test and training samples
y_train_predictions_mod3 <- predict(mod3, newdata=training)
y_test_predictions_mod3 <- predict(mod3, newdata=test)

#-------------------------------------------------------------------------------
# Model 4: Decision tree with full predictor set  excluding mother's race and 
# ethnicity, using cross validation to tune the model
#-------------------------------------------------------------------------------

#Reformulate allows us to write lbw ~ followed by a list all the variables without writing them out
mod4 <- with(training, 
             rpart(reformulate(exclude_race, "lbw"), 
                    control = rpart.control(xval = 10)
                    )
              )


printcp(mod4) # print complexity parameter table using cross validation (xerror)

#Visualize tree
plot(mod4) # plot tree
text(mod4) # add labels to tree

### generate predictions for all observations in test and training samples
y_train_predictions_mod4 <- predict(mod4, newdata=training)
y_test_predictions_mod4 <- predict(mod4, newdata=test)


#-------------------------------------------------------------------------------
# Model 5: Random forests with full predictor set  excluding mother's race and 
# ethnicity, choosing the number of trees in the forest and number of predictors in a sensible way
#-------------------------------------------------------------------------------

#Random Forest from 400 Bootstrapped Samples (ntree=400)
#Random Forests may take a while to run!  Be patient!
mod5 <- randomForest(reformulate(exclude_race, "lbw"), 
                               ntree=200, 
                               mtry=10,
                               importance=TRUE, ## add importance=TRUE so that we store the variable importance information
                               data=training)

#Tuning parameters are ntree and mtry
#ntree is number of trees in your forest
#mtry is the number of predictors considered at each split (default is number of predictors divided by 3)

### Try changing mtry and ntree

mod5 #Review the Random Forest Results

### generate predictions for all observations in test and training samples
y_train_predictions_mod5 <- predict(mod5, newdata=training)
y_test_predictions_mod5 <- predict(mod5, newdata=test)

#Plot the out-of-bag error (pseudo out of sample) vs. number of trees
plot(mod5) #Plot the Random Forest Results

#What is the increase in error that occurs when the variable is NOT used to define tree splits
importance(mod5)
varImpPlot(mod5, type=1) #Plot the Random Forest Results

#type	is either 1 or 2, specifying the type of importance measure 
#(1=mean decrease in accuracy, 2=mean decrease in node impurity)


#-------------------------------------------------------------------------------
# Model 6: Random forests with full predictor set  INCLUDING mother's race and ethnicity
#-------------------------------------------------------------------------------

#Random Forest from 400 Bootstrapped Samples (ntree=400)
#Random Forests may take a while to run!  Be patient!
mod6 <- randomForest(reformulate(all_predictors, "lbw"), 
                     ntree=200, 
                     mtry=10,
                     importance=TRUE, ## add importance=TRUE so that we store the variable importance information
                     data=training)

#Tuning parameters are ntree and mtry
#ntree is number of trees in your forest
#mtry is the number of predictors considered at each split (default is number of predictors divided by 3)

### Try changing mtry and ntree

mod6 #Review the Random Forest Results

### generate predictions for all observations in test and training samples
y_train_predictions_mod6 <- predict(mod6, newdata=training)
y_test_predictions_mod6 <- predict(mod6, newdata=test)

#Plot the out-of-bag error (pseudo out of sample) vs. number of trees
plot(mod6) #Plot the Random Forest Results

#What is the increase in error that occurs when the variable is NOT used to define tree splits
importance(mod6)
varImpPlot(mod6, type=1) #Plot the Random Forest Results

#type	is either 1 or 2, specifying the type of importance measure 
#(1=mean decrease in accuracy, 2=mean decrease in node impurity)



#-------------------------------------------------------------------------------
# Compare RMSE for models 1-6
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Calculate and compare the mean squared error in the training sample: 
#-------------------------------------------------------------------------------

## Root mean squared error in  the training sample.
p <- 6
RMSE <- matrix(0, p, 1)
RMSE[1] <- sqrt(mean((training$lbw - y_train_predictions_mod1)^2, na.rm=TRUE))
RMSE[2] <- sqrt(mean((training$lbw - y_train_predictions_mod2)^2, na.rm=TRUE))
RMSE[3] <- sqrt(mean((training$lbw - y_train_predictions_mod3)^2, na.rm=TRUE))
RMSE[4] <- sqrt(mean((training$lbw - y_train_predictions_mod4)^2, na.rm=TRUE))
RMSE[5] <- sqrt(mean((training$lbw - y_train_predictions_mod5)^2, na.rm=TRUE))
RMSE[6] <- sqrt(mean((training$lbw - y_train_predictions_mod6)^2, na.rm=TRUE))

data_for_graph <- data.frame(RMSE, c("Model 1 - OLS", 
                                     "Model 2 - OLS", 
                                     "Model 3 - Tree",
                                     "Model 4 - Tree",
                                     "Model 5 - Forest",
                                     "Model 6 - Forest"
                                     ))  


# Change name of 1st column of df to "RMSE"
names(data_for_graph)[1] <- "RMSE in Traning Data"

# Change name of 2nd column of df to "Method"
names(data_for_graph)[2] <- "Method"

# Bar graph displaying results
p <- ggplot(data=data_for_graph, aes(x=Method, y=RMSE)) +
  geom_bar(stat="identity", fill="red4") +
  labs(y = "Training Data RMSE")
# Horizontal bar plot
p + coord_flip()

ggsave("rmse_training.png")


#-------------------------------------------------------------------------------
# Calculate and compare the mean squared error in  the lock box data 
#-------------------------------------------------------------------------------


## Root mean squared error in  the test sample.
p <- 6
RMSE <- matrix(0, p, 1)
RMSE[1] <- sqrt(mean((test$lbw - y_test_predictions_mod1)^2, na.rm=TRUE))
RMSE[2] <- sqrt(mean((test$lbw - y_test_predictions_mod2)^2, na.rm=TRUE))
RMSE[3] <- sqrt(mean((test$lbw - y_test_predictions_mod3)^2, na.rm=TRUE))
RMSE[4] <- sqrt(mean((test$lbw - y_test_predictions_mod4)^2, na.rm=TRUE))
RMSE[5] <- sqrt(mean((test$lbw - y_test_predictions_mod5)^2, na.rm=TRUE))
RMSE[6] <- sqrt(mean((test$lbw - y_test_predictions_mod6)^2, na.rm=TRUE))

data_for_graph <- data.frame(RMSE, c("Model 1 - OLS", 
                                     "Model 2 - OLS", 
                                     "Model 3 - Tree",
                                     "Model 4 - Tree",
                                     "Model 5 - Forest",
                                     "Model 6 - Forest"
))  

# Change name of 1st column of df to "RMSE"
names(data_for_graph)[1] <- "Out of Sample RMSE"

# Change name of 2nd column of df to "Method"
names(data_for_graph)[2] <- "Method"

# Bar graph displaying results
p <- ggplot(data=data_for_graph, aes(x=Method, y=RMSE)) +
  geom_bar(stat="identity", fill="navy") +
  labs(y = "Out of Sample RMSE in Lock Box data")
# Horizontal bar plot
p + coord_flip()

ggsave("rmse_test.png")



#-------------------------------------------------------------------------------
# Export test data set with predictions
#-------------------------------------------------------------------------------

#Export data set with training data + predictions from the six models
proj3 <- test

proj3$y_test_predictions_mod1 <- y_test_predictions_mod1
proj3$y_test_predictions_mod2 <- y_test_predictions_mod2
proj3$y_test_predictions_mod3 <- y_test_predictions_mod3
proj3$y_test_predictions_mod4 <- y_test_predictions_mod4
proj3$y_test_predictions_mod5 <- y_test_predictions_mod5
proj3$y_test_predictions_mod6 <- y_test_predictions_mod6

write_dta(proj3, "proj3_results.dta")


##### Q7: 

# generate percentile ranks of predictions
# we choose mod2 as the preferred prediction model because it produces the lowest test RMSE

# percentile rank function
percentile_rank <- function(variable) {
  r <- ifelse(is.na(variable), NA, rank(variable))
  100 * r/max(r, na.rm = T)
}
#dat$rankings <- percentile_rank(y_test_predictions_mod2)
proj3$rankings <- percentile_rank(proj3$y_test_predictions_mod2)

# binscatter plots:

# binscatter functions
binscatter<-function(data, xvar, yvar, TITLE, YTITLE){
  ggplot(data, aes(x = xvar , y = yvar)) +
    stat_binmean(n = 20, geom = "line") + 
    stat_binmean(n = 20, geom = "point") +
    geom_vline(xintercept=55) + geom_vline(xintercept=97.5)+
    labs(title = TITLE, 
         y = YTITLE,
         x = "Percentile of Algorithm Risk Score")
}
binscatter_strat<-function(data, xvar, yvar, TITLE, YTITLE){
  ggplot(data, aes(x = xvar , y = yvar, color = race)) +
    stat_binmean(n = 20, geom = "line") + 
    stat_binmean(n = 20, geom = "point") +
    geom_vline(xintercept=55) + geom_vline(xintercept=97.5)+
    labs(title = TITLE, 
         y = YTITLE,
         x = "Percentile of Algorithm Risk Score")
}

# binscatter 1: lbw vs. percentile rank
with(proj3, binscatter(proj3, rankings, lbw, "Model 2", "Low Birthweight"))

# binscatter 2: stratified by race
proj3$race[which(proj3$P_mom_race_white == 1)] <- "White"
proj3$race[which(proj3$P_mom_race_black == 1)] <- "Black"
proj3$race[which(proj3$P_mom_race_native == 1)] <- "Native"
proj3$race[which(proj3$P_mom_race_asian == 1)] <- "Asian"

with(proj3, binscatter_strat(proj3, rankings, lbw, "Model 2", "Low Birthweight"))

###### Q8: 

# subset eligible population
eligible <- subset(proj3, rankings >= 75)

# racial makeup of eligible
prop.table(table(eligible$race))

# low birth weight counts, by racial group:
table(eligible$lbw[eligible$race == "White"])
table(eligible$lbw[eligible$race == "Black"])
table(eligible$lbw[eligible$race == "Native"])
table(eligible$lbw[eligible$race == "Asian"])