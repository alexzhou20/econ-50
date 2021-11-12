# Alex Zhou
#Economics 50, Spring 2021
#Harvard University

rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

## load packages
if (!require(lmtest)) install.packages("lmtest"); library(lmtest)
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(sandwich)) install.packages("sandwich"); library(sandwich)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(rdrobust)) install.packages("rdrobust"); library(rdrobust)

# read data
third_grade = read_dta("third_grade.dta")

##### Q2: 
# summarize scores
summary(third_grade$math)
summary(third_grade$verb)

# compute mean exam z scores/percentile rankings:

# z scores:
third_grade$z_math <- (third_grade$math - mean(third_grade$math, na.rm = TRUE))/sd(third_grade$math, na.rm = TRUE)
third_grade$z_verb <- (third_grade$verb - mean(third_grade$verb, na.rm = TRUE))/sd(third_grade$verb, na.rm = TRUE)
third_grade$zscore <- (third_grade$z_math + third_grade$z_verb)/2

# percentiles:
third_grade$r_math <- ifelse(is.na(third_grade$math), NA, rank(third_grade$math))
third_grade$r_math <- 100 * third_grade$r_math / max(third_grade$r_math, na.rm = T)
third_grade$r_verb <- ifelse(is.na(third_grade$verb), NA, rank(third_grade$verb))
third_grade$r_verb <- 100 * third_grade$r_verb / max(third_grade$r_verb, na.rm = T)
third_grade$perc <- (third_grade$r_math + third_grade$r_verb)/2

# summarize zscores/percentile rank
summary(third_grade$zscore)
summary(third_grade$perc)

##### Q3:

# i. visualize relationship btwn SES_index and z score
plot(third_grade$ses_index, third_grade$zscore, xlab = "SES index", ylab = "Mean z-score",
     main = "Relationship between SES Index and Exam Z-scores")

# ii. visualize relationship btwn SES_index and perc. rank
plot(third_grade$ses_index, third_grade$perc, xlab = "SES index", ylab = "Mean percentile rank",
     main = "Relationship between SES Index and mean exam percentile rankings")

# iii. class size/SES index
plot(third_grade$class_size, third_grade$ses_index, xlab = "Class size", ylab = "SES index",
     main = "Relationship between class size and SES index")

# iv. class size/z score
rdplot(x = third_grade$class_size,
       third_grade$zscore, 
       p = 1, #p = 1 is linear best fit line. p >= 2 is nonlinear
       nbins = c(20, 20), #number of bins on each side of threshold
       binselect = "es", #option to use "equal spaced" binning
       x.label = "Class size",
       y.label = "Average exam Z-score"
)

# v. class size/perc. rank
rdplot(third_grade$class_size, 
       third_grade$perc,
       p = 1, #p = 1 is linear best fit line. p >= 2 is nonlinear
       nbins = c(20, 20), #number of bins on each side of threshold
       c = 10,
       binselect = "es", #option to use "equal spaced" binning
       x.label = "Class size",
       y.label = "Average score percentile rank"
)

##### Q5:

# a. binned scatter plot of class size w/ cutoff at 40 students
rdplot(third_grade$school_enrollment,
       third_grade$class_size,
       p = 2,
       nbins = c(20, 20),
       c = 40,
       binselect = "es",
       x.label = "School enrollment",
       y.label = "Class size"
)

# b. binned scatter plots of school enrollment vs.:

# i. test score index
rdplot(x=third_grade$school_enrollment,
       third_grade$zscore,
       p = 1,
       nbins = c(20, 20),
       c = 40,
       binselect = "es",
       x.label = "School enrollment",
       y.label = "Test score index"
)

# ii. percentile rank
rdplot(third_grade$school_enrollment,
       third_grade$perc,
       p = 1,
       nbins = c(20, 20),
       c = 40,
       binselect = "es",
       x.label = "School enrollment",
       y.label = "Score percentile rank"
)

##### Q6: regressions
narrow <- subset(third_grade, school_enrollment < 80)
below <- subset(third_grade, school_enrollment < 40)
above <- subset(third_grade, school_enrollment >= 40 & school_enrollment < 80)

# i. enrollment vs class size
mod1 <- lm(class_size ~ school_enrollment, data = below)
mod1_2 <- lm(class_size ~ school_enrollment, data = above)
prediction1 <- mod1$coefficients["(Intercept)"] + 40 * mod1$coefficients["school_enrollment"]
prediction1_2 <- mod1_2$coefficients["(Intercept)"] + 40 * mod1_2$coefficients["school_enrollment"]
prediction1_2 - prediction1

# ii. score index 
mod2 <- lm(zscore ~ school_enrollment, data = below)
mod2_2 <- lm(zscore ~ school_enrollment, data = above)
prediction2 <- mod2$coefficients["(Intercept)"] + 40 * mod2$coefficients["school_enrollment"]
prediction2_2 <- mod2_2$coefficients["(Intercept)"] + 40 * mod2_2$coefficients["school_enrollment"]
prediction2_2 - prediction2

# iii. percentile rank
mod3 <- lm(perc ~ school_enrollment, data = below)
mod3_2 <- lm(perc ~ school_enrollment, data = above)
prediction3 <- mod3$coefficients["(Intercept)"] + 40 * mod3$coefficients["school_enrollment"]
prediction3_2 <- mod3_2$coefficients["(Intercept)"] + 40 * mod3_2$coefficients["school_enrollment"]
prediction3_2 - prediction3

##### Q7: confidence intervals
confint(mod1)
confint(mod2)
confint(mod3)

##### Q9: predict score change when class size goes from 40 to 35
mod_z <- lm(zscore ~ class_size, data = narrow)
mod_perc <- lm(perc ~ class_size, data = narrow)
confint(mod_z)
confint(mod_perc)

# predictions
pred_z <- 5 * 0.007837
pred_perc <- 5 * 0.191664

##### Q12: visualizations

# a. binned scatter plots of student/school characteristics
rdplot(narrow$ses_index,
       narrow$school_enrollment,
       p = 1,
       nbins = c(20, 20),
       c = 40,
       binselect = "es",
       x.label = "Enrollment (# students)",
       y.label = "SES Index",
       title = "Balance of Baseline Characteristics"
)

rdplot(narrow$boy,
       narrow$school_enrollment,
       p = 1,
       nbins = c(20, 20),
       c = 40,
       binselect = "es",
       x.label = "Enrollment (# students)",
       y.label = "Male proportion",
       title = "Balance of Baseline Characteristics"
)

rdplot(narrow$religious,
       narrow$school_enrollment,
       p = 1,
       nbins = c(20, 20),
       c = 40,
       binselect = "es",
       x.label = "Enrollment (# students)",
       y.label = "Proportion of schools religious",
       title = "Balance of Baseline Characteristics"
)

rdplot(narrow$number_schools,
       narrow$school_enrollment,
       p = 1,
       nbins = c(20, 20),
       c = 40,
       binselect = "es",
       x.label = "Enrollment (# students)",
       y.label = "Number of schools in town",
       title = "Balance of Baseline Characteristics"
)

# b. hist
ggplot(narrow, aes(school_enrollment)) + geom_histogram(bins = 400) + geom_vline(xintercept = 40, color = "red")
