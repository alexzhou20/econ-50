#Alex Zhou
#Economics 50, Spring 2021
#Harvard University

#Send corrections and suggestions to gbruich@fas.harvard.edu

rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

if (!require(haven)) install.packages("haven"); library(haven)
if (!require(sandwich)) install.packages("sandwich"); library(sandwich)
if (!require(lmtest)) install.packages("lmtest"); library(lmtest)
if (!require(stargazer)) install.packages("stargazerr"); library(stargazer)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(lubridate)) install.packages("lubridate"); library(lubridate)
if (!require(statar)) install.packages("statar"); library(statar)


#********************************************************************************
#********************************************************************************
#***********Part 1: Analyzing the County Level Data******************************
#********************************************************************************
#********************************************************************************

#Load in data
dat <- read_dta("Downloads/tracker_county_daily.dta")

#Define calendar date as t using as_date from lubridate
dat$t <- as_date(dat$daten)
summary(dat$t)

#Summary statistics for April 2020, all counties in CA
summary(subset(dat, 
       select = c("spend_all", "emp_combined_inclow", "emp_combined", "new_death_rate"),
       stateabbrev == "CA" & month == 4 & year == 2020))

#Summary statistics for January 2021, all counties in CA
summary(subset(dat, 
               select = c("spend_all", "emp_combined_inclow", "emp_combined", "new_death_rate"),
               stateabbrev == "CA" & month == 1 & year == 2021))

#Summary statistics since January 2021, all counties in Pacific Division
summary(subset(dat, 
               select = c("spend_all", "emp_combined_inclow", "emp_combined", "new_death_rate"),
               division_name == "Pacific Division" & t >= "2021-01-01"))


#Summary statistics between April 2020 and August 2020, all counties in Pacific Division
summary(subset(dat, 
               select = c("spend_all", "emp_combined_inclow", "emp_combined", "new_death_rate"),
               division_name == "Pacific Division" & t >= "2020-04-01" & t <= "2020-08-01"))


#Employment changes by income group (national). 
ggplot(dat, aes(x = daten)) +
  stat_binmean(n = 20, geom = "line", aes(y=emp_combined_inclow, colour = "Low")) + 
  stat_binmean(n = 20, geom = "point", aes(y=emp_combined_inclow, colour = "Low")) +
  stat_binmean(n = 20, geom = "line", aes(y=emp_combined_incmiddle, colour = "Middle")) + 
  stat_binmean(n = 20, geom = "point", aes(y=emp_combined_incmiddle, colour = "Middle")) +
  stat_binmean(n = 20, geom = "line", aes(y=emp_combined_inchigh, colour = "High")) + 
  stat_binmean(n = 20, geom = "point", aes(y=emp_combined_inchigh, colour = "High")) +
  labs(title = "Employment for Low, Middle, and High Income Workers", 
       y = "Change Relative to January 2020",
       x = "Date")

ggsave("employment_national_county.png")

##### Question 2: Initial effects of COVID-19 and hardest-hit groups
#_________________________________________________________________

#Collapse data to analyze relationship between low wage employment losses, etc. in April 2020

#Subset data to April 2020
april2020 <- subset(dat, month == 4 & year == 2020)

##Collapse data to means in April 2020
by_county <- group_by(april2020, countyfips, countyname, cz, czname, statefips, stateabbrev, state_name, region_numeric, division_numeric, region_name, division_name)
april2020_collapsed <- summarise(by_county, 
                                 spend_all = mean(spend_all, na.rm = TRUE), 
                                 emp_combined_inclow = mean(emp_combined_inclow, na.rm = TRUE),
                                 emp_combined = mean(emp_combined, na.rm = TRUE),
                                 new_death_rate = mean(new_death_rate, na.rm = TRUE),
                                 popdensity2010= mean(popdensity2010, na.rm = TRUE),
                                 rent_twobed2015= mean(rent_twobed2015, na.rm = TRUE),
                                 share_black2010= mean(share_black2010, na.rm = TRUE),
                                 share_hisp2010= mean(share_hisp2010, na.rm = TRUE),
                                 share_asian2010= mean(share_asian2010, na.rm = TRUE), 
                                 share_white2010= mean(share_white2010, na.rm = TRUE),
                                 poor_share2010= mean(poor_share2010, na.rm = TRUE),
                                 county_pop2019= mean(county_pop2019, na.rm = TRUE)
                                 )
str(april2020_collapsed) 

## Question: does the fall in low-wage employment vary between low-income and high-income areas?

# Low-wage employment changes by share of poverty (national). 
g <- ggplot(april2020_collapsed, aes(x = poor_share2010, y=emp_combined_inclow)) +
  stat_binmean(n = 20, geom = "point") +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Low-wage employment in April 2020", 
       y = "Change Relative to January 2020",
       x = "Share Below Poverty Line in 2010")
g

# You can "zoom in" using the following ggplot command (thanks to Dhruv Mohnot)
g <- g + coord_cartesian(xlim=c(0,0.25), ylim=c(-0.4, -0.25))
g

# Low-wage employment changes by income group (Pacific Census Division). 
ggplot(subset(april2020_collapsed, division_name == "Pacific Division"), 
       aes(x = poor_share2010, y=emp_combined_inclow)) +
  stat_binmean(n = 20, geom = "point") +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Low-wage employment in April 2020 in Pacific Division", 
       y = "Change Relative to January 2020",
       x = "Share Below Poverty Line in 2010")

# overall employment changes by income
ggplot(subset(april2020_collapsed, division_name == "Pacific Division"), 
       aes(x = poor_share2010, y=emp_combined)) +
  stat_binmean(n = 20, geom = "point") +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Employment in April 2020 in Pacific Division", 
       y = "Change Relative to January 2020",
       x = "Share Below Poverty Line in 2010")

# Low-wage employment changes by income group (California). 
# We don't have a lot of data here
ggplot(subset(april2020_collapsed, stateabbrev == "CA"), 
       aes(x = poor_share2010, y=emp_combined_inclow)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Low-wage employment in April 2020 in California", 
       y = "Change Relative to January 2020",
       x = "Share Below Poverty Line in 2010")


## Were educated areas more heavily affected health-wise?

# Case rate vs. higher education (Pacific Census Division). 
ggplot(subset(april2020, division_name == "Pacific Division"), 
       aes(x = frac_coll_plus2010, y=new_case_rate)) +
  stat_binmean(n = 20, geom = "point") +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "COVID-19 Cases in April 2020 in Pacific Division", 
       y = "New Daily Case Rate",
       x = "Share With A College Degree in 2010")

# CA:
ggplot(subset(april2020, stateabbrev == "CA"), 
       aes(x = frac_coll_plus2010, y=new_case_rate)) +
  stat_binmean(n = 20, geom = "point") +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "COVID-19 Cases in April 2020 in California", 
       y = "New Daily Case Rate",
       x = "Share With A College Degree in 2010")


## Did spending drop more in high-rent or in low-rent areas?

# Spending vs. rent (West South Central Census Division). 
ggplot(subset(april2020_collapsed, division_name == "Pacific Division"), 
       aes(x = rent_twobed2015, y=spend_all)) +
  stat_binmean(n = 20, geom = "point") +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Spending Changes in April 2020 in Pacific Division", 
       y = "Adjusted spending relative to January 2020",
       x = "Rent")

# CA:
ggplot(subset(april2020_collapsed, stateabbrev = "CA"), 
       aes(x = rent_twobed2015, y=spend_all)) +
  stat_binmean(n = 20, geom = "point") +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Spending Changes in April 2020 in California", 
       y = "Adjusted spending relative to January 2020",
       x = "Rent")


###### Question 3: How do different groups/counties recover?

# Subset data to January 2021
jan2021 <- subset(dat, month == 1 & year == 2021)

##Collapse data to means in April 2020
by_county <- group_by(jan2021, countyfips, countyname, cz, czname, statefips, stateabbrev, state_name, region_numeric, division_numeric, region_name, division_name)
jan2021_collapsed <- summarise(by_county, 
                                 spend_all = mean(spend_all, na.rm = TRUE), 
                                 emp_combined_inclow = mean(emp_combined_inclow, na.rm = TRUE),
                                 emp_combined = mean(emp_combined, na.rm = TRUE),
                                 new_death_rate = mean(new_death_rate, na.rm = TRUE),
                                 popdensity2010= mean(popdensity2010, na.rm = TRUE),
                                 rent_twobed2015= mean(rent_twobed2015, na.rm = TRUE),
                                 share_black2010= mean(share_black2010, na.rm = TRUE),
                                 share_hisp2010= mean(share_hisp2010, na.rm = TRUE),
                                 share_asian2010= mean(share_asian2010, na.rm = TRUE), 
                                 share_white2010= mean(share_white2010, na.rm = TRUE),
                                 poor_share2010= mean(poor_share2010, na.rm = TRUE),
                                 county_pop2019= mean(county_pop2019, na.rm = TRUE)
)
str(jan2021_collapsed) 

## low-wage employment vs. poverty:
ggplot(subset(jan2021_collapsed, division_name == "Pacific Division"), 
aes(x = poor_share2010, y=emp_combined_inclow)) +
  stat_binmean(n = 20, geom = "point") +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Low-wage employment in January 2021 in Pacific Division", 
       y = "Change Relative to January 2020",
       x = "Share Below Poverty Line in 2010")

# overall employment changes vs. poverty
ggplot(subset(jan2021_collapsed, division_name == "Pacific Division"), 
       aes(x = poor_share2010, y=emp_combined)) +
  stat_binmean(n = 20, geom = "point") +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Employment in January 2021 in Pacific Division", 
       y = "Change Relative to January 2020",
       x = "Share Below Poverty Line in 2010")

# Low-wage employment changes by income group (California). 
# Also not a lot of data for this
ggplot(subset(jan2021_collapsed, stateabbrev == "CA"), 
       aes(x = poor_share2010, y=emp_combined_inclow)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Low-wage employment in January 2021 in California", 
       y = "Change Relative to January 2020",
       x = "Share Below Poverty Line in 2010")

## education and caserate?
ggplot(subset(jan2021, division_name == "Pacific Division"), 
       aes(x = frac_coll_plus2010, y=new_case_rate)) +
  stat_binmean(n = 20, geom = "point") +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "COVID-19 Cases in January 2021 in Pacific Division", 
       y = "New Daily Case Rate",
       x = "Share With A College Degree in 2010")

# CA:
ggplot(subset(jan2021, stateabbrev == "CA"), 
       aes(x = frac_coll_plus2010, y=new_case_rate)) +
  stat_binmean(n = 20, geom = "point") +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "COVID-19 Cases in January 2021 in California", 
       y = "New Daily Case Rate",
       x = "Share With A College Degree in 2010")

## spending vs. rent?
ggplot(subset(jan2021_collapsed, division_name == "Pacific Division"), 
       aes(x = rent_twobed2015, y=spend_all)) +
  stat_binmean(n = 20, geom = "point") +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Spending Changes in January 2021 in Pacific Division", 
       y = "Adjusted spending relative to January 2020",
       x = "Rent")

# CA:
ggplot(subset(jan2021_collapsed, stateabbrev = "CA"), 
       aes(x = rent_twobed2015, y=spend_all)) +
  stat_binmean(n = 20, geom = "point") +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Spending Changes in January 2021 in California", 
       y = "Adjusted spending relative to January 2020",
       x = "Rent")



#********************************************************************************
#********************************************************************************
#***********Part 2: Analyzing the State Level Data*******************************
#********************************************************************************
#********************************************************************************

dat_state <- read_dta("Downloads/tracker_state_daily.dta")

#Define calendar date as t using as_date from lubridate
dat_state$t <- as_date(dat_state$daten)
summary(dat_state$t)

#Employment changes by income group (national). 
ggplot(dat_state, aes(x = daten)) +
  stat_binmean(n = 20, geom = "line", aes(y=emp_combined_inclow, colour = "Low")) + 
  stat_binmean(n = 20, geom = "point", aes(y=emp_combined_inclow, colour = "Low")) +
  stat_binmean(n = 20, geom = "line", aes(y=emp_combined_incmiddle, colour = "Middle")) + 
  stat_binmean(n = 20, geom = "point", aes(y=emp_combined_incmiddle, colour = "Middle")) +
  stat_binmean(n = 20, geom = "line", aes(y=emp_combined_inchigh, colour = "High")) + 
  stat_binmean(n = 20, geom = "point", aes(y=emp_combined_inchigh, colour = "High")) +
  labs(title = "Employment for Low, Middle, and High Income Workers", 
       y = "Change Relative to January 2020",
       x = "Date")

ggsave("employment_national_state.png")

ggplot(dat_state, aes(x = daten)) +
  stat_binmean(n = 20, geom = "line", aes(y=spend_all_inclow , colour = "Low")) + 
  stat_binmean(n = 20, geom = "point", aes(y=spend_all_inclow , colour = "Low")) +
  stat_binmean(n = 20, geom = "line", aes(y=spend_all_incmiddle, colour = "Middle")) + 
  stat_binmean(n = 20, geom = "point", aes(y=spend_all_incmiddle, colour = "Middle")) +
  stat_binmean(n = 20, geom = "line", aes(y=spend_all_inchigh, colour = "High")) + 
  stat_binmean(n = 20, geom = "point", aes(y=spend_all_inchigh, colour = "High")) +
  labs(title = "Spending for Low, Middle, and High Income Workers", 
       y = "Change Relative to January 2020",
       x = "Date")

ggsave("spending_national_state.png")



### Estimating differences in differences regression:

# indicator for california
dat_state$ca <- 0
dat_state$ca[which(dat_state$stateabbrev=="CA")] <- 1
summary(dat_state$ca)

# post-stay at home policy indicator
dat_state$post_stay <- 0
dat_state$post_stay[which(dat_state$t >= "2020-03-19")] <- 1

# interaction term 
dat_state$did_stay <- dat_state$ca * dat_state$post_stay

# simple differences in differences
# cases:
reg1 <- lm(new_case_rate ~ post_stay + ca + did_stay, data = dat_state)
coeftest(reg1, vcov = vcovHC(reg1, type = "HC1"))

# spending:
reg2 <- lm(spend_all ~ post_stay + ca + did_stay, data = dat_state)
coeftest(reg2, vcov = vcovHC(reg2, type = "HC1"))

# employment:
reg3 <- lm(emp_combined ~ post_stay + ca + did_stay, data = dat_state)
coeftest(reg3, vcov = vcovHC(reg3, type = "HC1"))

# post reopening indicator
dat_state$post_open <- 0
dat_state$post_open[which(dat_state$t >= "2020-05-22")] <- 1

# interaction term
dat_state$did_open <-dat_state$ca * dat_state$post_open

# simple differences in differences
# cases:
reg4 <- lm(new_case_rate ~ post_open + ca + did_open, data = dat_state)
coeftest(reg4, vcov = vcovHC(reg4, type = "HC1"))

# interestingly, the did_open coefficient for cases is not statistically significant at the
# alpha = 0.05 level. 

# spending:
reg5 <- lm(spend_all ~ post_open + ca + did_open, data = dat_state)
coeftest(reg5, vcov = vcovHC(reg5, type = "HC1"))

# employment:
reg6 <- lm(emp_combined ~ post_open + ca + did_open, data = dat_state)
coeftest(reg6, vcov = vcovHC(reg6, type = "HC1"))