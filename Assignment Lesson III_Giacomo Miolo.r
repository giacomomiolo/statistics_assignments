################################################################################
#                                                                              #
#                  ASSIGNMENT LESSON III - DESCRIPTIVES (II)                   #
#                                                                              #
################################################################################

# Note: - Write your code under each point, togeter with results (if needed).
#       - Rename this file as "Assignment Lesson III_name surname.r"

# The dataset "oecdpisa data germany.txt" contains the score in science of n=1680 german students
# who underwent the OECD-PISA evaluation at age 15.
# The legend is reported in the "read me - PISA variable" file.

setwd("/Users/giacomomiolo/Desktop/BABD/Statistics/Ieva/Dataset for assignments/Giacomo_Miolo_Assignment_3")
data_ger <- read.table("oecdpisa data germany.txt", header=TRUE)
dim(data_ger)
head(data_ger)

attach(data_ger)


# 1) Compute the main location and shape indexes related to the response variable "pv3scie"  (2 points)

# Location indexes

mean(pv3scie) # 540.7008
median(pv3scie) # 545.3185

# Dispersion indexes

range(pv3scie) # 193.950 832.487
var(pv3scie) # 9196.88
sd(pv3scie) # 95.90036
quantile(pv3scie)
#       0%      25%      50%      75%     100% 
#193.9500 475.6633 545.3185 608.7020 832.4870 
IQR(pv3scie) # 133.0387

# Shape indexes

library(moments)
skewness(pv3scie) # -0.2337731
kurtosis(pv3scie) # 2.909106




# 2) Compute the 85th quantile of the distribution of the variable "pv3scie" and explains what does it represent  (1 points)

quantile(pv3scie, 0.85) # 637.9645

# 85% percent of the values of variable "pv3scie" are less than this number (637.9645).

# 3) Do boys enjoy math more than girls?  (2.5 points)
# Remind that female = 1

# Assuming variable "enjoy_science" measures the enjoyment of math.
tapply(enjoy_science, gender, summary)

# Boys
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.1154 -0.8208  0.0992  0.1456  1.0311  2.1635 

# Girls
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.1154 -1.0264 -0.4593 -0.3496  0.5094  2.1635 

x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,1))
par(mar=c(4, 4, 2, 2))
boxplot(enjoy_science[gender==1],
        horizontal=TRUE,
        main="Girls who like M4th",
        col="#d063c9") 
boxplot(enjoy_science[gender==0],
        horizontal=TRUE,
        main="Boys who like M4th",
        col="#6081ec")

# Yes, boys enjoy math more than girls.
graphics.off()

# 4) Do girls study more than boys?   (0.5 points)
# Remind that female = 1

# Assuming variable "time_homework" measures quantity of study.
tapply(time_homework, gender, summary)

# Boys
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   4.000   7.000   9.932  12.000  61.000 

# Girls
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     5.0     8.0    11.1    14.0    68.0 

# Yes, girls study more than boys.


# 5) Look at the distribution of "time homework" as a whole. Do you expect the median being lower/nearly equal/higher than the mean? Why?   (1 point)

hist(time_homework)
boxplot(time_homework)

# The distribution is skewed to the right (positively skewed).
# The asymmetry affects heavily the mean.
# In this case I expect the median to be lower than the mean (influenced more by the many outliers).


# 6) Is the ESCS Normally distributed?   (1 points)

hist(ESCS, breaks = seq(-3,3,0.25))
skewness(ESCS) # -0.1104042 => Negatively skewed
kurtosis(ESCS) # 2.327471 => Less than 3, Platykurtic

# ESCS isn't normally distributed, skewness should be very close to 0 and kurtosis very close to 3.


# 7) Is the number of the students higher in villages and small tows with respect to greater cities?  (2 points)

# NB: The ID of the school is repeated for each pupil attending the school. Therefore it is necessary to select schools only once. This is possible using the following commands:

# Create a dataset with one row for each school (and not student as in the previous case)
schools <- unique(school_id)
length(schools)  # there are 160 schools
# for each school, we extract the number of students and the type of the city
nstud <- NA
typecity <- NA
for(i in schools){
  temp1 <- Nstud[school_id==i]
  temp11 <- temp1[1]
  nstud <- c(nstud,temp11)
  temp2 <- type_city[school_id==i]
  temp22 <- temp2[1]
  typecity <- c(typecity,temp22)
}
nstud <- nstud[-1]
typecity <- typecity[-1]

# 1=village, 2=small town, 3=town, 4=city, 5=large city
typecity_factor <- factor(typecity, levels = c('1', '2', '3', '4', '5'))
x11()
boxplot(nstud ~ typecity_factor, names = c('Village', 'Small town', 'Town', 'City', 'Large city'))

village <- sum(nstud[typecity == 1])
small_town <- sum(nstud[typecity == 2])
town <- sum(nstud[typecity == 3])
city <- sum(nstud[typecity == 4])
large_city <- sum(nstud[typecity == 5])
c(village, small_town, town, city, large_city)
sum(village+small_town)
sum(town+city+large_city)

# Villages + Small Towns = 36725
# Assuming "greater cities" as Town + City + Large City = 82388
# The number of students in "greater cities" is higher than in Villages + Small Towns.


