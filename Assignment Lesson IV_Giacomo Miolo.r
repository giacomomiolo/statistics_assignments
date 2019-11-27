################################################################################
#                                                                              #
#           ASSIGNMENT LESSON IV - MULTIVARIATE DATA - Correlation             #
#                                                                              #
################################################################################

# Note: - Write your code under each point, togeter with results (if needed).
#       - Rename this file as "Assignment Lesson IV_name surname.r"

# The dataset "oecdpisa data.txt" contains the score in science of students
# who underwent the OECD-PISA evaluation at age 15.
# The legend is reported in the "read me - PISA variable" file.

setwd("/Users/giacomomiolo/Desktop/BABD/Statistics/Ieva/Dataset for assignments/Giacomo_Miolo_Assignment_3")
data_jap <- read.table("oecdpisa data japan.txt", header=TRUE)
data_ger <- read.table("oecdpisa data germany.txt", header=TRUE)

dim(data_ger)
dim(data_jap)

names(data_jap)



# 1) Select variables "pv3scie", "enjoy_science", "time_homework", "anxtest", "ESCS", "motivat" from the german dataset.
#    Plot the variables and evaluate (both graphically and numerically) the correlations among them.
#    (2.5 points)

### Selecting the variables from german dataset

names(data_ger)

vars_ger <- data_ger[c(9,4,5,7,8,10)]

names(vars_ger)

### Plotting the variables

# pv3scie
x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0, 4, 1, 2))
boxplot(vars_ger[,1],
        horizontal = TRUE,
        main = paste("Boxplot and Histogram of", names(vars_ger)[1]))
par(mar=c(4, 4, 1, 2))
hist(vars_ger[,1],
     prob = TRUE,
     xlab = names(vars_ger)[1],
     main = "")

# enjoy_science
x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0, 4, 1, 2))
boxplot(vars_ger[,2],
        horizontal = TRUE,
        main = paste("Boxplot and Histogram of", names(vars_ger)[2]))
par(mar=c(4, 4, 1, 2))
hist(vars_ger[,2],
     prob = TRUE,
     xlab = names(vars_ger)[2],
     main = "")

# time_homework
x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0, 4, 1, 2))
boxplot(vars_ger[,3],
        horizontal = TRUE,
        main = paste("Boxplot and Histogram of", names(vars_ger)[3]))
par(mar=c(4, 4, 1, 2))
hist(vars_ger[,3],
     prob = TRUE,
     xlab = names(vars_ger)[3],
     main = "")

# anxtest
x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0, 4, 1, 2))
boxplot(vars_ger[,4],
        horizontal = TRUE,
        main = paste("Boxplot and Histogram of", names(vars_ger)[4]))
par(mar=c(4, 4, 1, 2))
hist(vars_ger[,4],
     prob = TRUE,
     xlab = names(vars_ger)[4],
     main = "")

# ESCS
x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0, 4, 1, 2))
boxplot(vars_ger[,5],
        horizontal = TRUE,
        main = paste("Boxplot and Histogram of", names(vars_ger)[5]))
par(mar=c(4, 4, 1, 2))
hist(vars_ger[,5],
     prob = TRUE,
     xlab = names(vars_ger)[5],
     main = "")

# motivat
x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0, 4, 1, 2))
boxplot(vars_ger[,6],
        horizontal = TRUE,
        main = paste("Boxplot and Histogram of", names(vars_ger)[6]))
par(mar=c(4, 4, 1, 2))
hist(vars_ger[,6],
     prob = TRUE,
     xlab = names(vars_ger)[6],
     main = "")

### Evaluating correlation graphically and numerically
library(Hmisc)
library(corrplot)
library(RColorBrewer)
library(hexbin)


corr_data_ger <- rcorr(as.matrix(vars_ger))
corr_data_ger

# pv3scie enjoy_science time_homework anxtest  ESCS motivat
# pv3scie          1.00          0.33         -0.20   -0.22  0.39    0.10
# enjoy_science    0.33          1.00          0.04   -0.16  0.17    0.19
# time_homework   -0.20          0.04          1.00    0.09 -0.03    0.02
# anxtest         -0.22         -0.16          0.09    1.00 -0.15    0.00
# ESCS             0.39          0.17         -0.03   -0.15  1.00    0.09
# motivat          0.10          0.19          0.02    0.00  0.09    1.00


x11()
corrplot(corr_data_ger$r, type="upper", order="hclust", col=brewer.pal(n=8, name="RdYlBu"))

# Blanks where correlation is not significant, p_value < 0.05
x11()
corrplot(corr_data_ger$r, type="upper", order="hclust", col=brewer.pal(n=8, name="RdYlBu"), p.mat = corr_data_ger$P,sig.level = 0.05, insig = "blank")


# 2) Compute the mean vectors for the variables listed above of the two datasets and comment on their similaryty/diversity 
#    (1.5 points)

### Selecting the variables from japanese dataset
names(data_jap)
vars_jap <- data_jap[c(9,4,5,7,8,10)]
names(vars_jap)

df <- data.frame(rbind(colMeans(vars_ger),colMeans(vars_jap)), row.names = c("Germany","Japan"))
df

# pv3scie enjoy_science time_homework    anxtest       ESCS    motivat
# Germany 540.7008    -0.1246586      10.56786 -0.3073166  0.2828459 -0.3751927
# Japan   554.5484    -0.2351844      13.43900  0.3403328 -0.1357461 -0.4469364

# pv3scie: score of the PISA test of the student in science
# For Japan the mean is slightly higher.

# enjoy_science: index for how much the student enjoys science
# The mean is negative for both Germany and Japan, Japan mean value for "enjoy_science" is slightly lower than Germany's.

# time_homework: number of hours for homework of the student per week
# Japanese students spend more hours/week doing homeworks on average.

# anxtest: index of how much the student is anxious at school
# The mean value is negative for Germany, while for Japan it is positive.

# ESCS: socio-economic index of the student
# The mean value is negative for Japan, while for Germany it is positive.

# motivat: index of how much the student is self-motivated at school
# Both negative, Japan more negative value.

# 3) Look at the graph obtained from the code

x11()
plot(data_ger$ESCS, data_ger$pv3scie, main="Scatterplot outcome vs ESCS", xlab="ESCS", ylab="math score")

#   Will be the correlation among the two variables high or low? Positive or negative? Why?
#   (2 points)

# The correlation between these two variables is positive:
# as ESCS values increase, pv3scie increases as well.
# The correlation doesn't seem to be too high, around 0.4,
# because the points are not distributed like a completely disordered cloud,
# but in an elliptical shape.



# 4) Where the homewrok time of students is more efficient? (i.e., the correlation among working time and results is higher)
#    (2 points)

# German students
rcorr(as.matrix(cbind(data_ger$time_homework, data_ger$pv3scie)))
plot(data_ger$time_homework, data_ger$pv3scie, main="German students: Homework time vs Results in math", xlab="Homework time - time_homework", ylab="Results in math - pv3scie")

# Correlation between homework time and results in math is -0.2 in Germany.

# Japanese students
rcorr(as.matrix(cbind(data_jap$time_homework, data_jap$pv3scie)))
plot(data_jap$time_homework, data_jap$pv3scie, main="Japanese students: Homework time vs Results in math", xlab="Homework time - time_homework", ylab="Results in math - pv3scie")

# Correlation between homework time and results in math is 0.07 in Japan.

# In Japan homework time is more efficient than in Germany.


# 5) Which kind of effect does the aniety produces on German students' achievements?
#     (2 points)


rcorr(as.matrix(cbind(data_ger$anxtest, data_ger$pv3scie)))
plot(data_ger$anxtest, data_ger$pv3scie, main="German students: Anxiety vs Results in math", xlab="Anxiety", ylab="Results in math")
# There is a negative correlation (-0.22) between anxiety and results in math.
# The higher the level of anxiety, the lower the results in math.


