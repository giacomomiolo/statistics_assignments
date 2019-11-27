####################################################################
#                                                                  #
#        ASSIGNMENT LESSON VI - MULTIVARIATE DATA - Depths         #
#                                                                  #
####################################################################

# Note: - Write your code under each point, togeter with results (if needed).
#       - Rename this file as "Assignment Lesson VI_name surname.r"

# The dataset "invalsi data.txt" contains the score in science ad reading of 
# n=1000 Italian students who underwent the INVALSI evaluation at grade 6 of primary school.
# The legend is reported in the "read me - INVALSI variable" file.

setwd("/Users/giacomomiolo/Desktop/BABD/Statistics/Ieva/Dataset for assignments/INVALSI")
data <- read.table("invalsi data.txt", header=TRUE)

colnames(data)[10:13] <- c("math score","math score 5", "reading score", "reading score 5")

dim(data)
names(data)



# 1) Select variables "math score", "math score 5", "reading score", "reading score 5" from the dataset.
#    Plot the variables.
#    (1 point)

math_score <- data[,10] # Math score grade 6
math_score_5 <- data[,11] # Math score grade 5
reading_score <- data[,12] # Reading score grade 6
reading_score_5 <- data[,13] # Reading score grade 5

all_vars <- data[10:13]

# Math Score
x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0, 4, 1, 2))
boxplot(math_score,
        horizontal = TRUE,
        main = "Boxplot and Histogram of Math Score")
par(mar=c(4, 4, 1, 2))
hist(math_score,
     prob = TRUE,
     xlab = "Math Score Grade 6",
     main = "")

# Math Score 5
x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0, 4, 1, 2))
boxplot(math_score_5,
        horizontal = TRUE,
        main = "Boxplot and Histogram of Math Score 5")
par(mar=c(4, 4, 1, 2))
hist(math_score_5,
     prob = TRUE,
     xlab = "Math Score Grade 5",
     main = "")

# Reading Score
x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0, 4, 1, 2))
boxplot(reading_score,
        horizontal = TRUE,
        main = "Boxplot and Histogram of Reading Score")
par(mar=c(4, 4, 1, 2))
hist(reading_score,
     prob = TRUE,
     xlab = "Reading Score Grade 6",
     main = "")

# Reading Score 5
x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
par(mar=c(0, 4, 1, 2))
boxplot(reading_score_5,
        horizontal = TRUE,
        main = "Boxplot and Histogram of Reading Score 5")
par(mar=c(4, 4, 1, 2))
hist(reading_score_5,
     prob = TRUE,
     xlab = "Reading Score Grade 5",
     main = "")

x11()
plot(all_vars)

# 2) Do you expect a child who is good at math to be good also in reading?
#    (1.5 points)

# Graphical Interpretation

# Yes,
# Because from the scatterplot of math score vs reading score and of math score 5 vs reading score 5
# it can be see that at the increase of one, the other also increases.
# It seems a correlation higher than 0.5.

# Numerical
round(cor(all_vars),3)

# math score math score 5 reading score reading score 5
# math score           1.000        0.537         0.594           0.445
# math score 5         0.537        1.000         0.436           0.646
# reading score        0.594        0.436         1.000           0.503
# reading score 5      0.445        0.646         0.503           1.000

# Correlation between math score and reading score: 0.594
# Correlation between math score 5 and reading score 5: 0.646


# 3) Use the hexbin package to plot the graph related to "math score" and "reading score".
#    Note: use from 5 to 20 bins
#    (2.5 points)

library(hexbin)

bin <- hexbin(c(math_score, reading_score), xbins = 13, xlab = "Math Score", ylab = "Reading Score")
plot(bin, main = 'Hexagonal binning')


# 4) Which is the deepest point? Which is the depth of point (mean(math score),mean(reading score))?
#    Give interpretation of results.
#    (5 points)

library(depth)

df <- data.frame(math_score, reading_score)

## Median
deepest <- med(df, approx = TRUE)
deepest

# median
# 47.85598 66.33443
# depth
# 0.456

## Mean
s_mean <- apply(df,2,mean)
depth(s_mean, df) # 0.44

x11()
plot(df, main = "Math score vs Reading score")
points(deepest$median[1], deepest$median[2], pch=19, col="red")
points(s_mean[1],s_mean[2], pch=19, col="orange")
legend(5,95, legend = c("Deepest point","Mean point"), col = c("red","orange"), lty=1:1, cex=0.9)


# Depth with median: 0.456
# Depth with mean: 0.44

# The deepest point, found through the Tukey method, is the one with maximum depth, the most central one.
# Most of the data is concentrated in the middle area of the scatterplot, with the presence of very few outliers.
# Mean and median are similar.

