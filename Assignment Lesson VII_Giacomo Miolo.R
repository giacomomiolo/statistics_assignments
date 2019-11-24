
##############################################
#                                            #
#        ASSIGNMENT LESSON VII - PCA         #
#                                            #
##############################################

# Note: - Write your code under each point, togeter with results (if needed).
#       - Rename this file as "Assignment Lesson VII_name surname.r"


library(MASS)

# The Boston data frame has 506 rows and 14 columns.
# Details on variables can be obtained as follows:

help(Boston)

dim(Boston)
head(Boston)


# 1) Perform a dimensionality reduction of the dataset through a principal component analysis and interpret the obtained components
#    (10 points)

# Data Exploration

x11()
boxplot(Boston, main = "Boxplots of original dataset")

# "tax" and "age" have a large variability => need to scale, otherwise tax will be used to explain to vast majority of the variance.

boston_sd <- scale(Boston)
boston_sd <- data.frame(boston_sd)

x11()
boxplot(boston_sd, main = "Boxplots of scaled dataset")

# Confront:
x11()
layout(matrix(c(1,2),2))
boxplot(Boston, main = "Boxplots of original dataset")
boxplot(boston_sd, main = "Boxplots of scaled dataset")

# PC
pc <- princomp(boston_sd, scores = TRUE)
summary(pc)
x11()
plot(pc)
names(pc)

# Importance of components:
#                        Comp.1    Comp.2     Comp.3     Comp.4     Comp.5     Comp.6     Comp.7     Comp.8
# Standard deviation     2.5559837 1.2830713 1.16027587 0.94063160 0.92153015 0.81160730 0.73099431 0.63425546
# Proportion of Variance 0.4675707 0.1178237 0.09635042 0.06332428 0.06077853 0.04714363 0.03824363 0.02879118
# Cumulative Proportion  0.4675707 0.5853944 0.68174481 0.74506909 0.80584762 0.85299125 0.89123488 0.92002606
#                        Comp.9    Comp.10   Comp.11    Comp.12     Comp.13     Comp.14
# Standard deviation     0.52603767 0.50175582 0.4608359 0.42734747 0.365711437 0.245372035
# Proportion of Variance 0.01980454 0.01801839 0.0151993 0.01307054 0.009572121 0.004309047
# Cumulative Proportion  0.93983060 0.95784899 0.9730483 0.98611883 0.995690953 1.000000000

x11()
layout(matrix(c(2,3,1,3),2,byrow=T))

# Explained Variance
barplot(sapply(boston_sd,sd), las = 2, main = "Original variables", ylim = c(0,7), ylab = "Variances")

# As a consequence of the standardization of the variables, all the vars now have a single standard deviation.

plot(pc, las = 2, main ="Principal components", ylim = c(0,7) )

# Cumulative sum of the explained variance
plot(cumsum(pc$sdev^2)/sum(pc$sdev^2), type="b", axes=F, xlab="Number of components", ylab="Contribution to the total variance", ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(boston_sd),labels=1:ncol(boston_sd),las=2)


# Principal Components are new variables (which are linear composites of the original variables).
# The first Principal Component explains 46.757% of the total variability.
# In order to explain 70% of the total variability, we need to consider the fist 4 Principal Components.
# In order to explain 80% of the total variability, we need to consider the fist 5 Principal Components.
# In order to explain 90% of the total variability, we need to consider the fist 8 Principal Components.

# Loadings
loading <- pc$loadings
loading

# Visualization of the first 5 PCs Loadings
x11()
par(mar = c(2,2,2,1), mfrow=c(5,1))

for (i in 1:5)
  barplot(loading[,i], ylim = c(-1,1), main = paste("Loadings PC ",i,sep = ""))

# Loadings interpretation:
# The loadings are the weights.
# Their sums of the squares within each component are the eigenvalues (SS loadings row in the output).
# They indicate the positive or negative correlation (sign of the loading) of the variable with the respective PC.
# Ex. loading of indus 0.332 for PC1: high values of indus => high values of PC1.
# Ex. loading of rm -0.434 for PC2: high values of rm => low values of PC2.
# The missing values (like "chas" variable in PC1) indicate the loading is 0 or almost 0 (-0.005027133 for "chas" in PC1).


# Scores
scores <- pc$scores
scores <- data.frame(scores)
scores

x11()
layout(matrix(c(1,2),2))
boxplot(boston_sd, las = 2, col = "blue", main = "Original variables")
boxplot(scores, las = 2, col = "green", main = "Principal components")


# Plot
library(ggfortify)
x11()
autoplot(prcomp(boston_sd), data = boston_sd,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


