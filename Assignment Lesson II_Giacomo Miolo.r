##############################################################################
#                                                                            #
#                  ASSIGNMENT LESSON II - DESCRIPTIVES (I)                   #
#                                                                            #
##############################################################################

# The dataset "temperature.txt" contains n=130 measurements of temperature and cardiac frequency of patients, 
# together with the corresponding gender of the patient (U=man, F=women). 

setwd("/Users/giacomomiolo/Desktop/BABD/Statistics/Ieva/Dataset for assignments/Giacomo_Miolo_Assignment_2")
data <- read.table("temperature.txt", header=TRUE)
dim(data)
head(data)

attach(data)


# 1) Give suitable representation of the variables "Temperature" and "Cardiac Frequency"  (1.5 points)

summary(Temperature)

x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,6))
par(mar=c(0, 4, 1.1, 2.1))
boxplot(Temperature,
        horizontal = TRUE,
        col = "#a72222",
        outcol="#a72222")
par(mar=c(4, 4, 1.1, 2.1))
hist(Temperature,
     breaks = "Sturges", # Default
     probability = TRUE,
     main = "",
     col = "#a72222",
     xlab = "Temperature °F",
     ylab = "Density"
)

summary(CardFreq)

x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,6))
par(mar=c(0, 4, 1.1, 2.1))
boxplot(CardFreq,
        horizontal = TRUE,
        col = "#44658a",
        outcol="#44658a")
par(mar=c(4, 4, 1.1, 2.1))
hist(CardFreq,
     breaks = "Sturges", # Default
     probability = TRUE,
     main = "",
     col = "#44658a",
     xlab = "Cardiac Frequency Values",
     ylab = "Density"
)


graphics.off()

# 2) Modify the number of classes for the variable Temperature in order to have only 4 classes for the entire range. Is it a better or worse representation of the variable? Why?   (1.5 points)
x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,1))
par(mar=c(4, 4, 2, 2))
hist(Temperature,
     probability = TRUE,
     main = "Default Number of classes",
     col = "#a72222",
     xlab = "Temperature °F",
     ylab = "Density"
)
par(mar=c(4, 4, 2, 2))
hist(Temperature,
     breaks = seq(min(Temperature), max(Temperature), length.out = 4+1),
     probability = TRUE,
     main = "4 classes",
     col = "#a72222",
     xlab = "Temperature °F",
     ylab = "Density"
)


# It's a worse representation because it distors the simmetry of the distribution,
# making it appear more centered.
# It also over-represents the values of the temperature over 99 °F.


# 3) Buid the frequency table for the variable Cardiac Frequency (4 points)

# Absolute Frequencies
h <- hist(CardFreq)
abs_cardfreq <- h$counts
abs_cardfreq
tot_obs <- sum(abs_cardfreq) # 130
tot_obs

# Cumulative Frequencies
cumulf_cardfreq <- cumsum(abs_cardfreq)
cumulf_cardfreq

# Relative Frequencies
relf_cardfreq <- abs_cardfreq/tot_obs
relf_cardfreq

# Cumulative Relative Frequencies
cumulrelf_cardfreq <- cumsum(relf_cardfreq)
cumulrelf_cardfreq

# Density
dens_cardfreq <- h$density
dens_cardfreq

# Intervals
ints_table <- table(cut(CardFreq, breaks = h$breaks))
ints_table
df_ints <- data.frame(ints_table)
names(df_ints)[1] <- "Intervals"
df_ints

# Frequency Table

df <- data.frame("Intervals" = df_ints[1],
                 "AbsFreq" = abs_cardfreq,
                 "RelFreq" = round(relf_cardfreq,3),
                 "CumulAbsFreq" = cumulf_cardfreq,
                 "CumulRelFreq" = round(cumulrelf_cardfreq,3),
                 "Density" = round(dens_cardfreq,3))
df

# Intervals AbsFreq RelFreq CumulAbsFreq CumulRelFreq Density
# 1   (55,60]       4   0.031            4        0.031   0.006
# 2   (60,65]      14   0.108           18        0.138   0.022
# 3   (65,70]      25   0.192           43        0.331   0.038
# 4   (70,75]      32   0.246           75        0.577   0.049
# 5   (75,80]      31   0.238          106        0.815   0.048
# 6   (80,85]      19   0.146          125        0.962   0.029
# 7   (85,90]       5   0.038          130        1.000   0.008

# 4) Provide a tabular description of variable "Gender"   (0.5 points)

gen <- factor(Gender)
gen
df_gen <- data.frame(table(gen))
df_gen["RelFreq"] <- table(gen)/length(gen)
df_gen

# gen Freq RelFreq
# 1   D   65     0.5
# 2   U   65     0.5

# 5) Compare graphically Temperatures and Cardiac Frequencies of Men and Women (2.5 points)

x11()
boxplot(Temperature ~ gen,
        col = c("#d063c9","#6081ec"),
        outcol = c("#d063c9","#6081ec"),
        xlab = "Gender",
        ylab = "Temperature °F",
        main = "Boxplot Temperature D vs U")

# Women's median temperature value is higher than men's.
# There are 4 outliers for women's temperature, there aren't outliers for men's temperature.
# Men's temperature has a slightly higher variability.

graphics.off()
detach(data)

