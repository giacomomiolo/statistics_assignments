
##############################################################################
#                                                                            #
#                  ASSIGNMENT LESSON I - INTRODUCTION TO R                   #
#                                                                            #
##############################################################################

# Note: - Write your code under each point, togeter with results (if needed).
#       - Rename this file as "Assignment Lesson I_name surname.r"



# 1) Import the "toy_HF" dataset from the folder HF, using the first row as column titles (0.5 point)

setwd("/Users/giacomomiolo/Desktop/BABD/Statistics/Ieva/Dataset for assignments/HF")
df_toyhf <- read.csv(file="toy_HF.txt", header=TRUE, sep="")

# 2) What are the sizes of the dataset? (0.5 points)

nrow(df_toyhf) # Rows 2090
length(df_toyhf) # Columns 25
dim(df_toyhf) # df shape (2090, 25)


# 3) Extract the "age", "sex", and "adm_number" columns, and give the new dataset the column's names "AGE", "SEX" and "nADM" (2 point)
# Note: once you build the new dataset, use the command "x <- as.data.frame(x)" to make it a data.frame

# Renaming using base functions
df <- data.frame(df_toyhf$age,df_toyhf$sex,df_toyhf$adm_number)

names(df)

names(df)[names(df) == "df_toyhf.age"] <- "AGE"
names(df)[names(df) == "df_toyhf.sex"] <- "SEX"
names(df)[names(df) == "df_toyhf.adm_number"] <- "nADM"

names(df)

# Renaming using column index
df <- data.frame(df_toyhf$age,df_toyhf$sex,df_toyhf$adm_number)

names(df)

names(df)[1] <- "AGE"
names(df)[2] <- "SEX"
names(df)[3] <- "nADM"

names(df)

# Renaming using dplyr library
library(dplyr)

df <- data.frame(df_toyhf$age,df_toyhf$sex,df_toyhf$adm_number)

names(df)

df <- df %>% rename(
    AGE = df_toyhf.age, # new_name = old_name
    SEX = df_toyhf.sex,
    nADM = df_toyhf.adm_number
  )

names(df)

# 4) How many patients have 1 admission? How many 2? How many 3?  (1.5 points)

sum(df$nADM == 1) # 1000
sum(df$nADM == 2) # 418
sum(df$nADM == 3) # 235


# 5) What is the percentage of readmitted patients in the dataset?   (1 point)

readmitted <- sum(df$nADM != 1) # Those admitted more than once, if there were some patients with 0 admissions -not this case- sum(df$nADM >= 2)
total <- length(df$nADM)
percent_readmitted <- readmitted/total*100
percent_readmitted # 52,15%

# 6) Select the age of patients at their first admission (1.5 points)

# Base R
head(subset(df, nADM == 1, select = AGE)) # head of the age of the patients admitted once

# dplyr library
admitted_once <- df %>% filter(nADM == 1)
head(admitted_once)
head(admitted_once %>% select(AGE)) # head of the age of the patients admitted once


# 7) Compute the mean of the AGE vector. Use help command, if needed  (0.5 points)
head(df)
dim(df)
mean(df[["AGE"]]) # 71.18 - Also mean(df[,"AGE"]) 


# 8) Compute the mean age of men and women at their first hospitalization. What is higher? (2.5 points)
# Note: men have sex=1

mean(subset(df, SEX == 1 & nADM == 1, select = AGE, TRUE)) # 66.78 Men 1st hospitalization mean age
mean(subset(df, SEX == 0 & nADM == 1, select = AGE, TRUE)) # 74.51 Women 1st hospitalization mean age
# Women have an higher average age at their first hospitalization

