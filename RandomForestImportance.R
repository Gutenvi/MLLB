rm(list=ls())

Input_file <- "OSA_extreme_both.xlsx"

Data_Directory <- "C:\\Users\\Alvaro\\Desktop\\PRDL\\OSA\\DATA\\"

# Using readxl package to read an Excel file
# Install the readxl package is nor already installed

library(readxl)

df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))
summary(df_OSA)
df_OSA$OSA = factor(df_OSA$OSA)

library(randomForest)
attach(df_OSA)

model = randomForest(OSA ~ Gender + Weight + Height + Age + Cervical + Smoker + Snorer + BMI)
model$importance
model$importanceSD
varImpPlot(model, main="Feature Importance")

importanceOrder=order(-model$importance)
names=rownames(model$importance)[importanceOrder][1:8]
par(mfrow=c(2, 4), xpd=NA)
par(mar=c(1,1,1,1))
for (name in names)
  partialPlot(model, as.data.frame(df_OSA), eval(name), main=name, xlab=name)
              
              