rm(list=ls())

Input_file <- "OSA_DB_Classification.xlsx"

Data_Directory <- "C:\\Users\\Alvaro\\Desktop\\PRDL\\OSA\\DATA\\"

# Using readxl package to read an Excel file
# Install the readxl package is nor already installed

library(readxl)

df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))

summary(df_OSA)

df_OSA$OSA <- rep("Healthy",642)
df_OSA$OSA[df_OSA$IAH>10]<-"Normal"
df_OSA$OSA[df_OSA$IAH>30]<-"Severe"

# Define OSA column as a factor for being used be
# classification models
df_OSA$OSA = factor(df_OSA$OSA)

summary(df_OSA)

df_OSA <- subset(df_OSA,select=-IAH)


# Loading package 
library(e1071) 
library(caTools) 
library(caret) 
# Splitting data into train 
# and test data 
split <- sample.split(df_OSA$Gender, SplitRatio = 0.8) 
train_cl <- subset(df_OSA, split == "TRUE") 
test_cl <- subset(df_OSA, split == "FALSE") 
# Feature Scaling 
train_scale <- scale(train_cl[, 1:9]) 
test_scale <- scale(test_cl[, 1:9]) 
# Fitting Naive Bayes Model  
# to training dataset 
set.seed(120)  # Setting Seed 
classifier_cl <- naiveBayes(OSA ~ Gender+Weight+Height+Age+Cervical+Smoker+Snorer+BMI, data = train_cl) 
classifier_cl

# Predicting on test data' 
y_pred <- predict(classifier_cl, newdata = test_cl) 

# Confusion Matrix 
cm <- table(test_cl$OSA, y_pred) 
cm 

# Model Evauation 
confusionMatrix(cm)


library(pROC)
bayesPredict <- as.numeric(predict(classifier_cl,newdata = test_cl))
bayesROC <- multiclass.roc(test_cl$OSA,bayesPredict)
bayesROC
rs <- bayesROC[['rocs']]
plot.roc(rs[[1]],xlim=c(1,0),col="red")
colors=c("red","green","black")
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=colors[i]))
legend(0.2,0.8, legend = c("Healthy","Normal","Severe"),
       lty = c(1,1,1), col = c("green","black","red"))

#Confusion Matrix
predictions <- predict(classifier_cl, df_OSA)
# summarize results
confusionMatrix(predictions, df_OSA$OSA)

