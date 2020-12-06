########################################
# OSA data analysis using:
#         - linear regression models
#

categorical_encode <- function(data) {
  return_data <- data.frame(data)
  return_data$data <- unfactor(return_data$data)
  new_data <- data.frame(data)
  table <- table(data)
  data <- data.frame(data)
  frequencies <- prop.table(table)
  table <- data.frame(table)
  table$freq <- frequencies
  data_sorted <- table[order(table$freq),]
  num_class <- nrow(table)
  encoding <- 1:num_class
  for(i in 1:nrow(data)){
    for(j in 1:num_class){
      #print(new_data[i,1])
      #print(data_sorted$data[j])
      #print(new_data[i,1]==data_sorted$data[j])
      if(new_data[i,1]==data_sorted$data[j]){
        return_data[i,1]<- as.numeric(j)
        break
      }
    }
  }
  return(as.numeric(return_data$data))
}

Input_file <- "OSA_DB_UPM_Alvaro_Gutierrez.xlsx"

Data_Directory <- "C:/Users/Alvaro/Desktop/PRDL/OSA/DATA/"

# Using readxl package to read an Excel file
# Install the readxl package is nor already installed

library(readxl)
library(dplyr)
library(varhandle)

df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))

df_OSA = as.data.frame(df_OSA)
names(df_OSA)
df_OSA <- select(df_OSA, -Patient)
dim(df_OSA)

## Describe the Database

# First define Gender as a factor!
library(DataCombine)

df_OSA$Gender <- factor(df_OSA$Gender)
df_OSA$Smoker <- factor(df_OSA$Smoker)
df_OSA$Snorer <- factor(df_OSA$Snorer)
df_OSA$BMI <- df_OSA$Weight / ((df_OSA$Height/100)^2)
summary(df_OSA)
df_OSA$Smoker[df_OSA$Smoker=='ns']<-0
df_OSA$Smoker[df_OSA$Smoker=='no']<-1
df_OSA$Smoker[df_OSA$Smoker=='antiguo']<-2
df_OSA$Smoker[df_OSA$Smoker=='si']<-3

df_OSA$Snorer[df_OSA$Snorer=='ns']<-0
df_OSA$Snorer[df_OSA$Snorer=='no']<-1
df_OSA$Snorer[df_OSA$Snorer=='CPAP']<-2
df_OSA$Snorer[df_OSA$Snorer=='si']<-3

df_OSA$Smoker<- as.numeric(df_OSA$Smoker)
df_OSA$Snorer<- as.numeric(df_OSA$Snorer)

summary(df_OSA)
df_OSA$Smoker<-categorical_encode(df_OSA$Smoker)
df_OSA$Gender<-categorical_encode(df_OSA$Gender)
df_OSA$Snorer<-categorical_encode(df_OSA$Snorer)
#df_OSA$Weight <- as.numeric(df_OSA$Weight)
summary(df_OSA)
library(writexl)

write_xlsx(df_OSA,"C:\\Users\\Alvaro\\Desktop\\PRDL\\OSA\\DATA\\OSA_DB_Classification.xlsx")

#df_OSA <- DropNA(df_OSA,Var = "Weight")
# See relations between variables
attach(df_OSA)
pairs(~ IAH + Gender + Weight + Height + Cervical + Age + Smoker + Snorer + BMI)

## PLOT Correlation Matrix

# FIRST
# install corrplot and then load it
library(corrplot)
# back to as.numeric for including it..

df_OSA_C=df_OSA

#df_OSA_C$Gender = as.numeric(df_OSA_C$Gender)
#df_OSA_C$Smoker = as.numeric(df_OSA_C$Smoker)
#df_OSA_C$Snorer = as.numeric(df_OSA_C$Snorer)
M <- cor(subset(df_OSA_C))
corrplot(M, method="number")
corrplot(M, method="circle")
corrplot.mixed(M)

# Study the use of Simple and Multiple LR models
EF<- Weight*Cervical
EF1<- (EF)^2

lm.fit.BMI=lm(IAH ~ BMI)
plot(BMI,IAH, col = ifelse(IAH>30,'red','green'))
abline(lm.fit.BMI, col='blue')
summary(lm.fit.BMI)

lm.fit.Cervical=lm(IAH ~ Cervical)
plot(Cervical,IAH, col = ifelse(IAH>30,'red','green'))
abline(lm.fit.Cervical, col='blue')
summary(lm.fit.Cervical)

lm.fit.EF=lm(IAH ~ EF)
plot(EF,IAH, col = ifelse(IAH>30,'red','green'))
abline(lm.fit.EF, col='blue')
summary(lm.fit.EF)

lm.fit.EF=lm(IAH ~ EF)
plot(EF,IAH, col = ifelse(IAH>30,'red','green'))
abline(lm.fit.EF, col='blue')
summary(lm.fit.EF)

library(AICcmodavg)

model.set <- list(lm.fit.BMI,lm.fit.Cervical,lm.fit.EF)
model.names <- c("BMI", "Cervical", "EF")
aictab(model.set, modnames = model.names)

library(lmtest)
anova(lm.fit.BMI,lm.fit.Cervical,lm.fit.EF)
lrtest(lm.fit.BMI,lm.fit.Cervical,lm.fit.EF)

EF1<- EF*Age

lm.fit=lm(IAH~Cervical+BMI)
summary(lm.fit)

lm.fit.Snorer=lm(IAH~EF+Age+Snorer)
summary(lm.fit.Snorer)

library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# Train the model
model <- train(IAH ~ Cervical+BMI, data = df_OSA, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

df_OSA$EF<-EF
# Train the model
model.EF <- train(IAH ~ EF+Age+Snorer, data = df_OSA, method = "lm",
               trControl = train.control)
# Summarize the results
print(model.EF)


model.set <- list(lm.fit.BMI,lm.fit.Cervical,lm.fit.EF,lm.fit)
model.names <- c("BMI", "Cervical", "EF","Multiple")

aictab(model.set, modnames = model.names)

# Study independently male and female populations

### Male population
summary(df_OSA_C)
df_OS$EF<-EF
df_OSA_male=subset(df_OSA, Gender==2)

# Another way
# df_OSA_male = df_OSA_C[df_OSA_C$Gender == 1, ]

names(df_OSA_male)
attach(df_OSA_male)

df_OSA_male$EF<-df_OSA_male$Cervical*df_OSA_male$BMI

train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# Train the model
model.male.knn <- train(IAH ~ EF+Age, data = df_OSA_male, method = "knn",
                  trControl = train.control,tuneLength = 20)
model.male.knn
plot(model.male.knn,lwd=5)
model.male.rf <- train(IAH ~ EF+Age+Cervical+Smoker+Snorer+Weight+Height, data = df_OSA_male, method = "rf",
                        trControl = train.control,tuneLength = 20)
model.male.rf
plot(model.male.rf,lwd=5)

############ Female population ################

df_OSA_female=subset(df_OSA_C, Gender==1)

# Another way
# df_OSA_female = df_OSA_C[df_OSA_C$Gender == 2, ]

names(df_OSA_female)
attach(df_OSA_female)

df_OSA_female$EF<-df_OSA_female$Cervical*df_OSA_female$BMI

train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# Train the model
library(UsingR)
model.female.knn <- train(IAH ~ EF+Age+Cervical+Smoker+Snorer+Weight+Height, data = df_OSA_female, method = "knn",
                        trControl = train.control,tuneLength = 20)
model.female.knn
plot(model.female.knn,lwd=5)
model.female.rf <- train(IAH ~ EF+Age+Cervical+Smoker+Snorer+Weight+Height, data = df_OSA_female, method = "rf",
                       trControl = train.control,tuneLength = 20)
model.female.rf
plot(model.female.rf,lwd=5)

########################################
########################################
####   NEXT STEPS:
####
####  You can try:
####        - Regularization
####        - Feature selection
####        - other Regression models
####
####  BUT you can wait to see Classification Script
####           and use regression with some libraries as CARET
####


####

