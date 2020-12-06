###################################################################
#                      DATA EXPLORATION                           #
###################################################################
#DATA Working directory
setwd("C:/Users/Alvaro/Desktop/PRDL/OSA/DATA")

#Libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(DataCombine)
library(arsenal)
library(sjmisc)
library(visdat)

#Load OSA messy data
df <- read_excel("Info_BDApnea_QuironMalaga.xlsx", 
                sheet = 1)

#View its class
class(df)

#view dimensions (rows x columns)
dim(df)

#look columns names
names(df)

#structure
str(df)

#structure but with dplyr
glimpse(df)

#Summary
summary(df)

vis_dat(df)

#Representing some numeric data
hist(df$IAH)
hist(df$HiperT)

###################################################################
#                       DATA CLEANSING                            #
###################################################################

#Only columns that matter
df1 <- df %>% select(Patient, Gender, IAH, Peso, Talla, Edad, PerCervical, Fumador, Roncador)

#Renaming
df1 <- df1 %>% 
  rename(
    Height = Talla,
    Weight = Peso,
    Age = Edad,
    Cervical = PerCervical,
    Smoker = Fumador,
    Snorer = Roncador
  )

summary(df1)

#Change non numeric values in numeric columns
df1$Weight <- as.numeric(df1$Weight)
#Changing -1 for NA
df1$Height[df1$Height == -1] <- NA 
df1$Cervical[df1$Cervical == -1] <- NA
df1$Weight[df1$Weight == -1] <- NA
df1$Gender[!(tolower(df1$Gender)=="hombre" | tolower(df1$Gender)=="mujer")] <- NA
df1$Smoker[df1$Smoker=="poco" | df1$Smoker=="si (poco)"] <- "si"
df1$Snorer[df1$Snorer=="no con CPAD"] <- "CPAP"
df1$Snorer[df1$Snorer=="poco"] <- "si"
df1$Snorer[grepl("si",df1$Snorer)] <- "si"
df1$Age[df1$Age == -1] <- NA
#Factor Gender, Smoker and Snorer
df1$Gender = factor(df1$Gender)
df1$Smoker = factor(df1$Smoker)
df1$Snorer = factor(df1$Snorer)

summary(df1)
vis_dat(df1)


#DropNA
df2 <- DropNA(df1,Var = "IAH")
df2 <- DropNA(df2,Var = "Age")
df3 <- DropNA(df1)
summary(df2)
vis_dat(df2)
df2 <- df2[-c(3, 373), ]
#See indexes with NA remaining in missing features
cervicalNA<-which(df2$Cervical %in% c(NA))
df2$Gender[cervicalNA]
heightNA<-which(df2$Height %in% c(NA))
WeightNA<-which(df2$Weight %in% c(NA))
df2$Gender[WeightNA]

#Cervical men model
df_cervical <- DropNA(df2)
df_cervical_men <- filter(df_cervical,df_cervical$Gender=="hombre")
attach(df_cervical_men)
peso<-df_cervical_men$Weight
altura<-df_cervical_men$Height
lm.fit=lm(df_cervical_men$Cervical ~ df_cervical_men$Weight+df_cervical_men$Height)

for(val in cervicalNA){
  newdata <- data.frame(peso=df2$Weight[val],altura=df2$Height[val])
  df2$Cervical[val]<-predict(lm.fit,newdata=newdata)
  print(val)
  print(df2$Cervical[val])
}
summary(df2)

#Cervical Weight model
df_weight <- DropNA(df2)
df_weight_women <- filter(df_weight,df_weight$Gender=="mujer")
#attach(df_cervical_men)
cervicall<-df_weight_women$Cervical
altura<-df_weight_women$Height
lm.fit=lm(df_weight_women$Weight ~ cervicall+altura)

for(val in WeightNA){
  newdata <- data.frame(cervicall=df2$Cervical[val],altura=df2$Height[val])
  print(newdata)
  df2$Weight[val]<-predict(lm.fit,newdata=newdata)
  print(val)
  print(df2$Weight[val])
}
summary(df2)
vis_dat(df2)

##NA IMPUTATION WITH MICE
library(mice)
# Imputing the values using mice
imputed_df2 <- mice(df2, m=5, method = 'pmm', seed = 101)
# Building regression model
model_fit <- with(data = imputed_df2, exp = lm(Cervical ~ IAH + Weight + Height + Age)) 
summary(model_fit)
# combining results of all 5 models using pool() function
pooled_output <- pool(model_fit)
summary(pooled_output)
pooled_lm = model_fit$analyses[[1]]
pooled_lm$coefficients = summary(pooled_output)$estimate
imputed_df2 <- df2
for(val in cervicalNA){
  print(coefs[1]+coefs[2]*imputed_df2$IAH[val]+coefs[3]*imputed_df2$Weight[val]+coefs[4]*imputed_df2$Height[val]+coefs[5]*imputed_df2$Age[val])
  imputed_df2$Cervical[val]<-coefs[1]+coefs[2]*imputed_df2$IAH[val]+coefs[3]*imputed_df2$Weight[val]+coefs[4]*imputed_df2$Height[val]+coefs[5]*imputed_df2$Age[val]
}

imputed_df2[cervicalNA,]
summary(imputed_df2)


for(val in cervicalNA){
  #newdata <- data.frame(IAH= imputed_df2$IAH[val],Weight=imputed_df2$Weight[val],Height= imputed_df2$Height[val],Age= imputed_df2$Age[val])
  imputed_df2$Cervical[val]<-predict(pooled_output,IAH= imputed_df2$IAH[val],Weight=imputed_df2$Weight[val],Height= imputed_df2$Height[val],Age= imputed_df2$Age[val])
  print(val)
  print(df2$Cervical[val])
}

#NA imputation with knn
df_knn_men <- filter(df2,df2$Gender=="hombre")
df_knn_men <- df_knn_men %>% select(IAH, Weight, Height, Age, Cervical)
summary(df_knn_men)
cervicalNA<-which(df2$Cervical %in% c(NA))
knn_cervicalNA<-which(df_knn_men$Cervical %in% c(NA))
knn_cervicalNA

library(bnstruct)
df_knn_imputed <- knn.impute(as.matrix(df_knn_men), k = 5, cat.var = c(), to.impute = c(2,5),
           using = 1:nrow(df_knn_men))
summary(df_knn_imputed)
df_knn_imputed[knn_cervicalNA,5]
df2_knn_imputed <- df2
df2_knn_imputed$Cervical[cervicalNA] <- df_knn_imputed[knn_cervicalNA,5]
summary(df2)
summary(df2_knn_imputed)
###########
library(varhandle)
df_knn_women <- filter(df2,df2$Gender=="mujer")
df_knn_women$Smoker <- unfactor(df_knn_women$Smoker)
df_knn_women$Smoker[df_knn_women$Smoker=="antiguo"]<- "si_antes"
df_knn_women$Smoker[df_knn_women$Smoker=="si"]<- "si_despues"
df_knn_women$Smoker <- as.numeric(factor(df_knn_women$Smoker))
df_knn_women <- df_knn_women %>% select(IAH, Weight, Height, Age, Cervical, Smoker)
summary(df_knn_women)
summary(df_knn_women)
weightNA<-which(df2$Weight %in% c(NA))
knn_weighNA<-which(df_knn_women$Weight %in% c(NA))
knn_weighNA

library(bnstruct)
df_knn_imputed <- knn.impute(as.matrix(df_knn_women), k = 5, cat.var = 1:ncol(df_knn_women), to.impute = 1:nrow(df_knn_women),
                             using = 1:nrow(df_knn_women))
summary(df_knn_imputed)
df_knn_imputed[knn_weighNA,2]
df2_knn_imputed$Weight[weightNA] <- df_knn_imputed[knn_weighNA,2]
summary(df2)
summary(df2_knn_imputed)

#IMPUTATION WITH RANDOM FOREST
library(caret)
df2_forest <- select(df2,Gender,IAH,Weight,Height,Age,Cervical,Smoker,Snorer)
model.rf <- train(Cervical ~ Age+Smoker+Snorer+Weight+Height, data = df3, method = "rf")
imputed_df2 <- df2
for(val in cervicalNA){
  print(predict(model.rf,Age=df2$Age[val],Smoker=df2$Smoker[val],Snorer=df2$Snorer[val],Weight=df2$Weight[val],Height=df2$Height[val]))
  imputed_df2$Cervical[val]<-predict(model.rf,Age=df2$Age[val],Smoker=df2$Smoker[val],Snorer=df2$Snorer[val],Weight=df2$Weight[val],Height=df2$Height[val])
}
library(randomForest)
df2<-select(df2,-Patient)
imputed_df2 <- rfImpute(IAH ~ ., df2)
imputed_df2$Cervical[cervicalNA]


cols <- character(nrow(df2))
cols[] <- 'blue'
cols[df2$IAH >=30] <- "red"
cols[df2$IAH <30 && df2$IAH>=10] <- "blue"
cols[df2$IAH<10] <- "green"

library(psych)
attach(df2)
pairs(~ IAH + Gender + Weight + Height + Cervical + Age + Smoker + Snorer,col=cols,lower.panel=NULL,cex.labels=2)
par(xpd=TRUE)
legend(x = c(0.1, 0.6), y = c(0.3, 0), c("Healthy","Mild","Severe"),  
       fill=c("green", "blue", "red"))

#legend(0,1,legend=c("Healthy","Mild","Severe"),fill=c("green","blue","red"))
library(corrplot)


df2$Gender<-as.numeric(df2$Gender)
df2$Smoker<-as.numeric(df2$Smoker)
df2$Snorer<-as.numeric(df2$Snorer)
M <- cor(subset(df2, select = - Patient))
corrplot(M, method="number")
corrplot(M, method="circle")




install.packages("devtools")
library(devtools)
install_github("https://github.com/kassambara/easyGgplot2", "kassambara")
library(easyGgplot2)


#df2$state[df2$IAH<30] <- "healthy" 
#df2$state[df2$IAH>=30] <- "severe" 
healthy <-subset(df2, df2$IAH<30)
severe <-subset(df2, df2$IAH>=30)
par(mfrow=c(1,2))
hist(healthy$IAH, col="blue")
hist(severe$IAH, col="red")
par(mfrow=c(1,2))
hist(healthy$Weight, col="blue")
hist(severe$Weight, col="red")
par(mfrow=c(1,2))
hist(healthy$Height, col="blue")
hist(severe$Height, col="red")
par(mfrow=c(1,2))
hist(healthy$Cervical, col="blue")
hist(severe$Cervical, col="red")

BMI <- df2$Weight / ((df2$Height/100)^2)
df2$BMI <- BMI

summary(df2)
vis_dat(df2)


attach(df2)
library(corrplot)
M <- cor(df2)






#If all OK save into student's file
write_xlsx(
  df2,
  path = "OSA_DB_UPM_Alvaro_Gutierrez_lm.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)

