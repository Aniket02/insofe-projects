rm(list=ls(all=TRUE))
eclswave7 <- read.csv("eclswave7.csv", na.strings = c("NOT APPLICABLE","REFUSED","DON'T KNOW","NOT ASCERTAINED"))
write.csv(eclswave7,"eclswave77.csv",row.names = F)
eclswave77 <- read.csv("eclswave77.csv", header = T)
str(eclswave77)
rm(eclswave7)
########################################
#PREPROCESSING OF DATA
########################################

#subet data into categorical and numerical variables
numeric_ecls7 = eclswave77[,sapply(eclswave77,is.numeric)]
cat_ecls7 = eclswave77[,sapply(eclswave77,is.factor)]

#Remove P7FSSCAL because this infromation is already given by attribute P7FSRAW
numeric_ecls7 = subset(numeric_ecls7, select = -c(P7FSSCAL))

#values less than 0,so (-1 or -7, -8 or -9) replaced by NA, except for W1SESL
numeric7_no_W8SESL = numeric_ecls7[-1]
numeric7_no_W8SESL[numeric7_no_W8SESL < 0] <- NA
numeric_ecls7 = cbind(numeric_ecls7[1],numeric7_no_W8SESL)
rm(numeric7_no_W8SESL)
#combine numerical and categorical again
eclswave7_1 <- as.data.frame(cbind(cat_ecls7,numeric_ecls7))

#calculating the sum of NA's in each column
na_count7 <-sapply(eclswave7_1, function(y) sum(length(which(is.na(y))))) 
#creating a datafreclswave17the count values, it seems like many rows have a lot of Na's)
na_count7 <- data.frame(na_count7)  

#Removing columns which have NA occuring more than 30% of the time
columns_with_highNA7 <- which(colSums(is.na(eclswave7_1)) > 0.30*nrow(eclswave7_1))
eclswave7_cl <- eclswave7_1[, -(columns_with_highNA7)]
rm(columns_with_highNA7)
#identifying the indexes of rows that have greater than 20% NA's
rows_with_highNA7 <- which(rowSums(is.na(eclswave7_cl)) > 0.20*ncol(eclswave7_cl))

#removing the identified rows
eclswave7_cl <- eclswave7_cl[-(rows_with_highNA7),]
rm(rows_with_highNA7)

summary(eclswave7_cl)
rm(cat_ecls7,numeric_ecls7)
#---KNN Imputation---#
require(DMwR)
eclswave7_cl_Imput = knnImputation(eclswave7_cl)

#Split into numeric and categorical for standardization & Correlation
numeric_w7imput = eclswave7_cl_Imput[,sapply(eclswave7_cl_Imput,is.numeric)]
cat_w7imput = eclswave7_cl_Imput[,sapply(eclswave7_cl_Imput,is.factor)]
#Correlation
corw7 <- as.data.frame(cor(numeric_w7imput))
corw7 <- as.matrix(corw7)
corw7[which(abs(corw7) > 0.85)]

#Remove a couple variables that have high correlation with other attributes
numeric_w7imput = subset(numeric_w7imput, select = -c(P7HTOTAL))
rm(corw7)
#Seperating out target attributes before standardizing  
targetnumw7_vars = subset(numeric_w7imput, select = c(C7R4MSCL,C7R4RSCL,C7R2SSCL))
targetcatw7_vars = subset(cat_w7imput, select = c(C7ANGRY, C7LIKRD,C7LIKMTH,C7ENJRD,C7ENJMTH,C7FLGOOD))
numeric_w7imput = subset(numeric_w7imput, select = -c(C7R4MSCL,C7R4RSCL,C7R2SSCL))
cat_w7imput = subset(cat_w7imput, select = -c(C7ANGRY, C7LIKRD,C7LIKMTH,C7ENJRD,C7ENJMTH,C7FLGOOD))
#Standardizing  
library(vegan)
numeric_w7imputstd <- decostand(numeric_w7imput, "range")
#Combining categorical,target, and standardized numerical variables
eclswave7_model <- cbind(cat_w7imput,numeric_w7imputstd,targetnumw7_vars,targetcatw7_vars)
rm(numeric_w7imput,cat_w7imput)
race_levels <- as.data.frame(levels(eclswave7_model$RACE))
#splitting data based upon poverty level of student
#Data of students above poverty level
eclsw7_APvrty <- eclswave7_model[eclswave7_model$W8POVRTY == "AT OR ABOVE POVERTY THRESHOLD",]

#Data of students below poverty level
eclsw7_BPvrty <- eclswave7_model[eclswave7_model$W8POVRTY == "BELOW POVERTY THRESHOLD",]


########################################
#WAVE 7 WITH STUDENTS ABOVE POVERTY 
########################################

  ###################
  #Numerical Attributes
  ###################
eclsw7_APvrty = eclsw7_APvrty[-1]
eclsw7A_MSCL = subset(eclsw7_APvrty, select=-c(C7R2SSCL,C7R4RSCL,C7ANGRY, C7LIKRD,C7LIKMTH,C7ENJRD,C7ENJMTH,C7FLGOOD)) #DATA with only Math scores as target variable
eclsw7A_RSCL = subset(eclsw7_APvrty, select=-c(C7R4MSCL,C7R2SSCL,C7ANGRY, C7LIKRD,C7LIKMTH,C7ENJRD,C7ENJMTH,C7FLGOOD))  #DATA with only Reading scores as target variable
eclsw7A_SSCL = subset(eclsw7_APvrty, select=-c(C7R4MSCL,C7R4RSCL,C7ANGRY, C7LIKRD,C7LIKMTH,C7ENJRD,C7ENJMTH,C7FLGOOD))

#### Analysis of Math Scores ####
#ATTRIBUTE SELECTION USING RANDOM FOREST# 
library(randomForest)
set.seed(1234)
fit.forest_eclsw7 <- randomForest(C7R4MSCL ~ . , data= eclsw7A_MSCL,
                                  na.action=na.roughfix,
                                  importance=TRUE, ntree = 170)

# following function determines variable importance
importance(fit.forest_eclsw7, type=2)
#Arrange in decreasing order
order_rf7 <- sort(importance(fit.forest_eclsw7, type=2)[,1], decreasing = TRUE)
order_rf7
# Top 20 Attributes
feature_stringw7 <- paste(names(order_rf7)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw7M_impAttri <- eclsw7A_MSCL[, feature_stringw7]

#Adding back the target attribute
eclsw7A_MSCL_RF <- cbind(eclsw7M_impAttri, C7R4MSCL = eclsw7A_MSCL$C7R4MSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw7_MSCL <- stepAIC(lm(C7R4MSCL ~ ., data=eclsw7A_MSCL_RF), direction="both")
summary(stepw7_MSCL)
stepw7A_M_vars <- (names(stepw7_MSCL$model))
stepw7A_M_edit <- gsub("^..",'',stepw7A_M_vars)

  # ###Lasso Regression###
  # numeric_data7_MSCL = eclsw7A_MSCL_RF[,sapply(eclsw7A_MSCL_RF,is.numeric)]
  # numeric_data7_MSCL = subset(numeric_data7_MSCL, select=-c(C7R4MSCL))
  # cat_data7_MSCL = eclsw7A_MSCL_RF[,sapply(eclsw7A_MSCL_RF,is.factor)]
  # target7_MSCL = cbind(C7R4MSCL = eclsw7A_MSCL_RF$C7R4MSCL,cat_data7_MSCL)
  # xfactors7_MSCL <- model.matrix(C7R4MSCL ~ ., data = target7_MSCL)[,-1]
  # MSCL7_data <- as.matrix(data.frame(numeric_data7_MSCL, xfactors7_MSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit7_MSCL <- glmnet(MSCL7_data, eclsw7A_MSCL_RF$C7R4MSCL, alpha=1)
  # 
  # plot(fit7_MSCL,xvar="lambda",label=TRUE)
  # plot(fit7_MSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit7_MSCL)
  # cv.lasso7_MSCL=cv.glmnet(MSCL7_data, eclsw7A_MSCL_RF$C7R4MSCL)
  # plot(cv.lasso7_MSCL)
  # coef(cv.lasso7_MSCL)
  # summary(cv.lasso7_MSCL)


####Analysis for Reading scores####
library(randomForest)
set.seed(1234)
fit.forest_eclsw7R <- randomForest(C7R4RSCL ~ . , data= eclsw7A_RSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7R, type=2)
#Arrange in decreasing order
order_rf7R <- sort(importance(fit.forest_eclsw7R, type=2)[,1], decreasing = TRUE)
order_rf7R
# Top 20 Attributes
feature_stringw7R <- paste(names(order_rf7R)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw7R_impAttri <- eclsw7A_RSCL[, feature_stringw7R]

#Adding back the target attribute
eclsw7A_RSCL_RF <- cbind(eclsw7R_impAttri, C7R4RSCL = eclsw7A_RSCL$C7R4RSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw7_RSCL <- stepAIC(lm(C7R4RSCL ~ ., data=eclsw7A_RSCL_RF), direction="both")
summary(stepw7_RSCL)
stepw7A_R_vars <- (names(stepw7_RSCL$model))
stepw7A_R_edit <- gsub("^..",'',stepw7A_R_vars)
###Lasso Regression###

numeric_data7_RSCL = eclsw7A_RSCL_RF[,sapply(eclsw7A_RSCL_RF,is.numeric)]
numeric_data7_RSCL = subset(numeric_data7_RSCL, select=-c(C7R4RSCL))
cat_data7_RSCL = eclsw7A_RSCL_RF[,sapply(eclsw7A_RSCL_RF,is.factor)]
target7_RSCL = cbind(C7R4RSCL = eclsw7A_RSCL_RF$C7R4RSCL,cat_data7_RSCL)
xfactors7_RSCL <- model.matrix(C7R4RSCL ~ ., data = target7_RSCL)[,-1]
RSCL7_data <- as.matrix(data.frame(numeric_data7_RSCL, xfactors7_RSCL))

  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit_7RSCL <- glmnet(RSCL7_data, eclsw7A_RSCL_RF$C7R4RSCL, alpha=1)
  # 
  # plot(fit_7RSCL,xvar="lambda",label=TRUE)
  # plot(fit_7RSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit_7RSCL)
  # cv.lasso7_RSCL=cv.glmnet(RSCL7_data, eclsw7A_RSCL_RF$C7R4RSCL)
  # plot(cv.lasso7_RSCL)
  # coef(cv.lasso7_RSCL)
  # summary(cv.lasso7_RSCL)

####Analysis for Science scores####
library(randomForest)
set.seed(1234)
fit.forest_eclsw7S <- randomForest(C7R2SSCL ~ . , data= eclsw7A_SSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7S, type=2)
#Arrange in decreasing order
order_rf7S <- sort(importance(fit.forest_eclsw7S, type=2)[,1], decreasing = TRUE)
order_rf7S
# Top 20 Attributes
feature_stringw7S <- paste(names(order_rf7S)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw7S_impAttri <- eclsw7A_SSCL[, feature_stringw7S]

#Adding back the target attribute
eclsw7A_SSCL_RF <- cbind(eclsw7S_impAttri, C7R2SSCL = eclsw7A_SSCL$C7R2SSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw7_SSCL <- stepAIC(lm(C7R2SSCL ~ ., data=eclsw7A_SSCL_RF), direction="both")
summary(stepw7_SSCL)
stepw7A_SC_vars <- (names(stepw7_SSCL$model))
stepw7A_SC_edit <- as.data.frame(gsub("^..",'',stepw7A_SC_vars))

  # ###Lasso Regression###
  # 
  # numeric_data7_SSCL = eclsw7A_SSCL_RF[,sapply(eclsw7A_SSCL_RF,is.numeric)]
  # numeric_data7_SSCL = subset(numeric_data7_SSCL, select=-c(C7R2SSCL))
  # cat_data7_SSCL = eclsw7A_SSCL_RF[,sapply(eclsw7A_SSCL_RF,is.factor)]
  # target7_SSCL = cbind(C7R2SSCL = eclsw7A_SSCL_RF$C7R2SSCL,cat_data7_SSCL)
  # xfactors7_SSCL <- model.matrix(C7R2SSCL ~ ., data = target7_SSCL)[,-1]
  # SSCL7_data <- as.matrix(data.frame(numeric_data7_SSCL, xfactors7_SSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit_7SSCL <- glmnet(SSCL7_data, eclsw7A_SSCL_RF$C7R2SSCL, alpha=1)
  # 
  # plot(fit_7SSCL,xvar="lambda",label=TRUE)
  # plot(fit_7SSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit_7SSCL)
  # cv.lasso7_SSCL=cv.glmnet(SSCL7_data, eclsw7A_SSCL_RF$C7R2SSCL)
  # plot(cv.lasso7_SSCL)
  # coef(cv.lasso7_SSCL)
  # summary(cv.lasso7_SSCL)

  ##############################
  #Categorical Target Attributes
  ##############################
eclsw7A_ANGRY = subset(eclsw7_APvrty, select=-c(CHILDID,C7R2SSCL,C7R4MSCL,C7R4RSCL,C7LIKRD,C7LIKMTH,C7ENJRD,C7ENJMTH,C7FLGOOD)) #DATA with only Math scores as target variable
eclsw7A_LIKRD = subset(eclsw7_APvrty, select=-c(CHILDID,C7R4MSCL,C7R4RSCL,C7R2SSCL,C7ANGRY,C7LIKMTH,C7ENJRD,C7ENJMTH,C7FLGOOD))  #DATA with only Reading scores as target variable
eclsw7A_LIKMTH = subset(eclsw7_APvrty, select=-c(CHILDID,C7R4MSCL,C7R2SSCL,C7R4RSCL,C7ANGRY, C7LIKRD,C7ENJRD,C7ENJMTH,C7FLGOOD))
eclsw7A_ENJRD = subset(eclsw7_APvrty, select=-c(CHILDID,C7R2SSCL,C7R4MSCL,C7R4RSCL,C7ANGRY, C7LIKRD,C7LIKMTH,C7ENJMTH,C7FLGOOD)) #DATA with only Math scores as target variable
eclsw7A_ENJMTH = subset(eclsw7_APvrty, select=-c(CHILDID,C7R4MSCL,C7R4RSCL,C7R2SSCL,C7ANGRY, C7LIKRD,C7LIKMTH,C7ENJRD,C7FLGOOD))  #DATA with only Reading scores as target variable
eclsw7A_FLGOOD = subset(eclsw7_APvrty, select=-c(CHILDID,C7R4MSCL,C7R2SSCL,C7R4RSCL,C7ANGRY, C7LIKRD,C7LIKMTH,C7ENJRD,C7ENJMTH))

#ANGRY#
library(randomForest)
set.seed(1234)
fit.forest_eclsw7ANGRY <- randomForest(C7ANGRY ~ . , data= eclsw7A_ANGRY,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7ANGRY, type=2)
#Arrange in decreasing order
order_rf7ANGRY <- sort(importance(fit.forest_eclsw7ANGRY, type=2)[,1], decreasing = TRUE)
order_rf7ANGRY
# Top 20 Attributes
feature_stringw7ANGRY <- paste(names(order_rf7ANGRY)[1:10])

#Data with top 20 attributes that will be used further for modeling
eclsw7ANGRY_impAttri <- eclsw7A_ANGRY[, feature_stringw7ANGRY]

#Adding back the target attribute
eclsw7A_ANGRY_RF <- cbind(eclsw7ANGRY_impAttri, C7ANGRY = eclsw7A_ANGRY$C7ANGRY)

library(C50)
model.c50ANGRY <- C5.0(C7ANGRY~., data= eclsw7A_ANGRY_RF, rules=T)
summary(model.c50ANGRY)

#LIKRD#
library(randomForest)
set.seed(1234)
fit.forest_eclsw7LIKRD <- randomForest(C7LIKRD ~ . , data= eclsw7A_LIKRD,
                                       na.action=na.roughfix,
                                       importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7LIKRD, type=2)
#Arrange in decreasing order
order_rf7LIKRD <- sort(importance(fit.forest_eclsw7LIKRD, type=2)[,1], decreasing = TRUE)
order_rf7LIKRD
# Top 20 Attributes
feature_stringw7LIKRD <- paste(names(order_rf7LIKRD)[1:10])

#Data with top 20 attributes that will be used further for modeling
eclsw7LIKRD_impAttri <- eclsw7A_LIKRD[, feature_stringw7LIKRD]

#Adding back the target attribute
eclsw7A_LIKRD_RF <- cbind(eclsw7LIKRD_impAttri, C7LIKRD = eclsw7A_LIKRD$C7LIKRD)

library(C50)
model.c50LIKRD <- C5.0(C7LIKRD~., data= eclsw7A_LIKRD_RF, rules=T)
summary(model.c50LIKRD)

#LIKMTH#
library(randomForest)
set.seed(1234)
fit.forest_eclsw7LIKMTH <- randomForest(C7LIKMTH ~ . , data= eclsw7A_LIKMTH,
                                       na.action=na.roughfix,
                                       importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7LIKMTH, type=2)
#Arrange in decreasing order
order_rf7LIKMTH <- sort(importance(fit.forest_eclsw7LIKMTH, type=2)[,1], decreasing = TRUE)
order_rf7LIKMTH
# Top 20 Attributes
feature_stringw7LIKMTH <- paste(names(order_rf7LIKMTH)[1:10])

#Data with top 20 attributes that will be used further for modeling
eclsw7LIKMTH_impAttri <- eclsw7A_LIKMTH[, feature_stringw7LIKMTH]

#Adding back the target attribute
eclsw7A_LIKMTH_RF <- cbind(eclsw7LIKMTH_impAttri, C7LIKMTH = eclsw7A_LIKMTH$C7LIKMTH)

library(C50)
model.c50LIKMTH <- C5.0(C7LIKMTH~., data= eclsw7A_LIKMTH_RF, rules=T)
summary(model.c50LIKMTH)

#ENJRD#
library(randomForest)
set.seed(1234)
fit.forest_eclsw7ENJRD <- randomForest(C7ENJRD ~ . , data= eclsw7A_ENJRD,
                                       na.action=na.roughfix,
                                       importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7ENJRD, type=2)
#Arrange in decreasing order
order_rf7ENJRD <- sort(importance(fit.forest_eclsw7ENJRD, type=2)[,1], decreasing = TRUE)
order_rf7ENJRD
# Top 20 Attributes
feature_stringw7ENJRD <- paste(names(order_rf7ENJRD)[1:10])

#Data with top 20 attributes that will be used further for modeling
eclsw7ENJRD_impAttri <- eclsw7A_ENJRD[, feature_stringw7ENJRD]

#Adding back the target attribute
eclsw7A_ENJRD_RF <- cbind(eclsw7ENJRD_impAttri, C7ENJRD = eclsw7A_ENJRD$C7ENJRD)

library(C50)
model.c50ENJRD <- C5.0(C7ENJRD~., data= eclsw7A_ENJRD_RF, rules=T)
summary(model.c50ENJRD)

#ENJMTH#
library(randomForest)
set.seed(1234)
fit.forest_eclsw7ENJMTH <- randomForest(C7ENJMTH ~ . , data= eclsw7A_ENJMTH,
                                       na.action=na.roughfix,
                                       importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7ENJMTH, type=2)
#Arrange in decreasing order
order_rf7ENJMTH <- sort(importance(fit.forest_eclsw7ENJMTH, type=2)[,1], decreasing = TRUE)
order_rf7ENJMTH
# Top 20 Attributes
feature_stringw7ENJMTH <- paste(names(order_rf7ENJMTH)[1:10])

#Data with top 20 attributes that will be used further for modeling
eclsw7ENJMTH_impAttri <- eclsw7A_ENJMTH[, feature_stringw7ENJMTH]

#Adding back the target attribute
eclsw7A_ENJMTH_RF <- cbind(eclsw7ENJMTH_impAttri, C7ENJMTH = eclsw7A_ENJMTH$C7ENJMTH)

library(C50)
model.c50ENJMTH <- C5.0(C7ENJMTH~., data= eclsw7A_ENJMTH_RF, rules=T)
summary(model.c50ENJMTH)

#FLGOOD#
library(randomForest)
set.seed(1234)
fit.forest_eclsw7FLGOOD <- randomForest(C7FLGOOD ~ . , data= eclsw7A_FLGOOD,
                                       na.action=na.roughfix,
                                       importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7FLGOOD, type=2)
#Arrange in decreasing order
order_rf7FLGOOD <- sort(importance(fit.forest_eclsw7FLGOOD, type=2)[,1], decreasing = TRUE)
order_rf7FLGOOD
# Top 20 Attributes
feature_stringw7FLGOOD <- paste(names(order_rf7FLGOOD)[1:10])

#Data with top 20 attributes that will be used further for modeling
eclsw7FLGOOD_impAttri <- eclsw7A_FLGOOD[, feature_stringw7FLGOOD]

#Adding back the target attribute
eclsw7A_FLGOOD_RF <- cbind(eclsw7FLGOOD_impAttri, C7FLGOOD = eclsw7A_FLGOOD$C7FLGOOD)

library(C50)
model.c50FLGOOD <- C5.0(C7FLGOOD~., data= eclsw7A_FLGOOD_RF, rules=T)
summary(model.c50FLGOOD)

########################################
#WAVE 7 WITH STUDENTS BELOW POVERTY 
########################################

eclsw7_BPvrty = eclsw7_BPvrty[-1]
eclsw7B_MSCL = subset(eclsw7_BPvrty, select=-c(C7R4RSCL,C7R2SSCL,C7ANGRY, C7LIKRD,C7LIKMTH,C7ENJRD,C7ENJMTH,C7FLGOOD)) #DATA with only Math scores as target variable
eclsw7B_RSCL = subset(eclsw7_BPvrty, select=-c(C7R4MSCL,C7R2SSCL,C7ANGRY, C7LIKRD,C7LIKMTH,C7ENJRD,C7ENJMTH,C7FLGOOD)) #DATA with only Reading scores as target variable
eclsw7B_SSCL = subset(eclsw7_BPvrty, select=-c(C7R4MSCL,C7R4RSCL,C7ANGRY, C7LIKRD,C7LIKMTH,C7ENJRD,C7ENJMTH,C7FLGOOD))

#### Analysis of Math Scores ####
#ATTRIBUTE SELECTION USING RANDOM FOREST# 
library(randomForest)
set.seed(1234)
fit.forest_eclsw7B <- randomForest(C7R4MSCL ~ . , data= eclsw7B_MSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7B, type=2)
#Arrange in decreasing order
order_Brf7 <- sort(importance(fit.forest_eclsw7B, type=2)[,1], decreasing = TRUE)
order_Brf7
# Top 20 Attributes
feature_Bstringw7 <- paste(names(order_Brf7)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw7BM_impAttri <- eclsw7B_MSCL[, feature_Bstringw7]

#Adding back the target attribute
eclsw7B_MSCL_RF <- cbind(eclsw7BM_impAttri, C7R4MSCL = eclsw7B_MSCL$C7R4MSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw7_BMSCL <- stepAIC(lm(C7R4MSCL ~ ., data=eclsw7B_MSCL_RF), direction="both")
summary(stepw7_BMSCL)
stepw7B_M_vars <- (names(stepw7_BMSCL$model))
stepw7B_M_edit <- as.data.frame(gsub("^..",'',stepw7B_M_vars))

  # ###Lasso Regression###
  # numeric_data7_BMSCL = eclsw7B_MSCL_RF[,sapply(eclsw7B_MSCL_RF,is.numeric)]
  # numeric_data7_BMSCL = subset(numeric_data7_BMSCL, select=-c(C7R4MSCL))
  # cat_data7_BMSCL = eclsw7B_MSCL_RF[,sapply(eclsw7B_MSCL_RF,is.factor)]
  # target7_BMSCL = cbind(C7R4MSCL = eclsw7B_MSCL_RF$C7R4MSCL,cat_data7_BMSCL)
  # xfactors7_BMSCL <- model.matrix(C7R4MSCL ~ ., data = target7_BMSCL)[,-1]
  # BMSCL7_data <- as.matrix(data.frame(numeric_data7_BMSCL, xfactors7_BMSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit7_BMSCL <- glmnet(BMSCL7_data, eclsw7B_MSCL_RF$C7R4MSCL, alpha=1)
  # 
  # plot(fit7_BMSCL,xvar="lambda",label=TRUE)
  # plot(fit7_BMSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit7_BMSCL)
  # cv.lasso7_BMSCL=cv.glmnet(BMSCL7_data, eclsw7B_MSCL_RF$C7R4MSCL)
  # plot(cv.lasso7_BMSCL)
  # coef(cv.lasso7_BMSCL)
  # summary(cv.lasso7_BMSCL)


####Analysis for Reading scores####
library(randomForest)
set.seed(1234)
fit.forest_eclsw7RB <- randomForest(C7R4RSCL ~ . , data= eclsw7B_RSCL,
                                    na.action=na.roughfix,
                                    importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7RB, type=2)
#Arrange in decreasing order
order_7rfRB <- sort(importance(fit.forest_eclsw7RB, type=2)[,1], decreasing = TRUE)
order_7rfRB
# Top 20 Attributes
feature_stringw7RB <- paste(names(order_7rfRB)[1:20])

#Data with top 70 attributes that will be used further for modeling
eclsw7RB_impAttri <- eclsw7B_RSCL[, feature_stringw7RB]

#Adding back the target attribute
eclsw7B_RSCL_RF <- cbind(eclsw7RB_impAttri, C7R4RSCL = eclsw7B_RSCL$C7R4RSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw7B_RSCL <- stepAIC(lm(C7R4RSCL ~ ., data=eclsw7B_RSCL_RF), direction="both")
summary(stepw7B_RSCL)
stepw7B_R_vars <- (names(stepw7B_RSCL$model))
stepw7B_R_edit <- as.data.frame(gsub("^..",'',stepw7B_R_vars))

  # ###Lasso Regression###
  # 
  # numeric_data7_BRSCL = eclsw7B_RSCL_RF[,sapply(eclsw7B_RSCL_RF,is.numeric)]
  # numeric_data7_BRSCL = subset(numeric_data7_BRSCL, select=-c(C7R4RSCL))
  # cat_data7_BRSCL = eclsw7B_RSCL_RF[,sapply(eclsw7B_RSCL_RF,is.factor)]
  # target7_BRSCL = cbind(C7R4RSCL = eclsw7B_RSCL_RF$C7R4RSCL,cat_data7_BRSCL)
  # xfactors7_BRSCL <- model.matrix(C7R4RSCL ~ ., data = target7_BRSCL)[,-1]
  # BRSCL7_data <- as.matrix(data.frame(numeric_data7_BRSCL, xfactors7_BRSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit7_BRSCL <- glmnet(BRSCL7_data, eclsw7B_RSCL_RF$C7R4RSCL, alpha=1)
  # 
  # plot(fit7_BRSCL,xvar="lambda",label=TRUE)
  # plot(fit7_BRSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit7_BRSCL)
  # cv.lasso7_BRSCL=cv.glmnet(BRSCL7_data, eclsw7B_RSCL_RF$C7R4RSCL)
  # plot(cv.lasso7_BRSCL)
  # coef(cv.lasso7_BRSCL)
  # summary(cv.lasso7_BRSCL)

####Analysis for Science scores####
library(randomForest)
set.seed(1234)
fit.forest_eclsw7SB <- randomForest(C7R2SSCL ~ . , data= eclsw7B_SSCL,
                                    na.action=na.roughfix,
                                    importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7SB, type=2)
#Arrange in decreasing order
order_rf7SB <- sort(importance(fit.forest_eclsw7SB, type=2)[,1], decreasing = TRUE)
order_rf7SB
# Top 20 Attributes
feature_stringw7SB <- paste(names(order_rf7SB)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw7SB_impAttri <- eclsw7B_SSCL[, feature_stringw7SB]

#Adding back the target attribute
eclsw7B_SSCL_RF <- cbind(eclsw7SB_impAttri, C7R2SSCL = eclsw7B_SSCL$C7R2SSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw7B_SSCL <- stepAIC(lm(C7R2SSCL ~ ., data=eclsw7B_SSCL_RF), direction="both")
summary(stepw7B_SSCL)
stepw7B_SC_vars <- (names(stepw7B_SSCL$model))
stepw7B_SC_edit <- as.data.frame(gsub("^..",'',stepw7B_SC_vars))

  # ###Lasso Regression###
  # 
  # numeric_data7B_SSCL = eclsw7B_SSCL_RF[,sapply(eclsw7B_SSCL_RF,is.numeric)]
  # numeric_data7B_SSCL = subset(numeric_data7B_SSCL, select=-c(C7R2SSCL))
  # cat_data7B_SSCL = eclsw7B_SSCL_RF[,sapply(eclsw7B_SSCL_RF,is.factor)]
  # target7B_SSCL = cbind(C7R2SSCL = eclsw7B_SSCL_RF$C7R2SSCL,cat_data7B_SSCL)
  # xfactors7B_SSCL <- model.matrix(C7R2SSCL ~ ., data = target7B_SSCL)[,-1]
  # SSCL7B_data <- as.matrix(data.frame(numeric_data7B_SSCL, xfactors7B_SSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fitB_7SSCL <- glmnet(SSCL7B_data, eclsw7B_SSCL_RF$C7R2SSCL, alpha=1)
  # 
  # plot(fitB_7SSCL,xvar="lambda",label=TRUE)
  # plot(fitB_7SSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fitB_7SSCL)
  # cv.lasso7B_SSCL=cv.glmnet(SSCL7B_data, eclsw7B_SSCL_RF$C7R2SSCL)
  # plot(cv.lasso7B_SSCL)
  # coef(cv.lasso7B_SSCL)
  # summary(cv.lasso7B_SSCL)


##############################
#Categorical Target Attributes
##############################
eclsw7B_ANGRY = subset(eclsw7_BPVRTY, select=-c(C7R2SSCL,C7R4MSCL,C7R4RSCL,C7LIKRD,C7LIKMTH,C7ENJRD,C7ENJMTH,C7FLGOOD)) #DATA with only Math scores as target variable
eclsw7B_LIKRD = subset(eclsw7_BPVRTY, select=-c(C7R4MSCL,C7R4RSCL,C7R2SSCL,C7ANGRY,C7LIKMTH,C7ENJRD,C7ENJMTH,C7FLGOOD))  #DATA with only Reading scores as target variable
eclsw7B_LIKMTH = subset(eclsw7_BPVRTY, select=-c(C7R4MSCL,C7R2SSCL,C7R4RSCL,C7ANGRY, C7LIKRD,C7ENJRD,C7ENJMTH,C7FLGOOD))
eclsw7B_ENJRD = subset(eclsw7_BPVRTY, select=-c(C7R2SSCL,C7R4MSCL,C7R4RSCL,C7ANGRY, C7LIKRD,C7LIKMTH,C7ENJMTH,C7FLGOOD)) #DATA with only Math scores as target variable
eclsw7B_ENJMTH = subset(eclsw7_BPVRTY, select=-c(C7R4MSCL,C7R4RSCL,C7R2SSCL,C7ANGRY, C7LIKRD,C7LIKMTH,C7ENJRD,C7FLGOOD))  #DATA with only Reading scores as target variable
eclsw7B_FLGOOD = subset(eclsw7_BPVRTY, select=-c(C7R4MSCL,C7R2SSCL,C7R4RSCL,C7ANGRY, C7LIKRD,C7LIKMTH,C7ENJRD,C7ENJMTH))

#ANGRY#
library(randomForest)
set.seed(1234)
fit.forest_eclsw7ANGRY_B <- randomForest(C7ANGRY ~ . , data= eclsw7B_ANGRY,
                                       na.action=na.roughfix,
                                       importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7ANGRY_B, type=2)
#Arrange in decreasing order
order_rf7BANGRY <- sort(importance(fit.forest_eclsw7ANGRY_B, type=2)[,1], decreasing = TRUE)
order_rf7BANGRY
# Top 20 Attributes
feature_stringw7ABNGRY <- paste(names(order_rf7BANGRY)[1:10])

#Data with top 20 attributes that will be used further for modeling
eclsw7ANGRY_B_impAttri <- eclsw7B_ANGRY[, feature_stringw7BANGRY]

#Adding back the target attribute
eclsw7B_ANGRY_RF <- cbind(eclsw7ANGRY_B_impAttri, C7ANGRY = eclsw7B_ANGRY$C7ANGRY)

library(C50)
model.c50BANGRY <- C5.0(C7ANGRY~., data= eclsw7B_ANGRY_RF, rules=T)
summary(model.c50BANGRY)

#LIKRD#
library(randomForest)
set.seed(1234)
fit.forest_eclsw7BLIKRD <- randomForest(C7LIKRD ~ . , data= eclsw7B_LIKRD,
                                       na.action=na.roughfix,
                                       importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7BLIKRD, type=2)
#Arrange in decreasing order
order_rf7BLIKRD <- sort(importance(fit.forest_eclsw7BLIKRD, type=2)[,1], decreasing = TRUE)
order_rf7BLIKRD
# Top 20 Attributes
feature_stringw7BLIKRD <- paste(names(order_rf7BLIKRD)[1:10])

#Data with top 20 attributes that will be used further for modeling
eclsw7BLIKRD_impAttri <- eclsw7B_LIKRD[, feature_stringw7BLIKRD]

#Adding back the target attribute
eclsw7B_LIKRD_RF <- cbind(eclsw7BLIKRD_impAttri, C7LIKRD = eclsw7B_LIKRD$C7LIKRD)

library(C50)
model.c50BLIKRD <- C5.0(C7LIKRD~., data= eclsw7B_LIKRD_RF, rules=T)
summary(model.c50BBLIKRD)

#LIKMTH#
library(randomForest)
set.seed(1234)
fit.forest_eclsw7BLIKMTH <- randomForest(C7LIKMTH ~ . , data= eclsw7B_LIKMTH,
                                        na.action=na.roughfix,
                                        importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7BLIKMTH, type=2)
#Arrange in decreasing order
order_rf7BLIKMTH <- sort(importance(fit.forest_eclsw7BLIKMTH, type=2)[,1], decreasing = TRUE)
order_rf7BLIKMTH
# Top 20 Attributes
feature_stringw7BLIKMTH <- paste(names(order_rf7BLIKMTH)[1:10])

#Data with top 20 attributes that will be used further for modeling
eclsw7BLIKMTH_impAttri <- eclsw7B_LIKMTH[, feature_stringw7BLIKMTH]

#Adding back the target attribute
eclsw7B_LIKMTH_RF <- cbind(eclsw7BLIKMTH_impAttri, C7LIKMTH = eclsw7B_LIKMTH$C7LIKMTH)

library(C50)
model.c50BLIKMTH <- C5.0(C7LIKMTH~., data= eclsw7B_LIKMTH_RF, rules=T)
summary(model.c50BLIKMTH)

#ENJRD#
library(randomForest)
set.seed(1234)
fit.forest_eclsw7BENJRD <- randomForest(C7ENJRD ~ . , data= eclsw7B_ENJRD,
                                       na.action=na.roughfix,
                                       importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7BENJRD, type=2)
#Arrange in decreasing order
order_rf7BENJRD <- sort(importance(fit.forest_eclsw7ENJRD, type=2)[,1], decreasing = TRUE)
order_rf7BENJRD
# Top 20 Attributes
feature_stringw7BENJRD <- paste(names(order_rf7BENJRD)[1:10])

#Data with top 20 attributes that will be used further for modeling
eclsw7BENJRD_impAttri <- eclsw7B_ENJRD[, feature_stringw7BENJRD]

#Adding back the target attribute
eclsw7B_ENJRD_RF <- cbind(eclsw7BENJRD_impAttri, C7ENJRD = eclsw7B_ENJRD$C7ENJRD)

library(C50)
model.c50BENJRD <- C5.0(C7ENJRD~., data= eclsw7B_ENJRD_RF, rules=T)
summary(model.c50BENJRD)

#ENJMTH#
library(randomForest)
set.seed(1234)
fit.forest_eclsw7BENJMTH <- randomForest(C7ENJMTH ~ . , data= eclsw7B_ENJMTH,
                                        na.action=na.roughfix,
                                        importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7BENJMTH, type=2)
#Arrange in decreasing order
order_rf7BENJMTH <- sort(importance(fit.forest_eclsw7BENJMTH, type=2)[,1], decreasing = TRUE)
order_rf7BENJMTH
# Top 20 Attributes
feature_stringw7BENJMTH <- paste(names(order_rf7BENJMTH)[1:10])

#Data with top 20 attributes that will be used further for modeling
eclsw7BENJMTH_impAttri <- eclsw7B_ENJMTH[, feature_stringw7BENJMTH]

#Adding back the target attribute
eclsw7B_ENJMTH_RF <- cbind(eclsw7BENJMTH_impAttri, C7ENJMTH = eclsw7B_ENJMTH$C7ENJMTH)

library(C50)
model.c50BENJMTH <- C5.0(C7ENJMTH~., data= eclsw7B_ENJMTH_RF, rules=T)
summary(model.c50BENJMTH)

#FLGOOD#
library(randomForest)
set.seed(1234)
fit.forest_eclsw7BFLGOOD <- randomForest(C7FLGOOD ~ . , data= eclsw7B_FLGOOD,
                                        na.action=na.roughfix,
                                        importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw7BFLGOOD, type=2)
#Arrange in decreasing order
order_rf7BFLGOOD <- sort(importance(fit.forest_eclsw7BFLGOOD, type=2)[,1], decreasing = TRUE)
order_rf7BFLGOOD
# Top 20 Attributes
feature_stringw7BFLGOOD <- paste(names(order_rf7BFLGOOD)[1:10])

#Data with top 20 attributes that will be used further for modeling
eclsw7BFLGOOD_impAttri <- eclsw7B_FLGOOD[, feature_stringw7BFLGOOD]

#Adding back the target attribute
eclsw7B_FLGOOD_RF <- cbind(eclsw7BFLGOOD_impAttri, C7FLGOOD = eclsw7B_FLGOOD$C7FLGOOD)

library(C50)
model.c50BFLGOOD <- C5.0(C7FLGOOD~., data= eclsw7B_FLGOOD_RF, rules=T)
summary(model.c50BFLGOOD)