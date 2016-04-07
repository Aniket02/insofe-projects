rm(list=ls(all=TRUE))
eclswave3 <- read.csv("eclswave3.csv", na.strings = c("NOT APPLICABLE","REFUSED","DON'T KNOW","NOT ASCERTAINED"))
write.csv(eclswave3,"eclswave33.csv",row.names = F)
eclswave33 <- read.csv("eclswave33.csv", header = T)
str(eclswave33)

########################################
#PREPROCESSING OF DATA
########################################

#subet data into categorical and numerical variables
numeric_ecls3 = eclswave33[,sapply(eclswave33,is.numeric)]
cat_ecls3 = eclswave33[,sapply(eclswave33,is.factor)]

#values less than 0,so (-1 or -7, -8 or -9) replaced by NA, except for WKSESL
numeric3_no_W1SESL = numeric_ecls3[-1]
numeric3_no_W1SESL[numeric3_no_W1SESL < 0] <- NA
numeric_ecls3 = cbind(numeric_ecls3[1],numeric3_no_W1SESL)
rm(numeric3_no_W1SESL)
#combine numerical and categorical again
eclswave3_1 <- as.data.frame(cbind(cat_ecls3,numeric_ecls3))

#calculating the sum of NA's in each column
na_count3 <-sapply(eclswave3_1, function(y) sum(length(which(is.na(y))))) 
#creating a datafreclswave12the count values, it seems like many rows have a lot of Na's)
na_count3 <- data.frame(na_count3)  

#Removing columns which have NA occuring more than 30% of the time
columns_with_highNA3 <- which(colSums(is.na(eclswave3_1)) > 0.30*nrow(eclswave3_1))
eclswave3_cl <- eclswave3_1
rm(columns_with_highNA3)
#identifying the indexes of rows that have greater than 20% NA's
rows_with_highNA3 <- which(rowSums(is.na(eclswave3_cl)) > 0.20*ncol(eclswave3_cl))

#removing the identified rows
eclswave3_cl <- eclswave3_cl[-(rows_with_highNA3),]
rm(rows_with_highNA3)

summary(eclswave3_cl)
rm(cat_ecls3,numeric_ecls3)

#####KNN Imputation, and Standardization####
require(DMwR)
eclswave3_cl_Imput = knnImputation(eclswave3_cl)

#Split into numeric and categorical for standardization & Correlation
numeric_w3imput = eclswave3_cl_Imput[,sapply(eclswave3_cl_Imput,is.numeric)]
cat_w3imput = eclswave3_cl_Imput[,sapply(eclswave3_cl_Imput,is.factor)]
#Correlation
corw3 <- as.data.frame(cor(numeric_w3imput))
corw3 <- as.matrix(corw3)
corw3[which(abs(corw3) > 0.85)]

#Remove a couple variables that have high correlation with other attributes
#numeric_w3imput = subset(numeric_w3imput, select = -c(P2LESS18))
#rm(corw3)
#Seperating out target attributes before standardizing  
targetw3_vars = subset(numeric_w3imput, select = c(C3R4MSCL,C3R4RSCL))
numeric_w3imput = subset(numeric_w3imput, select = -c(C3R4MSCL,C3R4RSCL,C3RGSCAL))

#Standardizing  
library(vegan)
numeric_w3imputstd <- decostand(numeric_w3imput, "range")
#Combining categorical,target, and standardized numerical variables
eclswave3_model <- cbind(cat_w3imput,numeric_w3imputstd,targetw3_vars)
rm(numeric_w3imput,cat_w3imput)
#splitting data based upon poverty level of student
#Data of students above poverty level
eclsw3_APvrty <- eclswave3_model[eclswave3_model$W1POVRTY == "AT OR ABOVE POVERTY THRESHOLD",]

#Data of students below poverty level
eclsw3_BPvrty <- eclswave3_model[eclswave3_model$W1POVRTY == "BELOW POVERTY THRESHOLD",]


########################################
#WAVE 3 WITH STUDENTS ABOVE POVERTY 
########################################

eclsw3_APvrty = eclsw3_APvrty[-1]
eclsw3A_MSCL = subset(eclsw3_APvrty, select=-c(C3R4RSCL)) #DATA with only Math scores as target variable
eclsw3A_RSCL = subset(eclsw3_APvrty, select=-c(C3R4MSCL)) #DATA with only Reading scores as target variable

####Ananlysis of Math Scores####
#ATTRIBUTE SELECTION USING RANDOM FOREST 
library(randomForest)
set.seed(1234)
fit.forest_eclsw3 <- randomForest(C3R4MSCL ~ . , data= eclsw3A_MSCL,
                                  na.action=na.roughfix,
                                  importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw3, type=2)
#Arrange in decreasing order
order_rf3 <- sort(importance(fit.forest_eclsw3, type=2)[,1], decreasing = TRUE)
order_rf3
# Top 13 Attributes
feature_stringw3 <- paste(names(order_rf3)[1:13])

#Data with top 20 attributes that will be used further for modeling
eclsw3M_impAttri <- eclsw3A_MSCL[, feature_stringw3]

#Adding back the target attribute
eclsw3A_MSCL_RF <- cbind(eclsw3M_impAttri, C3R4MSCL = eclsw3A_MSCL$C3R4MSCL)
str(eclsw3A_MSCL_RF)
levels(eclsw3A_MSCL_RF$R3ELIG)
eclsw3A_MSCL_RF <- subset(eclsw3A_MSCL_RF,select = -c(W1POVRTY,R3ELIG))
#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw3_MSCL <- stepAIC(lm(C3R4MSCL ~ ., data=eclsw3A_MSCL_RF), direction="both")
summary(stepw3_MSCL)
stepw3A_M_vars <- (names(stepw3_MSCL$model))
stepw3A_M_edit <- gsub("^..",'',stepw3A_M_vars)

  # ###Lasso Regression###
  # numeric_data3_MSCL = eclsw3A_MSCL_RF[,sapply(eclsw3A_MSCL_RF,is.numeric)]
  # numeric_data3_MSCL = subset(numeric_data3_MSCL, select=-c(C3R4MSCL))
  # cat_data3_MSCL = eclsw3A_MSCL_RF[,sapply(eclsw3A_MSCL_RF,is.factor)]
  # target3_MSCL = cbind(C3R4MSCL = eclsw3A_MSCL_RF$C3R4MSCL,cat_data3_MSCL)
  # xfactors3_MSCL <- model.matrix(C3R4MSCL ~ ., data = target3_MSCL)[,-1]
  # MSCL3_data <- as.matrix(data.frame(numeric_data3_MSCL, xfactors3_MSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit3_MSCL <- glmnet(MSCL3_data, eclsw3A_MSCL_RF$C3R4MSCL, alpha=1)
  # 
  # plot(fit3_MSCL,xvar="lambda",label=TRUE)
  # plot(fit3_MSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit3_MSCL)
  # cv.lasso3_MSCL=cv.glmnet(MSCL3_data, eclsw3A_MSCL_RF$C3R4MSCL)
  # plot(cv.lasso3_MSCL)
  # coef(cv.lasso3_MSCL)
  # summary(cv.lasso3_MSCL)


####Analysis for Reading scores####
library(randomForest)
set.seed(1234)
fit.forest_eclsw3R <- randomForest(C3R4RSCL ~ . , data= eclsw3A_RSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw3R, type=2)
#Arrange in decreasing order
order_rf3R <- sort(importance(fit.forest_eclsw3R, type=2)[,1], decreasing = TRUE)
order_rf3R
# Top 12 Attributes
feature_stringw3R <- paste(names(order_rf3R)[1:11])

#Data with top 20 attributes that will be used further for modeling
eclsw3R_impAttri <- eclsw3A_RSCL[, feature_stringw3R]
#Adding back the target attribute
eclsw3A_RSCL_RF <- cbind(eclsw3R_impAttri, C3R4RSCL = eclsw3A_RSCL$C3R4RSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw3_RSCL <- stepAIC(lm(C3R4RSCL ~ ., data=eclsw3A_RSCL_RF), direction="both")
summary(stepw3_RSCL)
stepw3A_R_vars <- (names(stepw3_RSCL$model))
stepw3A_R_edit <- gsub("^..",'',stepw3A_R_vars)

  # ###Lasso Regression###
  # 
  # numeric_data3_RSCL = eclsw3A_RSCL_RF[,sapply(eclsw3A_RSCL_RF,is.numeric)]
  # numeric_data3_RSCL = subset(numeric_data3_RSCL, select=-c(C3R4RSCL))
  # cat_data3_RSCL = eclsw3A_RSCL_RF[,sapply(eclsw3A_RSCL_RF,is.factor)]
  # target3_RSCL = cbind(C3R4RSCL = eclsw3A_RSCL_RF$C3R4RSCL,cat_data3_RSCL)
  # xfactors3_RSCL <- model.matrix(C3R4RSCL ~ ., data = target3_RSCL)[,-1]
  # RSCL3_data <- as.matrix(data.frame(numeric_data3_RSCL, xfactors3_RSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit_3RSCL <- glmnet(RSCL3_data, eclsw3A_RSCL_RF$C3R4RSCL, alpha=1)
  # 
  # plot(fit_3RSCL,xvar="lambda",label=TRUE)
  # plot(fit_3RSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit_3RSCL)
  # cv.lasso3_RSCL=cv.glmnet(RSCL3_data, eclsw3A_RSCL_RF$C3R4RSCL)
  # plot(cv.lasso3_RSCL)
  # coef(cv.lasso3_RSCL)
  # summary(cv.lasso3_RSCL)

########################################
#WAVE 3 WITH STUDENTS BELOW POVERTY 
########################################

eclsw3_BPvrty = eclsw3_BPvrty[-1]
eclsw3B_MSCL = subset(eclsw3_BPvrty, select=-c(C3R4RSCL)) #DATA with only Math scores as target variable
eclsw3B_RSCL = subset(eclsw3_BPvrty, select=-c(C3R4MSCL)) #DATA with only Reading scores as target variable

#### Analysis of Math Scores ####
#ATTRIBUTE SELECTION USING RANDOM FOREST# 
library(randomForest)
set.seed(1234)
fit.forest_eclsw3B <- randomForest(C3R4MSCL ~ . , data= eclsw3B_MSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw3B, type=2)
#Arrange in decreasing order
order_Brf3 <- sort(importance(fit.forest_eclsw3B, type=2)[,1], decreasing = TRUE)
order_Brf3
# Top 12 Attributes
feature_Bstringw3 <- paste(names(order_Brf3)[1:11])

#Data with top 12 attributes that will be used further for modeling
eclsw3BM_impAttri <- eclsw3B_MSCL[, feature_Bstringw3]

#Adding back the target attribute
eclsw3B_MSCL_RF <- cbind(eclsw3BM_impAttri, C3R4MSCL = eclsw3B_MSCL$C3R4MSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw3_BMSCL <- stepAIC(lm(C3R4MSCL ~ ., data=eclsw3B_MSCL_RF), direction="both")
summary(stepw3_BMSCL)
stepw3B_M_vars <- (names(stepw3_BMSCL$model))
stepw3B_M_edit <- as.data.frame(gsub("^..",'',stepw3B_M_vars))

  # ###Lasso Regression###
  # numeric_data3_BMSCL = eclsw3B_MSCL_RF[,sapply(eclsw3B_MSCL_RF,is.numeric)]
  # numeric_data3_BMSCL = subset(numeric_data3_BMSCL, select=-c(C3R4MSCL))
  # cat_data3_BMSCL = eclsw3B_MSCL_RF[,sapply(eclsw3B_MSCL_RF,is.factor)]
  # target3_BMSCL = cbind(C3R4MSCL = eclsw3B_MSCL_RF$C3R4MSCL,cat_data3_BMSCL)
  # xfactors3_BMSCL <- model.matrix(C3R4MSCL ~ ., data = target3_BMSCL)[,-1]
  # BMSCL3_data <- as.matrix(data.frame(numeric_data3_BMSCL, xfactors3_BMSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit3_BMSCL <- glmnet(BMSCL3_data, eclsw3B_MSCL_RF$C3R4MSCL, alpha=1)
  # 
  # plot(fit3_BMSCL,xvar="lambda",label=TRUE)
  # plot(fit3_BMSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit3_BMSCL)
  # cv.lasso3_BMSCL=cv.glmnet(BMSCL3_data, eclsw3B_MSCL_RF$C3R4MSCL)
  # plot(cv.lasso3_BMSCL)
  # coef(cv.lasso3_BMSCL)
  # summary(cv.lasso3_BMSCL)


#### Analysis for Reading scores #####
library(randomForest)
set.seed(1234)
fit.forest_eclsw3RB <- randomForest(C3R4RSCL ~ . , data= eclsw3B_RSCL,
                                    na.action=na.roughfix,
                                    importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw3RB, type=2)
#Arrange in decreasing order
order_3rfRB <- sort(importance(fit.forest_eclsw3RB, type=2)[,1], decreasing = TRUE)
order_3rfRB
# Top 12 Attributes
feature_stringw3RB <- paste(names(order_3rfRB)[1:11])

#Data with top 12 attributes that will be used further for modeling
eclsw3RB_impAttri <- eclsw3B_RSCL[, feature_stringw3RB]

#Adding back the target attribute
eclsw3B_RSCL_RF <- cbind(eclsw3RB_impAttri, C3R4RSCL = eclsw3B_RSCL$C3R4RSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw3B_RSCL <- stepAIC(lm(C3R4RSCL ~ ., data=eclsw3B_RSCL_RF), direction="both")
summary(stepw3B_RSCL)
stepw3B_R_vars <- (names(stepw3B_RSCL$model))
stepw3B_R_edit <- as.data.frame(gsub("^..",'',stepw3B_R_vars))

  # ###Lasso Regression###
  # 
  # numeric_data3_BRSCL = eclsw3B_RSCL_RF[,sapply(eclsw3B_RSCL_RF,is.numeric)]
  # numeric_data3_BRSCL = subset(numeric_data3_BRSCL, select=-c(C3R4RSCL))
  # cat_data3_BRSCL = eclsw3B_RSCL_RF[,sapply(eclsw3B_RSCL_RF,is.factor)]
  # target3_BRSCL = cbind(C3R4RSCL = eclsw3B_RSCL_RF$C3R4RSCL,cat_data3_BRSCL)
  # xfactors3_BRSCL <- model.matrix(C3R4RSCL ~ ., data = target3_BRSCL)[,-1]
  # BRSCL3_data <- as.matrix(data.frame(numeric_data3_BRSCL, xfactors3_BRSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit3_BRSCL <- glmnet(BRSCL3_data, eclsw3B_RSCL_RF$C3R4RSCL, alpha=1)
  # 
  # plot(fit3_BRSCL,xvar="lambda",label=TRUE)
  # plot(fit3_BRSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit3_BRSCL)
  # cv.lasso3_BRSCL=cv.glmnet(BRSCL3_data, eclsw3B_RSCL_RF$C3R4RSCL)
  # plot(cv.lasso3_BRSCL)
  # coef(cv.lasso3_BRSCL)
  # summary(cv.lasso3_BRSCL)
