rm(list=ls(all=TRUE))
eclswave1 <- read.csv("eclswave1.csv", na.strings = c("NOT APPLICABLE","REFUSED","DON'T KNOW","NOT ASCERTAINED"))
write.csv(eclswave1,"eclswave11.csv",row.names = F)
eclswave11 <- read.csv("eclswave11.csv", header = T)
str(eclswave11)

########################################
#PREPROCESSING OF DATA
########################################

#subet data into categorical and numerical variables
  numeric_ecls1 = eclswave11[,sapply(eclswave11,is.numeric)]
  cat_ecls1 = eclswave11[,sapply(eclswave11,is.factor)]
  
#Subset numeric attributes that require -1 to be substituted by 0
  numeric_ecls1_0 = subset(numeric_ecls1, select=c(P1HSDAYS,P1HSHRS,P1EARLY,P1ADLTL2))
  #replace them with 0
  numeric_ecls1_0[numeric_ecls1_0 == -1] <- 0

#merge those attributes with the other numeric variables
  numeric_ecls1 = cbind(subset(numeric_ecls1, select= -c(P1HSDAYS,P1HSHRS,P1EARLY,P1ADLTL2)),numeric_ecls1_0)
  rm(numeric_ecls1_0)

#values less than 0,so (-1 or -7, -8 or -9) replaced by NA, except for WKSESL
  numeric_no_WKSESL = numeric_ecls1[-1]
  numeric_no_WKSESL[numeric_no_WKSESL < 0] <- NA
  numeric_ecls1 = cbind(numeric_ecls1[1],numeric_no_WKSESL)
  rm(numeric_no_WKSESL)
#combine numerical and categorical again
  eclswave12 <- as.data.frame(cbind(cat_ecls1,numeric_ecls1))
  
#calculating the sum of NA's in each column
  na_count <-sapply(eclswave12, function(y) sum(length(which(is.na(y))))) 
  #creating a datafreclswave12the count values, it seems like many rows have a lot of Na's)
    na_count <- data.frame(na_count)  
    
#Removing columns which have NA occuring more than 30% of the time
  columns_with_highNA <- which(colSums(is.na(eclswave12)) > 0.30*nrow(eclswave12))
  eclswave1_cl <- eclswave12[, -(columns_with_highNA)]
  rm(columns_with_highNA)
#identifying the indexes of rows that have greater than 20% NA's
  rows_with_highNA <- which(rowSums(is.na(eclswave1_cl)) > 0.20*ncol(eclswave1_cl))
                                                                 
#removing the identified rows
  eclswave1_cl <- eclswave1_cl[-(rows_with_highNA),]
  rm(rows_with_highNA)

summary(eclswave1_cl)
rm(cat_ecls1,numeric_ecls1)

#### KNN Imputation, and Standardization ####
require(DMwR)
  eclswave_cl_Imput = knnImputation(eclswave1_cl)

#Split into numeric and categorical for standardization & Correlation
  numeric_w1imput = eclswave_cl_Imput[,sapply(eclswave_cl_Imput,is.numeric)]
  cat_w1imput = eclswave_cl_Imput[,sapply(eclswave_cl_Imput,is.factor)]
  #Correlation
  corw1 <- as.data.frame(cor(numeric_w1imput))
  corw1 <- as.matrix(corw1)
  corw1[which(abs(corw1) > 0.7)]
  
  #Remove a couple variables that have high correlation with other attributes
  numeric_w1imput = subset(numeric_w1imput, select = -c(P1HSDAYS,P1NUMSIB))
  rm(corw1)
#Seperating out target attributes before standardizing  
  targetw1_vars = subset(numeric_w1imput, select = c(C1R4MSCL,C1R4RSCL,P1SADLON))
  numeric_w1imput = subset(numeric_w1imput, select = -c(C1R4MSCL,C1R4RSCL,P1SADLON,C1RGSCAL))

#Standardizing  
library(vegan)
  numeric_w1imputstd <- decostand(numeric_w1imput, "range")
#Combining categorical,target, and standardized numerical variables
  eclswave1_model <- cbind(cat_w1imput,numeric_w1imputstd,targetw1_vars)
  rm(numeric_w1imput,cat_w1imput)
#splitting data based upon poverty level of student
#Data of students above poverty level
  eclsw1_APvrty <- eclswave1_model[eclswave1_model$WKPOV_R == "AT OR ABOVE POVERTY THRESHOLD",]

#Data of students below poverty level
  eclsw1_BPvrty <- eclswave1_model[eclswave1_model$WKPOV_R == "BELOW POVERTY THRESHOLD",]

#checking if Math scores follow a normal curve, an assumption made for the t-test
qqnorm(eclsw1_APvrty$C1R4MSCL)
qqnorm(eclsw1_BPvrty$C1R4MSCL)
t.test( eclsw1_APvrty$C1R4MSCL, eclsw1_BPvrty$C1R4MSCL)

########################################
#WAVE 1 WITH STUDENTS ABOVE POVERTY 
########################################

eclsw1_APvrty = eclsw1_APvrty[-1]
eclsw1A_MSCL = subset(eclsw1_APvrty, select=-c(C1R4RSCL,P1SADLON)) #DATA with only Math scores as target variable
eclsw1A_RSCL = subset(eclsw1_APvrty, select=-c(C1R4MSCL,P1SADLON)) #DATA with only Reading scores as target variable
eclsw1A_SAD  = subset(eclsw1_APvrty, select=-c(C1R4MSCL,C1R4RSCL))

#### Analysis of Science Scores ####
#ATTRIBUTE SELECTION USING RANDOM FOREST# 
library(randomForest)
set.seed(1234)
fit.forest_eclsw1 <- randomForest(C1R4MSCL ~ . , data= eclsw1A_MSCL,
                                  na.action=na.roughfix,
                                  importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw1, type=2)
#Arrange in decreasing order
order_rf <- sort(importance(fit.forest_eclsw1, type=2)[,1], decreasing = TRUE)
order_rf
# Top 20 Attributes
feature_stringw1 <- paste(names(order_rf)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw1M_impAttri <- eclsw1A_MSCL[, feature_stringw1]

#Adding back the target attribute
eclsw1A_MSCL_RF <- cbind(eclsw1M_impAttri, C1R4MSCL = eclsw1A_MSCL$C1R4MSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw1_MSCL <- stepAIC(lm(C1R4MSCL ~ ., data=eclsw1A_MSCL_RF), direction="both")
summary(stepw1_MSCL)
stepw1A_M_vars <-(names(stepw1_MSCL$model))
stepw1A_M_edit <- gsub("^..",'',stepw1A_M_vars)
stepw1A_M_vars

  # ###Lasso Regression###
  # numeric_data_MSCL = eclsw1A_MSCL_RF[,sapply(eclsw1A_MSCL_RF,is.numeric)]
  # numeric_data_MSCL = subset(numeric_data_MSCL, select=-c(C1R4MSCL))
  # cat_data_MSCL = eclsw1A_MSCL_RF[,sapply(eclsw1A_MSCL_RF,is.factor)]
  # target_MSCL = cbind(C1R4MSCL = eclsw1A_MSCL_RF$C1R4MSCL,cat_data_MSCL)
  # xfactors_MSCL <- model.matrix(C1R4MSCL ~ ., data = target_MSCL)[,-1]
  # MSCL_data <- as.matrix(data.frame(numeric_data_MSCL, xfactors_MSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit_MSCL <- glmnet(MSCL_data, eclsw1A_MSCL_RF$C1R4MSCL, alpha=1)
  # 
  # plot(fit_MSCL,xvar="lambda",label=TRUE)
  # plot(fit_MSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit_MSCL)
  # cv.lasso_MSCL=cv.glmnet(MSCL_data, eclsw1A_MSCL_RF$C1R4MSCL)
  # plot(cv.lasso_MSCL)
  # coef(cv.lasso_MSCL)
  # summary(cv.lasso_MSCL)


#### Analysis for Reading scores ####
library(randomForest)
set.seed(1234)
fit.forest_eclsw1R <- randomForest(C1R4RSCL ~ . , data= eclsw1A_RSCL,
                                  na.action=na.roughfix,
                                  importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw1R, type=2)
#Arrange in decreasing order
order_rfR <- sort(importance(fit.forest_eclsw1R, type=2)[,1], decreasing = TRUE)
order_rfR
# Top 20 Attributes
feature_stringw1R <- paste(names(order_rfR)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw1R_impAttri <- eclsw1A_RSCL[, feature_stringw1R]

#Adding back the target attribute
eclsw1A_RSCL_RF <- cbind(eclsw1R_impAttri, C1R4RSCL = eclsw1A_RSCL$C1R4RSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw1_RSCL <- stepAIC(lm(C1R4RSCL ~ ., data=eclsw1A_RSCL_RF), direction="both")
summary(stepw1_RSCL)
stepw1A_R_vars <-(names(stepw1_RSCL$model))
stepw1A_R_edit <- gsub("^..",'',stepw1A_R_vars)

  # ###Lasso Regression###
  # 
  # numeric_data_RSCL = eclsw1A_RSCL_RF[,sapply(eclsw1A_RSCL_RF,is.numeric)]
  # numeric_data_RSCL = subset(numeric_data_RSCL, select=-c(C1R4RSCL))
  # cat_data_RSCL = eclsw1A_RSCL_RF[,sapply(eclsw1A_RSCL_RF,is.factor)]
  # target_RSCL = cbind(C1R4RSCL = eclsw1A_RSCL_RF$C1R4RSCL,cat_data_RSCL)
  # xfactors_RSCL <- model.matrix(C1R4RSCL ~ ., data = target_RSCL)[,-1]
  # RSCL_data <- as.matrix(data.frame(numeric_data_RSCL, xfactors_RSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit_RSCL <- glmnet(RSCL_data, eclsw1A_RSCL_RF$C1R4RSCL, alpha=1)
  # 
  # plot(fit_RSCL,xvar="lambda",label=TRUE)
  # plot(fit_RSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit_RSCL)
  # cv.lasso_RSCL=cv.glmnet(RSCL_data, eclsw1A_RSCL_RF$C1R4RSCL)
  # plot(cv.lasso_RSCL)
  # coef(cv.lasso_RSCL)
  # summary(cv.lasso_RSCL)

####Analysis of P1SADLON#### 
library(randomForest)
set.seed(1234)
fit.forest_eclsw1S <- randomForest(P1SADLON ~ . , data= eclsw1A_SAD,
                                  na.action=na.roughfix,
                                  importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw1S, type=2)
#Arrange in decreasing order
order_rfS <- sort(importance(fit.forest_eclsw1S, type=2)[,1], decreasing = TRUE)
order_rfS
# Top 20 Attributes
feature_stringw1S <- paste(names(order_rfS)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw1S_impAttri <- eclsw1A_SAD[, feature_stringw1S]

#Adding back the target attribute
eclsw1A_SAD_RF <- cbind(eclsw1S_impAttri, P1SADLON = eclsw1A_SAD$P1SADLON)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw1_SAD <- stepAIC(lm(P1SADLON ~ ., data=eclsw1A_SAD_RF), direction="both")
summary(stepw1_SAD)
stepw1A_S_vars <-(names(stepw1_SAD$model))
stepw1A_S_edit <- as.data.frame(gsub("^..",'',stepw1A_S_vars))

  # ###Lasso Regression###
  # numeric_data_SAD = eclsw1A_SAD_RF[,sapply(eclsw1A_SAD_RF,is.numeric)]
  # numeric_data_SAD = subset(numeric_data_SAD, select=-c(P1SADLON))
  # cat_data_SAD = eclsw1A_SAD_RF[,sapply(eclsw1A_SAD_RF,is.factor)]
  # target_SAD = cbind(P1SADLON = eclsw1A_SAD_RF$P1SADLON,cat_data_SAD)
  # xfactors_SAD <- model.matrix(P1SADLON ~ ., data = target_SAD)[,-1]
  # SAD_data <- as.matrix(data.frame(numeric_data_SAD, xfactors_SAD))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit_SAD <- glmnet(SAD_data, eclsw1A_SAD_RF$P1SADLON, alpha=1)
  # 
  # plot(fit_SAD,xvar="lambda",label=TRUE)
  # plot(fit_SAD,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit_SAD)
  # cv.lasso_SAD=cv.glmnet(SAD_data, eclsw1A_SAD_RF$P1SADLON)
  # plot(cv.lasso_SAD)
  # coef(cv.lasso_SAD)
  # summary(cv.lasso_SAD)

########################################
#WAVE 1 WITH STUDENTS BELOW POVERTY 
########################################

eclsw1_BPvrty = eclsw1_BPvrty[-1]
eclsw1B_MSCL = subset(eclsw1_BPvrty, select=-c(C1R4RSCL,P1SADLON)) #DATA with only Math scores as target variable
eclsw1B_RSCL = subset(eclsw1_BPvrty, select=-c(C1R4MSCL,P1SADLON)) #DATA with only Reading scores as target variable
eclsw1B_SAD  = subset(eclsw1_BPvrty, select=-c(C1R4MSCL,C1R4RSCL))

#### Analysis of Math Scores ####
#ATTRIBUTE SELECTION USING RANDOM FOREST# 
library(randomForest)
set.seed(1234)
fit.forest_eclsw1B <- randomForest(C1R4MSCL ~ . , data= eclsw1B_MSCL,
                                  na.action=na.roughfix,
                                  importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw1B, type=2)
#Arrange in decreasing order
order_Brf <- sort(importance(fit.forest_eclsw1B, type=2)[,1], decreasing = TRUE)
order_Brf
# Top 20 Attributes
feature_Bstringw1 <- paste(names(order_Brf)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw1BM_impAttri <- eclsw1B_MSCL[, feature_Bstringw1]

#Adding back the target attribute
eclsw1B_MSCL_RF <- cbind(eclsw1BM_impAttri, C1R4MSCL = eclsw1B_MSCL$C1R4MSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw1_BMSCL <- stepAIC(lm(C1R4MSCL ~ ., data=eclsw1B_MSCL_RF), direction="both")
summary(stepw1_BMSCL)
stepw1B_M_vars <-(names(stepw1_BMSCL$model))
stepw1B_M_edit <- as.data.frame(gsub("^..",'',stepw1B_M_vars))

  # ###Lasso Regression###
  # numeric_data_BMSCL = eclsw1B_MSCL_RF[,sapply(eclsw1B_MSCL_RF,is.numeric)]
  # numeric_data_BMSCL = subset(numeric_data_BMSCL, select=-c(C1R4MSCL))
  # cat_data_BMSCL = eclsw1B_MSCL_RF[,sapply(eclsw1B_MSCL_RF,is.factor)]
  # target_BMSCL = cbind(C1R4MSCL = eclsw1B_MSCL_RF$C1R4MSCL,cat_data_BMSCL)
  # xfactors_BMSCL <- model.matrix(C1R4MSCL ~ ., data = target_BMSCL)[,-1]
  # BMSCL_data <- as.matrix(data.frame(numeric_data_BMSCL, xfactors_BMSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit_BMSCL <- glmnet(BMSCL_data, eclsw1B_MSCL_RF$C1R4MSCL, alpha=1)
  # 
  # plot(fit_BMSCL,xvar="lambda",label=TRUE)
  # plot(fit_BMSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit_BMSCL)
  # cv.lasso_BMSCL=cv.glmnet(BMSCL_data, eclsw1B_MSCL_RF$C1R4MSCL)
  # plot(cv.lasso_BMSCL)
  # coef(cv.lasso_BMSCL)
  # summary(cv.lasso_BMSCL)


#### Analysis for Reading scores ####
library(randomForest)
set.seed(1234)
fit.forest_eclsw1RB <- randomForest(C1R4RSCL ~ . , data= eclsw1B_RSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw1RB, type=2)
#Arrange in decreasing order
order_rfRB <- sort(importance(fit.forest_eclsw1RB, type=2)[,1], decreasing = TRUE)
order_rfRB
# Top 20 Attributes
feature_stringw1RB <- paste(names(order_rfRB)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw1RB_impAttri <- eclsw1B_RSCL[, feature_stringw1RB]

#Adding back the target attribute
eclsw1B_RSCL_RF <- cbind(eclsw1RB_impAttri, C1R4RSCL = eclsw1B_RSCL$C1R4RSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw1B_RSCL <- stepAIC(lm(C1R4RSCL ~ ., data=eclsw1B_RSCL_RF), direction="both")
summary(stepw1B_RSCL)
stepw1B_R_vars <-(names(stepw1B_RSCL$model))
stepw1B_R_edit <- as.data.frame(gsub("^..",'',stepw1B_R_vars))

  # ###Lasso Regression###
  # 
  # numeric_data_BRSCL = eclsw1B_RSCL_RF[,sapply(eclsw1B_RSCL_RF,is.numeric)]
  # numeric_data_BRSCL = subset(numeric_data_BRSCL, select=-c(C1R4RSCL))
  # cat_data_BRSCL = eclsw1B_RSCL_RF[,sapply(eclsw1B_RSCL_RF,is.factor)]
  # target_BRSCL = cbind(C1R4RSCL = eclsw1B_RSCL_RF$C1R4RSCL,cat_data_BRSCL)
  # xfactors_BRSCL <- model.matrix(C1R4RSCL ~ ., data = target_BRSCL)[,-1]
  # BRSCL_data <- as.matrix(data.frame(numeric_data_BRSCL, xfactors_BRSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit_BRSCL <- glmnet(BRSCL_data, eclsw1B_RSCL_RF$C1R4RSCL, alpha=1)
  # 
  # plot(fit_BRSCL,xvar="lambda",label=TRUE)
  # plot(fit_BRSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit_BRSCL)
  # cv.lasso_BRSCL=cv.glmnet(BRSCL_data, eclsw1B_RSCL_RF$C1R4RSCL)
  # plot(cv.lasso_BRSCL)
  # coef(cv.lasso_BRSCL)
  # summary(cv.lasso_BRSCL)

####Analysis of P1SADLON#### 
library(randomForest)
set.seed(1234)
fit.forest_eclsw1BS <- randomForest(P1SADLON ~ . , data= eclsw1B_SAD,
                                    na.action=na.roughfix,
                                    importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw1BS, type=2)
#Arrange in decreasing order
order_rf1BS <- sort(importance(fit.forest_eclsw1BS, type=2)[,1], decreasing = TRUE)
order_rf1BS
# Top 20 Attributes
feature_stringw1BS <- paste(names(order_rf1BS)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw1BS_impAttri <- eclsw1B_SAD[, feature_stringw1BS]

#Adding back the target attribute
eclsw1B_SAD_RF <- cbind(eclsw1BS_impAttri, P1SADLON = eclsw1B_SAD$P1SADLON)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw1B_SAD <- stepAIC(lm(P1SADLON ~ ., data=eclsw1B_SAD_RF), direction="both")
summary(stepw1B_SAD)
stepw1B_S_vars <-(names(stepw1B_SAD$model))
stepw1B_S_edit <- as.data.frame(gsub("^..",'',stepw1B_S_vars))

  # ###Lasso Regression###
  # numeric_data1B_SAD = eclsw1B_SAD_RF[,sapply(eclsw1B_SAD_RF,is.numeric)]
  # numeric_data1B_SAD = subset(numeric_data1B_SAD, select=-c(P1SADLON))
  # cat_data1B_SAD = eclsw1B_SAD_RF[,sapply(eclsw1B_SAD_RF,is.factor)]
  # target1B_SAD = cbind(P1SADLON = eclsw1B_SAD_RF$P1SADLON,cat_data1B_SAD)
  # xfactors1B_SAD <- model.matrix(P1SADLON ~ ., data = target1B_SAD)[,-1]
  # BSAD1_data <- as.matrix(data.frame(numeric_data1B_SAD, xfactors1B_SAD))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit1B_SAD <- glmnet(BSAD1_data, eclsw1B_SAD_RF$P1SADLON, alpha=1)
  # 
  # plot(fit1B_SAD,xvar="lambda",label=TRUE)
  # plot(fit1B_SAD,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit1B_SAD)
  # cv.lasso1B_SAD=cv.glmnet(SAD1_data, eclsw1B_SAD_RF$P1SADLON)
  # plot(cv.lasso1B_SAD)
  # coef(cv.lasso1B_SAD)
  # summary(cv.lasso1B_SAD)
