rm(list=ls(all=TRUE))
eclswave4 <- read.csv("eclswave4.csv", na.strings = c("NOT APPLICABLE","REFUSED","DON'T KNOW","NOT ASCERTAINED"))
write.csv(eclswave4,"eclswave44.csv",row.names = F)
eclswave44 <- read.csv("eclswave44.csv", header = T)
str(eclswave44)

########################################
#PREPROCESSING OF DATA
########################################

#subet data into categorical and numerical variables
numeric_ecls4 = eclswave44[,sapply(eclswave44,is.numeric)]
cat_ecls4 = eclswave44[,sapply(eclswave44,is.factor)]

#Remove P4FSSCAL because this infromation is already given by attribute P4FSRAW
#numeric_ecls4 = subset(numeric_ecls4, select = -c(P4FSSCAL))

#values less than 0,so (-1 or -7, -8 or -9) replaced by NA, except for W1SESL
numeric4_no_W1SESL = numeric_ecls4[-1]
numeric4_no_W1SESL[numeric4_no_W1SESL < 0] <- NA
numeric_ecls4 = cbind(numeric_ecls4[1],numeric4_no_W1SESL)
rm(numeric4_no_W1SESL)
#combine numerical and categorical again
eclswave4_1 <- as.data.frame(cbind(cat_ecls4,numeric_ecls4))

#calculating the sum of NA's in each column
na_count4 <-sapply(eclswave4_1, function(y) sum(length(which(is.na(y))))) 
#creating a datafreclswave14the count values, it seems like many rows have a lot of Na's)
na_count4 <- data.frame(na_count4)  

#Removing columns which have NA occuring more than 30% of the time
columns_with_highNA4 <- which(colSums(is.na(eclswave4_1)) > 0.30*nrow(eclswave4_1))
eclswave4_cl <- eclswave4_1[, -(columns_with_highNA4)]
rm(columns_with_highNA4)
#identifying the indexes of rows that have greater than 20% NA's
rows_with_highNA4 <- which(rowSums(is.na(eclswave4_cl)) > 0.20*ncol(eclswave4_cl))

#removing the identified rows
eclswave4_cl <- eclswave4_cl[-(rows_with_highNA4),]
rm(rows_with_highNA4)

summary(eclswave4_cl)
rm(cat_ecls4,numeric_ecls4)
#---KNN Imputation---#
require(DMwR)
eclswave4_cl_Imput = knnImputation(eclswave4_cl)

#Split into numeric and categorical for standardization & Correlation
numeric_w4imput = eclswave4_cl_Imput[,sapply(eclswave4_cl_Imput,is.numeric)]
cat_w4imput = eclswave4_cl_Imput[,sapply(eclswave4_cl_Imput,is.factor)]
#Correlation
corw4 <- as.data.frame(cor(numeric_w4imput))
corw4 <- as.matrix(corw4)
corw4[which(abs(corw4) > 0.85)]

#Remove a couple variables that have high correlation with other attributes
numeric_w4imput = subset(numeric_w4imput, select = -c(P4LESS18))
rm(corw4)
#Seperating out target attributes before standardizing  
targetw4_vars = subset(numeric_w4imput, select = c(C4R4MSCL,C4R4RSCL))
numeric_w4imput = subset(numeric_w4imput, select = -c(C4R4MSCL,C4R4RSCL,C4RGSCAL))

#Standardizing  
library(vegan)
numeric_w4imputstd <- decostand(numeric_w4imput, "range")
#Combining categorical,target, and standardized numerical variables
eclswave4_model <- cbind(cat_w4imput,numeric_w4imputstd,targetw4_vars)
rm(numeric_w4imput,cat_w4imput)
#splitting data based upon poverty level of student
#Data of students above poverty level
eclsw4_APvrty <- eclswave4_model[eclswave4_model$W1POVRTY == "AT OR ABOVE POVERTY THRESHOLD",]

#Data of students below poverty level
eclsw4_BPvrty <- eclswave4_model[eclswave4_model$W1POVRTY == "BELOW POVERTY THRESHOLD",]


########################################
#WAVE 4 WITH STUDENTS ABOVE POVERTY 
########################################

eclsw4_APvrty = eclsw4_APvrty[-1]
eclsw4A_MSCL = subset(eclsw4_APvrty, select=-c(C4R4RSCL,P4SADLON)) #DATA with only Math scores as target variable
eclsw4A_RSCL = subset(eclsw4_APvrty, select=-c(C4R4MSCL,P4SADLON)) #DATA with only Reading scores as target variable
eclsw4A_SAD  = subset(eclsw4_APvrty, select=-c(C4R4MSCL,C4R4RSCL))

#### Analysis of Math Scores ####
#ATTRIBUTE SELECTION USING RANDOM FOREST# 
library(randomForest)
set.seed(1234)
fit.forest_eclsw4 <- randomForest(C4R4MSCL ~ . , data= eclsw4A_MSCL,
                                  na.action=na.roughfix,
                                  importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw4, type=2)
#Arrange in decreasing order
order_rf4 <- sort(importance(fit.forest_eclsw4, type=2)[,1], decreasing = TRUE)
order_rf4
# Top 20 Attributes
feature_stringw4 <- paste(names(order_rf4)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw4M_impAttri <- eclsw4A_MSCL[, feature_stringw4]

#Adding back the target attribute
eclsw4A_MSCL_RF <- cbind(eclsw4M_impAttri, C4R4MSCL = eclsw4A_MSCL$C4R4MSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw4_MSCL <- stepAIC(lm(C4R4MSCL ~ ., data=eclsw4A_MSCL_RF), direction="both")
summary(stepw4_MSCL)
stepw4A_M_vars <- (names(stepw4_MSCL$model))
stepw4A_M_edit <- gsub("^..",'',stepw4A_M_vars)
  # 
  # ###Lasso Regression###
  # numeric_data4_MSCL = eclsw4A_MSCL_RF[,sapply(eclsw4A_MSCL_RF,is.numeric)]
  # numeric_data4_MSCL = subset(numeric_data4_MSCL, select=-c(C4R4MSCL))
  # cat_data4_MSCL = eclsw4A_MSCL_RF[,sapply(eclsw4A_MSCL_RF,is.factor)]
  # target4_MSCL = cbind(C4R4MSCL = eclsw4A_MSCL_RF$C4R4MSCL,cat_data4_MSCL)
  # xfactors4_MSCL <- model.matrix(C4R4MSCL ~ ., data = target4_MSCL)[,-1]
  # MSCL4_data <- as.matrix(data.frame(numeric_data4_MSCL, xfactors4_MSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit4_MSCL <- glmnet(MSCL4_data, eclsw4A_MSCL_RF$C4R4MSCL, alpha=1)
  # 
  # plot(fit4_MSCL,xvar="lambda",label=TRUE)
  # plot(fit4_MSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit4_MSCL)
  # cv.lasso4_MSCL=cv.glmnet(MSCL4_data, eclsw4A_MSCL_RF$C4R4MSCL)
  # plot(cv.lasso4_MSCL)
  # coef(cv.lasso4_MSCL)
  # summary(cv.lasso4_MSCL)


#### Analysis for Reading scores ####
library(randomForest)
set.seed(1234)
fit.forest_eclsw4R <- randomForest(C4R4RSCL ~ . , data= eclsw4A_RSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw4R, type=2)
#Arrange in decreasing order
order_rf4R <- sort(importance(fit.forest_eclsw4R, type=2)[,1], decreasing = TRUE)
order_rf4R
# Top 20 Attributes
feature_stringw4R <- paste(names(order_rf4R)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw4R_impAttri <- eclsw4A_RSCL[, feature_stringw4R]

#Adding back the target attribute
eclsw4A_RSCL_RF <- cbind(eclsw4R_impAttri, C4R4RSCL = eclsw4A_RSCL$C4R4RSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw4_RSCL <- stepAIC(lm(C4R4RSCL ~ ., data=eclsw4A_RSCL_RF), direction="both")
summary(stepw4_RSCL)
stepw4A_R_vars <- (names(stepw4_RSCL$model))
stepw4A_R_edit <- gsub("^..",'',stepw4A_R_vars)

  # ###Lasso Regression###
  # 
  # numeric_data4_RSCL = eclsw4A_RSCL_RF[,sapply(eclsw4A_RSCL_RF,is.numeric)]
  # numeric_data4_RSCL = subset(numeric_data4_RSCL, select=-c(C4R4RSCL))
  # cat_data4_RSCL = eclsw4A_RSCL_RF[,sapply(eclsw4A_RSCL_RF,is.factor)]
  # target4_RSCL = cbind(C4R4RSCL = eclsw4A_RSCL_RF$C4R4RSCL,cat_data4_RSCL)
  # xfactors4_RSCL <- model.matrix(C4R4RSCL ~ ., data = target4_RSCL)[,-1]
  # RSCL4_data <- as.matrix(data.frame(numeric_data4_RSCL, xfactors4_RSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit_4RSCL <- glmnet(RSCL4_data, eclsw4A_RSCL_RF$C4R4RSCL, alpha=1)
  # 
  # plot(fit_4RSCL,xvar="lambda",label=TRUE)
  # plot(fit_4RSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit_4RSCL)
  # cv.lasso4_RSCL=cv.glmnet(RSCL4_data, eclsw4A_RSCL_RF$C4R4RSCL)
  # plot(cv.lasso4_RSCL)
  # coef(cv.lasso4_RSCL)
  # summary(cv.lasso4_RSCL)

####Analysis of P4SADLON#### 
library(randomForest)
set.seed(1234)
fit.forest_eclsw4S <- randomForest(P4SADLON ~ . , data= eclsw4A_SAD,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw4S, type=2)
#Arrange in decreasing order
order_rf4S <- sort(importance(fit.forest_eclsw4S, type=2)[,1], decreasing = TRUE)
order_rf4S
# Top 20 Attributes
feature_stringw4S <- paste(names(order_rf4S)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw4S_impAttri <- eclsw4A_SAD[, feature_stringw4S]

#Adding back the target attribute
eclsw4A_SAD_RF <- cbind(eclsw4S_impAttri, P4SADLON = eclsw4A_SAD$P4SADLON)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw4_SAD <- stepAIC(lm(P4SADLON ~ ., data=eclsw4A_SAD_RF), direction="both")
summary(stepw4_SAD)
stepw4A_S_vars <- (names(stepw4_SAD$model))
stepw4A_S_edit <- as.data.frame(gsub("^..",'',stepw4A_S_vars))

  # ###Lasso Regression###
  # numeric_data4_SAD = eclsw4A_SAD_RF[,sapply(eclsw4A_SAD_RF,is.numeric)]
  # numeric_data4_SAD = subset(numeric_data4_SAD, select=-c(P4SADLON))
  # cat_data4_SAD = eclsw4A_SAD_RF[,sapply(eclsw4A_SAD_RF,is.factor)]
  # target4_SAD = cbind(P4SADLON = eclsw4A_SAD_RF$P4SADLON,cat_data4_SAD)
  # xfactors4_SAD <- model.matrix(P4SADLON ~ ., data = target4_SAD)[,-1]
  # SAD4_data <- as.matrix(data.frame(numeric_data4_SAD, xfactors4_SAD))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit4_SAD <- glmnet(SAD4_data, eclsw4A_SAD_RF$P4SADLON, alpha=1)
  # 
  # plot(fit4_SAD,xvar="lambda",label=TRUE)
  # plot(fit4_SAD,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit4_SAD)
  # cv.lasso4_SAD=cv.glmnet(SAD4_data, eclsw4A_SAD_RF$P4SADLON)
  # plot(cv.lasso4_SAD)
  # coef(cv.lasso4_SAD)
  # summary(cv.lasso4_SAD)


########################################
#WAVE 4 WITH STUDENTS BELOW POVERTY 
########################################

eclsw4_BPvrty = eclsw4_BPvrty[-1]
eclsw4B_MSCL = subset(eclsw4_BPvrty, select=-c(C4R4RSCL,P4SADLON)) #DATA with only Math scores as target variable
eclsw4B_RSCL = subset(eclsw4_BPvrty, select=-c(C4R4MSCL,P4SADLON)) #DATA with only Reading scores as target variable
eclsw4B_SAD  = subset(eclsw4_BPvrty, select=-c(C4R4MSCL,C4R4RSCL))
str(eclsw4B_MSCL)

#### Analysis of Math Scores ####
#ATTRIBUTE SELECTION USING RANDOM FOREST# 
library(randomForest)
set.seed(1234)
fit.forest_eclsw4B <- randomForest(C4R4MSCL ~ . , data= eclsw4B_MSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw4B, type=2)
#Arrange in decreasing order
order_Brf4 <- sort(importance(fit.forest_eclsw4B, type=2)[,1], decreasing = TRUE)
order_Brf4
# Top 20 Attributes
feature_Bstringw4 <- paste(names(order_Brf4)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw4BM_impAttri <- eclsw4B_MSCL[, feature_Bstringw4]

#Adding back the target attribute
eclsw4B_MSCL_RF <- cbind(eclsw4BM_impAttri, C4R4MSCL = eclsw4B_MSCL$C4R4MSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw4_BMSCL <- stepAIC(lm(C4R4MSCL ~ ., data=eclsw4B_MSCL_RF), direction="both")
summary(stepw4_BMSCL)
stepw4B_M_vars <- (names(stepw4_BMSCL$model))
stepw4B_M_edit <- as.data.frame(gsub("^..",'',stepw4B_M_vars))

  # ###Lasso Regression###
  # numeric_data4_BMSCL = eclsw4B_MSCL_RF[,sapply(eclsw4B_MSCL_RF,is.numeric)]
  # numeric_data4_BMSCL = subset(numeric_data4_BMSCL, select=-c(C4R4MSCL))
  # cat_data4_BMSCL = eclsw4B_MSCL_RF[,sapply(eclsw4B_MSCL_RF,is.factor)]
  # target4_BMSCL = cbind(C4R4MSCL = eclsw4B_MSCL_RF$C4R4MSCL,cat_data4_BMSCL)
  # xfactors4_BMSCL <- model.matrix(C4R4MSCL ~ ., data = target4_BMSCL)[,-1]
  # BMSCL4_data <- as.matrix(data.frame(numeric_data4_BMSCL, xfactors4_BMSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit4_BMSCL <- glmnet(BMSCL4_data, eclsw4B_MSCL_RF$C4R4MSCL, alpha=1)
  # 
  # plot(fit4_BMSCL,xvar="lambda",label=TRUE)
  # plot(fit4_BMSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit4_BMSCL)
  # cv.lasso4_BMSCL=cv.glmnet(BMSCL4_data, eclsw4B_MSCL_RF$C4R4MSCL)
  # plot(cv.lasso4_BMSCL)
  # coef(cv.lasso4_BMSCL)
  # summary(cv.lasso4_BMSCL)


#### Analysis for Reading scores ####
library(randomForest)
set.seed(1234)
fit.forest_eclsw4RB <- randomForest(C4R4RSCL ~ . , data= eclsw4B_RSCL,
                                    na.action=na.roughfix,
                                    importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw4RB, type=2)
#Arrange in decreasing order
order_4rfRB <- sort(importance(fit.forest_eclsw4RB, type=2)[,1], decreasing = TRUE)
order_4rfRB
# Top 20 Attributes
feature_stringw4RB <- paste(names(order_4rfRB)[1:20])

#Data with top 40 attributes that will be used further for modeling
eclsw4RB_impAttri <- eclsw4B_RSCL[, feature_stringw4RB]

#Adding back the target attribute
eclsw4B_RSCL_RF <- cbind(eclsw4RB_impAttri, C4R4RSCL = eclsw4B_RSCL$C4R4RSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw4B_RSCL <- stepAIC(lm(C4R4RSCL ~ ., data=eclsw4B_RSCL_RF), direction="both")
summary(stepw4B_RSCL)
stepw4B_R_vars <- (names(stepw4B_RSCL$model))
stepw4B_R_edit <- as.data.frame(gsub("^..",'',stepw4B_R_vars))

  # ###Lasso Regression###
  # 
  # numeric_data4_BRSCL = eclsw4B_RSCL_RF[,sapply(eclsw4B_RSCL_RF,is.numeric)]
  # numeric_data4_BRSCL = subset(numeric_data4_BRSCL, select=-c(C4R4RSCL))
  # cat_data4_BRSCL = eclsw4B_RSCL_RF[,sapply(eclsw4B_RSCL_RF,is.factor)]
  # target4_BRSCL = cbind(C4R4RSCL = eclsw4B_RSCL_RF$C4R4RSCL,cat_data4_BRSCL)
  # xfactors4_BRSCL <- model.matrix(C4R4RSCL ~ ., data = target4_BRSCL)[,-1]
  # BRSCL4_data <- as.matrix(data.frame(numeric_data4_BRSCL, xfactors4_BRSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit4_BRSCL <- glmnet(BRSCL4_data, eclsw4B_RSCL_RF$C4R4RSCL, alpha=1)
  # 
  # plot(fit4_BRSCL,xvar="lambda",label=TRUE)
  # plot(fit4_BRSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit4_BRSCL)
  # cv.lasso4_BRSCL=cv.glmnet(BRSCL4_data, eclsw4B_RSCL_RF$C4R4RSCL)
  # plot(cv.lasso4_BRSCL)
  # coef(cv.lasso4_BRSCL)
  # summary(cv.lasso4_BRSCL)

####Analysis of P4SADLON#### 
library(randomForest)
set.seed(1234)
fit.forest_eclsw4BS <- randomForest(P4SADLON ~ . , data= eclsw4B_SAD,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw4BS, type=2)
#Arrange in decreasing order
order_rf4BS <- sort(importance(fit.forest_eclsw4BS, type=2)[,1], decreasing = TRUE)
order_rf4BS
# Top 20 Attributes
feature_stringw4BS <- paste(names(order_rf4BS)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw4BS_impAttri <- eclsw4B_SAD[, feature_stringw4BS]

#Adding back the target attribute
eclsw4B_SAD_RF <- cbind(eclsw4BS_impAttri, P4SADLON = eclsw4B_SAD$P4SADLON)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw4B_SAD <- stepAIC(lm(P4SADLON ~ ., data=eclsw4B_SAD_RF), direction="both")
summary(stepw4B_SAD)
stepw4B_S_vars <- (names(stepw4B_SAD$model))
stepw4B_S_edit <- as.data.frame(gsub("^..",'',stepw4B_S_vars))

  # ###Lasso Regression###
  # numeric_data4B_SAD = eclsw4B_SAD_RF[,sapply(eclsw4B_SAD_RF,is.numeric)]
  # numeric_data4B_SAD = subset(numeric_data4B_SAD, select=-c(P4SADLON))
  # cat_data4B_SAD = eclsw4B_SAD_RF[,sapply(eclsw4B_SAD_RF,is.factor)]
  # target4B_SAD = cbind(P4SADLON = eclsw4B_SAD_RF$P4SADLON,cat_data4B_SAD)
  # xfactors4B_SAD <- model.matrix(P4SADLON ~ ., data = target4B_SAD)[,-1]
  # BSAD4_data <- as.matrix(data.frame(numeric_data4B_SAD, xfactors4B_SAD))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit4B_SAD <- glmnet(BSAD4_data, eclsw4B_SAD_RF$P4SADLON, alpha=1)
  # 
  # plot(fit4B_SAD,xvar="lambda",label=TRUE)
  # plot(fit4B_SAD,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit4B_SAD)
  # cv.lasso4B_SAD=cv.glmnet(SAD4_data, eclsw4B_SAD_RF$P4SADLON)
  # plot(cv.lasso4B_SAD)
  # coef(cv.lasso4B_SAD)
  # summary(cv.lasso4B_SAD)
