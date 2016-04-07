rm(list=ls(all=TRUE))
eclswave5 <- read.csv("eclswave5.csv", na.strings = c("NOT APPLICABLE","REFUSED","DON'T KNOW","NOT ASCERTAINED"))
write.csv(eclswave5,"eclswave55.csv",row.names = F)
eclswave55 <- read.csv("eclswave55.csv", header = T)
str(eclswave55)
########################################
#PREPROCESSING OF DATA
########################################

#subet data into categorical and numerical variables
numeric_ecls5 = eclswave55[,sapply(eclswave55,is.numeric)]
cat_ecls5 = eclswave55[,sapply(eclswave55,is.factor)]

#Remove P5FSSCAL because this infromation is already given by attribute P5FSRAW
#numeric_ecls5 = subset(numeric_ecls5, select = -c(P5FSSCAL))

#values less than 0,so (-1 or -7, -8 or -9) replaced by NA, except for W1SESL
numeric5_no_W3SESL = numeric_ecls5[-1]
numeric5_no_W3SESL[numeric5_no_W3SESL < 0] <- NA
numeric_ecls5 = cbind(numeric_ecls5[1],numeric5_no_W3SESL)
rm(numeric5_no_W3SESL)
#combine numerical and categorical again
eclswave5_1 <- as.data.frame(cbind(cat_ecls5,numeric_ecls5))

#calculating the sum of NA's in each column
na_count5 <-sapply(eclswave5_1, function(y) sum(length(which(is.na(y))))) 
#creating a datafreclswave15the count values, it seems like many rows have a lot of Na's)
na_count5 <- data.frame(na_count5)  

#Removing columns which have NA occuring more than 30% of the time
columns_with_highNA5 <- which(colSums(is.na(eclswave5_1)) > 0.30*nrow(eclswave5_1))
eclswave5_cl <- eclswave5_1[, -(columns_with_highNA5)]
rm(columns_with_highNA5)
#identifying the indexes of rows that have greater than 20% NA's
rows_with_highNA5 <- which(rowSums(is.na(eclswave5_cl)) > 0.20*ncol(eclswave5_cl))

#removing the identified rows
eclswave5_cl <- eclswave5_cl[-(rows_with_highNA5),]
rm(rows_with_highNA5)

summary(eclswave5_cl)
rm(cat_ecls5,numeric_ecls5)
#---KNN Imputation---#
require(DMwR)
eclswave5_cl_Imput = knnImputation(eclswave5_cl)

#Split into numeric and categorical for standardization & Correlation
numeric_w5imput = eclswave5_cl_Imput[,sapply(eclswave5_cl_Imput,is.numeric)]
cat_w5imput = eclswave5_cl_Imput[,sapply(eclswave5_cl_Imput,is.factor)]
#Correlation
corw5 <- as.data.frame(cor(numeric_w5imput))
corw5 <- as.matrix(corw5)
corw5[which(abs(corw5) > 0.85)]

#Remove a couple variables that have high correlation with other attributes
numeric_w5imput = subset(numeric_w5imput, select = -c(P5LESS18,S5FLNCH))
rm(corw5)
#Seperating out target attributes before standardizing  
targetw5_vars = subset(numeric_w5imput, select = c(C5R4MSCL,C5R4RSCL,C5R2SSCL))
numeric_w5imput = subset(numeric_w5imput, select = -c(C5R4MSCL,C5R4RSCL,C5R2SSCL))

#Standardizing  
library(vegan)
numeric_w5imputstd <- decostand(numeric_w5imput, "range")
#Combining categorical,target, and standardized numerical variables
eclswave5_model <- cbind(cat_w5imput,numeric_w5imputstd,targetw5_vars)
rm(numeric_w5imput,cat_w5imput)
#splitting data based upon poverty level of student
#Data of students above poverty level
eclsw5_APvrty <- eclswave5_model[eclswave5_model$W3POVRTY == "AT OR ABOVE POVERTY THRESHOLD",]

#Data of students below poverty level
eclsw5_BPvrty <- eclswave5_model[eclswave5_model$W3POVRTY == "BELOW POVERTY THRESHOLD",]


########################################
#WAVE 5 WITH STUDENTS ABOVE POVERTY 
########################################

eclsw5_APvrty = eclsw5_APvrty[-1]
eclsw5A_MSCL = subset(eclsw5_APvrty, select=-c(C5R4RSCL,C5R2SSCL)) #DATA with only Math scores as target variable
eclsw5A_RSCL = subset(eclsw5_APvrty, select=-c(C5R4MSCL,C5R2SSCL)) #DATA with only Reading scores as target variable
eclsw5A_SSCL = subset(eclsw5_APvrty, select=-c(C5R4MSCL,C5R4RSCL))

#### Analysis of Math Scores ####
#ATTRIBUTE SELECTION USING RANDOM FOREST#
library(randomForest)
set.seed(1235)
fit.forest_eclsw5 <- randomForest(C5R4MSCL ~ . , data= eclsw5A_MSCL,
                                  na.action=na.roughfix,
                                  importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw5, type=2)
#Arrange in decreasing order
order_rf5 <- sort(importance(fit.forest_eclsw5, type=2)[,1], decreasing = TRUE)
order_rf5
# Top 20 Attributes
feature_stringw5 <- paste(names(order_rf5)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw5M_impAttri <- eclsw5A_MSCL[, feature_stringw5]

#Adding back the target attribute
eclsw5A_MSCL_RF <- cbind(eclsw5M_impAttri, C5R4MSCL = eclsw5A_MSCL$C5R4MSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw5_MSCL <- stepAIC(lm(C5R4MSCL ~ ., data=eclsw5A_MSCL_RF), direction="both")
summary(stepw5_MSCL)
stepw5A_M_vars <- (names(stepw5_MSCL$model))
stepw5A_M_edit <- gsub("^..",'',stepw5A_M_vars)

  # ###Lasso Regression###
  # numeric_data5_MSCL = eclsw5A_MSCL_RF[,sapply(eclsw5A_MSCL_RF,is.numeric)]
  # numeric_data5_MSCL = subset(numeric_data5_MSCL, select=-c(C5R4MSCL))
  # cat_data5_MSCL = eclsw5A_MSCL_RF[,sapply(eclsw5A_MSCL_RF,is.factor)]
  # target5_MSCL = cbind(C5R4MSCL = eclsw5A_MSCL_RF$C5R4MSCL,cat_data5_MSCL)
  # xfactors5_MSCL <- model.matrix(C5R4MSCL ~ ., data = target5_MSCL)[,-1]
  # MSCL5_data <- as.matrix(data.frame(numeric_data5_MSCL, xfactors5_MSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit5_MSCL <- glmnet(MSCL5_data, eclsw5A_MSCL_RF$C5R4MSCL, alpha=1)
  # 
  # plot(fit5_MSCL,xvar="lambda",label=TRUE)
  # plot(fit5_MSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit5_MSCL)
  # cv.lasso5_MSCL=cv.glmnet(MSCL5_data, eclsw5A_MSCL_RF$C5R4MSCL)
  # plot(cv.lasso5_MSCL)
  # coef(cv.lasso5_MSCL)
  # summary(cv.lasso5_MSCL)


####Analysis for Reading scores####
  library(randomForest)
  set.seed(1235)
  fit.forest_eclsw5R <- randomForest(C5R4RSCL ~ . , data= eclsw5A_RSCL,
                                     na.action=na.roughfix,
                                     importance=TRUE, ntree = 150)
  
  # following function determines variable importance
  importance(fit.forest_eclsw5R, type=2)
  #Arrange in decreasing order
  order_rf5R <- sort(importance(fit.forest_eclsw5R, type=2)[,1], decreasing = TRUE)
  order_rf5R
  # Top 20 Attributes
  feature_stringw5R <- paste(names(order_rf5R)[1:20])
  
  #Data with top 20 attributes that will be used further for modeling
  eclsw5R_impAttri <- eclsw5A_RSCL[, feature_stringw5R]
  
  #Adding back the target attribute
  eclsw5A_RSCL_RF <- cbind(eclsw5R_impAttri, C5R4RSCL = eclsw5A_RSCL$C5R4RSCL)
  
  #Performing stepAic on the top attributes selected through Random Forest
  library(MASS)
  stepw5_RSCL <- stepAIC(lm(C5R4RSCL ~ ., data=eclsw5A_RSCL_RF), direction="both")
  summary(stepw5_RSCL)
  stepw5A_R_vars <- (names(stepw5_RSCL$model))
  stepw5A_R_edit <- gsub("^..",'',stepw5A_R_vars)

  # ###Lasso Regression###
  # 
  # numeric_data5_RSCL = eclsw5A_RSCL_RF[,sapply(eclsw5A_RSCL_RF,is.numeric)]
  # numeric_data5_RSCL = subset(numeric_data5_RSCL, select=-c(C5R4RSCL))
  # cat_data5_RSCL = eclsw5A_RSCL_RF[,sapply(eclsw5A_RSCL_RF,is.factor)]
  # target5_RSCL = cbind(C5R4RSCL = eclsw5A_RSCL_RF$C5R4RSCL,cat_data5_RSCL)
  # xfactors5_RSCL <- model.matrix(C5R4RSCL ~ ., data = target5_RSCL)[,-1]
  # RSCL5_data <- as.matrix(data.frame(numeric_data5_RSCL, xfactors5_RSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit_5RSCL <- glmnet(RSCL5_data, eclsw5A_RSCL_RF$C5R4RSCL, alpha=1)
  # 
  # plot(fit_5RSCL,xvar="lambda",label=TRUE)
  # plot(fit_5RSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit_5RSCL)
  # cv.lasso5_RSCL=cv.glmnet(RSCL5_data, eclsw5A_RSCL_RF$C5R4RSCL)
  # plot(cv.lasso5_RSCL)
  # coef(cv.lasso5_RSCL)
  # summary(cv.lasso5_RSCL)

####Analysis for Science scores####
library(randomForest)
set.seed(1234)
fit.forest_eclsw5S <- randomForest(C5R2SSCL ~ . , data= eclsw5A_SSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw5S, type=2)
#Arrange in decreasing order
order_rf5S <- sort(importance(fit.forest_eclsw5S, type=2)[,1], decreasing = TRUE)
order_rf5S
# Top 20 Attributes
feature_stringw5S <- paste(names(order_rf5S)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw5S_impAttri <- eclsw5A_SSCL[, feature_stringw5S]

#Adding back the target attribute
eclsw5A_SSCL_RF <- cbind(eclsw5S_impAttri, C5R2SSCL = eclsw5A_SSCL$C5R2SSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw5_SSCL <- stepAIC(lm(C5R2SSCL ~ ., data=eclsw5A_SSCL_RF), direction="both")
summary(stepw5_SSCL)
stepw5A_SC_vars <- (names(stepw5_SSCL$model))
stepw5A_SC_edit <- as.data.frame(gsub("^..",'',stepw5A_SC_vars))

  # ###Lasso Regression###
  # 
  # numeric_data5_SSCL = eclsw5A_SSCL_RF[,sapply(eclsw5A_SSCL_RF,is.numeric)]
  # numeric_data5_SSCL = subset(numeric_data5_SSCL, select=-c(C5R2SSCL))
  # cat_data5_SSCL = eclsw5A_SSCL_RF[,sapply(eclsw5A_SSCL_RF,is.factor)]
  # target5_SSCL = cbind(C5R2SSCL = eclsw5A_SSCL_RF$C5R2SSCL,cat_data5_SSCL)
  # xfactors5_SSCL <- model.matrix(C5R2SSCL ~ ., data = target5_SSCL)[,-1]
  # SSCL5_data <- as.matrix(data.frame(numeric_data5_SSCL, xfactors5_SSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit_5SSCL <- glmnet(SSCL5_data, eclsw5A_SSCL_RF$C5R2SSCL, alpha=1)
  # 
  # plot(fit_5SSCL,xvar="lambda",label=TRUE)
  # plot(fit_5SSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit_5SSCL)
  # cv.lasso5_SSCL=cv.glmnet(SSCL5_data, eclsw5A_SSCL_RF$C5R2SSCL)
  # plot(cv.lasso5_SSCL)
  # coef(cv.lasso5_SSCL)
  # summary(cv.lasso5_SSCL)
########################################
#WAVE 5 WITH STUDENTS BELOW POVERTY 
########################################
eclsw5_BPvrty = eclsw5_BPvrty[-1]
eclsw5B_MSCL = subset(eclsw5_BPvrty, select=-c(C5R4RSCL,C5R2SSCL)) #DATA with only Math scores as target variable
eclsw5B_RSCL = subset(eclsw5_BPvrty, select=-c(C5R4MSCL,C5R2SSCL)) #DATA with only Reading scores as target variable
eclsw5B_SSCL = subset(eclsw5_BPvrty, select=-c(C5R4MSCL,C5R4RSCL))

#### Analysis of Math Scores ####
#ATTRIBUTE SELECTION USING RANDOM FOREST# 
library(randomForest)
set.seed(1234)
fit.forest_eclsw5B <- randomForest(C5R4MSCL ~ . , data= eclsw5B_MSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw5B, type=2)
#Arrange in decreasing order
order_Brf5 <- sort(importance(fit.forest_eclsw5B, type=2)[,1], decreasing = TRUE)
order_Brf5
# Top 20 Attributes
feature_Bstringw5 <- paste(names(order_Brf5)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw5BM_impAttri <- eclsw5B_MSCL[, feature_Bstringw5]

#Adding back the target attribute
eclsw5B_MSCL_RF <- cbind(eclsw5BM_impAttri, C5R4MSCL = eclsw5B_MSCL$C5R4MSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw5_BMSCL <- stepAIC(lm(C5R4MSCL ~ ., data=eclsw5B_MSCL_RF), direction="both")
summary(stepw5_BMSCL)
stepw5B_M_vars <- (names(stepw5_BMSCL$model))
stepw5B_M_edit <- as.data.frame(gsub("^..",'',stepw5B_M_vars))

  # ###Lasso Regression###
  # numeric_data5_BMSCL = eclsw5B_MSCL_RF[,sapply(eclsw5B_MSCL_RF,is.numeric)]
  # numeric_data5_BMSCL = subset(numeric_data5_BMSCL, select=-c(C5R4MSCL))
  # cat_data5_BMSCL = eclsw5B_MSCL_RF[,sapply(eclsw5B_MSCL_RF,is.factor)]
  # target5_BMSCL = cbind(C5R4MSCL = eclsw5B_MSCL_RF$C5R4MSCL,cat_data5_BMSCL)
  # xfactors5_BMSCL <- model.matrix(C5R4MSCL ~ ., data = target5_BMSCL)[,-1]
  # BMSCL5_data <- as.matrix(data.frame(numeric_data5_BMSCL, xfactors5_BMSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit5_BMSCL <- glmnet(BMSCL5_data, eclsw5B_MSCL_RF$C5R4MSCL, alpha=1)
  # 
  # plot(fit5_BMSCL,xvar="lambda",label=TRUE)
  # plot(fit5_BMSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit5_BMSCL)
  # cv.lasso5_BMSCL=cv.glmnet(BMSCL5_data, eclsw5B_MSCL_RF$C5R4MSCL)
  # plot(cv.lasso5_BMSCL)
  # coef(cv.lasso5_BMSCL)
  # summary(cv.lasso5_BMSCL)


####Analysis for Reading scores####
library(randomForest)
set.seed(1234)
fit.forest_eclsw5RB <- randomForest(C5R4RSCL ~ . , data= eclsw5B_RSCL,
                                    na.action=na.roughfix,
                                    importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw5RB, type=2)
#Arrange in decreasing order
order_5rfRB <- sort(importance(fit.forest_eclsw5RB, type=2)[,1], decreasing = TRUE)
order_5rfRB
# Top 20 Attributes
feature_stringw5RB <- paste(names(order_5rfRB)[1:20])

#Data with top 50 attributes that will be used further for modeling
eclsw5RB_impAttri <- eclsw5B_RSCL[, feature_stringw5RB]

#Adding back the target attribute
eclsw5B_RSCL_RF <- cbind(eclsw5RB_impAttri, C5R4RSCL = eclsw5B_RSCL$C5R4RSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw5B_RSCL <- stepAIC(lm(C5R4RSCL ~ ., data=eclsw5B_RSCL_RF), direction="both")
summary(stepw5B_RSCL)
stepw5B_R_vars <- (names(stepw5B_RSCL$model))
stepw5B_R_edit <- as.data.frame(gsub("^..",'',stepw5B_R_vars))

  # ###Lasso Regression###
  # 
  # numeric_data5_BRSCL = eclsw5B_RSCL_RF[,sapply(eclsw5B_RSCL_RF,is.numeric)]
  # numeric_data5_BRSCL = subset(numeric_data5_BRSCL, select=-c(C5R4RSCL))
  # cat_data5_BRSCL = eclsw5B_RSCL_RF[,sapply(eclsw5B_RSCL_RF,is.factor)]
  # target5_BRSCL = cbind(C5R4RSCL = eclsw5B_RSCL_RF$C5R4RSCL,cat_data5_BRSCL)
  # xfactors5_BRSCL <- model.matrix(C5R4RSCL ~ ., data = target5_BRSCL)[,-1]
  # BRSCL5_data <- as.matrix(data.frame(numeric_data5_BRSCL, xfactors5_BRSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit5_BRSCL <- glmnet(BRSCL5_data, eclsw5B_RSCL_RF$C5R4RSCL, alpha=1)
  # 
  # plot(fit5_BRSCL,xvar="lambda",label=TRUE)
  # plot(fit5_BRSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit5_BRSCL)
  # cv.lasso5_BRSCL=cv.glmnet(BRSCL5_data, eclsw5B_RSCL_RF$C5R4RSCL)
  # plot(cv.lasso5_BRSCL)
  # coef(cv.lasso5_BRSCL)
  # summary(cv.lasso5_BRSCL)

#-----Analysis for Science scores-----#
library(randomForest)
set.seed(1234)
fit.forest_eclsw5SB <- randomForest(C5R2SSCL ~ . , data= eclsw5B_SSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw5SB, type=2)
#Arrange in decreasing order
order_rf5SB <- sort(importance(fit.forest_eclsw5SB, type=2)[,1], decreasing = TRUE)
order_rf5SB
# Top 20 Attributes
feature_stringw5SB <- paste(names(order_rf5SB)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw5SB_impAttri <- eclsw5B_SSCL[, feature_stringw5SB]

#Adding back the target attribute
eclsw5B_SSCL_RF <- cbind(eclsw5SB_impAttri, C5R2SSCL = eclsw5B_SSCL$C5R2SSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw5B_SSCL <- stepAIC(lm(C5R2SSCL ~ ., data=eclsw5B_SSCL_RF), direction="both")
summary(stepw5B_SSCL)
stepw5B_SC_vars <- (names(stepw5B_SSCL$model))
stepw5B_SC_edit <- as.data.frame(gsub("^..",'',stepw5B_SC_vars))

  # ###Lasso Regression###
  # 
  # numeric_data5B_SSCL = eclsw5B_SSCL_RF[,sapply(eclsw5B_SSCL_RF,is.numeric)]
  # numeric_data5B_SSCL = subset(numeric_data5B_SSCL, select=-c(C5R2SSCL))
  # cat_data5B_SSCL = eclsw5B_SSCL_RF[,sapply(eclsw5B_SSCL_RF,is.factor)]
  # target5B_SSCL = cbind(C5R2SSCL = eclsw5B_SSCL_RF$C5R2SSCL,cat_data5B_SSCL)
  # xfactors5B_SSCL <- model.matrix(C5R2SSCL ~ ., data = target5B_SSCL)[,-1]
  # SSCL5B_data <- as.matrix(data.frame(numeric_data5B_SSCL, xfactors5B_SSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fitB_5SSCL <- glmnet(SSCL5B_data, eclsw5B_SSCL_RF$C5R2SSCL, alpha=1)
  # 
  # plot(fitB_5SSCL,xvar="lambda",label=TRUE)
  # plot(fitB_5SSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fitB_5SSCL)
  # cv.lasso5B_SSCL=cv.glmnet(SSCL5B_data, eclsw5B_SSCL_RF$C5R2SSCL)
  # plot(cv.lasso5B_SSCL)
  # coef(cv.lasso5B_SSCL)
  # summary(cv.lasso5B_SSCL)
