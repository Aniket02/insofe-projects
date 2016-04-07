rm(list=ls(all=TRUE))
eclswave6 <- read.csv("eclswave6.csv", na.strings = c("NOT APPLICABLE","REFUSED","DON'T KNOW","NOT ASCERTAINED"))
write.csv(eclswave6,"eclswave66.csv",row.names = F)
eclswave66 <- read.csv("eclswave66.csv", header = T)
str(eclswave66)
rm(eclswave6)
########################################
#PREPROCESSING OF DATA
########################################

#subet data into categorical and numerical variables
numeric_ecls6 = eclswave66[,sapply(eclswave66,is.numeric)]
cat_ecls6 = eclswave66[,sapply(eclswave66,is.factor)]

#Remove P6FSSCAL because this infromation is already given by attribute P6FSRAW
#numeric_ecls6 = subset(numeric_ecls6, select = -c(P6FSSCAL))

#values less than 0,so (-1 or -7, -8 or -9) replaced by NA, except for W1SESL
numeric6_no_W5SESL = numeric_ecls6[-1]
numeric6_no_W5SESL[numeric6_no_W5SESL < 0] <- NA
numeric_ecls6 = cbind(numeric_ecls6[1],numeric6_no_W5SESL)
rm(numeric6_no_W5SESL)
#combine numerical and categorical again
eclswave6_1 <- as.data.frame(cbind(cat_ecls6,numeric_ecls6))

#calculating the sum of NA's in each column
na_count6 <-sapply(eclswave6_1, function(y) sum(length(which(is.na(y))))) 
#creating a datafreclswave16the count values, it seems like many rows have a lot of Na's)
na_count6 <- data.frame(na_count6)  

#Removing columns which have NA occuring more than 30% of the time
columns_with_highNA6 <- which(colSums(is.na(eclswave6_1)) > 0.30*nrow(eclswave6_1))
eclswave6_cl <- eclswave6_1[, -(columns_with_highNA6)]
rm(columns_with_highNA6)
#identifying the indexes of rows that have greater than 20% NA's
rows_with_highNA6 <- which(rowSums(is.na(eclswave6_cl)) > 0.20*ncol(eclswave6_cl))

#removing the identified rows
eclswave6_cl <- eclswave6_cl[-(rows_with_highNA6),]
rm(rows_with_highNA6)

summary(eclswave6_cl)
rm(cat_ecls6,numeric_ecls6)
#---KNN Imputation---#
require(DMwR)
eclswave6_cl_Imput = knnImputation(eclswave6_cl)

#Split into numeric and categorical for standardization & Correlation
numeric_w6imput = eclswave6_cl_Imput[,sapply(eclswave6_cl_Imput,is.numeric)]
cat_w6imput = eclswave6_cl_Imput[,sapply(eclswave6_cl_Imput,is.factor)]
#Correlation
corw6 <- as.data.frame(cor(numeric_w6imput))
corw6 <- as.matrix(corw6)
corw6[which(abs(corw6) > 0.86)]

#Remove a couple variables that have high correlation with other attributes
numeric_w6imput = subset(numeric_w6imput, select = -c(P6LESS18))
rm(corw6)
#Seperating out target attributes before standardizing  
targetw6_vars = subset(numeric_w6imput, select = c(C6R4MSCL,C6R4RSCL,C6R2SSCL))
numeric_w6imput = subset(numeric_w6imput, select = -c(C6R4MSCL,C6R4RSCL,C6R2SSCL))

#Standardizing  
library(vegan)
numeric_w6imputstd <- decostand(numeric_w6imput, "range")
#Combining categorical,target, and standardized numerical variables
eclswave6_model <- cbind(cat_w6imput,numeric_w6imputstd,targetw6_vars)
rm(numeric_w6imput,cat_w6imput)
#splitting data based upon poverty level of student
#Data of students above poverty level
eclsw6_APvrty <- eclswave6_model[eclswave6_model$W5POVRTY == "AT OR ABOVE POVERTY THRESHOLD",]

#Data of students below poverty level
eclsw6_BPvrty <- eclswave6_model[eclswave6_model$W5POVRTY == "BELOW POVERTY THRESHOLD",]


########################################
#WAVE 6 WITH STUDENTS ABOVE POVERTY 
########################################

eclsw6_APvrty = eclsw6_APvrty[-1]
eclsw6A_MSCL = subset(eclsw6_APvrty, select=-c(C6R4RSCL,C6R2SSCL)) #DATA with only Math scores as target variable
eclsw6A_RSCL = subset(eclsw6_APvrty, select=-c(C6R4MSCL,C6R2SSCL)) #DATA with only Reading scores as target variable
eclsw6A_SSCL = subset(eclsw6_APvrty, select=-c(C6R4MSCL,C6R4RSCL))

#### Analysis of Math Scores ####
#ATTRIBUTE SELECTION USING RANDOM FOREST# 
library(randomForest)
set.seed(1234)
fit.forest_eclsw6 <- randomForest(C6R4MSCL ~ . , data= eclsw6A_MSCL,
                                  na.action=na.roughfix,
                                  importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw6, type=2)
#Arrange in decreasing order
order_rf6 <- sort(importance(fit.forest_eclsw6, type=2)[,1], decreasing = TRUE)
order_rf6
# Top 20 Attributes
feature_stringw6 <- paste(names(order_rf6)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw6M_impAttri <- eclsw6A_MSCL[, feature_stringw6]

#Adding back the target attribute
eclsw6A_MSCL_RF <- cbind(eclsw6M_impAttri, C6R4MSCL = eclsw6A_MSCL$C6R4MSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw6_MSCL <- stepAIC(lm(C6R4MSCL ~ ., data=eclsw6A_MSCL_RF), direction="both")
summary(stepw6_MSCL)
stepw6A_M_vars <- (names(stepw6_MSCL$model))
stepw6A_M_edit <- gsub("^..",'',stepw6A_M_vars)
###Lasso Regression###
numeric_data6_MSCL = eclsw6A_MSCL_RF[,sapply(eclsw6A_MSCL_RF,is.numeric)]
numeric_data6_MSCL = subset(numeric_data6_MSCL, select=-c(C6R4MSCL))
cat_data6_MSCL = eclsw6A_MSCL_RF[,sapply(eclsw6A_MSCL_RF,is.factor)]
target6_MSCL = cbind(C6R4MSCL = eclsw6A_MSCL_RF$C6R4MSCL,cat_data6_MSCL)
xfactors6_MSCL <- model.matrix(C6R4MSCL ~ ., data = target6_MSCL)[,-1]
MSCL6_data <- as.matrix(data.frame(numeric_data6_MSCL, xfactors6_MSCL))

  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit6_MSCL <- glmnet(MSCL6_data, eclsw6A_MSCL_RF$C6R4MSCL, alpha=1)
  # 
  # plot(fit6_MSCL,xvar="lambda",label=TRUE)
  # plot(fit6_MSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit6_MSCL)
  # cv.lasso6_MSCL=cv.glmnet(MSCL6_data, eclsw6A_MSCL_RF$C6R4MSCL)
  # plot(cv.lasso6_MSCL)
  # coef(cv.lasso6_MSCL)
  # summary(cv.lasso6_MSCL)


####Analysis for Reading score####
library(randomForest)
set.seed(1236)
fit.forest_eclsw6R <- randomForest(C6R4RSCL ~ . , data= eclsw6A_RSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw6R, type=2)
#Arrange in decreasing order
order_rf6R <- sort(importance(fit.forest_eclsw6R, type=2)[,1], decreasing = TRUE)
order_rf6R
# Top 20 Attributes
feature_stringw6R <- paste(names(order_rf6R)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw6R_impAttri <- eclsw6A_RSCL[, feature_stringw6R]

#Adding back the target attribute
eclsw6A_RSCL_RF <- cbind(eclsw6R_impAttri, C6R4RSCL = eclsw6A_RSCL$C6R4RSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw6_RSCL <- stepAIC(lm(C6R4RSCL ~ ., data=eclsw6A_RSCL_RF), direction="both")
summary(stepw6_RSCL)
stepw6A_R_vars <- (names(stepw6_RSCL$model))
stepw6A_R_edit <- gsub("^..",'',stepw6A_R_vars)

  # ###Lasso Regression###
  # numeric_data6_RSCL = eclsw6A_RSCL_RF[,sapply(eclsw6A_RSCL_RF,is.numeric)]
  # numeric_data6_RSCL = subset(numeric_data6_RSCL, select=-c(C6R4RSCL))
  # cat_data6_RSCL = eclsw6A_RSCL_RF[,sapply(eclsw6A_RSCL_RF,is.factor)]
  # target6_RSCL = cbind(C6R4RSCL = eclsw6A_RSCL_RF$C6R4RSCL,cat_data6_RSCL)
  # xfactors6_RSCL <- model.matrix(C6R4RSCL ~ ., data = target6_RSCL)[,-1]
  # RSCL6_data <- as.matrix(data.frame(numeric_data6_RSCL, xfactors6_RSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit_6RSCL <- glmnet(RSCL6_data, eclsw6A_RSCL_RF$C6R4RSCL, alpha=1)
  # 
  # plot(fit_6RSCL,xvar="lambda",label=TRUE)
  # plot(fit_6RSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit_6RSCL)
  # cv.lasso6_RSCL=cv.glmnet(RSCL6_data, eclsw6A_RSCL_RF$C6R4RSCL)
  # plot(cv.lasso6_RSCL)
  # coef(cv.lasso6_RSCL)
  # summary(cv.lasso6_RSCL)

####Analysis for Science score####
library(randomForest)
set.seed(1234)
fit.forest_eclsw6S <- randomForest(C6R2SSCL ~ . , data= eclsw6A_SSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 160)

# following function determines variable importance
importance(fit.forest_eclsw6S, type=2)
#Arrange in decreasing order
order_rf6S <- sort(importance(fit.forest_eclsw6S, type=2)[,1], decreasing = TRUE)
order_rf6S
# Top 20 Attributes
feature_stringw6S <- paste(names(order_rf6S)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw6S_impAttri <- eclsw6A_SSCL[, feature_stringw6S]

#Adding back the target attribute
eclsw6A_SSCL_RF <- cbind(eclsw6S_impAttri, C6R2SSCL = eclsw6A_SSCL$C6R2SSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw6_SSCL <- stepAIC(lm(C6R2SSCL ~ ., data=eclsw6A_SSCL_RF), direction="both")
summary(stepw6_SSCL)
stepw6A_SC_vars <- (names(stepw6_SSCL$model))
stepw6A_SC_edit <- as.data.frame(gsub("^..",'',stepw6A_SC_vars))

  # ###Lasso Regression###
  # 
  # numeric_data6_SSCL = eclsw6A_SSCL_RF[,sapply(eclsw6A_SSCL_RF,is.numeric)]
  # numeric_data6_SSCL = subset(numeric_data6_SSCL, select=-c(C6R2SSCL))
  # cat_data6_SSCL = eclsw6A_SSCL_RF[,sapply(eclsw6A_SSCL_RF,is.factor)]
  # target6_SSCL = cbind(C6R2SSCL = eclsw6A_SSCL_RF$C6R2SSCL,cat_data6_SSCL)
  # xfactors6_SSCL <- model.matrix(C6R2SSCL ~ ., data = target6_SSCL)[,-1]
  # SSCL6_data <- as.matrix(data.frame(numeric_data6_SSCL, xfactors6_SSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit_6SSCL <- glmnet(SSCL6_data, eclsw6A_SSCL_RF$C6R2SSCL, alpha=1)
  # 
  # plot(fit_6SSCL,xvar="lambda",label=TRUE)
  # plot(fit_6SSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit_6SSCL)
  # cv.lasso6_SSCL=cv.glmnet(SSCL6_data, eclsw6A_SSCL_RF$C6R2SSCL)
  # plot(cv.lasso6_SSCL)
  # coef(cv.lasso6_SSCL)
  # summary(cv.lasso6_SSCL)
########################################
#WAVE 6 WITH STUDENTS BELOW POVERTY 
########################################

eclsw6_BPvrty = eclsw6_BPvrty[-1]
eclsw6B_MSCL = subset(eclsw6_BPvrty, select=-c(C6R4RSCL,C6R2SSCL)) #DATA with only Math scores as target variable
eclsw6B_RSCL = subset(eclsw6_BPvrty, select=-c(C6R4MSCL,C6R2SSCL)) #DATA with only Reading scores as target variable
eclsw6B_SSCL = subset(eclsw6_BPvrty, select=-c(C6R4MSCL,C6R4RSCL))

#### Analysis of Math Scores ####
#ATTRIBUTE SELECTION USING RANDOM FOREST# 
library(randomForest)
set.seed(1234)
fit.forest_eclsw6B <- randomForest(C6R4MSCL ~ . , data= eclsw6B_MSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 160)

# following function determines variable importance
importance(fit.forest_eclsw6B, type=2)
#Arrange in decreasing order
order_Brf6 <- sort(importance(fit.forest_eclsw6B, type=2)[,1], decreasing = TRUE)
order_Brf6
# Top 20 Attributes
feature_Bstringw6 <- paste(names(order_Brf6)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw6BM_impAttri <- eclsw6B_MSCL[, feature_Bstringw6]

#Adding back the target attribute
eclsw6B_MSCL_RF <- cbind(eclsw6BM_impAttri, C6R4MSCL = eclsw6B_MSCL$C6R4MSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw6_BMSCL <- stepAIC(lm(C6R4MSCL ~ ., data=eclsw6B_MSCL_RF), direction="both")
summary(stepw6_BMSCL)
stepw6B_M_vars <- (names(stepw6_BMSCL$model))
stepw6B_M_edit <- as.data.frame(gsub("^..",'',stepw6B_M_vars))

  # ###Lasso Regression###
  # numeric_data6_BMSCL = eclsw6B_MSCL_RF[,sapply(eclsw6B_MSCL_RF,is.numeric)]
  # numeric_data6_BMSCL = subset(numeric_data6_BMSCL, select=-c(C6R4MSCL))
  # cat_data6_BMSCL = eclsw6B_MSCL_RF[,sapply(eclsw6B_MSCL_RF,is.factor)]
  # target6_BMSCL = cbind(C6R4MSCL = eclsw6B_MSCL_RF$C6R4MSCL,cat_data6_BMSCL)
  # xfactors6_BMSCL <- model.matrix(C6R4MSCL ~ ., data = target6_BMSCL)[,-1]
  # BMSCL6_data <- as.matrix(data.frame(numeric_data6_BMSCL, xfactors6_BMSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit6_BMSCL <- glmnet(BMSCL6_data, eclsw6B_MSCL_RF$C6R4MSCL, alpha=1)
  # 
  # plot(fit6_BMSCL,xvar="lambda",label=TRUE)
  # plot(fit6_BMSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit6_BMSCL)
  # cv.lasso6_BMSCL=cv.glmnet(BMSCL6_data, eclsw6B_MSCL_RF$C6R4MSCL)
  # plot(cv.lasso6_BMSCL)
  # coef(cv.lasso6_BMSCL)
  # summary(cv.lasso6_BMSCL)


####Analysis for Reading scores####
library(randomForest)
set.seed(1234)
fit.forest_eclsw6RB <- randomForest(C6R4RSCL ~ . , data= eclsw6B_RSCL,
                                    na.action=na.roughfix,
                                    importance=TRUE, ntree = 160)

# following function determines variable importance
importance(fit.forest_eclsw6RB, type=2)
#Arrange in decreasing order
order_6rfRB <- sort(importance(fit.forest_eclsw6RB, type=2)[,1], decreasing = TRUE)
order_6rfRB
# Top 20 Attributes
feature_stringw6RB <- paste(names(order_6rfRB)[1:20])

#Data with top 60 attributes that will be used further for modeling
eclsw6RB_impAttri <- eclsw6B_RSCL[, feature_stringw6RB]

#Adding back the target attribute
eclsw6B_RSCL_RF <- cbind(eclsw6RB_impAttri, C6R4RSCL = eclsw6B_RSCL$C6R4RSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw6B_RSCL <- stepAIC(lm(C6R4RSCL ~ ., data=eclsw6B_RSCL_RF), direction="both")
summary(stepw6B_RSCL)
stepw6B_R_vars <- (names(stepw6B_RSCL$model))
stepw6B_R_edit <- as.data.frame(gsub("^..",'',stepw6B_R_vars))

  # ###Lasso Regression###
  # 
  # numeric_data6_BRSCL = eclsw6B_RSCL_RF[,sapply(eclsw6B_RSCL_RF,is.numeric)]
  # numeric_data6_BRSCL = subset(numeric_data6_BRSCL, select=-c(C6R4RSCL))
  # cat_data6_BRSCL = eclsw6B_RSCL_RF[,sapply(eclsw6B_RSCL_RF,is.factor)]
  # target6_BRSCL = cbind(C6R4RSCL = eclsw6B_RSCL_RF$C6R4RSCL,cat_data6_BRSCL)
  # xfactors6_BRSCL <- model.matrix(C6R4RSCL ~ ., data = target6_BRSCL)[,-1]
  # BRSCL6_data <- as.matrix(data.frame(numeric_data6_BRSCL, xfactors6_BRSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit6_BRSCL <- glmnet(BRSCL6_data, eclsw6B_RSCL_RF$C6R4RSCL, alpha=1)
  # 
  # plot(fit6_BRSCL,xvar="lambda",label=TRUE)
  # plot(fit6_BRSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit6_BRSCL)
  # cv.lasso6_BRSCL=cv.glmnet(BRSCL6_data, eclsw6B_RSCL_RF$C6R4RSCL)
  # plot(cv.lasso6_BRSCL)
  # coef(cv.lasso6_BRSCL)
  # summary(cv.lasso6_BRSCL)

####Analysis for Science scores####
library(randomForest)
set.seed(1234)
fit.forest_eclsw6SB <- randomForest(C6R2SSCL ~ . , data= eclsw6B_SSCL,
                                    na.action=na.roughfix,
                                    importance=TRUE, ntree = 160)

# following function determines variable importance
importance(fit.forest_eclsw6SB, type=2)
#Arrange in decreasing order
order_rf6SB <- sort(importance(fit.forest_eclsw6SB, type=2)[,1], decreasing = TRUE)
order_rf6SB
# Top 20 Attributes
feature_stringw6SB <- paste(names(order_rf6SB)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw6SB_impAttri <- eclsw6B_SSCL[, feature_stringw6SB]

#Adding back the target attribute
eclsw6B_SSCL_RF <- cbind(eclsw6SB_impAttri, C6R2SSCL = eclsw6B_SSCL$C6R2SSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw6B_SSCL <- stepAIC(lm(C6R2SSCL ~ ., data=eclsw6B_SSCL_RF), direction="both")
summary(stepw6B_SSCL)
stepw6B_SC_vars <- (names(stepw6B_SSCL$model))
stepw6B_SC_edit <- as.data.frame(gsub("^..",'',stepw6B_SC_vars))

  # ###Lasso Regression###
  # 
  # numeric_data6B_SSCL = eclsw6B_SSCL_RF[,sapply(eclsw6B_SSCL_RF,is.numeric)]
  # numeric_data6B_SSCL = subset(numeric_data6B_SSCL, select=-c(C6R2SSCL))
  # cat_data6B_SSCL = eclsw6B_SSCL_RF[,sapply(eclsw6B_SSCL_RF,is.factor)]
  # target6B_SSCL = cbind(C6R2SSCL = eclsw6B_SSCL_RF$C6R2SSCL,cat_data6B_SSCL)
  # xfactors6B_SSCL <- model.matrix(C6R2SSCL ~ ., data = target6B_SSCL)[,-1]
  # SSCL6B_data <- as.matrix(data.frame(numeric_data6B_SSCL, xfactors6B_SSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fitB_6SSCL <- glmnet(SSCL6B_data, eclsw6B_SSCL_RF$C6R2SSCL, alpha=1)
  # 
  # plot(fitB_6SSCL,xvar="lambda",label=TRUE)
  # plot(fitB_6SSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fitB_6SSCL)
  # cv.lasso6B_SSCL=cv.glmnet(SSCL6B_data, eclsw6B_SSCL_RF$C6R2SSCL)
  # plot(cv.lasso6B_SSCL)
  # coef(cv.lasso6B_SSCL)
  # summary(cv.lasso6B_SSCL)