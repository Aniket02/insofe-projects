rm(list=ls(all=TRUE))
eclswave2 <- read.csv("eclswave2.csv", na.strings = c("NOT APPLICABLE","REFUSED","DON'T KNOW","NOT ASCERTAINED"))
write.csv(eclswave2,"eclswave22.csv",row.names = F)
eclswave22 <- read.csv("eclswave22.csv", header = T)
str(eclswave22)

######################
#PREPROCESSING OF DATA
######################

#subet data into categorical and numerical variables
numeric_ecls2 = eclswave22[,sapply(eclswave22,is.numeric)]
cat_ecls2 = eclswave22[,sapply(eclswave22,is.factor)]

#Remove P2FSSCAL because this infromation is already given by attribute P2FSRAW
numeric_ecls2 = subset(numeric_ecls2, select = -c(P2FSSCAL))

#values less than 0,so (-1 or -7, -8 or -9) replaced by NA, except for WKSESL
numeric2_no_WKSESL = numeric_ecls2[-1]
numeric2_no_WKSESL[numeric2_no_WKSESL < 0] <- NA
numeric_ecls2 = cbind(numeric_ecls2[1],numeric2_no_WKSESL)
rm(numeric2_no_WKSESL)
#combine numerical and categorical again
eclswave2_1 <- as.data.frame(cbind(cat_ecls2,numeric_ecls2))

#calculating the sum of NA's in each column
na_count2 <-sapply(eclswave2_1, function(y) sum(length(which(is.na(y))))) 
#creating a datafreclswave12the count values, it seems like many rows have a lot of Na's)
na_count2 <- data.frame(na_count2)  

#Removing columns which have NA occuring more than 30% of the time
columns_with_highNA2 <- which(colSums(is.na(eclswave2_1)) > 0.30*nrow(eclswave2_1))
eclswave2_cl <- eclswave2_1[, -(columns_with_highNA2)]
rm(columns_with_highNA2)
#identifying the indexes of rows that have greater than 20% NA's
rows_with_highNA2 <- which(rowSums(is.na(eclswave2_cl)) > 0.20*ncol(eclswave2_cl))

#removing the identified rows
eclswave2_cl <- eclswave2_cl[-(rows_with_highNA2),]
rm(rows_with_highNA2)

summary(eclswave2_cl)
rm(cat_ecls2,numeric_ecls2)

####KNN Imputation, and Standardization####
  require(DMwR)
  eclswave2_cl_Imput = knnImputation(eclswave2_cl)
  
  #Split into numeric and categorical for standardization & Correlation
  numeric_w2imput = eclswave2_cl_Imput[,sapply(eclswave2_cl_Imput,is.numeric)]
  cat_w2imput = eclswave2_cl_Imput[,sapply(eclswave2_cl_Imput,is.factor)]
  #Correlation
  corw2 <- as.data.frame(cor(numeric_w2imput))
  corw2 <- as.matrix(corw2)
  corw2[which(abs(corw2) > 0.85)]
  
  #Remove a couple variables that have high correlation with other attributes
  numeric_w2imput = subset(numeric_w2imput, select = -c(P2LESS18))
  rm(corw2)
  #Seperating out target attributes before standardizing  
  targetw2_vars = subset(numeric_w2imput, select = c(C2R4MSCL,C2R4RSCL))
  numeric_w2imput = subset(numeric_w2imput, select = -c(C2R4MSCL,C2R4RSCL,C2RGSCAL))
  
  #Standardizing  
  library(vegan)
  numeric_w2imputstd <- decostand(numeric_w2imput, "range")
  #Combining categorical,target, and standardized numerical variables
  eclswave2_model <- cbind(cat_w2imput,numeric_w2imputstd,targetw2_vars)
  rm(numeric_w2imput,cat_w2imput)
  
####splitting data based upon poverty level of student####
  
  #Data of students above poverty level
  eclsw2_APvrty <- eclswave2_model[eclswave2_model$WKPOV_R == "AT OR ABOVE POVERTY THRESHOLD",]
  
  #Data of students below poverty level
  eclsw2_BPvrty <- eclswave2_model[eclswave2_model$WKPOV_R == "BELOW POVERTY THRESHOLD",]


########################################
#WAVE 2 WITH STUDENTS ABOVE POVERTY 
########################################

eclsw2_APvrty = eclsw2_APvrty[-1]
eclsw2A_MSCL = subset(eclsw2_APvrty, select=-c(C2R4RSCL,P2SADLON)) #DATA with only Math scores as target variable
eclsw2A_RSCL = subset(eclsw2_APvrty, select=-c(C2R4MSCL,P2SADLON)) #DATA with only Reading scores as target variable
eclsw2A_SAD  = subset(eclsw2_APvrty, select=-c(C2R4MSCL,C2R4RSCL)) #Data with only SAD scores as target variable


####Ananlysis of Math Scores####
  #ATTRIBUTE SELECTION USING RANDOM FOREST 
  library(randomForest)
  set.seed(1234)
  fit.forest_eclsw2 <- randomForest(C2R4MSCL ~ . , data= eclsw2A_MSCL,
                                    na.action=na.roughfix,
                                    importance=TRUE, ntree = 150)
  
  # following function determines variable importance
  importance(fit.forest_eclsw2, type=2)
  #Arrange in decreasing order
  order_rf2 <- sort(importance(fit.forest_eclsw2, type=2)[,1], decreasing = TRUE)
  order_rf2
  # Top 20 Attributes
  feature_stringw2 <- paste(names(order_rf2)[1:20])
  
  #Data with top 20 attributes that will be used further for modeling
  eclsw2M_impAttri <- eclsw2A_MSCL[, feature_stringw2]
  
  #Adding back the target attribute
  eclsw2A_MSCL_RF <- cbind(eclsw2M_impAttri, C2R4MSCL = eclsw2A_MSCL$C2R4MSCL)
  
  #Performing stepAic on the top attributes selected through Random Forest
  library(MASS)
  stepw2_MSCL <- stepAIC(lm(C2R4MSCL ~ ., data=eclsw2A_MSCL_RF), direction="both")
  summary(stepw2_MSCL)
  stepw2A_M_vars <-(names(stepw2_MSCL$model))
  stepw2A_M_edit <- gsub("^..",'',stepw2A_M_vars)
  
#   ###Lasso Regression###
#   numeric_data2_MSCL = eclsw2A_MSCL_RF[,sapply(eclsw2A_MSCL_RF,is.numeric)]
#   numeric_data2_MSCL = subset(numeric_data2_MSCL, select=-c(C2R4MSCL))
#   cat_data2_MSCL = eclsw2A_MSCL_RF[,sapply(eclsw2A_MSCL_RF,is.factor)]
#   target2_MSCL = cbind(C2R4MSCL = eclsw2A_MSCL_RF$C2R4MSCL,cat_data2_MSCL)
#   xfactors2_MSCL <- model.matrix(C2R4MSCL ~ ., data = target2_MSCL)[,-1]
#   MSCL2_data <- as.matrix(data.frame(numeric_data2_MSCL, xfactors2_MSCL))
# # Lasso Regression  using glmnet - L1 norm
# require(glmnet)
# # fit model
# fit2_MSCL <- glmnet(MSCL2_data, eclsw2A_MSCL_RF$C2R4MSCL, alpha=1)
# 
# plot(fit2_MSCL,xvar="lambda",label=TRUE)
# plot(fit2_MSCL,xvar="dev",label=TRUE)
# #Model Selection
# coef(fit2_MSCL)
# cv.lasso2_MSCL=cv.glmnet(MSCL2_data, eclsw2A_MSCL_RF$C2R4MSCL)
# plot(cv.lasso2_MSCL)
# coef(cv.lasso2_MSCL)
# summary(cv.lasso2_MSCL)


####Analysis for Reading scores####
library(randomForest)
set.seed(1234)
fit.forest_eclsw2R <- randomForest(C2R4RSCL ~ . , data= eclsw2A_RSCL,
                                   na.action=na.roughfix,
                                   importance=TRUE, ntree = 150)

# following function determines variable importance
importance(fit.forest_eclsw2R, type=2)
#Arrange in decreasing order
order_rf2R <- sort(importance(fit.forest_eclsw2R, type=2)[,1], decreasing = TRUE)
order_rf2R
# Top 20 Attributes
feature_stringw2R <- paste(names(order_rf2R)[1:20])

#Data with top 20 attributes that will be used further for modeling
eclsw2R_impAttri <- eclsw2A_RSCL[, feature_stringw2R]

#Adding back the target attribute
eclsw2A_RSCL_RF <- cbind(eclsw2R_impAttri, C2R4RSCL = eclsw2A_RSCL$C2R4RSCL)

#Performing stepAic on the top attributes selected through Random Forest
library(MASS)
stepw2_RSCL <- stepAIC(lm(C2R4RSCL ~ ., data=eclsw2A_RSCL_RF), direction="both")
summary(stepw2_RSCL)
stepw2A_R_vars <-(names(stepw2_RSCL$model))
stepw2A_R_edit <- gsub("^..",'',stepw2A_R_vars)
  
  # ###Lasso Regression###
  # 
  # numeric_data2_RSCL = eclsw2A_RSCL_RF[,sapply(eclsw2A_RSCL_RF,is.numeric)]
  # numeric_data2_RSCL = subset(numeric_data2_RSCL, select=-c(C2R4RSCL))
  # cat_data2_RSCL = eclsw2A_RSCL_RF[,sapply(eclsw2A_RSCL_RF,is.factor)]
  # target2_RSCL = cbind(C2R4RSCL = eclsw2A_RSCL_RF$C2R4RSCL,cat_data2_RSCL)
  # xfactors2_RSCL <- model.matrix(C2R4RSCL ~ ., data = target2_RSCL)[,-1]
  # RSCL2_data <- as.matrix(data.frame(numeric_data2_RSCL, xfactors2_RSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit_2RSCL <- glmnet(RSCL2_data, eclsw2A_RSCL_RF$C2R4RSCL, alpha=1)
  # 
  # plot(fit_2RSCL,xvar="lambda",label=TRUE)
  # plot(fit_2RSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit_2RSCL)
  # cv.lasso2_RSCL=cv.glmnet(RSCL2_data, eclsw2A_RSCL_RF$C2R4RSCL)
  # plot(cv.lasso2_RSCL)
  # coef(cv.lasso2_RSCL)
  # summary(cv.lasso2_RSCL)

####Analysis of P1SADLON#### 
  library(randomForest)
  set.seed(1234)
  fit.forest_eclsw2S <- randomForest(P2SADLON ~ . , data= eclsw2A_SAD,
                                     na.action=na.roughfix,
                                     importance=TRUE, ntree = 150)
  
  # following function determines variable importance
  importance(fit.forest_eclsw2S, type=2)
  #Arrange in decreasing order
  order_rf2S <- sort(importance(fit.forest_eclsw2S, type=2)[,1], decreasing = TRUE)
  order_rf2S
  # Top 20 Attributes
  feature_stringw2S <- paste(names(order_rf2S)[1:20])
  
  #Data with top 20 attributes that will be used further for modeling
  eclsw2S_impAttri <- eclsw2A_SAD[, feature_stringw2S]
  
  #Adding back the target attribute
  eclsw2A_SAD_RF <- cbind(eclsw2S_impAttri, P2SADLON = eclsw2A_SAD$P2SADLON)
  
  #Performing stepAic on the top attributes selected through Random Forest
  library(MASS)
  stepw2_SAD <- stepAIC(lm(P2SADLON ~ ., data=eclsw2A_SAD_RF), direction="both")
  summary(stepw2_SAD)
  stepw2A_S_vars <-(names(stepw2_SAD$model))
  stepw2A_S_edit <- as.data.frame(gsub("^..",'',stepw2A_S_vars))

  # ###Lasso Regression###
  # numeric_data2_SAD = eclsw2A_SAD_RF[,sapply(eclsw2A_SAD_RF,is.numeric)]
  # numeric_data2_SAD = subset(numeric_data2_SAD, select=-c(P2SADLON))
  # cat_data2_SAD = eclsw2A_SAD_RF[,sapply(eclsw2A_SAD_RF,is.factor)]
  # target2_SAD = cbind(P2SADLON = eclsw2A_SAD_RF$P2SADLON,cat_data2_SAD)
  # xfactors2_SAD <- model.matrix(P2SADLON ~ ., data = target2_SAD)[,-1]
  # SAD2_data <- as.matrix(data.frame(numeric_data2_SAD, xfactors2_SAD))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit2_SAD <- glmnet(SAD2_data, eclsw2A_SAD_RF$P2SADLON, alpha=1)
  # 
  # plot(fit2_SAD,xvar="lambda",label=TRUE)
  # plot(fit2_SAD,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit2_SAD)
  # cv.lasso2_SAD=cv.glmnet(SAD2_data, eclsw2A_SAD_RF$P2SADLON)
  # plot(cv.lasso2_SAD)
  # coef(cv.lasso2_SAD)
  # summary(cv.lasso2_SAD)


########################################
#WAVE 2 WITH STUDENTS BELOW POVERTY 
########################################
  
eclsw2_BPvrty = eclsw2_BPvrty[-1]
eclsw2B_MSCL = subset(eclsw2_BPvrty, select=-c(C2R4RSCL,P2SADLON)) #DATA with only Math scores as target variable
eclsw2B_RSCL = subset(eclsw2_BPvrty, select=-c(C2R4MSCL,P2SADLON)) #DATA with only Reading scores as target variable
eclsw2B_SAD  = subset(eclsw2_BPvrty, select=-c(C2R4MSCL,C2R4RSCL))

####Analysis of Math Scores ####
  #ATTRIBUTE SELECTION USING RANDOM FOREST# 
  library(randomForest)
  set.seed(1234)
  fit.forest_eclsw2B <- randomForest(C2R4MSCL ~ . , data= eclsw2B_MSCL,
                                     na.action=na.roughfix,
                                     importance=TRUE, ntree = 150)
  
  # following function determines variable importance
  importance(fit.forest_eclsw2B, type=2)
  #Arrange in decreasing order
  order_Brf2 <- sort(importance(fit.forest_eclsw2B, type=2)[,1], decreasing = TRUE)
  order_Brf2
  # Top 20 Attributes
  feature_Bstringw2 <- paste(names(order_Brf2)[1:20])
  
  #Data with top 20 attributes that will be used further for modeling
  eclsw2BM_impAttri <- eclsw2B_MSCL[, feature_Bstringw2]
  
  #Adding back the target attribute
  eclsw2B_MSCL_RF <- cbind(eclsw2BM_impAttri, C2R4MSCL = eclsw2B_MSCL$C2R4MSCL)
  
  #Performing stepAic on the top attributes selected through Random Forest
  library(MASS)
  stepw2_BMSCL <- stepAIC(lm(C2R4MSCL ~ ., data=eclsw2B_MSCL_RF), direction="both")
  summary(stepw2_BMSCL)
  stepw2B_M_vars <-(names(stepw2_BMSCL$model))
  stepw2B_M_edit <- as.data.frame(gsub("^..",'',stepw2B_M_vars))
  ###Lasso Regression###
  numeric_data2_BMSCL = eclsw2B_MSCL_RF[,sapply(eclsw2B_MSCL_RF,is.numeric)]
  numeric_data2_BMSCL = subset(numeric_data2_BMSCL, select=-c(C2R4MSCL))
  cat_data2_BMSCL = eclsw2B_MSCL_RF[,sapply(eclsw2B_MSCL_RF,is.factor)]
  target2_BMSCL = cbind(C2R4MSCL = eclsw2B_MSCL_RF$C2R4MSCL,cat_data2_BMSCL)
  xfactors2_BMSCL <- model.matrix(C2R4MSCL ~ ., data = target2_BMSCL)[,-1]
  BMSCL2_data <- as.matrix(data.frame(numeric_data2_BMSCL, xfactors2_BMSCL))
  
#   # Lasso Regression  using glmnet - L1 norm
#   require(glmnet)
#   # fit model
#   fit2_BMSCL <- glmnet(BMSCL2_data, eclsw2B_MSCL_RF$C2R4MSCL, alpha=1)
#   
#   plot(fit2_BMSCL,xvar="lambda",label=TRUE)
#   plot(fit2_BMSCL,xvar="dev",label=TRUE)
#   #Model Selection
#   coef(fit2_BMSCL)
#   cv.lasso2_BMSCL=cv.glmnet(BMSCL2_data, eclsw2B_MSCL_RF$C2R4MSCL)
#   plot(cv.lasso2_BMSCL)
#   coef(cv.lasso2_BMSCL)
#   summary(cv.lasso2_BMSCL)


#####Analysis for Reading scores####
  library(randomForest)
  set.seed(1234)
  fit.forest_eclsw2RB <- randomForest(C2R4RSCL ~ . , data= eclsw2B_RSCL,
                                      na.action=na.roughfix,
                                      importance=TRUE, ntree = 150)
  
  # following function determines variable importance
  importance(fit.forest_eclsw2RB, type=2)
  #Arrange in decreasing order
  order_2rfRB <- sort(importance(fit.forest_eclsw2RB, type=2)[,1], decreasing = TRUE)
  order_2rfRB
  # Top 20 Attributes
  feature_stringw2RB <- paste(names(order_2rfRB)[1:20])
  
  #Data with top 20 attributes that will be used further for modeling
  eclsw2RB_impAttri <- eclsw2B_RSCL[, feature_stringw2RB]
  
  #Adding back the target attribute
  eclsw2B_RSCL_RF <- cbind(eclsw2RB_impAttri, C2R4RSCL = eclsw2B_RSCL$C2R4RSCL)
  
  #Performing stepAic on the top attributes selected through Random Forest
  library(MASS)
  stepw2B_RSCL <- stepAIC(lm(C2R4RSCL ~ ., data=eclsw2B_RSCL_RF), direction="both")
  summary(stepw2B_RSCL)
  stepw2B_R_vars <-(names(stepw2B_RSCL$model))
  stepw2B_R_edit <- as.data.frame(gsub("^..",'',stepw2B_R_vars))
  
  # ###Lasso Regression###
  # 
  # numeric_data2_BRSCL = eclsw2B_RSCL_RF[,sapply(eclsw2B_RSCL_RF,is.numeric)]
  # numeric_data2_BRSCL = subset(numeric_data2_BRSCL, select=-c(C2R4RSCL))
  # cat_data2_BRSCL = eclsw2B_RSCL_RF[,sapply(eclsw2B_RSCL_RF,is.factor)]
  # target2_BRSCL = cbind(C2R4RSCL = eclsw2B_RSCL_RF$C2R4RSCL,cat_data2_BRSCL)
  # xfactors2_BRSCL <- model.matrix(C2R4RSCL ~ ., data = target2_BRSCL)[,-1]
  # BRSCL2_data <- as.matrix(data.frame(numeric_data2_BRSCL, xfactors2_BRSCL))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit2_BRSCL <- glmnet(BRSCL2_data, eclsw2B_RSCL_RF$C2R4RSCL, alpha=1)
  # 
  # plot(fit2_BRSCL,xvar="lambda",label=TRUE)
  # plot(fit2_BRSCL,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit2_BRSCL)
  # cv.lasso2_BRSCL=cv.glmnet(BRSCL2_data, eclsw2B_RSCL_RF$C2R4RSCL)
  # plot(cv.lasso2_BRSCL)
  # coef(cv.lasso2_BRSCL)
  # summary(cv.lasso2_BRSCL)

####Analysis of P4SADLON#### 
  library(randomForest)
  set.seed(1234)
  fit.forest_eclsw2BS <- randomForest(P2SADLON ~ . , data= eclsw2B_SAD,
                                      na.action=na.roughfix,
                                      importance=TRUE, ntree = 150)
  
  # following function determines variable importance
  importance(fit.forest_eclsw2BS, type=2)
  #Arrange in decreasing order
  order_rf2BS <- sort(importance(fit.forest_eclsw2BS, type=2)[,1], decreasing = TRUE)
  order_rf2BS
  # Top 20 Attributes
  feature_stringw2BS <- paste(names(order_rf2BS)[1:20])
  
  #Data with top 20 attributes that will be used further for modeling
  eclsw2BS_impAttri <- eclsw2B_SAD[, feature_stringw2BS]
  
  #Adding back the target attribute
  eclsw2B_SAD_RF <- cbind(eclsw2BS_impAttri, P2SADLON = eclsw2B_SAD$P2SADLON)
  
  #Performing stepAic on the top attributes selected through Random Forest
  library(MASS)
  stepw2B_SAD <- stepAIC(lm(P2SADLON ~ ., data=eclsw2B_SAD_RF), direction="both")
  summary(stepw2B_SAD)
  stepw2B_S_vars <-(names(stepw2B_SAD$model))
  stepw2B_S_edit <- as.data.frame(gsub("^..",'',stepw2B_S_vars))

  # ###Lasso Regression###
  # numeric_data2B_SAD = eclsw2B_SAD_RF[,sapply(eclsw2B_SAD_RF,is.numeric)]
  # numeric_data2B_SAD = subset(numeric_data2B_SAD, select=-c(P2SADLON))
  # cat_data2B_SAD = eclsw2B_SAD_RF[,sapply(eclsw2B_SAD_RF,is.factor)]
  # target2B_SAD = cbind(P2SADLON = eclsw2B_SAD_RF$P2SADLON,cat_data2B_SAD)
  # xfactors2B_SAD <- model.matrix(P2SADLON ~ ., data = target2B_SAD)[,-1]
  # BSAD2_data <- as.matrix(data.frame(numeric_data2B_SAD, xfactors2B_SAD))
  # # Lasso Regression  using glmnet - L1 norm
  # require(glmnet)
  # # fit model
  # fit2B_SAD <- glmnet(BSAD2_data, eclsw2B_SAD_RF$P2SADLON, alpha=1)
  # 
  # plot(fit2B_SAD,xvar="lambda",label=TRUE)
  # plot(fit2B_SAD,xvar="dev",label=TRUE)
  # #Model Selection
  # coef(fit2B_SAD)
  # cv.lasso2B_SAD=cv.glmnet(SAD2_data, eclsw2B_SAD_RF$P2SADLON)
  # plot(cv.lasso2B_SAD)
  # coef(cv.lasso2B_SAD)
  # summary(cv.lasso2B_SAD)
