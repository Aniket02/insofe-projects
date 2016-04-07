rm(list=ls(all=TRUE))

#import the ECLS Data file
ecls <- read.csv("ECLSData.csv")
str(ecls)

#Attributes that have type as "Weight" in attributes Description omitted 

#The attributes with type weight are in sequence from "C1CW0" to "C7CPTS0", hence
#the match function is used to find the column indexes
match(c("C1CW0","C7CPTS0"),colnames(ecls))
ecls1 <- ecls[,-c(38:108)] #columns in that range removed
rm(ecls)

#Omitting columns that have values "Suppressed" from the dataset

ecls2 <- as.data.frame(t(ecls1)) #transpose of ecls1
a <- which(with( ecls2, V1 == "SUPPRESSED" )) #gives index of rows that have the value "suppressed"
ecls3 <- ecls2[-a,] #removing those rows
ecls4 <- as.data.frame(t(ecls3)) #transposing the matrix back to how the original data was provided
rm(ecls2,ecls3,ecls1)

#Attributes that have more than 30% NA's are omitted
NA_limit <- round(0.3*nrow(ecls4))
l <- c()
for(i in 1:dim(ecls4)[2]){
  if(sum(is.na(ecls4[,i]))>=NA_limit){
    l = c(l,i)
  }
}
ecls5 <- ecls4[, -l] #ommiting the list of attributes that were identified by the function
rm(ecls4)

eclsWK <- ecls5[,grep("^WK", colnames(ecls5))] #attributes with prefix "WK"
eclsW1 <- ecls5[,grep("^W1", colnames(ecls5))] #attributes with prefix "W1"
eclsW3 <- ecls5[,grep("^W3", colnames(ecls5))] #attributes with prefix "W3"
eclsW5 <- ecls5[,grep("^W5", colnames(ecls5))] #attributes with prefix "W5"
eclsW8 <- ecls5[,grep("^W8", colnames(ecls5))] #attributes with prefix "W8"
ecls7 <- ecls5[,-grep("^W", colnames(ecls5))]  #checking if all columns have been accounted for
#filtering based on the second digit in column names
eclswave1 <- ecls7[, grep("^.1", colnames(ecls7))]
eclswave2 <- ecls7[, grep("^.2", colnames(ecls7))]
eclswave3 <- ecls7[, grep("^.3", colnames(ecls7))]
eclswave4 <- ecls7[, grep("^.4", colnames(ecls7))]
eclswave5 <- ecls7[, grep("^.5", colnames(ecls7))]
eclswave6 <- ecls7[, grep("^.6", colnames(ecls7))]
eclswave7 <- ecls7[, grep("^.7", colnames(ecls7))]

ecls8 <- ecls7[,-grep("^.1|^.2|^.3|^.4|^.5|^.6|^.7", colnames(ecls7))] #the remaining columns, apply to all waves

#Creating the waves from 1-7

eclswave1 <- cbind(ecls8,eclsWK,eclswave1)
write.csv(eclswave1,"eclswave1.csv",row.names = F)
eclswave2 <- cbind(ecls8,eclsWK,eclswave2)
write.csv(eclswave2,"eclswave2.csv",row.names = F)
eclswave3 <- cbind(ecls8,eclsW1,eclswave3)
write.csv(eclswave3,"eclswave3.csv",row.names = F)
eclswave4 <- cbind(ecls8,eclsW1,eclswave4)
write.csv(eclswave4,"eclswave4.csv",row.names = F)
eclswave5 <- cbind(ecls8,eclsW3,eclswave5)
write.csv(eclswave5,"eclswave5.csv",row.names = F)
eclswave6 <- cbind(ecls8,eclsW5,eclswave6)
write.csv(eclswave6,"eclswave6.csv",row.names = F)
eclswave7 <- cbind(ecls8,eclsW8,eclswave7)
write.csv(eclswave7,"eclswave7.csv",row.names = F)
str(eclswave1)
