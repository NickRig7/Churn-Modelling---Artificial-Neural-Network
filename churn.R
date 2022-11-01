# Reading and exploring the dataset
churn=read.table(file.choose(),header = T, sep = ",",na.strings =c(""," ","NA" ))
View(churn)
str(churn)
summary(churn)

# Preparing the data
#Removing unwanted columns
churn=churn[c(-1,-2,-3)]

#Converting to factor
churn[c(2,3)]=lapply(churn[c(2,3)],factor)

#Checking missing data
table(is.na(churn))
#churn=as.data.frame(lapply(churn,function(x){x[x==" "]=NA})) # removing blank cells
#Replacing NA
churn$CreditScore[which(is.na(churn$CreditScore))]=mean(churn$CreditScore,na.rm = T)
churn$Age[which(is.na(churn$Age))]=median(churn$Age,na.rm = T)
churn$Tenure[which(is.na(churn$Tenure))]=median(churn$Tenure,na.rm = T)
churn$Balance[which(is.na(churn$Balance))]=median(churn$Balance,na.rm = T)
churn$NumOfProducts[which(is.na(churn$NumOfProducts))]=median(churn$NumOfProducts,na.rm = T)
churn$HasCrCard[which(is.na(churn$HasCrCard))]=median(churn$HasCrCard,na.rm = T)
churn$IsActiveMember[which(is.na(churn$IsActiveMember))]=median(churn$IsActiveMember,na.rm = T)
churn$EstimatedSalary[which(is.na(churn$EstimatedSalary))]=median(churn$EstimatedSalary,na.rm = T)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
} # to calculate mode for factor
churn$Geography[which(is.na(churn$Geography))]=getmode(churn$Geography)
churn$Gender[which(is.na(churn$Gender))]=getmode(churn$Gender)
churn$Exited[which(is.na(churn$Exited))]=getmode(churn$Exited)

table(is.na(churn))

#dummy coding
dummy_geo<-function(x){
  if(x=="France"){
    return(0)
  }else if(x=="Germany"){
    return(1)
  }else if(x=="Spain"){
    return(2)}
}

dummy_gender<-function(x){
  if(x=="Female"){
    return(0)
  }else if(x=="Male"){
    return(1)
  }
}
geo=as.data.frame(sapply(churn$Geography,dummy_geo))
names(geo)[1]=paste("Geography")
gender=as.data.frame(sapply(churn$Gender,dummy_gender))
names(gender)[1]=paste("Gender")

churn=churn[c(-2,-3)]
churn=cbind(churn,geo,gender)

normalize=function(X){
  return((X-min(X))/(max(X)-min(X)))
}

churn[c(1,2,3,4,5,8,10)]=lapply(churn[c(1,2,3,4,5,8,10)],normalize)

# Creating training and testing dataset
train=churn[1:4500,]
test=churn[4501:5181,]

# Training the model
library(neuralnet)
model=neuralnet(Exited~.,data = train)
plot(model)

# Evaluating the model performance
model_result=compute(model,test[-9])
prob=model_result$net.result
predict_val=ifelse(prob>0.5,1,0)
head(predict_val)
cor(as.numeric(test$Exited),predict_val)

# Improving model performance
model=neuralnet(Exited~.,data = train,hidden =6)
plot(model)

#Evaluating
model_result=compute(model,test[-9])
prob=model_result$net.result
predict_val=ifelse(prob>0.5,1,0)
head(predict_val)
cor(as.numeric(test$Exited),predict_val)
