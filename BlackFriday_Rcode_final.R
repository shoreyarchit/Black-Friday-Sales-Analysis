
library(dplyr)
library(caret)
library(rpart)
library(randomForest)
library(rpart.plot)
library(data.table)
library(e1071)

# SPLITTING DATA ----------------------------------------------------------
#Creating randomise sample

BlackFridaydf <- BlackFriday[sample(nrow(BlackFriday)),]


#fetching 150000 rows 

BlackFridaydf2 <- sample_n(BlackFridaydf,150000)

#Sampling the datasets

index <- sample(2,nrow(BlackFridaydf2),replace=T,prob = c(0.7,0.3))

Blackfriday_train <- BlackFridaydf2[index==1,]

Blackfriday_test  <- BlackFridaydf2[index==2,]

#Exporting in csv

write.csv(Blackfriday_test,file="BlackFriday_test",row.names = FALSE)

write.csv(Blackfriday_train,file="BlackFriday_train",row.names = FALSE)


# DATA CLEANING & TRANSFORMATION ------------------------------------------
Blackfriday_train = read.csv('C:/Users/shore/Desktop/UIC/Projects/575/BlackFriday_train.csv')
Blackfriday_test = read.csv('C:/Users/shore/Desktop/UIC/Projects/575/BlackFriday_test.csv')

# Replacing NA's with 0
Blackfriday_train[is.na(Blackfriday_train)] = 0
Blackfriday_test[is.na(Blackfriday_test)] = 0
View(Blackfriday_train[,c(2,9,10,11,12)])


#Changing the data type to factor
col = c(1,2,3,5,6,7,8,9,10,11)
Blackfriday_train[col] <- lapply(Blackfriday_train[col], factor)
Blackfriday_test[col] <- lapply(Blackfriday_test[col], factor)
Blackfriday_train$Age <- as.character(Blackfriday_train$Age)

#Check columns which contain NAs
#colnames(BlackFriday)[colSums(is.na(BlackFriday)) > 0]


attach(Blackfriday_train)

#========== Exploratory Analysis ----------------------------

boxplot(BlackFriday[,c(5,9,10,11)],col = "Light Green")
title("Distribution of Features")

 # USER_ID,PRODUCT_ID,GENDER --------------------------------#
# No.of Total Customers and Product ID's
n_distinct(User_ID)
n_distinct(Product_ID)

#Product ID' and Purchase

boxplot(Purchase, col=c('lightblue'), xlab='Purchase Distribution')
summary(Purchase)

#One product has multiple prices
par(mfrow=c(1,2))
boxplot(Purchase[Product_ID == 'P00123742'],col = c('lightblue'),xlab='Purchase Distribution of Product of Cat11' )

#Identifying if all the products in the same Category have same purchase distribution
boxplot(Purchase[Product_Category_1 == '11'],col = c('lightblue'), xlab='Purchase Distribution of Cat11[all products]')

dev.off()

#Identifyig Customers which have atleast one purchase above $16333 (90 percentile)
high_paying_customers<-n_distinct(User_ID[Purchase > quantile(Purchase,.90)])

#Gender Exploration
gender_dis<- table(Gender)
barplot <- barplot(gender_dis,main = 'Gender Distribution',col= c('orange', 'lightblue'),legend=c('female','male'))

m_purchase<-(sum(Purchase[Gender=='M'])/sum(Purchase))*100
f_purchase<-(sum(Purchase[Gender=='F'])/sum(Purchase))*100


percent_purchase_dis <- c(m_purchase,f_purchase)

barplot <- barplot(percent_purchase_dis,main = 'Gender Purchase Distribution',ylab = '% contribution to Revenue', col= c('lightblue','orange'),legend=c('male','female'),ylim=c(0,90))

boxplot(Purchase[Gender == 'M'],col = c('lightblue'),xlab = 'Male Purchase Distribution' )
boxplot(Purchase[Gender == 'F'],col = c('lightblue'), xlab = 'Female Purchase Distribution')

#AGE --------------------------------------------#
# Number of Customer Across Different Age Group
barplot(table(Age),col = c("Orange","Light Blue","Light Green"),xlab = "Age of Customer",ylab = "Number of Customers",ylim=c(0,45000),main="Number of Customers across different Age Groups")

# Total Purchase Contribution Across Different Age Groups
sale_age<-list()
for (i in sort(unique(Age))){
  sale_age[i]<-(sum(Purchase[Age==i])/sum(Purchase))*100
}
sale_age<-unlist(sale_age)
names(sale_age)<-NULL
barplot(sale_age,col = c("Orange","Light Blue","Light Green"),ylim = c(0,60),names.arg = sort(unique(Age)),xlab="Age Group",ylab = "Percentage of Total Purchase",main = "Purchase Distribution across each Age segment")

# Purchase Amount distrbution across all Age Groups 
p_age_1<-plot(density(Purchase[Age=='0-17']),main="Purchase Behaviour at Age 0-17",col="Red")
p_age_2<-plot(density(Purchase[Age=='18-25']),main="Purchase Behaviour at Age 18-25",col="Blue")
p_age_3<-plot(density(Purchase[Age=='26-35']),main="Purchase Behaviour at Age 26-35",col="Dark Green")
p_age_4<-plot(density(Purchase[Age=='36-45']),main="Purchase Behaviour at Age 36-45",col="Black")
p_age_5<-plot(density(Purchase[Age=='46-50']),main="Purchase Behaviour at Age 46-50",col="darkorchid4")
p_age_6<-plot(density(Purchase[Age=='51-55']),main="Purchase Behaviour at Age 51-55",col="Orange")
p_age_7<-plot(density(Purchase[Age=='55+']),main="Purchase Behaviour at Age 55+",col="Light Blue")


# OCCUPATION ----#

# Number of Customer Across Different Occupation Level
barplot(table(Occupation),col=c("Orange","Light Blue","Light Green"),ylim=c(0,14000),ylab="Number of Customers", xlab = "Occupation of Customer",main = "Number of Customer with different Occupation")

# Total Purchase Contribution Across Different Occupation
sale_occupation<-list()
for (i in sort(unique(Occupation))){
  j=as.numeric(i)
  sale<-(sum(Purchase[Occupation==i])/sum(Purchase))*100
  j=j+1
  sale_occupation[j]<-sale
}
names(sale_occupation)<-NULL
sale_occupation<-unlist(sale_occupation)
barplot(sale_occupation,col = c("Orange","Light Blue","Light Green"),ylim = c(0,20),xlab="Occupation",names.arg = sort(unique(Occupation)),ylab = "Percentage of Total Purchase",main = "Purchase Distribution across each Occupation")

# Purchase Amount Behaviour across each Occupation 
par(mfrow = c(2,3))
for (i in sort(unique(Occupation))) {
  plot(density(Purchase[Occupation==i]),main=paste("Purchase amount Behaviour for Occupation ",i),xlab="Purchase Amount",col="Red")
  }
dev.off()
# CITY CATEGORY-----#

# Number of Customer Across Different City 
barplot(table(City_Category),col = c("Light Blue"),xlab="City Category of Customer",ylab = "Number of Customers",ylim=c(0,50000),main="Number of Customers across different Cities")

# Total Purchase Contribution Across Different City Categories
sale_city<-list()
for (i in sort(unique(City_Category))){
  sale_city[i]<-(sum(Purchase[City_Category==i])/sum(Purchase))*100
}
sale_city
sale_city<-unlist(sale_city)
names(sale_city)<-NULL
barplot(sale_city,col = c("Light Blue"),ylim = c(0,50),names.arg = sort(unique(City_Category)),xlab="City Category",ylab = "Percentage of Total Purchase",main = "Purchase Distribution across each City Category")


# Purchase Amount Behaviour across each City 
par(mfrow=c(1,1))
p_city_1<-plot(density(Purchase[City_Category=='A']),main="Purchase amount Behaviour for City A",col="Red")
p_city_2<-plot(density(Purchase[City_Category=='B']),main="Purchase amount Behaviour for City B",col="Blue")
p_city_3<-plot(density(Purchase[City_Category=='C']),main="Purchase amount Behaviour for City C",col="Dark Green")

dev.off()

# STAY IN CITY, MARITAL STATUS, PURCAHSE -------#
table( Marital_Status)

#proportion of unmarried shoppers are higher in dataset

table( Gender,Marital_Status)

 Marital_Status<- as.factor( Marital_Status)

ggplot(Blackfriday_train,aes(x=Gender, fill=Marital_Status))+geom_bar()+ facet_wrap(~Marital_Status) + labs(x="Gender", y="No. of Sales")

#As it can be seen, Singles generally buy more goods than married. Among all males Singles buy most products and married women least

#====Stay in city =======#

 Stay_In_Current_City_Years <-as.factor(Stay_In_Current_City_Years)

table( Stay_In_Current_City_Years)

#peopl who have stayed in city for 1 year shows higher buying patterns

table( City_Category,  Stay_In_Current_City_Years)

#write for each city

#Purchase Amount -

hist(Purchase,col = "orange")

#Normally distributed

max(Purchase) #23955
min(Purchase) #185

#summary of purchase 

summary(Purchase[City_Category=="A"])

summary(Purchase[City_Category=="B"])

summary(Purchase[City_Category=="C"])

#As per the purchase amounts, different city has almost same maximum amount around 24k and median range of purchases lies between $6500 - 8000$

# PRODUCT CATEGORIES -------------------#

# Check for correlation
x <- Blackfriday_train[9:11]
y <- Blackfriday_train[12]
#cor(x, y)

#Basic LM wrt Purchase
purchase.equation = "Purchase~Product_Category_1+Product_Category_2+Product_Category_3"
purchase.formula = as.formula(purchase.equation)
BlackFriday_lm = lm(formula = purchase.formula, data = Blackfriday_train)
summary(BlackFriday_lm)


par(mfrow=c(1,3))
plot(density( Product_Category_1), xlim=c(0,20), main = "Product Category 1 purchasing trend") 
plot(density(Product_Category_2), xlim=c(0,20), main = "Product Category 2 purchasing trend")
plot(density(Product_Category_3), xlim=c(0,20), main = "Product Category 3 purchasing trend")
dev.off()



#                       *******   AFTER MILESTONE   ********
#---------------------------------------------------------------------------------#

#======= Feature Engineering ======
#For Train
blackfriday_temp<-Blackfriday_train
attach(blackfriday_temp)

#--------------- Adding distribution of Product ID's as new features ------------#
train2 <- data.table(blackfriday_temp)
blackfriday_temp <-train2[,c("Q1","Med","Q3"):=as.list(quantile(Purchase,c(0.25,0.5,0.75))),by=Product_ID]
blackfriday_temp <- as.data.frame(blackfriday_temp)

# One hot encoding Occupation
for (i in sort(unique(Occupation))){
  blackfriday_temp[i]<-ifelse(blackfriday_temp$Occupation == i, 1,0)
  colnames(blackfriday_temp)[which(names(blackfriday_temp) == i)] <- paste("OCC_",i,sep = "")
}

# One hot encoding City Category
for (i in sort(unique(City_Category))){
  blackfriday_temp[i]<-ifelse(blackfriday_temp$City_Category == i, 1,0)
  colnames(blackfriday_temp)[which(names(blackfriday_temp) == i)] <- paste("City_Category_",i,sep = "")
}

# Taking count of User ID as column
for (i in sort(unique(User_ID))){
  blackfriday_temp$User_Count[blackfriday_temp$User_ID==i]<-length(blackfriday_temp$User_ID[blackfriday_temp$User_ID==i])
}

# Taking count of Product ID as column
for (i in sort(unique(Product_ID))){
  blackfriday_temp$Product_Count[blackfriday_temp$Product_ID==i]<-length(blackfriday_temp$Product_ID[blackfriday_temp$Product_ID==i])
}


#For Test
blackfriday_tempt<-Blackfriday_test
test3<-blackfriday_tempt
setDT(test3)

temp <- test3[test3$Product_ID %in% blackfriday_temp$Product_ID][,2]
prod_id<-as.vector(temp$Product_ID)

#--------- Taking out Q1 -------------#
for (i in sort(unique(prod_id))) {
  blackfriday_tempt$Q1[blackfriday_tempt$Product_ID==i] <- blackfriday_temp$Q1[blackfriday_temp$Product_ID==i]
}
#--------- Taking out Q3 -------------#
for (i in sort(unique(prod_id))) {
  blackfriday_tempt$Q3[blackfriday_tempt$Product_ID==i] <- blackfriday_temp$Q3[blackfriday_temp$Product_ID==i]
}
#--------- Taking out Median -------------#
for (i in sort(unique(prod_id))) {
  blackfriday_tempt$Med[blackfriday_tempt$Product_ID==i] <- blackfriday_temp$Med[blackfriday_temp$Product_ID==i]
}

#----------merge test3 with temp---------#

missing <- blackfriday_tempt[is.na(blackfriday_tempt$Q1)]
for (i in 1:dim(missing)[1]){
  
  missing[i,c("Q1","Med","Q3")]<-as.list(quantile(blackfriday_temp$Purchase[Product_Category_1== missing[i,Product_Category_1]],c(0.25,0.5,0.75)))

}


#replace rows with na's by imputed rows
blackfriday_tempt<-merge(blackfriday_tempt,missing,by=names(missing),all=TRUE)
blackfriday_tempt<- na.omit(blackfriday_tempt)
#blackfriday_tempt[complete.cases(blackfriday_tempt)]

#Sorting Test Data again with User ID
setkey(blackfriday_tempt,User_ID)


#--------- user count -----------#
for (i in sort(unique(blackfriday_tempt$User_ID))){
  blackfriday_tempt$User_Count[blackfriday_tempt$User_ID==i]<-length(blackfriday_tempt$User_ID[blackfriday_tempt$User_ID==i])
}
#--------- product count -----------#
for (i in sort(unique(blackfriday_tempt$Product_ID))){
  blackfriday_tempt$Product_Count[blackfriday_tempt$Product_ID==i]<-length(blackfriday_tempt$Product_ID[blackfriday_tempt$Product_ID==i])
}



#======== Model Building ================= 

#Simple GLM without Feature Engineering 
glm_1 <- train(Purchase~Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3,data=blackfriday_temp,method='glm', metric ="RMSE")
#train RMSE
rmse_glm_train_1<-RMSE( predict(glm_1,blackfriday_temp),blackfriday_temp$Purchase) #3002
#test RMSE
rmse_glm_test_1<-RMSE(predict(glm_1,blackfriday_tempt),blackfriday_tempt$Purchase) #2968
#variable Imprtance plot
ImpMeasure<-data.frame(varImp(glm_1)$importance)
ImpMeasure$Vars<-row.names(ImpMeasure)
IMP_1<-ImpMeasure[order(-ImpMeasure$Overall),][1:10,]
IMP_1<-IMP_1[order(IMP_1$Overall,decreasing = TRUE),]
ggplot(IMP,aes(y=IMP_1$Overall))+geom_col(aes(x=IMP_1$Vars))+coord_flip()+ggtitle("Variable Importance")+labs (y="Importance",x="Features")

residual_1<-residuals(glm_1)
plot(blackfriday_temp$Purchase,residual_1,main = "GLM Residual Plot",xlab = "Observed Value")
abline(0,0)

#Simple GLM with Feature Engineering 
glm_2 <- train(Purchase~Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3+User_Count+Q1+Med+Q3,data=blackfriday_temp,method='glm', metric ="RMSE")
#train
rmse_glm_train_2<-RMSE( predict(glm_2,blackfriday_temp),blackfriday_temp$Purchase) #2641
#test
predict_test<-predict(glm_2,blackfriday_tempt)
rmse_glm_test_2<-RMSE( predict(glm_2,blackfriday_tempt),blackfriday_tempt$Purchase) #2703
#variable Imprtance plot
ImpMeasure<-data.frame(varImp(glm_2)$importance)
ImpMeasure$Vars<-row.names(ImpMeasure)
IMP_2<-ImpMeasure[order(-ImpMeasure$Overall),][1:10,]
IMP_2<-IMP_2[order(IMP_1$Overall,decreasing = TRUE),]
ggplot(IMP,aes(y=IMP_2$Overall))+geom_col(aes(x=IMP_2$Vars))+coord_flip()+ggtitle("Variable Importance")+labs (y="Importance",x="Features")
residual<-residuals(glm_2)
plot(blackfriday_temp$Purchase,residual,main = "GLM Residual Plot",xlab = "Observed Value")
abline(0,0)

ggplot(blackfriday_tempt,aes(y=predict_test,x=blackfriday_tempt$Purchase)) +   geom_smooth(method = "glm") + geom_point() +   ggtitle("Observed VS Predicted Value") + labs(x= "Observed Value", y="Predicted Value")



plot(glm_2)



#Using Regularization and Cross Valdiation in GLM
#Lasso Regression, alpha=1, CV for 5 Folds
lambdas <- seq(0,10,0.2)
CARET.TRAIN.CTRL <- trainControl(method="cv",
                                 number=5)
glmnet_1 <- train(Purchase~Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                    Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3+
                    User_Count+Q1+Med+Q3,
                  data=blackfriday_temp,method='glmnet', metric ="RMSE",
                  trControl=CARET.TRAIN.CTRL,
                  tuneGrid=expand.grid(alpha=1,lambda=lambdas))
rmse_glmnet_train_1 <- RMSE( predict(glmnet_1,blackfriday_temp),blackfriday_temp$Purchase) #2641
rmse_glmnet_test_1 <- RMSE( predict(glmnet_1,blackfriday_tempt),blackfriday_tempt$Purchase) #2704
plot(glmnet_1)

predict_test<-predict(glmnet_1,blackfriday_tempt)
ggplot(blackfriday_tempt,aes(y=predict_test,x=blackfriday_tempt$Purchase)) +   geom_smooth(method = "glm") + geom_point() +   ggtitle("Observed VS Predicted Value") + labs(x= "Observed Value", y="Predicted Value")

#Ridge Regression, CV 5 Folds
glmnet_2 <- train(Purchase~Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                    Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3+
                    User_Count+Product_Count+Q1+Med+Q3,
                  data=blackfriday_temp,method='glmnet', metric ="RMSE",
                  trControl=CARET.TRAIN.CTRL,
                  tuneGrid=expand.grid(alpha=0,lambda=lambdas))
rmse_glmnet_train_2 <- RMSE( predict(glmnet_2,blackfriday_temp),blackfriday_temp$Purchase) #2645
rmse_glmnet_test_2 <- RMSE( predict(glmnet_2,blackfriday_tempt),blackfriday_tempt$Purchase) #2702
plot(glmnet_2)

#using cv.glmnet
input_matrix<-model.matrix(~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                             Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3+
                             User_Count+Product_Count+Q1+Med+Q3,
                           data=blackfriday_temp)
output_matrix <- as.matrix(blackfriday_temp[,12])
input_test_matrix<-model.matrix(~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                                  Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3
                                +User_Count+Product_Count+Q1+Med+Q3,
                                data=blackfriday_tempt)
output_test_matrix <- as.matrix(blackfriday_tempt[,12])
#ridge
ridge_reg <- cv.glmnet(y=output_matrix, x=input_matrix ,alpha=0)
bestlambda_ridge<-ridge_reg$lambda.min

rmse_ridge_train <-RMSE( predict(ridge_reg,input_matrix),blackfriday_temp$Purchase) #2652
rmse_ridge_test <-RMSE(predict(ridge_reg,input_test_matrix),blackfriday_tempt$Purchase) #2707

#lasso
lasso_reg <- cv.glmnet(y=output_matrix, x=input_matrix ,alpha=1)
bestlambda_lasso<-lasso_reg$lambda.min
rmse_lasso_train <-RMSE( predict(lasso_reg,input_matrix),blackfriday_temp$Purchase) #2653
rmse_lasso_test <-RMSE(predict(lasso_reg,input_test_matrix),blackfriday_tempt$Purchase) #2722


#H2O Cluster Setup
library(h2o)
localH2O <- h2o.init(nthreads = -2)   #Specify Number of CPU Threads
h2o.init()                            #Initialize H2O
train.h2o <- as.h2o(blackfriday_temp)
test.h2o <- as.h2o(blackfriday_tempt)
#h2o.shutdown()
#Random Forest in H2O without Feature Engineering
system.time(rforest.model_1 <- h2o.randomForest(y=12, x=c(3:11),
                                                training_frame = train.h2o
                                                ,ntrees = 100, mtries = 4, seed = 6666))
#train rmse
rmse_train_rf_1<-h2o.performance(rforest.model_1) #RMSE 2987
#test rmse
rmse_test_rf_1<-h2o.performance(rforest.model_1,newdata = test.h2o) #RMSE 2959
pred_test<-h2o.predict(rforest.model_1,test.h2o)
predict_test<-predict(glmnet_1,blackfriday_tempt)
pred_test<-as.vector(pred_test)
ggplot(blackfriday_tempt,aes(y=pred_test,x=blackfriday_tempt$Purchase)) +   geom_smooth(method = "glm") + geom_point() +   ggtitle("Observed VS Predicted Value") + labs(x= "Observed Value", y="Predicted Value")


h2o.r2(rforest.model_1) # 64%
h2o.varimp(rforest.model_1)
h2o.varimp_plot(rforest.model_1)
score_hist_1<-as.data.frame(h2o.scoreHistory(rforest.model_1))
ggplot(score_hist_1,aes(x=score_hist_1$number_of_trees))+geom_point(aes(y=score_hist_1$training_rmse))+geom_line(aes(y=score_hist_1$training_rmse)) + labs(title="No. of tree VS RMSE", x="No. of Trees" ,y="RMSE")

colnames(blackfriday_temp)

#Random Forest in H2O with Feature Engineering
system.time(rforest.model_2 <- h2o.randomForest(y=12, x=c(3:11,37:41), 
                                                training_frame = train.h2o
                                                ,ntrees = 100, mtries = 4))
#train rmse
rmse_train_rf_2<-h2o.rmse(rforest.model_2) #RMSE 2692
#test rmse
rmse_test_rf_2<- h2o.performance(rforest.model_2,test.h2o) #RMSE 2706
#evaluation
h2o.r2(rforest.model_2) # 70%
h2o.varimp(rforest.model_2)
h2o.varimp_plot(rforest.model_2) #Variable Importance
score_hist_2<-as.data.frame(h2o.scoreHistory(rforest.model_2))
ggplot(score_hist_2,aes(x=score_hist_2$number_of_trees))+geom_point(aes(y=score_hist_2$training_rmse))+geom_line(aes(y=score_hist_2$training_rmse)) + labs(title="No. of tree VS RMSE", x="No. of Trees" ,y="RMSE")


#Random Forest in H2O with Feature Engineering With Cross Validation
system.time(rforest.model_3 <- h2o.randomForest(y=12, x=c(3:11,37:41), 
                                                training_frame = train.h2o
                                                ,ntrees = 50,nfolds = 3, mtries = 4))
#train rmse
rmse_train_rf_3<-h2o.performance(rforest.model_3) #RMSE 2721
h2o.r2(rforest.model_3) #70%
#test rmse
rmse_test_rf_3<-h2o.performance(rforest.model_3,test.h2o) #RMSE 2718

#evaluation
score_hist_3<-as.data.frame(h2o.scoreHistory(rforest.model_3))
ggplot(score_hist_3,aes(x=score_hist_3$number_of_trees))+geom_point(aes(y=score_hist_3$training_rmse))+geom_line(aes(y=score_hist_3$training_rmse)) + labs(title="No. of tree VS RMSE", x="No. of Trees" ,y="RMSE")
h2o.varimp_plot(rforest.model_3) #Variable Importance


#Gradient Boosting Without Feature Engineering
system.time(
  gbm.model_1 <- h2o.gbm(y=12, x=c(3:11), training_frame = train.h2o,
                         ntrees = 500, max_depth = 4, 
                         learn_rate = 0.01, seed = 1122)
)
rmse_gbm_train_1<-h2o.performance(gbm.model_1) #RMSE 3005

rmse_gbm_test_1<-h2o.performance(gbm.model_1,test.h2o) #2963

gb_score_1<-as.data.frame(h2o.scoreHistory(gbm.model_1))
ggplot(gb_score_1,aes(x=gb_score_1$number_of_trees))+geom_point(aes(y=gb_score_1$training_rmse))+geom_line(aes(y=gb_score_1$training_rmse)) + labs(title="No. of tree VS RMSE", x="No. of Trees" ,y="RMSE")
h2o.varimp_plot(gbm.model_1) #Variable Importance


#Gradient Boosting With Feature Engineering
system.time(
  gbm.model_2 <- h2o.gbm(y=12, x=c(3:11,37:41), training_frame = train.h2o,
                         ntrees = 500, max_depth = 4, 
                         learn_rate = 0.01, seed = 1122)
)
rmse_gbm_train_2<-h2o.performance(gbm.model_2) #RMSE 3112 #4,2615 #5,2594 #2508

rmse_gbm_test_2<-h2o.performance(gbm.model_2,test.h2o)  #4,2693 #5,2691

#evaluate
gb_score_2<-as.data.frame(h2o.scoreHistory(gbm.model_2))
ggplot(gb_score_2,aes(x=gb_score_2$number_of_trees))+geom_point(aes(y=gb_score_2$training_rmse))+geom_line(aes(y=gb_score_2$training_rmse)) + labs(title="No. of tree VS RMSE", x="No. of Trees" ,y="RMSE")
h2o.varimp_plot(gbm.model_2) #Variable Importance


# XGBoost Models without feature engineering
system.time(xgb1 <- h2o.xgboost(y=12, x=c(3:11), training_frame = train.h2o, 
                                nfolds = 5,keep_cross_validation_predictions = TRUE, ntrees = 500,
                                seed = 1122))
rmse_xg_train_1<-h2o.performance(xgb1) #RMSE n=3,2871, n=5,2792

rmse_xg_test_1<-h2o.performance(xgb1,test.h2o)  #3,2920 #5,2908

xgb_score1<-as.data.frame(h2o.scoreHistory(xgb1))
ggplot(xgb_score1,aes(x=xgb_score1$number_of_trees))+geom_point(aes(y=xgb_score1$training_rmse))+geom_line(aes(y=xgb_score1$training_rmse)) + labs(title="No. of tree VS RMSE", x="No. of Trees" ,y="RMSE")
h2o.varimp_plot(xgb1) #Variable Importance



# XGBoost Models with feature engineering
system.time(xgb2 <- h2o.xgboost(y=12, x=c(3:11,37:41), training_frame = train.h2o, 
                                nfolds = 5,keep_cross_validation_predictions = TRUE, ntrees = 50,
                                seed = 1122))
rmse_xg_train_2<-h2o.performance(xgb2) #RMSE n=5,2400
rmse_xg_test_2<-h2o.performance(xgb2,test.h2o)  #5,2700

xgb_score2<-as.data.frame(h2o.scoreHistory(xgb2))
ggplot(xgb_score2,aes(x=xgb_score2$number_of_trees))+geom_point(aes(y=xgb_score2$training_rmse))+geom_line(aes(y=xgb_score2$training_rmse)) + labs(title="No. of tree VS RMSE", x="No. of Trees" ,y="RMSE")
h2o.varimp_plot(xgb2) #Variable Importance

pred_test<-h2o.predict(xgb2,test.h2o)

pred_test<-as.vector(pred_test)
ggplot(blackfriday_tempt,aes(y=pred_test,x=blackfriday_tempt$Purchase)) +   geom_smooth(method = "glm") + geom_point() +   ggtitle("Observed VS Predicted Value") + labs(x= "Observed Value", y="Predicted Value")


#===========  SVM  ================

#cross validation # cost=1
system.time(svm_bf_4<-svm(Purchase~Product_Category_1+Product_Category_2+Product_Category_3+Age+Occupation+City_Category+Marital_Status+Gender+Stay_In_Current_City_Years+User_Count+Product_Count+Q1+Q3+Med,data=blackfriday_temp,epsilon=0.1,cost=1,cross=3,scale = TRUE))
svm_bf_4
#calcluating w and b
W4 = t(svm_bf_4$coefs) %*% svm_bf_4$SV
b4 <-svm_bf_4$rho
#predicting value on train data
predsvm4<-predict(svm_bf_4,blackfriday_temp)
rmse_svm4<-RMSE(predsvm4,blackfriday_temp$Purchase)  #2656
#predicting values on test data 
pred_test_4<-predict(svm_bf_4,blackfriday_tempt)
rmse_test_svm4<-RMSE(pred_test_4,blackfriday_tempt$Purchase) #rmse 2746


#cost=0.125
system.time(svm_bf_5<-svm(Purchase~Product_Category_1+Product_Category_2+Product_Category_3+Age+Occupation+City_Category+Marital_Status+Gender+Stay_In_Current_City_Years+User_Count+Product_Count+Q1+Q3+Med,data=blackfriday_temp,epsilon=0.1,cost=0.125,cross=3,scale = TRUE))
svm_bf_5
#calcluating w and b
W5 = t(svm_bf_5$coefs) %*% svm_bf_5$SV
b5 <-svm_bf_5$rho
#predicting value on train data
predsvm5<-predict(svm_bf_5,blackfriday_temp)
rmse_svm5<-RMSE(predsvm5,blackfriday_temp$Purchase)  #RMSE 2673
#predicting values on test data 
pred_test_5<-predict(svm_bf_5,blackfriday_tempt)
rmse_test_svm5<-RMSE(pred_test_5,blackfriday_tempt$Purchase) #rmse 2749


