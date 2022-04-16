## Group 17 
## Read data 
# This is your directory (may vary in different computers)
#setwd('/Users/zhuxuxi/Desktop/AMS_SBU/Fall2021_AMS_572/572Project')  
# setwd('C:/users/81511/Documents') 
bank<-read.csv('BankChurners.csv')

##---1.1----------- Convert categorical data to ordinal data ---------------

Y=bank[,1]
x=bank[,2:20]
x[,2]=gsub("M",0,x[,2],perl=TRUE) 
x[,2]=gsub("F",1,x[,2],perl=TRUE)          
x[,2]=as.numeric(x[,2])
x[,4]=gsub("Uneducated",1,x[,4],perl=TRUE)  
x[,4]=gsub("High School",2,x[,4],perl=TRUE)   
x[,4]=gsub("College",3,x[,4],perl=TRUE) 
x[,4]=gsub("Post-Graduate",5,x[,4],perl=TRUE) 
x[,4]=gsub("Graduate",4,x[,4],perl=TRUE) 
x[,4]=gsub("Doctorate",6,x[,4],perl=TRUE) 
x[,4]=as.numeric(x[,4])
x[,5]=factor(bank$Marital_Status,levels=c("Single","Divorced","Married"))
x[,6]=gsub("Less than \\$40K",1,x[,6],perl=TRUE)   
x[,6]=gsub("\\$40K - \\$60K",2,x[,6],perl=TRUE) 
x[,6]=gsub("\\$60K - \\$80K",3,x[,6],perl=TRUE) 
x[,6]=gsub("\\$80K - \\$120K",4,x[,6],perl=TRUE) 
x[,6]=gsub("\\$120K \\+",5,x[,6],perl=TRUE) 
x[,6]=as.numeric(x[,6])
x[,7]=gsub("Blue",1,x[,7],perl=TRUE)   
x[,7]=gsub("Silver",2,x[,7],perl=TRUE) 
x[,7]=gsub("Gold",3,x[,7],perl=TRUE) 
x[,7]=gsub("Platinum",4,x[,7],perl=TRUE) 
x[,7]=as.numeric(x[,7])

##---1.2---------- First Hypothesis -----------------

# H0:mean(male age)=mean(female age) vs Ha: Not H0 at alpha=0.05
MaleAge<-x[which(x[,2]==0),1]
FemaleAge<-x[which(x[,2]==1),1]
t.test(MaleAge,FemaleAge,alternative="two.sided",mu=0)
# We fail to reject H0.


##---2.1---------- Linear Independence of Categorical Variables ---------------

# H0: Linear independent vs Ha: Not independent
cor.test(x[,1],x[,3])$p.value<0.05
cor.test(x[,6],x[,7])$p.value<0.05

##---2.2---------- Linear Independence of Continuous Variables ---------------

Correlation=matrix(nrow=8,ncol=8)
for (i in 12:19){
  for (j in 12:19){
    corr_temp=cor.test(x[,i],x[,j])$p.value
    Correlation[i-11,j-11]=corr_temp     
  }
}
Correlation[which(Correlation<0.0000001,arr.ind = T)]="dependent"
 
##---2.3---------- Second Hypothesis -----------------

# Model: Logit(Y)=β0+∑(βi*xi)
# Predictor: Age, Gender, Education, Income, Months on book, Credit Limit, Total_Trans_Amt, Avg_Utilization_Ratio
# H0: All βi=0 vs Ha: Not H0  at alpha=0.05
fit1<-glm(Y~x[,1]+x[,2]+x[,4]+x[,6]+x[,8]+x[,12]+x[,16]+x[,19],
          family=binomial(link="logit"))
summary(fit1)
# Remove the insignificant predictors
fit2<-glm(Y~x[,2]+x[,4]+x[,6]+x[,12]+x[,16]+x[,19],
          family=binomial(link="logit"))
summary(fit2)
# Variance inflation factor
library(MASS)
stepAIC(fit2)
library(car)
vif(fit2)
# Gender, Income, Education, Credit Limit,  Total_Trans_Amt, Avg_Utilization_Ratio are significant.

# -------------------EXTRA ANALYSIS--------------------

# looking at predictions made by our model
probabilities <- predict(fit2, type = 'response')
predicted.classes <- ifelse(probabilities > 0.5, 1,0)
library(caret)
predicted.classes = as.integer(predicted.classes)

confusionmatrix <- confusionMatrix(as.factor(predicted.classes),as.factor(Y))
ctable <- as.table(matrix(c(5967, 1109, 1, 4), nrow = 2, byrow = TRUE))
#visualizing confusion matrix
fourfoldplot(ctable, color = c("blue", "green"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")
# ------------------EXTRA ANALYSIS--------------------
##  visually checking linearity ASSUMPTION of continuous variables in our model
indexcontinuous <- c(12,16,19)
checklinearitydata <- x[,indexcontinuous]
check2 <- checklinearitydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(check2, aes(logit, predictor.value)) +
  geom_point(size=0.5,alpha=0.5)+
  geom_smooth(method = "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales = "free_y")




##---3.1------------ Missing Data ------------------

# Generate MCAR
set.seed(111)
a1=sample(1:7081,71,FALSE)
bank[a1,2]=NA
b1=bank[,2]
b2=bank[,4]
c1=data.frame(b1,b2)
library(mice)
md.pattern(c1)
AgeD <- mice(c1,m=2,maxit=50,meth='norm')
summary(AgeD)
AgeD$imp$b1
impData <- complete(AgeD,1)

library(VIM)


# Generate MNAR
# We suppose that customers with large age might be unwilling to tell their age.
b=bank
a3<-which(b[,2]>60) #age greater than 60
set.seed(222)
a4=sample(a3,71,FALSE)
b[a4,2]=NA
b3=b[,2]
c2=data.frame(b3,b2)
md.pattern(c2)
AgeD2 <- mice(c2,m=2,maxit=50,meth='norm')
summary(AgeD2)
AgeD2$imp$b1
impData2 <- complete(AgeD2,1)
# Compare the original data, MCAR and MNAR
summary(bank$Customer_Age)     # original
summary(impData[,1])     # MCAR
summary(impData2[,1])    # MNAR

##---3.2------------ 1st Hypothesis with MCAR ------------------

# H0:mean(male age)=mean(female age) vs Ha: Not H0 at alpha=0.05
MaleAgeMCAR<-impData[which(x[,2]==0),1]
FemaleAgeMCAR<-impData[which(x[,2]==1),1]
t.test(MaleAgeMCAR,FemaleAgeMCAR,alternative="two.sided",mu=0)
# We fail to reject H0.
# Compare MCAR with original data
t.test(MaleAge,FemaleAge,alternative="two.sided",mu=0)$conf.int
t.test(MaleAgeMCAR,FemaleAgeMCAR,alternative="two.sided",mu=0)$conf.int
t.test(MaleAge,FemaleAge,alternative="two.sided",mu=0)$p.value
t.test(MaleAgeMCAR,FemaleAgeMCAR,alternative="two.sided",mu=0)$p.value

##---3.3------------ 2nd Hypothesis with MCAR ------------------

# Model: Logit(Y)=β0+∑(βi*xi)
# Predictor: Age, Gender, Education, Income, Months on book, Credit Limit, Total_Trans_Amt, Avg_Utilization_Ratio
# H0: All βi=0 vs Ha: Not H0  at alpha=0.05
fit1MCAR<-glm(Y~impData[,1]+x[,2]+x[,4]+x[,6]+x[,8]+x[,12]+x[,16]+x[,19],
          family=binomial(link="logit"))
summary(fit1MCAR)
# Remove the insignificant predictors
fit2MCAR<-glm(Y~x[,2]+x[,4]+x[,6]+x[,12]+x[,16]+x[,19],
          family=binomial(link="logit"))
summary(fit2MCAR)
# Variance inflation factor
stepAIC(fit2MCAR)
vif(fit2MCAR)
# Gender, Income, Education, Credit Limit,  Total_Trans_Amt, Avg_Utilization_Ratio are significant.


##---4.1------------ 1st Hypothesis with MNAR ------------------

# H0:mean(male age)=mean(female age) vs Ha: Not H0 at alpha=0.05
MaleAgeMCAR2<-impData2[which(x[,2]==0),1]
FemaleAgeMCAR2<-impData2[which(x[,2]==1),1]
t.test(MaleAgeMCAR2,FemaleAgeMCAR2,alternative="two.sided",mu=0)
# We fail to reject H0.
# Compare MNAR with original data
t.test(MaleAge,FemaleAge,alternative="two.sided",mu=0)$conf.int
t.test(MaleAgeMCAR2,FemaleAgeMCAR2,alternative="two.sided",mu=0)$conf.int
t.test(MaleAge,FemaleAge,alternative="two.sided",mu=0)$p.value
t.test(MaleAgeMCAR2,FemaleAgeMCAR2,alternative="two.sided",mu=0)$p.value

##---4.2------------ 2nd Hypothesis with MNAR ------------------

# Model: Logit(Y)=β0+∑(βi*xi)
# Predictor: Age, Gender, Education, Income, Months on book, Credit Limit, Total_Trans_Amt, Avg_Utilization_Ratio
# H0: All βi=0 vs Ha: Not H0  at alpha=0.05
fit1MNAR<-glm(Y~impData2[,1]+x[,2]+x[,4]+x[,6]+x[,8]+x[,12]+x[,16]+x[,19],
              family=binomial(link="logit"))
summary(fit1MNAR)
# Remove the insignificant predictors
fit2MNAR<-glm(Y~x[,2]+x[,4]+x[,6]+x[,12]+x[,16]+x[,19],
          family=binomial(link="logit"))
summary(fit2MNAR)
# Variance inflation factor
stepAIC(fit2MNAR)
vif(fit2MNAR)
# Gender, Income, Education, Credit Limit,  Total_Trans_Amt, Avg_Utilization_Ratio are significant.



#-----------Extra Visualizations-----------
# column 12 is credit limit
hist(bank$Credit_Limit,
     main = "Histogram of Credit Limit",
     xlab = "Credit Limit",
     col = "blue")

hist(bank$Avg_Utilization_Ratio,
     main = "Histogram of Utilization Ratio",
     xlab = "Avg Utilization Ratio",
     col = "green")


library(corrplot)
correlations <- cor(x[,12:19])
corrplot(correlations, method="circle")

