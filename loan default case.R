rm(list = ls())
setwd("C:/Users/Harshita Prasad/Desktop/loan default case")
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", 
      "Information", "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees', "psych", 
      "usdm", "ggbeeswarm", "scales", "class", "yardstick")
lapply(x, require, character.only = TRUE)
rm(x)
original_data = read.csv('bank-loan.csv',header = T,na.strings = c(""," ","NA"))
df = original_data
str(df)
summary(df)
names(df)[names(df) == 'ed'] <- 'education'
names(df)[names(df) == 'employ'] <- 'employstatus'
names(df)[names(df) == 'othdebt'] <- 'otherdebt'
categorical_var = c('education', 'default')
numerical_var = c('age', 'employstatus', 'address', 'income','debtinc','creddebt','otherdebt')
typ_conv = function(df,var,type){
  df[var] = lapply(df[var], type)
  return(df)
}
df = typ_conv(df,categorical_var, factor)
str(df)

#visualizations
multi.hist(df$age, main = NA, dcol = c("blue", "red"), title(main = NULL,xlab = "Age",ylab = "Density"),
           dlty = c("solid", "solid"), bcol = "linen")
multi.hist(df$employstatus, main = NA, dcol = c("blue", "red"), title(main = NULL,xlab = "Employment Status",ylab = "Density"),
           dlty = c("solid", "solid"), bcol = "linen")
multi.hist(df$address, main = NA, dcol = c("blue", "red"), title(main = NULL,xlab = "Address",ylab = "Density"),
           dlty = c("solid", "solid"), bcol = "linen")
multi.hist(df$income, main = NA, dcol = c("blue", "red"), title(main = NULL,xlab = "Income",ylab = "Density"),
           dlty = c("solid", "solid"), bcol = "linen")
multi.hist(df$debtinc, main = NA, dcol = c("blue", "red"), title(main = NULL,xlab = "Debt Income Ratio",ylab = "Density"),
           dlty = c("solid", "solid"), bcol = "linen")
multi.hist(df$creddebt, main = NA, dcol = c("blue", "red"), title(main = NULL,xlab = "Credit Debit Ratio",ylab = "Density"),
           dlty = c("solid", "solid"), bcol = "linen")
multi.hist(df$otherdebt, main = NA, dcol = c("blue", "red"), title(main = NULL,xlab = "Other Debt",ylab = "Density"),
           dlty = c("solid", "solid"), bcol = "linen")

# To check for number of missing data in whole var or a variable
apply(df, 2, function(x) {sum(is.na(x))})
missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
row.names(missing_val) = NULL
names(missing_val)[1] =  "missing_percentage"
missing_val$missing_percentage = (missing_val$missing_percentage/nrow(df)) * 100
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Missing_values.csv", row.names = F)

#knn method
df = knnImputation(df)  
sum(is.na(df))

#outlier analysis
for (i in 1:length(numerical_var))
{
  assign(paste0("bp",i), ggplot(aes_string(x="default",y = (numerical_var[i])), d=df)+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="blue", fill = "indianred3" ,outlier.shape=18,
                        outlier.size=2, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(x="default status of loan cases", y=numerical_var[i])+
           ggtitle(paste("Box plot of default status of loan cases with",numerical_var[i])))
}
gridExtra::grid.arrange(bp1,ncol=1)
gridExtra::grid.arrange(bp2,ncol=1)
gridExtra::grid.arrange(bp3,ncol=1)
gridExtra::grid.arrange(bp4,ncol=1)
gridExtra::grid.arrange(bp5,ncol=1)
gridExtra::grid.arrange(bp6,ncol=1)
gridExtra::grid.arrange(bp7,ncol=1)

# Replace all outliers with NA and impute
for(i in numerical_var){
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  df[,i][df[,i] %in% val] = NA
}
df = knnImputation(df)

#correlation
corrgram(df[,numerical_var], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Chi-squared Test of Independence   - if p<0.05, reject null hypohesis, i.e keep the variable.
categorical_df = df[,categorical_var]
for (i in categorical_var){
  for (j in categorical_var){
    print(i)
    print(j)
    print(chisq.test(table(categorical_df[,i], categorical_df[,j]))$p.value)
  }
}

#ANOVA
anova_age =(lm(age ~ default, data = df))
summary(anova_age)
anova_employstatus =(lm(employstatus ~ default, data = df))
summary(anova_employstatus)
anova_address =(lm(address ~ default, data = df))
summary(anova_address)
anova_income =(lm(income ~ default, data = df))
summary(anova_income)
anova_debtinc =(lm(debtinc ~ default, data = df))
summary(anova_debtinc)
anova_creddebt =(lm(creddebt ~ default, data = df))
summary(anova_creddebt)
anova_otherdebt =(lm(otherdebt ~ default, data = df))
summary(anova_otherdebt)

#multicollinearity
vif(df)

#Normalisation
for(i in numerical_var){
  print(i)
  df[,i] = (df[,i] - min(df[,i]))/
    (max(df[,i] - min(df[,i])))
}

# by rank using beeswarm-syle plots
ggplot(df, aes(x = df$default, y = df$age, color = "rank")) +
  geom_quasirandom(alpha = 0.7, size = 1.5) +
  labs(title = "Age by Default Status", 
       x = "Default Status",
       y = "Age") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(df, aes(x = df$default, y = df$employstatus, color = "rank")) +
  geom_quasirandom(alpha = 0.7, size = 1.5) +
  labs(title = "Employment Status by Default Status", 
       x = "Default Status",
       y = "Employment Status") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(df, aes(x = df$default, y = df$address, color = "rank")) +
  geom_quasirandom(alpha = 0.7, size = 1.5) +
  labs(title = "Address by Default Status", 
       x = "Default Status",
       y = "Address") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(df, aes(x = df$default, y = df$income, color = "rank")) +
  geom_quasirandom(alpha = 0.7, size = 1.5) +
  labs(title = "Income by Default Status", 
       x = "Default Status",
       y = "Income") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(df, aes(x = df$default, y = df$debtinc, color = "rank")) +
  geom_quasirandom(alpha = 0.7, size = 1.5) +
  labs(title = "Debt to Income by Default Status", 
       x = "Default Status",
       y = "Debt to Income") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(df, aes(x = df$default, y = df$creddebt, color = "rank")) +
  geom_quasirandom(alpha = 0.7, size = 1.5) +
  labs(title = "Credit to Debt Ratio by Default Status", 
       x = "Default Status",
       y = "Credit to Debt Ratio") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(df, aes(x = df$default, y = df$otherdebt, color = "rank")) +
  geom_quasirandom(alpha = 0.7, size = 1.5) +
  labs(title = "Other Debt by Default Status", 
       x = "Default Status",
       y = "Other Debt") +
  theme_minimal() +
  theme(legend.position = "none")


#lets check categorical variables and target variables
for(i in 1:length(categorical_var))
{
  assign(paste0("b",i),ggplot(aes_string(y='default',x = (categorical_var[i])),
                              data=subset(df))+
           geom_bar(stat = "identity",fill = "green") +
           ggtitle(paste("Default Status with respect to",categorical_var[i])))+
    theme(axis.text.x = element_text( color="red", size=8))+
    theme(plot.title = element_text(face = "old"))
}
gridExtra::grid.arrange(b1,ncol=1)

#Clean the environment
rmExcept(c("original_data",'df'))

#Divide data into train and test using stratified sampling method
set.seed(1234)
train.index = createDataPartition(df$default, p = .80, list = FALSE)
train = df[ train.index,]
test  = df[-train.index,]

#Logistic Regression
logit_model = glm(default ~ ., data = train, family = "binomial")

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")

#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)

##Evaluate the performance of classification model
CM_LR = table(test$default, logit_Predictions)
confusionMatrix(CM_LR)
fourfoldplot(CM_LR)

# Area under ROC curve
roc.curve(test$default, logit_Predictions)

##False Negative rate
#FNR = FN/FN+TP 

#Accuracy: 78.70%
#FNR: 75.00%

##Decision tree for classification
#Develop Model on training data
C50_model = C5.0(default ~., train, trials = 100, rules = TRUE)

#Summary of DT model
summary(C50_model)

#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")

#Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-9], type = "class")

##Evaluate the performance of classification model
CM_C50 = table(test$default, C50_Predictions)
confusionMatrix(CM_C50)
fourfoldplot(CM_C50)

# Area under ROC curve
roc.curve(test$default, C50_Predictions)

#Accuracy: 80.47%
#FNR: 60.00%

###Random Forest
RF_model = randomForest(default ~ ., train, importance = TRUE, ntree = 100)

#Extract rules fromn random forest
#transform rf object to an inTrees' format
treeList = RF2List(RF_model)  

# Extract rules
exec = extractRules(treeList, train[,-9])  # R-executable conditions

# Visualize some rules
exec[1:2,]

# Make rules more readable:
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]

# Get rule metrics
ruleMetric = getRuleMetric(exec, train[,-9], train$default)  # get rule metrics

# evaulate few rules
ruleMetric[1:2,]

#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-9])

##Evaluate the performance of classification model
CM_RF = table(test$default, RF_Predictions)
confusionMatrix(CM_RF)
fourfoldplot(CM_RF)

# Area under ROC curve
roc.curve(test$default, RF_Predictions)

#Accuracy = 81.07%
#FNR = 67.5%

###Random Forest1
RF_model1 = randomForest(default ~ ., train, importance = TRUE, ntree = 500)

#Extract rules fromn random forest
#transform rf object to an inTrees' format
treeList1 = RF2List(RF_model1)  

# Extract rules
exec1 = extractRules(treeList1, train[,-9])  # R-executable conditions

# Visualize some rules
exec1[1:2,]

# Make rules more readable:
readableRules1 = presentRules(exec1, colnames(train))
readableRules1[1:2,]

# Get rule metrics
ruleMetric1 = getRuleMetric(exec1, train[,-9], train$default)  # get rule metrics

# evaulate few rules
ruleMetric1[1:2,]

#Predict test data using random forest model
RF_Predictions1 = predict(RF_model1, test[,-9])

##Evaluate the performance of classification model
CM_RF1 = table(test$default, RF_Predictions1)
confusionMatrix(CM_RF1)
fourfoldplot(CM_RF1)

# Area under ROC curve
roc.curve(test$default, RF_Predictions1)

#Accuracy = 81.07%
#FNR = 72.5%

##KNN Implementation
#Predict test data
KNN_Predictions = knn(train[, 1:8], test[, 1:8], train$default, k = 7)

#Confusion matrix
CM_KNN = table(test$default, KNN_Predictions)
confusionMatrix(CM_KNN)
fourfoldplot(CM_KNN)

# Area under ROC curve
roc.curve(test$default, KNN_Predictions)

#Accuracy
sum(diag(Conf_matrix))/nrow(test)

#Accuracy = 82.84%
#FNR = 21.05%

#naive Bayes
#Develop model
NB_model = naiveBayes(default ~ ., data = train)

#predict on test cases #raw
NB_Predictions = predict(NB_model, test[,1:8], type = 'class')

#Look at confusion matrix
CM_NB = table(test$default, NB_Predictions)
confusionMatrix(CM_NB)
fourfoldplot(CM_NB)

# Area under ROC curve
roc.curve(test$default, NB_Predictions)

#Accuracy: 82.25%
#FNR: 42.5%

write.csv(df, "df.csv", row.names = F)
