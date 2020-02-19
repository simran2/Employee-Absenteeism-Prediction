rm(list=ls())
#set working directory
setwd('C:/Users/simran.chhabra1/Desktop/extras/personal/Edwisor projects/Employee absenteeism')

#check current working directory
getwd()

#read data
library("readxl")
df = read_excel('Absenteeism_at_work_Project.xls')

#check data
str(df)
summary(df)
head(df)


#ID of an employee would not help in analysis. Also, we have BMI variable so Weight and Height is not needed.
drop_cols = c("ID","Weight","Height")
df = df[,!names(df) %in% drop_cols]

#Check values in Reason for absense. Note: value 20 is missing.
sort(unique(df$`Reason for absence`))

#fill nulls in Reason for absence with mode
df[is.na(df$`Reason for absence`), 'Reason for absence']
pcount_freq = table(df$`Reason for absence`)
mode_pcount = which.max(pcount_freq)
df[is.na(df$`Reason for absence`), 'Reason for absence'] = mode_pcount


#create dummy variables for categorical variable Reason for absence
reason_columns = dummies::dummy(df$`Reason for absence`, sep='_')
head(reason_columns)

df = df[,!names(df) %in% 'Reason for absence']

#Divide the newly created dummy variables into 4 categories.
df$Reason_1 = apply(reason_columns[,1:14], 1, max)
df$Reason_2 = apply(reason_columns[,15:17], 1, max)
df$Reason_3 = apply(reason_columns[,18:21], 1, max)
df$Reason_4 = apply(reason_columns[,22:28], 1, max)

#Outlier analysis
summary(df)
#No outlier observed in the summary of data  
boxplot(x=df$`Work load Average/day`)

#Fill missing values

#Bifurcate categorical and numerical columns.
categorical_cols = c('Reason_1', 'Reason_2', 'Reason_3', 'Reason_4', 'Month of absence',
                    'Day of the week', 'Seasons', 'Age',
                    'Hit target', 'Disciplinary failure',
                    'Education', 'Son', 'Social drinker', 'Social smoker', 'Pet')
numerical_cols = c()
for(col in names(df)){
  if(!col %in% categorical_cols){
    print(col)
    numerical_cols = c(numerical_cols,col)
  }
}


#filling missing values in categorical columns using mode and in numeric columns using mean.
col_names = names(df)
for(i in 1:ncol(df)){
  
  if(col_names[i] %in% categorical_cols){
    #print(col_names[i])
    pcount_freq = table(df[,i])
    mode_pcount = which.max(pcount_freq)
    #print(mode_pcount)
    df[is.na(df[,i]), i] = mode_pcount
  }
  else {
    #print(col_names[i])
    df[is.na(df[[i]]), i] = mean(df[[i]], na.rm = TRUE)
  }
}

#Feature Selection

heatmap(cor(df))

str(df)

#Dependency visualize

scatter.smooth(y = df$`Absenteeism time in hours`, x = df$`Transportation expense`)


require(caTools)
sample = sample.split(df, SplitRatio = 0.8)
train1 = subset(df, sample==TRUE)
test1 = subset(df, sample==FALSE)

train1_scaled = scale(train1)
test1_scaled = scale(test1)

target = test1$`Absenteeism time in hours`
test1 = test1[,!names(test1) %in% 'Absenteeism time in hours']


#1) Linear Regression
lm_model = lm(data = data.frame(train1), Absenteeism.time.in.hours~.)
pred = predict(lm_model, data.frame(test1))
summary(lm_model)
library(DMwR)
regr.eval(target, pred, stats = c('mae','rmse'))

#2) Random Forest regression
library(randomForest)
rf_model = randomForest(Absenteeism.time.in.hours~., data=data.frame(train1), mtry=1,importance=TRUE, na.action=na.omit, ntree=100)
pred = predict(rf_model, data.frame(test1))
regr.eval(target, pred, stats = c('mae','rmse'))
y = target
r2 = 1 - sum((y-pred)^2)/sum((y-mean(y))^2)
r2

#3) KNeighbors Regression
library(class)
library(FNN)
train2 = train1[,!names(train1) %in% 'Absenteeism time in hours']
pred = knn.reg(train= train2, test= test1, y=train1$`Absenteeism time in hours`)
regr.eval(target, pred$pred, stats = c('mae','rmse'))
y = target
r2 = 1 - sum((y-pred$pred)^2)/sum((y-mean(y))^2)
r2

