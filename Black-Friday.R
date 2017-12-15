
  
  #Import thr dataset

train = read.csv("C://Users//Administrator//Desktop//AnalytcVidhya//BlackFriday//train.csv")
test= read.csv("C://Users//Administrator//Desktop//AnalytcVidhya//BlackFriday//test.csv")
test$Purchase=NA


###Data-Massaging
# Combine the test and train dataset
#Inspect the structure of the dataset

combi=rbind(train,test)
library(caret)
str(combi)


#Step 1: Outlier and Missing Value treatment

apply(combi, 2, function(x) sum(is.na(x)*100/length(x)))
combi$Product_Category_2=ifelse(is.na(combi$Product_Category_2),yes=0,no=combi$Product_Category_2)
combi$Product_Category_3=ifelse(is.na(combi$Product_Category_3),yes=0,no=combi$Product_Category_3)

apply(combi, 2, function(x) sum(is.na(x)*100/length(x)))



#Step2:Convert all the variables to categorical

combi$Marital_Status=as.factor(combi$Marital_Status)
combi$Occupation=as.factor(combi$Occupation)
combi$Product_Category_1=as.factor(combi$Product_Category_1)
combi$Product_Category_2=as.factor(combi$Product_Category_2)
combi$Product_Category_3=as.factor(combi$Product_Category_3)
combi$User_ID=as.factor(combi$User_ID)

str(combi)
train=combi[1:550068,]
test=combi[550069:783667,]



#3 :DeepLearning

library(h2o)
h2o.init(nthreads = -1)
model = h2o.deeplearning(y = 'Purchase',
                         training_frame = as.h2o(train),
                         activation = 'Rectifier',
                         keep_cross_validation_predictions = T,
                         nfolds = 5,
                         hidden = c(5,5),
                         epochs = 5,
                         train_samples_per_iteration = -2,
                         variable_importances = T,
                         sparse = TRUE
                         
)


```
#Prediction

h2o.varimp(model)
h2o.varimp_plot(model)
h2o.mse(model, xval = TRUE)
purchase_pred = h2o.predict(model, newdata = as.h2o(test[,-12]))
h2o.shutdown(prompt = FALSE)


