dats <- readr::read_csv('data/Sub2.csv')
head(dats, 100)
dats$REGION <- as.factor(dats$REGION) 
dats$OWNERSHP<- as.factor(dats$OWNERSHP)
dats$FOODSTMP<-as.factor(dats$FOODSTMP)
dats$SEX <- as.factor(dats$SEX) 
dats$MARST <- as.factor(dats$MARST) 
dats$RACE <- as.factor(dats$RACE) 
dats$HISPAN <- as.factor(dats$HISPAN)
dats$EDUC <- as.factor(dats$EDUC)
dats$EMPSTAT <- as.factor(dats$EMPSTAT)
dats$GENERATION <- as.factor(dats$GENERATION)
dats$VETSTAT <- as.factor(dats$VETSTAT) 
dats$NCHILD <- as.factor(dats$NCHILD)
dats$POVERTY<-as.factor(dats$POVERTY)
dats$AGE<-as.factor(dats$AGE)
dats$INCTOT<-as.factor(dats$INCTOT)
dats$FTOTINC<-as.factor(dats$FTOTINC)
dats$HHINCOME<-as.factor(dats$HHINCOME)
p <- 0.3
set.seed(42)
train_rows <- sample(nrow(dats), size = p*nrow(dats))
test_rows  <- which(!1:nrow(dats)%in%train_rows)

features <- c("REGION", "OWNERSHIP", "FOODSTAMP", "AGE", "SEX", "MARST", "RACE", "HISPAN", "EDUC", "EMPSTAT", "NCHILD", "POVERTY", "VETSTAT", "GENERATION", "INCTOT", "HHTOTINC", "FTOTINC")
response <- c("HCOVANY")

Features <- which(colnames(dats)%in%features)
Response <- which(colnames(dats)%in%response)

x_train <- dats[train_rows, Features]
y_train <- dats[train_rows, Response]
x_test <- dats[test_rows, Features]
y_test <- dats[test_rows, Response]
train_data <- cbind(x_train, y_train)
test_data <- cbind(x_test, y_test)




linear <- lm(y_train ~ ., data = train_data)

summary(linear)

predicted_lin = predict(linear, x_test)

press <- sqrt(sum((y_test - predicted_lin)^2))/nrow(x_test)

#Logistic Regression
Y_train    <- as.numeric(y_train > 100)
Train_Data <- cbind(x_train, Y_train)

logistic   <- glm(Y_train ~ ., 
                  data = Train_Data, 
                  family = 'binomial')

summary(logistic)

predicted_log = predict(logistic, x_test, type = 'response')
#Decision Trees
needs('rpart')

# grow tree
fit_dt <- rpart(y_train ~ ., data = train_data, method = "class")

summary(fit_dt)

# Predict Output
predicted_dt = predict(fit_dt, x_test)

#Support Vector Machines
needs('e1071')

# Fitting model
fit_svm <- svm(y_train ~ ., data = train_data)

summary(fit_svm)

# Predict Output
predicted_svm = predict(fit_svm, x_test)

#Naive Bayes
needs('e1071')

# Fitting model
fit_nb <- naiveBayes(y_train ~ ., data = train_data)

summary(fit_nb)

# Predict Output
predicted_nb = predict(fit_nb, x_test)

#KNN
needs('class')

x <- cbind(x_train,y_train)
#Fitting model
fit_knn <- knn(y_train ~ ., data = train_data, k = 5)

summary(fit_knn)

#Predict Output
predicted_knn = predict(fit_knn, x_test)

#K Means
needs('cluster')

fit_km <- kmeans(X, 3)
#5 cluster solution

#Random Forrest
needs('randomForest')

#x <- cbind(x_train,y_train)
#Fitting model
fit_rf <- randomForest(y_train ~ .,  data = train_data, ntree = 500)

summary(fit_rf)

#Predict Output
predicted_rf = predict(fit_rf, x_test)

#Dimension Reduction Algorithms 
library(stats)

pca <- princomp(train, cor = TRUE)
train_reduced <- predict(pca,train)
test_reduced <- predict(pca,test)

# Gradient Boosting & Ada Boost
needs('caret')

# Fitting model
fitControl <- trainControl(method = "repeatedcv",
                           number = 4, 
                           repeats = 4)

fit_boost <- train(y_train ~ ., data = train_data, 
                   method = "gbm",
                   trControl = fitControl,
                   verbose = FALSE)

predicted_boost = predict(fit_boost, 
                          x_test,
                          type = "raw")[,2]
