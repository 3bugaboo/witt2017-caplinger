dats <- readr::read_csv('data/Sub2.csv')
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
p <- 0.7
set.seed(42)
train_rows <- sample(nrow(dats), size = p*nrow(dats))
test_rows  <- which(!1:nrow(dats)%in%train_rows)

features <- c("REGION", "OWNERSHP", "FOODSTMP", "AGE", "SEX", "MARST", "RACE", "HISPAN", "EDUC", "EMPSTAT", "NCHILD", "POVERTY", "VETSTAT", "GENERATION", "INCTOT", "HHINCOME", "FTOTINC")
response <- c("HCOVANY")

Features <- which(colnames(dats)%in%features)
Response <- which(colnames(dats)%in%response)

x_train <- dats[train_rows, Features]
y_train <- dats[train_rows, Response]
x_test <- dats[test_rows, Features]
y_test <- dats[test_rows, Response]
train_data <- cbind(x_train, y_train)
test_data <- cbind(x_test, y_test)

write.csv(train_data, 'data/train_data.csv', row.names = F)
write.csv(test_data, 'data/test_data.csv', row.names = F)

h2o::h2o.init()
train <- h2o::h2o.importFile('data/train_data.csv')
test <- h2o::h2o.importFile('data/test_data.csv')
logistic <- h2o::h2o.glm(x = features, 
                         y = response, 
                         training_frame = train,
                         family = 'binomial')

h2o_prediction <- h2o::h2o.predict(logistic, newdata = test)

stuff <- as.data.frame(h2o_prediction)
merge <- cbind(test_data$HCOVANY, stuff)

write.csv(merge, file = 'data/prediction.csv', row.names = F)
