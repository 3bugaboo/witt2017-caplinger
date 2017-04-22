file <- 'C:\\Users\\Jason\\Downloads\\Julies Snip of Data.xlsx'

library(XLConnect)

wb <- loadWorkbook(file)

sheet <- readWorksheet(wb, sheet = 1)

sheet <- sheet[-1,] # removes first row of NA

sheet$HCOVANY <- sheet$HCOVANY-1

## Set up a logistic regression based on subset of data

model <- glm(HCOVANY~AGE, data = sheet[1:100,], family = 'binomial')

## Make predictions using the model for some data that was reserved 

predict(model,AGE = data.frame(new.data), type = 'response')

file.root <- gsub('.xlsx', '.Rdata', file) 

save(list = 'sheet', file = file.root, compression_level = 9)


little.sheet1 <- sheet[1:60,]
little.sheet2 <- sheet[61:nrow(sheet),]

merge.sheet <- rbind(little.sheet1, little.sheet2)



