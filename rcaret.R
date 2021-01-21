install.packages("caret")
install.packages("ranger")
library(caret)
library(ranger)

train_ds <- read.csv(file = "C:/train.csv", 
                     header = TRUE, fileEncoding = "UTF-8-BOM")

test_ds <- read.csv(file = "C:/test_x.csv", 
                    header = TRUE, fileEncoding = "UTF-8-BOM")

set.seed(100)

rf_fit <- train(as.factor(voted) ~ ., data = train_ds, method = "ranger")

rf_fit



#tr <- trainControl(method = "repeatedCV", number=10, repeats = 5)

#step_model <- train(annual_pm ~ ., data= dplyr::select(lur, -site_id), method = "lmStepAIC", trControl = tr, trace=FALSE)
# compare predicted outcome and true outcome
#confusionMatrix(abalone_rf_pred, as.factor(abalone_test$old))


# predict the outcome on a test set
trainds_rf_pred <- predict(rf_fit, test_ds)

trainds_rf_pred





result_caret_rf <- data.frame(index = test_ds$index, voted = trainds_rf_pred)


write.csv(result_caret_rf, file = "result_caret_rf.csv", row.names = FALSE)