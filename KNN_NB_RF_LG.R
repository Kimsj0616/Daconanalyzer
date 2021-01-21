# install and load packages
install.packages("readr")
install.packages("caret")
install.packages("MASS")
install.packages("klaR")
install.packages("randomForest")

packageslist <- list("readr", "caret", "MASS", "klaR", "randomForest")
load_packages <- lapply(packageslist, require, character.only = T)

# laod train & test dataset
train_mach <- read_csv("C:/train.csv")
#getwd()
head(train_mach, n = 5)
test_mach <- read_csv("C:/test_x.csv")


# check missing values
missingcols <- sapply(train_mach, function(x) {
  any(is.na(x))
})  # no missing values

sum(missingcols)


#remove unwanted variables
#cancer_classify <- cancer_classify[, -1]  # removed patient ID variable

# choose variables
train_mach_variables_selected <- data.frame(QaA = train_mach$QaA, 
                                            QaE = train_mach$QaE, 
                                            QdA = train_mach$QdA,
                                            age_group = train_mach$age_group,
                                            education = train_mach$education,
                                            engnat = train_mach$engnat,
                                            familysize = train_mach$familysize,
                                            gender = train_mach$gender,
                                            hand = train_mach$hand,
                                            married = train_mach$married,
                                            religion = train_mach$religion,
                                            voted = as.factor(train_mach$voted)
)
class(train_mach_variables_selected$voted)

test_mach_variables_selected <- data.frame(QaA = test_mach$QaA, 
                                           QaE = test_mach$QaE, 
                                           QdA = test_mach$QdA,
                                           age_group = test_mach$age_group,
                                           education = test_mach$education,
                                           engnat = test_mach$engnat,
                                           familysize = test_mach$familysize,
                                           gender = test_mach$gender,
                                           hand = test_mach$hand,
                                           married = test_mach$married,
                                           religion = test_mach$religion,
                                           index = test_mach$index
                                           
)



# KNN
modelKNN <- train(voted ~ ., data = train_mach_variables_selected, method = "knn", preProcess = c("center", 
                                                                                                  "scale"))  # data is normalised using Preprocess
# Naive Bayes
modelNB <- train(voted ~ ., data = train_mach_variables_selected, method = "nb")

# Random Forest
modelRF <- train(voted ~ ., data = train_mach_variables_selected, method = "rf", ntree = 100, 
                 importance = T)
# Logisitic Regression
modelLG <- train(voted ~ ., data = train_mach_variables_selected, method = "glm", family = binomial)



# Predict on test set
pTestingKNN <- predict(modelKNN, test_mach_variables_selected)
head(pTestingKNN)

write.csv(pTestingKNN, file = "1020KNN.csv", row.names = FALSE)

pTestingNB <- predict(modelNB, test_mach_variables_selected)
head(pTestingNB)

write.csv(pTestingNB, file = "1020NB.csv", row.names = FALSE)

pTestingRF <- predict(modelRF, test_mach_variables_selected)
write.csv(pTestingRF, file = "1020RF.csv", row.names = FALSE)

pTestingLG <- predict(modelLG, test_mach_variables_selected)
write.csv(pTestingLG, file = "1020LG.csv", row.names = FALSE)