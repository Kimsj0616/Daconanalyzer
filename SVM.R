
library(rpart)
library(e1071)
library(caret)
library(tidyverse)
library(ROCR)
library(rattle)
library(MASS)

train_ds <- read.csv(file = "C:/train.csv", 
                    header = TRUE, fileEncoding = "UTF-8-BOM")

train_ds$voted <- as.factor(train_ds$voted)


#head(train_ds) #check train data set

test_ds <- read.csv(file = "C:/test_x.csv", 
                     header = TRUE, fileEncoding = "UTF-8-BOM")

#head(test_ds) #check test data sdfset



#----------------마키아벨리즘 성향 구해보기-------------------
#-------------variable------------------
#mac : 마키아벨리즘 점수를 더하거나 빼서 총 합을 구한 후 범위에 따라 설정
#mac_time : 설문에 응답하는 데 걸린 시간 총 합을 구한 후 범위에 따라 설정
#wr_total : wr 질문들에 대한 답변의 점수를 더해 범위에 따라 설정


train_ds$mac <- train_ds$QbA+train_ds$QcA+5-train_ds$QeA+5-train_ds$QfA+train_ds$QhA+train_ds$QjA+5-train_ds$QkA+train_ds$QmA+train_ds$QoA+5-train_ds$QqA+5-train_ds$QrA+train_ds$QsA
train_ds$mac_time <- train_ds$QaE + train_ds$QbE + train_ds$QcE + train_ds$QdE + train_ds$QeE + train_ds$QfE + train_ds$QgE + train_ds$QhE + train_ds$QiE + train_ds$QjE + train_ds$QkE + train_ds$QlE + train_ds$QmE + train_ds$QnE + train_ds$QoE + train_ds$QpE + train_ds$QqE + train_ds$QrE + train_ds$QsE + train_ds$QtE


train_ds[train_ds$mac >= 44, "mac"] = 3
train_ds[train_ds$mac < 44 & train_ds$mac >= 28 , "mac"] = 2
train_ds[train_ds$mac > 10, "mac"] = 1

train_ds[train_ds$mac_time >= 100000, "mac_time"] = 3
train_ds[train_ds$mac_time < 100000 & train_ds$mac_time >=50000, "mac_time"] = 2
train_ds[train_ds$mac_time >=10, "mac_time"] = 1

train_ds$wr_total <- train_ds$wr_01 + train_ds$wr_02 + train_ds$wr_03 + train_ds$wr_04 + train_ds$wr_05 + train_ds$wr_06 + train_ds$wr_07 + train_ds$wr_08 + train_ds$wr_09 + train_ds$wr_10 + train_ds$wr_11 + train_ds$wr_12 + train_ds$wr_13
train_ds[train_ds$wr_total < 3, "wr_total"] = 1
train_ds[train_ds$wr_total < 9 & train_ds$wr_total >=4, "wr_total"] = 2
train_ds[train_ds$wr_total >= 9, "wr_total"] = 3





test_ds$mac <- test_ds$QbA+test_ds$QcA+5-test_ds$QeA+5-test_ds$QfA+test_ds$QhA+test_ds$QjA+5-test_ds$QkA+test_ds$QmA+test_ds$QoA+5-test_ds$QqA+5-test_ds$QrA+test_ds$QsA
test_ds$mac_time <- test_ds$QaE + test_ds$QbE + test_ds$QcE + test_ds$QdE + test_ds$QeE + test_ds$QfE + test_ds$QgE + test_ds$QhE + test_ds$QiE + test_ds$QjE + test_ds$QkE + test_ds$QlE + test_ds$QmE + test_ds$QnE + test_ds$QoE + test_ds$QpE + test_ds$QqE + test_ds$QrE + test_ds$QsE + test_ds$QtE


test_ds[test_ds$mac >= 44, "mac"] = 3
test_ds[test_ds$mac < 44 & test_ds$mac >= 28 , "mac"] = 2
test_ds[test_ds$mac > 10, "mac"] = 1

test_ds[test_ds$mac_time >= 100000, "mac_time"] = 3
test_ds[test_ds$mac_time < 100000 & test_ds$mac_time >=50000, "mac_time"] = 2
test_ds[test_ds$mac_time >=10, "mac_time"] = 1

test_ds$wr_total <- test_ds$wr_01 + test_ds$wr_02 + test_ds$wr_03 + test_ds$wr_04 + test_ds$wr_05 + test_ds$wr_06 + test_ds$wr_07 + test_ds$wr_08 + test_ds$wr_09 + test_ds$wr_10 + test_ds$wr_11 + test_ds$wr_12 + test_ds$wr_13
test_ds[test_ds$wr_total < 3, "wr_total"] = 1
test_ds[test_ds$wr_total < 9 & test_ds$wr_total >=4, "wr_total"] = 2
test_ds[test_ds$wr_total >= 9, "wr_total"] = 3

head(train_ds)

#마키아벨리즘 성향과 응답시간은 투표 여부에 관련이 없는것으로 보임(.....) 마키아벨리즘의 세부적인 성향을 이용해야 할 것으로 보임


#---------------select variables-------------------------
#train_step <- subset(train_ds, select = c(voted, age_group, education, race, religion, wr_06, tp06, tp07, QqA, tp02, urban, tp09, wf_01, wf_02, wf_03, married, gender, familysize, engnat, mac, mac_time, wr_total))
#train_step <- subset(train_ds, select = c(wr_total, QaA, QcA, QcE, QdA, QeA, QfA, QfE, QhE, QjA, QkA, QlE, QmA, QnE, QoA, QqA, QsA, QsE, age_group, education, engnat, race, religion, tp01, tp02, tp03, tp04, tp06, tp07, tp08, tp09, tp10, urban, wf_01, wf_02, wf_03, wr_01, wr_02, wr_03, wr_05, wr_06, wr_13, voted, mac, mac_time, wr_total))
#m <- lm(voted ~ .,data=train_step)
#m2 <- step(m, direction = "both")

#---------------train dataset에서 변수 선택-------------
train <- data.frame(QaA = train_ds$QaA,
                 QcA = train_ds$QcA,
                 QcE = train_ds$QcE,
                 QdA = train_ds$QdA,
                 QeA = train_ds$QeA,
                 QfA = train_ds$QfA,
                 QfE = train_ds$QfE,
                 QhE = train_ds$QhE,
                 QmA = train_ds$QmA,
                 QlE = train_ds$QlE,
                 QnE = train_ds$QnE,
                 QoA = train_ds$QoA,
                 QqA = train_ds$QqA,
                 QsA = train_ds$QsA,
                 QsE = train_ds$QsE,
                 age_group = train_ds$age_group,
                 education = train_ds$education,
                 engnat = train_ds$engnat,
                 race = train_ds$race,
                 religion = train_ds$religion,
                 tp01 = train_ds$tp01,
                 tp02 = train_ds$tp02,
                 tp03 = train_ds$tp03,
                 tp04 = train_ds$tp04,
                 tp06 = train_ds$tp06,
                 tp07 = train_ds$tp07,
                 tp08 = train_ds$tp08,
                 tp09 = train_ds$tp09,
                 tp10 = train_ds$tp10,
                 urban = train_ds$urban,
                 wf_02 = train_ds$wf_02,
                 wf_03 = train_ds$wf_03,
                 wr_02 = train_ds$wr_02,
                 wr_03 = train_ds$wr_03,
                 wr_05 = train_ds$wr_05,
                 wr_06 = train_ds$wr_06,
                 wr_13 = train_ds$wr_13,
                 wr_total = train_ds$wr_total,
                 mac = train_ds$mac,
                 voted = as.factor(train_ds$voted)
                 )


class(train$voted)



#-------------test 데이터셋 변수 선택-------------------

test <- data.frame(QaA = test_ds$QaA,
                   QcA = test_ds$QcA,
                   QcE = test_ds$QcE,
                   QdA = test_ds$QdA,
                   QeA = test_ds$QeA,
                   QfA = test_ds$QfA,
                   QfE = test_ds$QfE,
                   QhE = test_ds$QhE,
                   QlE = test_ds$QlE,
                   QmA = test_ds$QmA,
                   QnE = test_ds$QnE,
                   QoA = test_ds$QoA,
                   QqA = test_ds$QqA,
                   QsA = test_ds$QsA,
                   QsE = test_ds$QsE,
                   age_group = test_ds$age_group,
                   education = test_ds$education,
                   engnat = test_ds$engnat,
                   race = test_ds$race,
                   religion = test_ds$religion,
                   tp01 = test_ds$tp01,
                   tp02 = test_ds$tp02,
                   tp03 = test_ds$tp03,
                   tp04 = test_ds$tp04,
                   tp06 = test_ds$tp06,
                   tp07 = test_ds$tp07,
                   tp08 = test_ds$tp08,
                   tp09 = test_ds$tp09,
                   tp10 = test_ds$tp10,
                   urban = test_ds$urban,
                   wf_02 = test_ds$wf_02,
                   wf_03 = test_ds$wf_03,
                   wr_02 = test_ds$wr_02,
                   wr_03 = test_ds$wr_03,
                   wr_05 = test_ds$wr_05,
                   wr_06 = test_ds$wr_06,
                   wr_13 = test_ds$wr_13,
                   wr_total = test_ds$wr_total,
                   mac = test_ds$mac
)





#-------------train 데이터셋에서 표본을 뽑아 정확도 측정-------

indexes = createDataPartition(train$voted, p = .7, list = F) 
train_train = train[indexes, ]
train_test = train[-indexes, ]


fit <- rpart(voted~., 
             data = train_train,
             cp = 0.00058156,
             minsplit = 4,
             minbucket = 2 )


pred = predict(fit, newdata = train_test, type = "class", )

print(data.frame(train_test, pred))

confusionMatrix(pred, train_test$voted)

printcp(fit)

#의사결정나무

#---------------------ROC 커브, AUC값 구해보기-----------------
indexes = createDataPartition(train$voted, p = .7, list = F) 
train_tr = train_ds[indexes, ]
train_te = train_ds[-indexes, ]


train_tr$voted <- as.factor(train_tr$voted)
out0 <- glm(voted~.,family = binomial(), data = train_tr )
pred = predict(out0, newdata = train_te, type = "response", )

pr <- prediction(pred, train_te$voted)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
win.graph()
plot(prf, main = " ROC Curve")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


DATANUM<-nrow(train)
numeric(DATANUM)
DATANUM*c(0.6,0.2,0.2)
  
slicing<-sample(1:DATANUM)
slicing[slicing>(DATANUM*0.8)]<-(-1)
slicing[slicing>(DATANUM*0.6)]<-(-2)
slicing[slicing>0]<-(-3)
slicing<-slicing+4

train_ROC<-train[slicing==1,]
valid_ROC<-train[slicing==2,]
test_ROC<-train[slicing==3,]

out0 <- glm(voted~., family = binomial(), data = train)

out1 <- step(out0, direction = "both")
qchisq(0.95, df = 1454)        #카이제곱분포에 따른 분석


pred = predict(out1, newdata = test, type = "response" )

pr <- prediction(pred, valid_ROC$voted)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
win.graph()
plot(prf, main = " ROC Curve")

#컷오프 찾기
optid<-(1:length(prf@y.values[[1]][-1]))[((prf@x.values[[1]][-1])^2 + (1-prf@y.values[[1]][-11])^2)
                                         ==min((prf@x.values[[1]][-1])^2 + (1-prf@y.values[[1]][-1])^2)]
points(prf@x.values[[1]][-1][optid],prf@y.values[[1]][-1][optid], col='red', pch=15)
auc<-prf@alpha.values[[1]][-1][optid]
auc


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



voted = data.frame(pred)
result <- data.frame(index = test_ds$index, voted = voted$pred)
head(result)

result['voted'] = result['voted']/0.5127426*0.5
head(result)

write.csv(result, file = "result_optcut.csv", row.names = FALSE)


#---------------pruning 과정-----------------------------------

library(tree)
p_fit <- prune(fit, cp=fit$cptable[which.min(fit$cptable[, "xerror"]), "CP"])
plot(p_fit)
prunepred = predict(p_fit, train_test, type = "class", )

confusionMatrix(prunepred, train_test$voted)
plot(tree)


printcp(p_fit)


#----------------- 의사결정나무로 정확도 측정-------------------
fit <- rpart(voted~., 
             data = train,
             cp = 0.00058156,
             minsplit = 4,
             minbucket = 2 ) 


voted = data.frame(predict(fit, test, type = "prob" ))
names(voted)
print(data.frame(test, voted$X2))

result <- data.frame(index = test_ds$index, voted = voted$X2)
head(result)

write.csv(result, file = "result.csv", row.names = FALSE)





#-----------------SVM 모델 학습----------------------------
indexes = createDataPartition(train_ds$voted, p = .7, list = F) 
svm_train = train[indexes, ]
svm_test = train[-indexes, ]
train_svm2 <- svm(voted ~., data = train, type = "C-classification")
pred = predict(train_svm2, svm_test)
tt <- table(svm_test$voted, pred)
sum(tt[row(tt) == col(tt)])/sum(tt)

lm_ZN <- lm(MEDV ~ ZN, data = train_ds)
accuracy(lm_ZN)

voted = data.frame(pred)
result <- data.frame(index = test_ds$index, voted = voted$pred)
head(result)

write.csv(result, file = "result3.csv", row.names = FALSE)



#--------------------KSVM 모델 학습------------------------
library("kernlab")

indexes = createDataPartition(train_ds$voted, p = .9, list = F)
ksvm_classfier <- ksvm(voted~., data = train, kernel = "vanilladot", C=1.0)
ksvm_classfier2 <- ksvm(voted~., data = train, type = "C-svc", prob.model =TRUE, kernel = "vanilladot", )

ksvm_pred <- predict(ksvm_classfier2, test, type = 'probabilities')
head(ksvm_pred)


voted = data.frame(ksvm_pred)
names(voted) <- letters[1:2]
head(voted)
result <- data.frame(index = test_ds$index, voted = voted$b)
head(result)

ksvm_presmp <- predict(ksvm_classfier2, svm_test)
head(ksvm_presmp)
tt <- table(svm_test$voted, ksvm_presmp)
sum(tt[row(tt) == col(tt)])/sum(tt)

write.csv(result, file = "result_ksvm.csv", row.names = FALSE)




#-------------------------neural network--------------------------
library(nnet)
library(NeuralNetTools)


indexes = createDataPartition(train$voted, p = .7, list = F) 
train_tr = train[indexes, ]
train_te = train[-indexes, ]


nn_model <- nnet(voted ~ .,
                 data = train_tr,
                 size = 2,
                 #rang =0.1,
                 decay = 5e-04,
                 maxit = 10000)



pred <- predict(nn_model, train_te, type = "class")


confusionMatrix(table(pred, train_te$voted))

library(neuralnet)



#범주화를 위해 str형 자료들을 int형으로 변환
train_tr$age_group2[train_tr$age_group == '10s'] <- 1
train_tr$age_group2[train_tr$age_group == '20s'] <- 2
train_tr$age_group2[train_tr$age_group == '30s'] <- 3
train_tr$age_group2[train_tr$age_group == '40s'] <- 4
train_tr$age_group2[train_tr$age_group == '50s'] <- 5
train_tr$age_group2[train_tr$age_group == '60s'] <- 6
train_tr$age_group2[train_tr$age_group == '+70s'] <- 7
train_tr$age_group <- NULL



train_tr$religion2[train_tr$religion == 'Agnostic'] <- 1
train_tr$religion2[train_tr$religion == 'Atheist'] <- 2
train_tr$religion2[train_tr$religion == 'Christian_Catholic'] <- 3
train_tr$religion2[train_tr$religion == 'Christian_Mormon'] <- 4
train_tr$religion2[train_tr$religion == 'Christian_Protestant'] <- 5
train_tr$religion2[train_tr$religion == 'Christian_Other'] <- 6
train_tr$religion2[train_tr$religion == 'Hindu'] <- 7
train_tr$religion2[train_tr$religion == 'Jewish'] <- 8
train_tr$religion2[train_tr$religion == 'Muslim'] <- 9
train_tr$religion2[train_tr$religion == 'Sikh'] <- 10
train_tr$religion2[train_tr$religion == 'Other'] <- 11
train_tr$religion <- NULL


train_tr$race2[train_tr$race == 'Asian'] <- 1
train_tr$race2[train_tr$race == 'Arab'] <- 2
train_tr$race2[train_tr$race == 'Indigenous Australian'] <- 3
train_tr$race2[train_tr$race == 'Native American'] <- 4
train_tr$race2[train_tr$race == 'White'] <- 5
train_tr$race2[train_tr$race == 'Other'] <- 6
train_tr$race <- NULL

#train_tr$gender2[train_tr$gender == 'Male'] <- 1
#train_tr$gender2[train_tr$gender == 'Female'] <- 2 
#train_tr$gender <- NULL

train_tr$religion2[is.na(train_tr$religion2)] <- 12
train_tr$race2[is.na(train_tr$race2)] <- 7


train_tr$voted <- as.numeric(train_tr$voted)




train_te$age_group2[train_te$age_group == '10s'] <- 1
train_te$age_group2[train_te$age_group == '20s'] <- 2
train_te$age_group2[train_te$age_group == '30s'] <- 3
train_te$age_group2[train_te$age_group == '40s'] <- 4
train_te$age_group2[train_te$age_group == '50s'] <- 5
train_te$age_group2[train_te$age_group == '60s'] <- 6
train_te$age_group2[train_te$age_group == '+70s'] <- 7
train_te$age_group <- NULL

train_te$religion2[train_te$religion == 'Agnostic'] <- 1
train_te$religion2[train_te$religion == 'Atheist'] <- 2
train_te$religion2[train_te$religion == 'Christian_Catholic'] <- 3
train_te$religion2[train_te$religion == 'Christian_Mormon'] <- 4
train_te$religion2[train_te$religion == 'Christian_Protestant'] <- 5
train_te$religion2[train_te$religion == 'Christian_Other'] <- 6
train_te$religion2[train_te$religion == 'Hindu'] <- 7
train_te$religion2[train_te$religion == 'Jewish'] <- 8
train_te$religion2[train_te$religion == 'Muslim'] <- 9
train_te$religion2[train_te$religion == 'Sikh'] <- 10
train_te$religion2[train_te$religion == 'Other'] <- 11
train_te$religion <- NULL


train_te$race2[train_te$race == 'Asian'] <- 1
train_te$race2[train_te$race == 'Arab'] <- 2
train_te$race2[train_te$race == 'Indigenous Australian'] <- 3
train_te$race2[train_te$race == 'Native American'] <- 4
train_te$race2[train_te$race == 'White'] <- 5
train_te$race2[train_te$race == 'Other'] <- 6
train_te$race <- NULL

#train_tr$gender2[train_tr$gender == 'Male'] <- 1
#train_tr$gender2[train_tr$gender == 'Female'] <- 2 
#train_tr$gender <- NULL

train_te$religion2[is.na(train_te$religion2)] <- 12
train_te$race2[is.na(train_te$race2)] <- 7



train_te$voted <- as.numeric(train_te$voted)

summary(train_te)



normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

#train_tr$voted <- factor(train_tr$voted, ordered = TRUE)

summary(train_tr)


train_norm <- as.data.frame(lapply(train_tr, normalize))

test_norm <- as.data.frame(lapply(train_te, normalize))

test_norm <- normalize(as.integer(train_te))

summary(test_norm)

nntrain <- neuralnet(voted ~ .,
                     data = train_norm,
                     hidden = c(3),
                     err.fct = "sse",
                     linear.output = FALSE,
                     likelihood = TRUE)


nntrain

model_result <- compute(nntrain, test_norm[1:39])










