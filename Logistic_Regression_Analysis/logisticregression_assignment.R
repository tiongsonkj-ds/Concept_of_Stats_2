# reading in dataset
library(readxl)
dataset <- read_excel("dataset.xls")
View(dataset)

######################
# splitting the data randomly into two sets: 1) 60 rows, 2) 40 rows (holdout/validation sample)
#create a list of random number ranging from 1 to number of rows from actual data 
#and 60% of the data into training data  
cols_we_want = dataset[, c('x6', 'x7', 'x8', 'x9', 'x10', 'x11', 'x12', 'x13', 'x14', 'x15', 'x16', 'x17', 'x18')]
split_data = sort(sample(nrow(columns_we_want), nrow(columns_we_want)*.6))

#creating training data set by selecting the output row values
train<-data[split_data,]

#creating test/holdout/validation data set by not selecting the output row values
test<-data[-split_data,]

### Table 6-2 ###
# creating the "null" model with no variables
model.null <- glm(x4 ~ 1, data=train, family=binomial(link="logit"))
# -2LL #
null_model.loglik <- -2 * logLik(model.null)

# skip this for now but can I get the score statistic and significance with variables not in the model?

### Table 6-3 ###
# model with x13 only
logmodel.x13 <- glm(x4 ~ x13, data=train, family=binomial(link="logit"))
# -2LL
logmodelx13.loglik <- -2 * logLik(logmodel.x13)
# Wald test for global test for significance
logmodel.x13.waldtest <- regTermTest(logmodel.x13, "x13")
# odds ratio
oddsratio.x13 <- exp(cbind(OR=coef(logmodel.x13), confint(logmodel.x13)))
# run model on analysis sample
x13.train.probs.analysis <- predict(logmodel.x13, train, type='response')
x13.pred.logit.train <- rep(0, length(x13.train.probs.analysis))
x13.pred.logit.train[x13.train.probs.analysis >= 0.5] <- 1
# classification matrix for analysis sample
classmatrix.x13.analysis <- table(x13.pred.logit.train, train$x4)
# run model on holdout sample
x13train.probs.holdout <- predict(logmodel.x13, test, type='response')
x13.pred.logit.test <- rep(0, length(x13train.probs.holdout))
x13.pred.logit.test[x13train.probs.holdout >= 0.5] <- 1
# classification matrix for holdout sample
classmatrix.x13.holdout <- table(x13.pred.logit.test, test$x4)

### Table 6-4 ###
# final
logmodel.final <- glm(x4 ~ x13 + x17, data=train, family=binomial(link="logit"))
# -2LL
logmodelfinal.loglik <- -2 * logLik(logmodel.final)
# Wald test for global test for significance
logmodel.final.waldtest <- regTermTest(logmodel.final, "x17")
# odds ratio
oddsratio.finalmodel <- exp(cbind(OR=coef(logmodel.final), confint(logmodel.final)))
# run model on analysis sample
finaltrain.probs.analysis <- predict(logmodel.final, train, type='response')
final.pred.logit.train <- rep(0, length(finaltrain.probs.analysis))
final.pred.logit.train[finaltrain.probs.analysis >= 0.5] <- 1
# classification matrix for analysis sample
classmatrix.final.analysis <- table(final.pred.logit.train, train$x4)
# model on holdout sample
finaltrain.probs.holdout <- predict(logmodel.final, test, type='response')
final.pred.logit.test <- rep(0, length(finaltrain.probs.holdout))
final.pred.logit.test[finaltrain.probs.holdout >= 0.5] <- 1
# classification matrix for holdout sample
classmatrix.final.holdout <- table(final.pred.logit.test, test$x4)

