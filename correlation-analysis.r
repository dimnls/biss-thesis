# CORRELATION ANALYSIS ON 20-stimuli table
install.packages('ggplot2')
install.packages('dplyr')
install.packages('sp')
install.packages('tidyverse')
install.packages('fields')
install.packages('ggpubr')
install.packages('corrplot')
install.packages('e1071')
install.packages('neuralnet')
install.packages('rpart')
install.packages('randomForest')
install.packages('caret')
install.packages('stargazer')
install.packages('nnet')
install.packages('Metrics')
install.packages('party')

library('ggplot2')
library('dplyr')
library('sp')
library('tidyverse')
library('fields')
library('ggpubr')
library('corrplot')
library('e1071')
library('neuralnet')
library('rpart')
library('randomForest')
library('caret')
library('stargazer')
library('nnet')
library('Metrics')
library('party')

audio.stimuli <- read.csv('20-stimuli.csv')
# select useful (numeric) columns for analysis
data <- select(audio.stimuli, c(4:10, adjX, adjY ))

# normalize tempo
audio.stimuli$tempo <- (audio.stimuli$tempo - min(audio.stimuli$tempo))/(max(audio.stimuli$tempo-min(audio.stimuli$tempo)))

audio.stimuli$adjX <- (audio.stimuli$adjX - min(audio.stimuli$adjX))/(max(audio.stimuli$adjX-min(audio.stimuli$adjX)))
audio.stimuli$adjY <- (audio.stimuli$adjY - min(audio.stimuli$adjY))/(max(audio.stimuli$adjY-min(audio.stimuli$adjY)))


# run Shapiro-Wilk test for normality
sw <- audio.stimuli %>%
  select(c(4:10)) %>%
  gather(key = 'variable_name', value = 'value') %>%
  group_by(variable_name) %>%
  do(broom::tidy(shapiro.test(.$value))) %>%
  ungroup() %>%
  select(variable_name, 'W' = statistic, 'p.value' = p.value)
write.table(sw, 'clipboard', sep='\t')

# correlation analysis and correlogram
cm <- cor(data, method = 'spearman')
cm_r <- round(cm, 3)
write.table(cm_r, 'clipboard', sep='\t')

corrplot(cm, type='upper')

# pca
pca <- prcomp(data, center = TRUE, scale = TRUE)
a<-(summary(pca))

# predictive modeling
set.seed(123)
index <- sample(1:nrow(data), size = 0.7*nrow(data))
train <- data[index, ]
test <- data[-index, ]


# linear regression
model.lm <- lm(adjY ~ (valence+energy+danceability+acousticness+liveness+tempo+speechiness), data = train)
predictions.lm <- predict.lm(model.lm, test[,1:7])
RMSE.lm <- sqrt(mean( (test$adjY - predictions.lm)^2 ))
MAE.lm <- mean(abs(test$adjY - predictions.lm))

# svm
model.svm <- svm(adjY ~ (valence+energy+danceability+acousticness+liveness+tempo+speechiness), data = train, kernel = 'sigmoid')
predictions.svm <- predict(model.svm, test[, 1:7])
RMSE.svm <- sqrt(mean( (test$adjY - predictions.svm)^2 ))
MAE.svm <- mean(abs(test$adjY - predictions.svm))

# neural net
set.seed(0202020)
model.nn <- neuralnet(adjX ~ (valence+energy+danceability+acousticness+liveness+tempo+speechiness), data = train, hidden = 1)
predictions.nn <- compute(model.nn, test[, 1:7])
RMSE.nn <- sqrt(mean( (test$adjX - predictions.nn$net.result)^2 ))
MAE.nn <- mean(abs(test$adjX - predictions.nn$net.result))

# MAPE.nn <- mean(abs((test$adjX - predictions.nn$net.result)/(test$adjX)))

# regression tree
model.rt <- rpart(adjX ~ (valence+energy+danceability+acousticness+liveness+tempo+speechiness), data = train)
predictions.rt <- predict(model.rt, test[, 1:7])
RMSE.rt <- sqrt(mean( (test$adjX - predictions.rt)^2 ))
MAE.rt <- mean(abs(test$adjX - predictions.rt))

MAPE.rt <- mean(abs((test$adjX - predictions.rt)/(test$adjX)))


# random forest
set.seed(123)
model.rf <- randomForest(adjY ~ (valence+energy+danceability+acousticness+liveness+tempo+speechiness), data = train, ntree = 100)
predictions.rf <- predict(model.rf, test[, 1:7])
RMSE.rf <- sqrt(mean( (test$adjY - predictions.rf)^2 ))
MAE.rf <- mean(abs(test$adjY - predictions.rf))

# k-fold validation
train_control <- trainControl(method = 'cv', number = 5)
print(model.kf <- train(adjX ~ (valence+energy+danceability+acousticness+liveness+tempo+speechiness), data = data, trControl = train_control, method = 'neuralnet', layer1=3) )

#####
ggplot(data, aes(x=adjX, y=adjY)) +
  geom_point(color='blue') +
  ggtitle("Audio Stimuli") +
  xlab("Warm - Cool") +
  ylab("Hard - Soft") +
  geom_vline(aes(xintercept=0), color='red', linetype='solid', size=1) +
  geom_hline(aes(yintercept=0), color='red', linetype='solid', size=1)

# quadrant characterization
data$WC <- NA
data$HS <- NA
for(i in 1:20) {
  if( data[i, 'adjX'] > 0 ) {
    data[i, 'WC'] <- 'C'
  }
  else {
    data[i, 'WC'] <- 'W'
  }
  if(data[i, 'adjY'] > 0 ) {
    data[i, 'HS'] <- 'S'
  }
  else {
    data[i, 'HS'] <- 'H'
  }
}
data$WC <- as.factor(data$WC)
data$HS <- as.factor(data$HS)

# predictive modeling
set.seed(123)
index <- sample(1:nrow(data), size = 0.7*nrow(data))
train <- data[index, ]
test <- data[-index, ]

# decision tree
class.tree <- ctree(WC ~ (valence+energy+danceability+acousticness+liveness+tempo+speechiness), data = train)
class.tree.pred <- predict(class.tree, test[, 1:7])
class.tree.cm <- as.matrix(table(Actual = test$WC, Predicted = class.tree.pred))

# cross validation for classification
train_control <- trainControl(method = 'cv', number = 5)
print(model.kf <- train(WC ~ (valence+energy+danceability+acousticness+liveness+tempo+speechiness), data = data, trControl = train_control, method = 'knn') )
