require(MASS)
require(ISLR)
require(tidyverse)
library(dplyr)

project<- "https://data.world/ncaldw01/election-results"
df <- read.csv("https://query.data.world/s/9AePdX0-UXY6R90eqhjXNuTAA1imZ9", header=TRUE, stringsAsFactors=FALSE);
names(df)

# Partitions data set randomly into 30 states for a training set and 20 states for a testing set
df_train <- df[sample(nrow(df), 30),]
df_test <- subset(df, !is.element(df$State, df_train$State))


# Function that prints histogram, boxplit, confusion matrix, percent predicted correctly,
# percent predicted incorrectly, and states predicted incorrectly
# Takes lda fit as input
lda_func <-function(lda.fit){
  print(lda.fit)
  
  lda.pred = predict(lda.fit, df_test)
  df.pred = data.frame(lda.pred)
  print(ggplot(df.pred) + geom_histogram(mapping = aes(x=LD1)) + facet_wrap(~ class))
  print(ggplot(df.pred) + geom_boxplot(mapping = aes(x=class, y=LD1)))
  print(table(lda.pred$class,df_test$pop_vote_winner))
  print(paste('Percent correct:', toString(mean(lda.pred$class==df_test$pop_vote_winner))))
  print(paste('Percent incorrect:', toString(mean(lda.pred$class!=df_test$pop_vote_winner))))
  subset(df_test, lda.pred$class!=df_test$pop_vote_winner)$State
  
}


# Looks at how education levels affect predictions
lda_func(fit_grad<- lda(pop_vote_winner~Graduate.Degree, data=df_train))
lda_func(fit_bach <- lda(pop_vote_winner~At.Least.Bachelor.s.Degree, data=df_train))
lda_func(fit_hs <- lda(pop_vote_winner~At.Least.High.School.Diploma, data=df_train))
lda_func(fit_less_hs <- lda(pop_vote_winner~Less.Than.High.School, data=df_train))

# Let's see if we can make At.Least.High.School.Diploma a better predictor
percent_incorrect=rep(0,5)
degree=1:5
for(d in degree){
  fit=lda(pop_vote_winner~poly(At.Least.High.School.Diploma,d), data=df_train)
  fit.pred = data.frame(predict(fit, df_test))
  percent_incorrect[d]=mean(fit.pred$class!=df_test$pop_vote_winner)
}
plot(degree,percent_incorrect,type="b")
percent_incorrect
# Not really

# Take random sample 5 times and average percent correct
percent_correct_func <- function(fit,df_test){
  fit.pred = data.frame(predict(fit, df_test))
  return(mean(fit.pred$class==df_test$pop_vote_winner))
}
tests = c("Graduate.Degree", "At.Least.Bachelor.s.Degree", "At.Least.High.School.Diploma","Less.Than.High.School")
df_percent_correct = data.frame(row.names = tests, avg_percent_correct = rep(0,4))
percent_correct_redos=rep(0,5)
for (i in 1:5){
  # Redoes random partition
  df_train_redo <- df[sample(nrow(df), 30),]
  df_test_redo <- subset(df, !is.element(df$State, df_train_redo$State))
  
  # Uses LDA on new partition
  fit=lda(pop_vote_winner~At.Least.High.School.Diploma, data=df_train_redo)
  percent_correct_redos[i] = percent_correct_func(fit, df_test_redo)
}
df_percent_correct['At.Least.High.School.Diploma','avg_percent_correct'] = mean(percent_correct_redos)

