require(MASS)
require(ISLR)
require(tidyverse)
library(dplyr)

project<- "https://data.world/ncaldw01/election-results"
df <- read.csv("https://query.data.world/s/9AePdX0-UXY6R90eqhjXNuTAA1imZ9", header=TRUE, stringsAsFactors=FALSE);
names(df)

sdf = dplyr::select(df, State, votes, votes16_trumpd, votes16_clintonh, votes16_johnsong, votes16_steinj, pop_vote_winner)
sdf = sdf[complete.cases(sdf),]
sdf = dplyr::mutate(sdf,perc_third_party = (votes16_johnsong+votes16_steinj)/votes)

# Partitions data set randomly into 30 states for a training set and 20 states for a testing set
df_train <- sdf[sample(nrow(sdf), 25),]
df_test <- subset(sdf, !is.element(sdf$State, df_train$State))

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


# Looks at how third party votes affected the election
lda_func(lda(pop_vote_winner~perc_third_party, data=df_train))

percent_incorrect=rep(0,5)
degree=1:5
for(d in degree){
  fit=lda(pop_vote_winner~poly(perc_third_party,d), data=df_train)
  fit.pred = data.frame(predict(fit, df_test))
  percent_incorrect[d]=mean(fit.pred$class!=df_test$pop_vote_winner)
}
plot(degree,percent_incorrect,type="b")
percent_incorrect


# Take random sample 5 times and average percent correct
percent_correct_func <- function(fit,df_test){
  fit.pred = data.frame(predict(fit, df_test))
  return(mean(fit.pred$class==df_test$pop_vote_winner))
}

df_percent_correct = data.frame(avg_percent_correct = rep(0,4))
percent_correct_redos=rep(0,5)
for (i in 1:5){
  # Redoes random partition
  df_train_redo <- sdf[sample(nrow(sdf), 25),]
  df_test_redo <- subset(sdf, !is.element(sdf$State, df_train_redo$State))
  
  # Uses LDA on new partition
  fit=lda(pop_vote_winner~perc_third_party, data=df_train_redo)
  percent_correct_redos[i] = percent_correct_func(fit, df_test_redo)
}
mean(percent_correct_redos)


sdf = dplyr::mutate(sdf,perc_johnson = (votes16_johnsong/votes))
sdf = dplyr::mutate(sdf,perc_stein = (votes16_steinj/votes))
df_train <- sdf[sample(nrow(sdf), 25),]
df_test <- subset(sdf, !is.element(sdf$State, df_train$State))
lda_func(lda(pop_vote_winner~perc_stein, data=df_train))
lda_func(lda(pop_vote_winner~perc_johnson, data=df_train))
lda_func(lda(pop_vote_winner~perc_johnson+perc_stein, data=df_train))
