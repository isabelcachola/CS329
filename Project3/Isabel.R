require(MASS)
require(ISLR)
require(tidyverse)
library(dplyr)

project<- "https://data.world/isabelcachola/f-17-eda-project-3"
df <- read.csv("https://query.data.world/s/0uetDd56RIiSaweOGdPqWuy2Idz44K", header=TRUE, stringsAsFactors=FALSE)
names(df)

sdf<- subset(df, country=='US')
sdf<- subset(sdf, state=='successful'|state=='failed')

set.seed(11)
sdf <- dplyr::mutate(sdf, launched_at_day_num = )
train <- sdf[sample(nrow(sdf), 8421),] # 70% of the data to train
test <- subset(sdf, !is.element(sdf$X, train$X))

################ Insight 1 ##########################
lda_func <-function(lda.fit){
  print(lda.fit)
  
  lda.pred = predict(lda.fit, test)
  df.pred = data.frame(lda.pred)
  test$lda_class = lda.pred$class
  print(ggplot(df.pred) + geom_histogram(mapping = aes(x=LD1)) + facet_wrap(~ class))
  print(ggplot(df.pred) + geom_boxplot(mapping = aes(x=class, y=LD1)))
  print(table(lda.pred$class,test$state))
  print(paste('Percent correct:', toString(mean(lda.pred$class==test$state))))
  print(paste('Percent incorrect:', toString(mean(lda.pred$class!=test$state))))
  
}

attach(sdf)
convert_weekday <- function(num){
  if (num=='Monday'){return(1)}
  if (num=='Tuesday'){return(2)}
  if (num=='Wednesday'){return(3)}
  if (num=='Thursday'){return(4)}
  if (num=='Friday'){return(5)}
  if (num=='Saturday'){return(6)}
  if (num=='Sunday'){return(7)}
  
}
sdf<- dplyr::mutate(sdf, day_launched=launched_at_weekday) 
sdf$day_launched <- recode(sdf$day_launched, 
                        "Sunday"=0,
                        "Monday"=1,
                        "Tuesday"=2,
                        "Wednesday"=3,
                        "Thursday"=4,
                        "Friday"=5,
                        "Saturday"=6)

train <- sdf[sample(nrow(sdf), 8421),] # 70% of the data to train
test <- subset(sdf, !is.element(sdf$X, train$X))

sdf$launched_at_weekday <- as.character(sdf$launched_at_weekday)
sdf$launched_at_weekday <- factor(sdf$launched_at_weekday, levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))


ggplot(data=sdf, mapping=aes(sdf$launched_at_weekday)) + 
  geom_bar(aes(fill=state)) + 
  geom_text(stat='count',aes(label=..count..),vjust=-1) 

lda_func(lda(state~day_launched, data=train))
lda.pred = data.frame(predict(lda(state~day_launched, data=train),test))
table(lda.pred$class,test$state)
paste('Percent correct:', toString(mean(lda.pred$class==test$state)))
paste('Percent incorrect:', toString(mean(lda.pred$class!=test$state)))
test$posterior_successful = lda.pred$posterior.successful
ggplot(data=test, mapping = aes(x=state,y=posterior_successful)) + geom_boxplot()
test$posterior_failed = lda.pred$posterior.failed
ggplot(data=test, mapping = aes(x=state,y=posterior_failed)) + geom_boxplot()



###################### Insight 2 ########################

