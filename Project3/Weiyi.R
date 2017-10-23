require(MASS)
require(ISLR)
require(tidyverse)
require(dplyr)
require(data.world)
require(ggplot2)

################ Reading the data from data.world #############################
project<- "https://data.world/isabelcachola/f-17-eda-project-3"
data.world::set_config(cfg_env("DW_API"))

df_w <- data.world::query(
  data.world::qry_sql("SELECT * FROM kickstarter_data_with_features_2"),
  dataset = project
)


################ Insight 1 #####################################
sdf_w = dplyr::select(df_w,country, created_at,backers_count,state)
sdf_w = dplyr::filter(sdf_w,country=="US",state %in% c("failed","successful"),backers_count<50000)

backersplot <- ggplot(sdf_w)+geom_point(aes(x=created_at,y=backers_count,color=state))
plot(backersplot)

## LOOCV Leave One Out Cross Validation
glm.fit=glm(backers_count~created_at, data=sdf_w) # Create a linear model


##Function using the formula for loocv for linear
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

## Now we try it out
loocv(glm.fit)

# fit some polynomials degrees 1-5
cv.error=rep(0,10)
degree=1:10
for(d in degree){
  glm.fit=glm(backers_count~poly(created_at,d), data=sdf_w) #make a model for each degree
  cv.error[d]=loocv(glm.fit) #Computes LOOCV error and puts it into error vector
}
plot(degree,cv.error,type="b") 
## 10-fold CV
# divide data into 10 pieces. Use 9 for training 1 for testing. Then proceed same as LOOCV
cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")



