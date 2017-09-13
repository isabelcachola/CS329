require(dplyr)
require(data.world)

project<- "https://data.world/isabelcachola/f-17-eda-project-1"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM WorldHappiness"),
  dataset = project
)

head(df)
cor(df[3:12])
pairs(df[3:12])

summary(df)
attach(df)
sdf = dplyr::select(df, happiness_score, economy_gdp_per_capita, family, health_life_expectancy) %>% sample_frac(.1) # The dplyr::select function was used to select only continuous variables so that the pairs() function works in default mode. The sample_frac() function returns a 10% sample of the data so that pairs doesn't choke.
pairs(sdf)
plot(happiness_score~economy_gdp_per_capita,df)
fitHappinessScoreGDP = lm(happiness_score~economy_gdp_per_capita,data=df)
fitHappinessScoreGDP
summary(fitHappinessScoreGDP)
abline(fitHappinessScoreGDP,col="red")
names(fitHappinessScoreGDP)
confint(fitHappinessScoreGDP)

fitHappinessScoreGDP2 = lm(happiness_score~economy_gdp_per_capita + poly(economy_gdp_per_capita, 2),data=df)
fitHappinessScoreGDP2
summary(fitHappinessScoreGDP2)
points(economy_gdp_per_capita, fitted(fitHappinessScoreGDP2), col="blue", pch=20)
confint(fitHappinessScoreGDP2)

fitHappinessScoreGDP <- lm(happiness_score~economy_gdp_per_capita, df)
fitHappinessScoreGDP
summary(fitHappinessScoreGDP)

fitHappinessFreedom <- lm(happiness_score~freedom, df)
fitHappinessFreedom
summary(fitHappinessFreedom)

fitHappinessGov <- lm(happiness_score~trust_government_corruption, df)
fitHappinessGov
summary(fitHappinessGov)
