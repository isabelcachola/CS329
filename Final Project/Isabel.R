project<- "https://data.world/wangweiyi722/f-17-eda-project-5"
df_budget_at_risk <- read.csv("https://query.data.world/s/LbLhuOcau1c77-TgQjrtfY3nIN3w_z", header=TRUE, stringsAsFactors=FALSE)
names(df_budget_at_risk)
summary(df_budget_at_risk[c(3:13,15)])
pairs(df_budget_at_risk[c(3:13,15)])
