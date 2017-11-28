########################## SET UP #############################

require(tidyverse)
require(data.world)
require(dplyr)
require(MASS)
require(ISLR)
require(tidyverse)
require(data.world)
require(ggplot2)
require(glmnet)
require(leaps)
require(boot)
#Mine
require(modelr)
require(leaps)
project<- "https://data.world/wangweiyi722/f-17-eda-project-5"

# data will take a while to read
fy13_budgeted_student_enrollment_data <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy13_budgeted_student_enrollment_data"),
  dataset = project
)
fy13_school_budget_data  <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy13_school_budget_data"),
  dataset = project
)
fy14_budgeted_student_enrollment_data <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy14_budgeted_student_enrollment_data"),
  dataset = project
)
fy15_budgeted_student_enrollment_data <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy15_budgeted_student_enrollment_data"),
  dataset = project
)
fy15_data_for_tableau  <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy15_data_for_tableau"),
  dataset = project
)
fy16_budgeted_student_enrollment_data <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy16_budgeted_student_enrollment_data"),
  dataset = project
)
fy16_school_budget_data  <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy16_school_budget_data"),
  dataset = project
)
initial_allocation_rollup_map <- data.world::query(
  data.world::qry_sql("SELECT * FROM initial_allocation_rollup_map"),
  dataset = project
)
initial_allocations_2_16_16 <- data.world::query(
  data.world::qry_sql("SELECT * FROM initial_allocations_2_16_16"),
  dataset = project
)
initial_at_risk_allocations <- data.world::query(
  data.world::qry_sql("SELECT * FROM initial_at_risk_allocations"),
  dataset = project
)
initial_budget_allocations <- data.world::query(
  data.world::qry_sql("SELECT * FROM initial_budget_allocations"),
  dataset = project
)
fy_15_budget_by_line_item <- data.world::query(
  data.world::qry_sql("SELECT * FROM fy15_data_for_tableau"),
  dataset = project
)



############################## Insight 1 ######################################
# Add at risk budget as column
df_at_risk_am <- dplyr::filter(fy16_school_budget_data, budget_allocation_category=="At-Risk")
df_at_risk16 <- fy16_budgeted_student_enrollment_data
df_at_risk16 <- df_at_risk16[order(df_at_risk16$school_name),]
df_at_risk_am <- df_at_risk_am[order(df_at_risk_am$school_name),]
df_at_risk16$at_risk_budget <- df_at_risk_am$amount

pairs(df_at_risk16[5:12])
names(df_at_risk16)

# Remove NA
df_at_risk16 <- subset(df_at_risk16, !is.na(df_at_risk16$special_education))

equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 2),
                  b = round(coef(x)[2], digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

fit = lm(at_risk_budget~special_education, data = df_at_risk16)
summary(fit)
ggplot(data=df_at_risk16,mapping = aes(y=at_risk_budget,x=special_education)) +
  geom_point() +
  geom_smooth(method='lm', colour="red") +
  scale_y_continuous(label=scales::comma)+
  annotate("rect", xmin = 140, xmax = 300, ymin =220000, ymax =320000, fill="white", colour="red") +
  annotate("text", x = 220, y = 270000, label = equation(fit), parse = TRUE)

df_num <- df_at_risk16[c(4:12)]
regfit.full=regsubsets(at_risk_budget~.,data=df_num)
reg.summary = summary(regfit.full)
reg.summary
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],pch=20,col="red")
plot(regfit.full,scale="Cp")
coef(regfit.full,which.min(reg.summary$cp))
fit4 = lm(at_risk_budget~ward+special_education+homeless_foster+direct_certs, data = df_at_risk16)
summary(fit4)

ggplot(data=df_at_risk16,mapping = aes(y=at_risk_budget,x=ward+special_education+homeless_foster+direct_certs)) +
  geom_point() +
  geom_smooth(method='lm', colour="red") +
  scale_y_continuous(label=scales::comma)+
  annotate("rect", xmin = 480, xmax = 1020, ymin =220000, ymax =320000, fill="white", colour="red") +
  annotate("text", x = 750, y = 270000, label = equation(fit4), parse = TRUE)

