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

project<- "https://data.world/wangweiyi722/f-17-eda-project-5"

project<- "https://data.world/wangweiyi722/f-17-eda-project-5"
data.world::set_config(cfg_env("DW_API"))
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

