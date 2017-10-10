require(dplyr)
require(data.world)
setwd("C:/Users/Weiyi/OneDrive/Documents/CS 329E")
df <- read.csv("ElectionsData.csv")

project <- "https://data.world/ncaldw01/electionsdata"

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Ondhbmd3ZWl5aTcyMiIsImlzcyI6ImFnZW50Ondhbmd3ZWl5aTcyMjo6OGRmYzgxYmYtOWNjMS00OWYzLWJkMzEtOTBjZjIxMjE0YTY2IiwiaWF0IjoxNDg0Njk3NDM5LCJyb2xlIjpbInVzZXJfYXBpX3JlYWQiLCJ1c2VyX2FwaV93cml0ZSJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.DA9rL8enFktjNWwhf141FieMdhmP5mh4bEix0LBGut5cz0Nce5hVQ0-y3mYC5A9JnWpRZmGusmpmRsLr7xzBPg"))

df <- data.world::query(
  data.world::qry_sql("SELECT * FROM electionsdata2"),
  dataset = project
)


attach(df)
sdf = dplyr::select(df, state, votes, votes16_trumpd, votes16_clintonh, pop_vote_winner, at_least_bachelor_s_degree, white_not_latino_population, african_american_population, native_american_population, asian_american_population, population_some_other_race_or_races, latino_population)

pairs(sdf)
sdf = dplyr::mutate(sdf,perc_minority =  african_american_population+ native_american_population+ asian_american_population+ population_some_other_race_or_races+ latino_population)%>%arrange(votes)



# Create a training data set using the 25 states with the lowest number of votes.
training = sdf%>% dplyr::filter(votes<2000000)
testing = sdf%>% dplyr::filter(votes>2000000)

# Use the perc_minority column from the testing data to make a lda model predicting whether Trump or Clinton won that state
election_lda = lda(pop_vote_winner~perc_minority,data = training)
plot(election_lda)
data.frame(election_lda)[1:5,]
election_lda_pred = predict(election_lda,testing)
table(election_lda_pred$class,testing$pop_vote_winner)
