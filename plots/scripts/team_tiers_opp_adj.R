library(pracma)

# lapply(1999:2019, function(x){

start_time <- Sys.time()
current_season <- year

# pbp <- purrr::map_df(current_season, function(x) {
#   readRDS(url(
#     glue::glue('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{x}.rds?raw=true')
#   ))
# # }) |> filter(week < 9)
# }) |> filter(season_type == 'REG') |> filter(!is.na(posteam) & (rush == 1 | pass == 1))
# print(current_season)

con <- fx.db_con(x.host = 'localhost')

library(recipes)


hfa

off_epa_df <- tbl(con, 'nflfastR_pbp') |> 
  filter(season == current_season & 
           season_type == 'REG' &
           !is.na(posteam) & 
           (rush == 1 | pass == 1) & 
           play_type != 'no_play') |> 
  arrange(game_id, play_id) |> 
  select(season,
         week,
         game_id,
         play_id,
         posteam,
         defteam,
         down,
         play_type,
         pass,
         rush,
         epa,
         success,
         home_team,
         away_team,
         posteam_score,
         NULL
         ) |> 
  collect()

library(recipes)
library(fastDummies)
library(glmnet)
train <- dummy_cols(off_epa_df |> 
                      # ungroup() |> 
                      # mutate(off_epa_per_play = off_epa/off_plays) |> 
                      select(posteam, epa, defteam),
                    select_columns = 'defteam',
                    remove_selected_columns = TRUE
                    )

y <- train |> pull(epa)
x <- data.matrix(train |> select(-epa))
model <- glmnet(x, y, alpha = 0)
summary(model)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 0)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)

Reduce(cbind2, lapply(x[,-1], Matrix, sparse = TRUE))

#produce Ridge trace plot
plot(model, xvar = "lambda")

#use fitted best model to make predictions
y_predicted <- predict(model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq
