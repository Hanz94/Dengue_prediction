library(ggplot2)
library(randomForest)
library(missForest)

preprocessData <- function(data_path, labels_path = NULL)
{
  # load data 
  df <- read.csv(data_path)
  
  # features we want
  features = c("reanalysis_specific_humidity_g_per_kg",
               "reanalysis_dew_point_temp_k",
               "station_avg_temp_c",
               "station_min_temp_c",
               "reanalysis_min_air_temp_k",
               "station_max_temp_c",
               "reanalysis_max_air_temp_k",
               "ndvi_ne")
  
  df %<>% dplyr::select(-week_start_date)
  
  df <- fill_missing_values("missforest",df,features)
  
  # add city if labels data aren't provided
  if (!is.null(labels_path)) {
    #features %<>% c("city", "year", "weekofyear")
    df %<>% dplyr::select(-city, -year, -weekofyear)
  }
  
  # select features we want
  #df <- df[features]
  
  # add labels to dataframe
  if (!is.null(labels_path)) df %<>% cbind(read.csv(labels_path))
  
  #remove data of 1994 spike
  df %<>% filter(. , year != 1994 )
  
  #df <- fill_missing_values("missforest",df,features)
  
  # filter by city
  df_sj <- filter(df, city == 'sj')
  df_iq <- filter(df, city == 'iq')
  
  # return a list with the 2 data frames 
  return(list(df_sj, df_iq))
}


# function that returns Mean Absolute Error
mae <- function(error) return(mean(abs(error)) )

fill_missing_values <- function(mod,df_with_missing,features)
{
  if(mod == "missforest"){
    temp <- missForest(df_with_missing)
    
    # fill missing values
    summary(temp$ximp)
    
    df_after_filling <- as.data.frame(temp$ximp)
    return(df_after_filling)
 }
  else{
    df_with_missing[features] %<>% na.locf(fromLast = TRUE)
    return(df_with_missing)
  }
}


get_bst_model_for_sj <- function(train)
{
  
  fit <- randomForest(total_cases ~ reanalysis_specific_humidity_g_per_kg +
                        reanalysis_dew_point_temp_k + 
                        station_avg_temp_c +
                        reanalysis_max_air_temp_k + ndvi_ne,
                      data=train, 
                      importance=TRUE, 
                      ntree=2000)
  return(fit)
}

get_bst_model_for_iq <- function(train, test)
{
  
  fit <- randomForest(total_cases ~ reanalysis_specific_humidity_g_per_kg +
                        reanalysis_dew_point_temp_k + 
                        reanalysis_min_air_temp_k +
                        station_avg_temp_c,
                      data=train, 
                      importance=TRUE, 
                      ntree=2000)
  return(fit)
}

Time_Series_Lags_for_sj <- function(sj_train_set)
{
  sj_train_set$reanalysis_specific_humidity_g_per_kg %<>%lag(., 7)
  sj_train_set$reanalysis_dew_point_temp_k %<>% lag(., 7) 
  sj_train_set$station_avg_temp_c %<>% lag(., 8) 
  sj_train_set$reanalysis_max_air_temp_k %<>% lag(., 6)
  sj_train_set$ndvi_ne %<>% lag(., 5)
  sj_train_set$station_min_temp_c %<>% lag(.,8)
  features = c("reanalysis_specific_humidity_g_per_kg",
               "reanalysis_dew_point_temp_k",
               "station_avg_temp_c",
               "station_min_temp_c",
               "reanalysis_min_air_temp_k",
               "station_max_temp_c",
               "reanalysis_max_air_temp_k",
               "ndvi_ne")
  sj_train_set[features] %<>% na.locf(fromLast = TRUE) 
  return(sj_train_set)
}

Time_Series_Lags_for_iq <- function(iq_train_set)
{
  iq_train_set$reanalysis_specific_humidity_g_per_kg %<>% lag(., 2)
  iq_train_set$reanalysis_dew_point_temp_k %<>% lag(., 2) 
  iq_train_set$reanalysis_min_air_temp_k %<>% lag(., 5) 
  iq_train_set$station_avg_temp_c %<>% lag(., 4)
  features = c("reanalysis_specific_humidity_g_per_kg",
               "reanalysis_dew_point_temp_k",
               "station_avg_temp_c",
               "station_min_temp_c",
               "reanalysis_min_air_temp_k",
               "station_max_temp_c",
               "reanalysis_max_air_temp_k")
  iq_train_set[features] %<>% na.locf(fromLast = TRUE) 
  return(iq_train_set)
}

preprocessData(data_path = 'G:/S7/Data Mining/Dengue/Data/dengue_features_train.csv', labels_path = 'G:/S7/Data Mining/Dengue/Data/dengue_labels_train.csv') -> trains
sj_train <- trains[[1]]; iq_train <- as.data.frame(trains[2])

#Apply Time series Lags for training set
sj_train <- Time_Series_Lags_for_sj(sj_train)
iq_train <- Time_Series_Lags_for_iq(iq_train)

# # split up the data
# sj_train_subtrain <- head(sj_train, 800)
# sj_train_subtest  <- tail(sj_train, nrow(sj_train) - 800)
# 
# iq_train_subtrain <- head(iq_train, 400)
# iq_train_subtest  <- tail(iq_train, nrow(sj_train) - 400)

sj_model <- get_bst_model_for_sj(sj_train)
print(importance(sj_model,type = 2)) 
iq_model <- get_bst_model_for_iq(iq_train)
print(importance(iq_model,type = 2))

# plot sj
sj_train$fitted <- predict(sj_model, sj_train)
sj_train %>% 
  mutate(index = as.numeric(row.names(.))) %>%
  ggplot(aes(x = index)) + ggtitle("San Jose") +
  geom_line(aes(y = total_cases, colour = "total_cases")) + 
  geom_line(aes(y = fitted, colour = "fitted"))

# plot iq
iq_train$fitted = predict(iq_model, iq_train)
iq_train %>% 
  mutate(index = as.numeric(row.names(.))) %>%
  ggplot(aes(x = index)) + ggtitle("Iquitos") + 
  geom_line(aes(y = total_cases, colour = "total_cases")) + 
  geom_line(aes(y = fitted, colour = "fitted"))

combinedRes = rbind(sj_train,iq_train)
error = combinedRes$total_cases - combinedRes$fitted

cat('\nmean average error is -: ', mae(error))


# submitting the predictions
tests <- preprocessData('G:/S7/Data Mining/Dengue/Data/dengue_features_test.csv')
sj_test <- tests[[1]]; iq_test <- tests[[2]]

#Apply Time series Lags for test set
sj_test %<>% Time_Series_Lags_for_sj
iq_test %<>% Time_Series_Lags_for_iq


sj_test$predicted = predict(sj_model , sj_test)
iq_test$predicted = predict(iq_model , iq_test)

submissions = read.csv('G:/S7/Data Mining/Dengue/Data/submission_format.csv')
inner_join(submissions, rbind(sj_test,iq_test)) %>%
  dplyr::select(city, year, weekofyear, total_cases = predicted) ->
  predictions

predictions$total_cases %<>% round()
write.csv(predictions, 'G:/S7/Data Mining/Dengue/Submissions/predictions_with_lags_random_forest_3.csv', row.names = FALSE)