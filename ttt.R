
library(missForest)


data("iris")
iris.mis <- prodNA(iris, noNA = 0.1)

summary(iris.mis)

a <- missForest(iris.mis)

summary(iris.imp)


# load data 
df <- read.csv("G:/S7/Data Mining/Dengue/Data/dengue_features_train.csv")

# features we want
features = c("reanalysis_specific_humidity_g_per_kg",
             "reanalysis_dew_point_temp_k",
             "station_avg_temp_c",
             "station_min_temp_c",
             "reanalysis_min_air_temp_k",
             "station_max_temp_c",
             "reanalysis_max_air_temp_k")

temp <- missForest(df[features])

# fill missing values
summary(temp$ximp)

df_new <- temp$ximp
