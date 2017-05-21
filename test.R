# load libraries
pkgs <- c('tidyverse', 'corrplot', 'magrittr', 'zoo', 'RColorBrewer', 'gridExtra','MASS')
invisible(lapply(pkgs, require, character.only = T))

## Data Loading
train_features = read.csv('G:/S7/Data Mining/Dengue/dengue_features_train.csv')
train_labels   = read.csv('G:/S7/Data Mining/Dengue/dengue_labels_train.csv')

str(train_features)

# Seperate data by city
sj_train_features = train_features %>% filter(city == 'sj')
sj_train_labels   = train_labels   %>% filter(city == 'sj')

iq_train_features = train_features %>% filter(city == 'iq')
iq_train_labels   = train_labels   %>% filter(city == 'iq')


# data shape for each city
cat('\nSan Juan\n',
    '\t features: ', sj_train_features %>% ncol, 
    '\t entries: ' , sj_train_features %>% nrow,
    '\t labels: '  , sj_train_labels %>% nrow)

cat('\nIquitos\n',
    '\t features: ', iq_train_features %>% ncol, 
    '\t entries: ' , iq_train_features %>% nrow,
    '\t labels: '  , iq_train_labels %>% nrow)


# Remove `week_start_date` string.
sj_train_features %<>% dplyr::select(-week_start_date)
iq_train_features %<>% dplyr::select(-week_start_date)

# impute NAs by the latest value
sj_train_features$ndvi_ne %<>% na.locf(fromLast = TRUE)
iq_train_features$ndvi_ne %<>% na.locf(fromLast = TRUE)


# corerlations between features
sj_train_features %<>% mutate('total_cases' = sj_train_labels$total_cases)
iq_train_features %<>% mutate('total_cases' = iq_train_labels$total_cases)

# plot san juan correlation matrix
sj_train_features %>% 
  dplyr::select(-city, -year, -weekofyear) %>%
  cor(use = 'pairwise.complete.obs') -> M1

corrplot(M1, type="lower", method="color",
         col=brewer.pal(n=8, name="RdBu"),diag=FALSE)