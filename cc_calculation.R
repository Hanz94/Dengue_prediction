df <- read.csv("G:/S7/Data Mining/Dengue/Data/dengue_features_train.csv")
lables <- read.csv("G:/S7/Data Mining/Dengue/Data/dengue_labels_train.csv")
library(ggplot2)


combined <- cbind(subset( df, select = -c(city, weekofyear,year ) ),lables)

Find_Max_CCF<- function(a,b) 
{ 
  d <- ccf(a, b, plot = FALSE, lag.max=4, na.action = na.contiguous) 
  cor = d$acf[,,1] 
  lag = d$lag[,,1] 
  res = data.frame(cor,lag) 
  res <- filter(res, lag >= 0)         #consider only lags not leads
  res_max = res[which.max(res$cor),] 
  return(res_max) 
}

Get_Best_lag<- function(city,attri)
{
  acf = c()
  lag = c()
  if (city == "sj") {
    for (attr in attri) {
      temp <- Find_Max_CCF(df_sj["total_cases"], df_sj[attr])
      acf<- append(acf,temp$cor)
      lag<- append(lag,temp$lag)
    }
  }
  else
  {
    for (attr in attri) {
      temp <- Find_Max_CCF(df_iq["total_cases"], df_iq[attr])
      acf<- append(acf,temp$cor)
      lag<- append(lag,temp$lag)
    }
  }
  alldata <- data.frame(attri,acf,lag)
  return(alldata)
}


df_sj <- filter(combined, city == 'sj')
df_iq <- filter(combined, city == 'iq')


#use this for ccf visualization
ccf(df_sj$total_cases, df_sj$reanalysis_specific_humidity_g_per_kg, lag.max=8, plot=TRUE, main="tt", na.action = na.contiguous)
ccf(df_iq$total_cases, df_iq$reanalysis_specific_humidity_g_per_kg,lag.max=8, plot=TRUE, main="tt", na.action = na.contiguous)


#print(Find_Max_CCF(df_sj["total_cases"], df_sj$reanalysis_specific_humidity_g_per_kg))

sj_cc_all <- Get_Best_lag("sj",c("reanalysis_specific_humidity_g_per_kg",
                                 "reanalysis_dew_point_temp_k",
                                 "station_avg_temp_c",
                                 "station_min_temp_c",
                                 "reanalysis_min_air_temp_k",
                                 "station_max_temp_c",
                                 "reanalysis_max_air_temp_k"))

sj_cc_all <- sj_cc_all[order(sj_cc_all$acf),]
print(sj_cc_all)

ggplot(data=sj_cc_all, aes(x=attri, y=acf, fill=lag)) +
  geom_bar(colour="black", stat="identity", position="dodge")


iq_cc_all <- Get_Best_lag("iq",c("reanalysis_specific_humidity_g_per_kg",
                                 "reanalysis_dew_point_temp_k",
                                 "station_avg_temp_c",
                                 "station_min_temp_c",
                                 "reanalysis_min_air_temp_k",
                                 "station_max_temp_c",
                                 "reanalysis_max_air_temp_k"))

iq_cc_all <- iq_cc_all[order(iq_cc_all$acf),]
print(iq_cc_all)

ggplot(data=iq_cc_all, aes(x=attri, y=acf, fill=lag)) +
  geom_bar(colour="black", stat="identity", position="dodge")

