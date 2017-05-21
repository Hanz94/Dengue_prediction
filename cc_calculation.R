df <- read.csv("G:/S7/Data Mining/Dengue/Data/dengue_features_train.csv")
lables <- read.csv("G:/S7/Data Mining/Dengue/Data/dengue_labels_train.csv")
library(ggplot2)


combined <- cbind(subset( df, select = -c(city, weekofyear,year ) ),lables)

Find_Max_CCF<- function(a,b) 
{ 
  d <- ccf(a, b, plot = FALSE, lag.max=8, na.action = na.contiguous) 
  cor = d$acf[,,1] 
  lag = d$lag[,,1] 
  res = data.frame(cor,lag) 
  res_max = res[which.max(abs(res$cor)),] 
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
      Find_Max_CCF(df_iq["total_cases"], df_iq[attr])
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

#print(Find_Max_CCF(df_sj["total_cases"], df_sj$reanalysis_specific_humidity_g_per_kg))

sj_cc_all <- Get_Best_lag("sj",c("reanalysis_specific_humidity_g_per_kg" , "reanalysis_dew_point_temp_k" , "station_avg_temp_c" ,"reanalysis_max_air_temp_k"))
print(sj_cc_all)

ggplot(data=sj_cc_all, aes(x=attri, y=acf, fill=acf)) +
  geom_bar(colour="black", stat="identity")


