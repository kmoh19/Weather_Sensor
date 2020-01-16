library(rpart)
library(rpart.plot)
weather<-read.csv('C:\\Users\\7222115\\Desktop\\DS_sensor_weather.csv',header=TRUE,sep=',')
weather_nna<-weather %>% na.omit()

weather_nna_nr<-weather_nna[,-11]

weather_nna_nr_rf<-(weather_nna_nr %>% mutate(relative_humidity_pm_rf=relative_humidity_pm>30))[,-10]



weather_nna_nr_rf$relative_humidity_pm_r2<-factor(weather_nna_nr_rf$relative_humidity_pm_rf, levels = c(TRUE,FALSE), labels = c("High","Low"))

weather_nna_nr_rf$relative_humidity_pm_r2 <- relevel(weather_nna_nr_rf$relative_humidity_pm_r2, ref = "Low")



weather_cln<-weather_nna_nr_rf[,-10]

train_ind <- sample(seq_len(nrow(weather_cln)), size = smp_size)

train <- weather_cln[train_ind, ]

test <- weather_cln[-train_ind, ]

weather_mod<-rpart(relative_humidity_pm_r2~.,train,method='class',control= rpart.control(cp=0))

pred<-predict(weather_mod,test,type='class')

confusionMatrix(pred,test$relative_humidity_pm_r2)

rpart.plot(weather_mod, type = 3, box.palette = c("green","red"), fallen.leaves = TRUE)

weather_mod_pruned<-rpart(relative_humidity_pm_r2~.,train,method='class',control= rpart.control(cp=0.1))

pred_pruned<-predict(weather_mod_pruned,test,type='class')

confusionMatrix(pred_pruned,test$relative_humidity_pm_r2)
