library(dplyr)

library(corrplot)

library(pROC)

library(tidyr)

library(ggplot2)

library(data.table)

library(caret)

library(gridExtra)

library(grid)

library(scales)

library(ggthemes)





ConfusionMatrixInfo <- function( data, predict, actual, cutoff )
  
{          
  
  # extract the column ;
  
  # relevel making 1 appears on the more commonly seen position in
  
  # a two by two confusion matrix     
  
  predict <- data[[predict]]
  
  actual  <- relevel( as.factor( data[[actual]] ), "High")
  
  
  
  result <- data.table( actual = actual, predict = predict )
  
  
  
  # caculating each pred falls into which category for the confusion matrix
  
  result[ , type := ifelse( predict >= cutoff & actual == "High", "TP",
                            
                            ifelse( predict >= cutoff & actual == "Low", "FP",
                                    
                                    ifelse( predict <  cutoff & actual == "High", "FN", "TN" ) ) ) %>% as.factor() ]
  
  
  
  # jittering : can spread the points along the x axis
  
  plot <- ggplot( result, aes( actual, predict, color = type ) ) +
    
    geom_violin( fill = "grey", color = NA ) +
    
    geom_jitter( shape = 1 ) +
    
    geom_hline( yintercept = cutoff, color = "blue", alpha = 0.6 ) +
    
    scale_y_continuous( limits = c( 0, 1 ) ) +
    
    scale_color_discrete( breaks = c( "TP", "FN", "FP", "TN" ) ) + # ordering of the legend
    
    guides( col = guide_legend( nrow = 2 ) ) + # adjust the legend to have two rows 
    
    ggtitle( sprintf( "Confusion Matrix with Cutoff at %.2f", cutoff ) )+
    
    theme_economist()
  
  
  
  return( list( data = result, plot = plot ) )
  
}





set.seed(123)

#weather<-read.csv('~/Downloads/DS_sensor_weather.csv',header=TRUE,sep=',')

weather<-read.csv('C:\\Users\\7222115\\Desktop\\DS_sensor_weather.csv',header=TRUE,sep=',')






summary(weather)



weather_nna<-weather %>% na.omit()



weather_nna_nr<-weather_nna[,-11]



corrplot(cor(weather_nna_nr))



weather_nna_nr_rf<-(weather_nna_nr %>% mutate(relative_humidity_pm_rf=relative_humidity_pm>30))[,-10]



weather_nna_nr_rf$relative_humidity_pm_r2<-factor(weather_nna_nr_rf$relative_humidity_pm_rf, levels = c(TRUE,FALSE), labels = c("High","Low"))

weather_nna_nr_rf$relative_humidity_pm_r2 <- relevel(weather_nna_nr_rf$relative_humidity_pm_r2, ref = "Low")



weather_cln<-weather_nna_nr_rf[,-10]

ggplot(gather(weather_cln[,-10]), aes(value)) +
  
  geom_histogram(bins = 15) +
  
  facet_wrap(~key, scales = 'free_x')



View(weather_cln)



prop.table( table(weather_cln$relative_humidity_pm_r2) )



smp_size <- floor(0.75 * nrow(weather_cln))



train_ind <- sample(seq_len(nrow(weather_cln)), size = smp_size)

train <- weather_cln[train_ind, ]

test <- weather_cln[-train_ind, ]

model_noscaling<-glm(relative_humidity_pm_r2~.,train,family='binomial')

train$pred<-predict(model_noscaling,newdata = train,type="response")

test$pred<-predict(model_noscaling,newdata = test,type="response")





mean(test$relative_humidity_pm_r2== ifelse(test$pred>0.46,'High','Low'))



confusionMatrix(as.factor(ifelse(train$pred>=0.5,'High','Low')),train$relative_humidity_pm_r2)



confusionMatrix(as.factor(ifelse(test$pred>=0.46,'High','Low')),test$relative_humidity_pm_r2)



ggplot(train, aes( pred, color = as.factor(relative_humidity_pm_r2) ) ) +
  
  geom_density( size = 1 ) +
  
  ggtitle( "Training Set's Predicted Score" )+
  
  scale_color_economist( name = "data", labels = c( "Low", "High" ) ) +
  
  theme_economist()



ggplot(test, aes( pred, color = as.factor(relative_humidity_pm_r2) ) ) +
  
  geom_density( size = 1 ) +
  
  ggtitle( "Test Set's Predicted Score" )+
  
  scale_color_economist( name = "data", labels = c( "Low", "High" ) ) +
  
  theme_economist()





summary_model<-summary(model_noscaling)



list( summary_model$coefficient,
      
      round( 1 - ( summary_model$deviance / summary_model$null.deviance ), 2 ) )


ConfusionMatrixInfo( data = test, predict = "pred", 
                     actual = "relative_humidity_pm_r2", cutoff = .46 )



ROC <- roc(test$relative_humidity_pm_r2, test$pred)

plot(ROC)

auc(ROC)


ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(relative_humidity_pm_r2~.,  data=weather_cln, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)

pred = predict(mod_fit, newdata=test[,-11])

confusionMatrix(pred,test$relative_humidity_pm_r2,positive = 'High')

mod_fit$finalModel
mod_fit$results




