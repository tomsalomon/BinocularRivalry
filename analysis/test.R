# Plot Experiment 2 interaction model
BR_data_3d=subset(BR_data2,(ValidTrials & (is.na(BR_data2$Delta_Val_rating)==0)  & (is.na(BR_data2$Delta_Aro_rating)==0) ))
mid = 0
interaction_model = lm(DiffTime ~ Delta_Aro_rating * Delta_Val_rating ,data = BR_data_3d,na.action =na.omit) # fixed effects model, ignoring Subject random effect, just to get approximate coefficients
interaction_model_mixed = lmer(DiffTime ~ Delta_Aro_rating * Delta_Val_rating + (1|SubjectCode),data = BR_data_3d,na.action =na.omit) # mixed effects model, with Subject random effect
BR_data_3d$predicted = predict(interaction_model_mixed)
x <- seq(-1,1,length.out=101) # on respective x and y axis
y <- seq(.5,1,length.out=101)
prediction_plane_df <- expand.grid(x=x, y=y) #grid for colors
colnames(prediction_plane_df)= c("Delta_Aro_rating","Delta_Val_rating")
prediction_plane_df$PredictedDominance = predict(interaction_model,prediction_plane_df) # Model prediction as the color factor
prediction_plane_df$DiffTime=NaN
mid=0
ggplot(data=BR_data_3d,aes(x = Delta_Aro_rating , y= Delta_Val_rating, color = DiffTime))+  
  geom_raster(data = prediction_plane_df, aes(Delta_Aro_rating, Delta_Val_rating,fill = PredictedDominance)) + # Prediction plane as background
  scale_fill_gradient2(midpoint = mid, high = "green", low = "blue", mid="white") + # color scheme for the prediction plane
  geom_point(alpha = 0.5 ,size = 4) + # Actual data
  scale_color_gradient (high = "green", low = "blue", space ="Lab") + # color scheme for the actual data
  #scale_color_gradient2 (midpoint = mid, high = "green", low = "blue", mid="white", space ="Lab") + # color scheme for the actual data
  labs(title="Average Dominance Duration of High-Value Stimuli:\nModel Prediction VS Actual Results", color='Actual Dominance') + # header and labels
  xlab(expression(paste(Delta," Subjective Arousal"))) +
  ylab(expression(paste(Delta," Subjective Value"))) +
  xlim(-1,1)+ ylim(.5,1)+
  coord_cartesian(expand = F) + # no padding for border
  theme_bw()

scatter3d(DiffFraction ~ Delta_Aro_rating + Delta_Val_rating , BR_data_3d, 
          parallel=FALSE, fit="smooth",
          surface.col="blue" ,surface.alpha = list(0.3),axis.scales=FALSE, point.col="black",
          xlab = "Delta Subjective Arousal", 
          ylab = "Dominance of HV stimuli", 
          zlab = "Delta Subjective Value",
          neg.res.col = NA,
          pos.res.col = NA) 