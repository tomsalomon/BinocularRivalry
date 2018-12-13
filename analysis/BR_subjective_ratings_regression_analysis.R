
# Load rquired R packages
require("lme4")
require("lmerTest")
require("Rcmdr")
require("rstudioapi")
require("viridis")

# clear workspace
rm(list=ls())

# Define the current script location as the working directory
pwd = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(pwd)
data_path=paste0(pwd,'/processed_data/')
experiment_names = c('BR_Celebrities','BR_Politicians','BR_IAPS') 

model_i = 0
summary_df=as.data.frame(c())
models = list(c())
for (experiment_num in 1:length(experiment_names)) { 
  experiment_name = experiment_names[experiment_num]
  print(paste("analyzing:", experiment_name))
  data_file = Sys.glob(paste(data_path, "/BR_data_",experiment_name,"*.txt",sep=""))
  subjects_file = Sys.glob(paste(data_path, "/valid_subjects_*",experiment_name,"*.txt",sep=""))
  subjects = unlist(read.table(subjects_file))
  BR_data=read.table(data_file,header=T)
  BR_data=BR_data[BR_data$SubjectCode %in% subjects,] # discard invalid subjects
  BR_data$SubjectCode=as.factor(BR_data$SubjectCode)
  
  if (experiment_num ==3) {
    # in Experiment 3, Trial type 2: Stim1 is the low value stimulus, while stim2 is the high-value. switch the stim in this condition to match with other trial (stim 1 is the high)
    ind2switch = (BR_data$Delta_Val == -1)
    BR_data_tmp = BR_data # original data
    levels(BR_data$Stim1Name) = unique(c(levels(BR_data$Stim1Name),levels(BR_data$Stim2Name))) # To avoid unrecognized level
    levels(BR_data$Stim2Name) = unique(c(levels(BR_data$Stim1Name),levels(BR_data$Stim2Name)))
    
    colnames(BR_data_tmp)[8] = "InitialStim" # remove the "1" to identify Stim1 variables
    stim1cols = grep('1',colnames(BR_data_tmp)) # find the variable names for Stim2 (e.g. "Stim2Time", "Stim2Fraction" etc.)
    stim2cols = grep('2',colnames(BR_data_tmp)) # find the variable names for Stim2 (e.g. "Stim2Time", "Stim2Fraction" etc.)
    BR_data[ind2switch,stim1cols] = BR_data_tmp[ind2switch,stim2cols]
    BR_data[ind2switch,stim2cols] = BR_data_tmp[ind2switch,stim1cols]
    BR_data$InitialStim1[ind2switch] = 1 - BR_data_tmp$InitialStim[ind2switch] 
    BR_data$Delta_Val[ind2switch] = -1*BR_data_tmp$Delta_Val[ind2switch]
  }
  
  BR_data$TrialType = 1 # High versus Low Value
  BR_data$TrialType[BR_data$Delta_Aro==0 & BR_data$IsHighAro==1] = 1 # High Arousal - Different Value
  BR_data$TrialType[BR_data$Delta_Aro==0 & BR_data$IsHighAro==0] = 2 # Low Arousal - Different Value
  BR_data$TrialType[BR_data$Delta_Val==0 & BR_data$IsHighVal==1] = 3 # High Value - Different Arousal
  BR_data$TrialType[BR_data$Delta_Val==0 & BR_data$IsHighVal==0] = 4 # Low Value - Different Arousal
  BR_data$TrialType2=as.factor(BR_data$TrialType)
  if (experiment_num ==3) {
    levels(BR_data$TrialType2)=c('High Arousal: Different Value','Low Arousal: Different Value','High Value: Different Arousal','Low Value: Different Arousal')
  } else { levels(BR_data$TrialType2)=c("Different Value") }
  
  # Exclude invalid trials: participant pressed 2 keys simoultanously or did not percieve any stimulus (all fusion)
  BR_data$ValidTrials=TRUE
  BR_data$ValidTrials[BR_data$IsCorrupted | ((BR_data$Stim1Time+BR_data$Stim2Time) == 0)] = FALSE 
  
  # Scale rankings t0 [0,1] so models can converge
  if (experiment_num >=2){
    BR_data$Aro1_Ranking = BR_data$Aro1_Ranking/10
    BR_data$Aro2_Ranking = BR_data$Aro2_Ranking/10
  } 
  if (experiment_num ==3) {
    BR_data$Val1_Ranking = BR_data$Val1_Ranking/10
    BR_data$Val2_Ranking = BR_data$Val2_Ranking/10 
  }
  
  BR_data$Delta_Val_rating=BR_data$Val1_Ranking-BR_data$Val2_Ranking
  BR_data$Delta_Aro_rating=BR_data$Aro1_Ranking-BR_data$Aro2_Ranking
  BR_data$DiffFraction = (BR_data$Stim1Fraction - BR_data$Stim2Fraction)
  BR_data$DiffTime = (BR_data$Stim1Time - BR_data$Stim2Time)
  BR_data$DiffQuantity = (BR_data$Stim1Quantity - BR_data$Stim2Quantity)
  measurement_names = c('DiffFraction','DiffTime','DiffQuantity','InitialStim1')
  
  # -------- Run Models
  for (TrialType_i in 1:max(BR_data$TrialType)){
    for (measurement_i in 1:4) {
      model_i = model_i+1
      measurement_name = measurement_names [measurement_i]
      if (experiment_num ==1) { # Subjective value in experiment 1
        model_formula = formula(paste0(measurement_name, " ~ 1  + Delta_Val_rating + (1|SubjectCode)"))
      } else if (experiment_num >=2) {  # Subjective value and arousal in experiments 2 and 3
        model_formula = formula(paste0(measurement_name, " ~ 1  + Delta_Val_rating * Delta_Aro_rating + (1|SubjectCode)"))
      }
      if (measurement_i <=3) { # Continuous dependent variables
        models[[model_i]] = lmer(model_formula, data=subset(BR_data,ValidTrials & TrialType==TrialType_i), na.action=na.omit)
      } else if (measurement_i ==4) { # Binomial dependent variable - is stim1 first percept
        models[[model_i]] = glmer(model_formula, data=subset(BR_data,ValidTrials & TrialType==TrialType_i), na.action=na.omit, family = binomial)
      }
      
      analysis_summary = summary(models[[model_i]])
      colnames(analysis_summary$coefficients)[grep('Pr',(colnames(analysis_summary$coefficients)))] = "p" # replace Pr(>|z|) / Pr(>|t|) with p
      coeff_selection = colnames(analysis_summary$coefficients) %in% c("Estimate","Std. Error","p") # make sure the number of column match
      summary_df_tmp = as.data.frame(analysis_summary$coefficients[,coeff_selection],row.names = FALSE)
      summary_df_tmp$experiment_name = experiment_name
      summary_df_tmp$experiment_num = experiment_num
      summary_df_tmp$TrialType = TrialType_i
      summary_df_tmp$measurement = measurement_name
      summary_df_tmp$coefficient = rownames(analysis_summary$coefficients)
      summary_df_tmp = summary_df_tmp[,c(4:ncol(summary_df_tmp),1:3)] # order df with coefficients as last columns
      summary_df = rbind(summary_df,summary_df_tmp)
    }
  }
  if (experiment_num == 1) {BR_data1 = BR_data
  } else if (experiment_num == 2) {BR_data2 = BR_data
  } else if (experiment_num == 3) {BR_data3 = BR_data}
}

summary_df$asterisk = ""
summary_df$asterisk[summary_df$p < .1] = "#"
summary_df$asterisk[summary_df$p < .05] = "*"
summary_df$asterisk[summary_df$p < .01] = "**"
summary_df$asterisk[summary_df$p < .001] = "***"


# Plot Experiment 2 interaction model
BR_data_3d=subset(BR_data2,(ValidTrials & (is.na(BR_data2$Delta_Val_rating)==0)  & (is.na(BR_data2$Delta_Aro_rating)==0) ))
mid = 0
interaction_model = lm(DiffTime ~ Delta_Aro_rating * Delta_Val_rating ,data = BR_data_3d,na.action =na.omit )
BR_data_3d$predicted = predict(mymodel)
x <- seq(-1,1,length.out=101) # on respective x and y axis
y <- seq(.5,1,length.out=101)
prediction_plane_df <- expand.grid(x=x, y=y) #grid for colors
colnames(prediction_plane_df)= c("Delta_Aro_rating","Delta_Val_rating")
prediction_plane_df$PredictedDominance = predict(interaction_model,prediction_plane_df) # Model prediction as the color factor
prediction_plane_df$DiffTime=NaN
mid=0
ggplot(data=BR_data_3d,aes(x = Delta_Aro_rating , y= Delta_Val_rating, color = DiffTime))+  
  geom_raster(data =prediction_plane_df, aes(Delta_Aro_rating, Delta_Val_rating,fill = PredictedDominance))+
  scale_fill_gradient2(midpoint = mid, high = "green", low = "blue", mid="white")+ # color scheme
  #scale_fill_viridis() +
  geom_point(alpha = 0.5 ,size = 4)+
  labs(title="Average Dominance Duration of High-Value Stimuli:\nModel Prediction VS Actual Results")+
  scale_color_gradient (high = "green", low = "blue", space ="Lab")+
  #scale_color_gradient2 (midpoint = mid, high = "green", low = "blue", mid="white", space ="Lab")+
  theme(axis.line.x=element_line(),
        axis.line.y=element_line())+ 
  xlim(-1,1)+ ylim(.5,1)+
  coord_cartesian(expand = F) + # no padding for border
  xlab(expression(paste(Delta," Subjective Arousal"))) +
  ylab(expression(paste(Delta," Subjective Value"))) +
  labs(color='Actual Dominance')+
  theme_bw()

scatter3d(DiffFraction ~ Delta_Aro_rating + Delta_Val_rating , BR_data_3d, 
          parallel=FALSE, fit="smooth",
          surface.col="blue" ,surface.alpha = list(0.3),axis.scales=FALSE, point.col="black",
          xlab = "Delta Subjective Arousal", 
          ylab = "Dominance of HV stimuli", 
          zlab = "Delta Subjective Value",
          neg.res.col = NA,
          pos.res.col = NA) 


