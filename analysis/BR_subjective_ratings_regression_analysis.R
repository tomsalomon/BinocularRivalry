
# Load rquired R packages
library("rstudioapi") # to define current working directory
library("lme4") # for mixed effect models
library("lmerTest") # get p-values for mixed effect models
library("ggplot2") # plot package
library("lm.beta")
library("reshape2")
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
analysis_summaries = list(c())
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
  
  # Scale rankings to [0,1] so models can converge
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
  BR_data$DiffFraction = (BR_data$Stim1Fraction - 0.5) #   BR_data$DiffFraction = (BR_data$Stim1Fraction - BR_data$Stim2Fraction)
  BR_data$DiffTime = (BR_data$Stim1Time - BR_data$Stim2Time)
  BR_data$DiffQuantity = (BR_data$Stim1Quantity - BR_data$Stim2Quantity)
  measurement_names = c('DiffFraction','DiffTime','DiffQuantity','InitialStim1')
  
  # -------- Run Models
  for (TrialType_i in 1:max(BR_data$TrialType)){
    for (measurement_i in 1:4) {
      model_i = model_i+1
      measurement_name = measurement_names [measurement_i]
      if (experiment_num ==1) { # Subjective value in experiment 1
        #model_formula = formula(paste0(measurement_name, " ~ 1  + Delta_Val_rating + (1 + Delta_Val_rating|SubjectCode)")) # with random slopes
        model_formula = formula(paste0(measurement_name, " ~ 1  + Delta_Val_rating + (1 |SubjectCode)"))
      } else if (experiment_num >=2) {  # Subjective value and arousal in experiments 2 and 3
        #model_formula = formula(paste0(measurement_name, " ~ 1  + Delta_Val_rating * Delta_Aro_rating + (1 + Delta_Val_rating + Delta_Aro_rating|SubjectCode)")) # with random slopes
        model_formula = formula(paste0(measurement_name, " ~ 1  + Delta_Val_rating * Delta_Aro_rating + (1 |SubjectCode)"))
      }
      if (measurement_i <=3) { # Continuous dependent variables
        models[[model_i]] = lmer(model_formula, data=subset(BR_data,ValidTrials & TrialType==TrialType_i), na.action=na.omit)
      } else if (measurement_i ==4) { # Binomial dependent variable - is stim1 first percept
        models[[model_i]] = glmer(model_formula, data=subset(BR_data,ValidTrials & TrialType==TrialType_i), na.action=na.omit, family = binomial)
      }
      analysis_summary = summary(models[[model_i]])
      analysis_summaries[[model_i]] = analysis_summary
      colnames(analysis_summary$coefficients)[grep('Pr',(colnames(analysis_summary$coefficients)))] = "p" # replace Pr(>|z|) / Pr(>|t|) with p
      colnames(analysis_summary$coefficients)[grep('value',(colnames(analysis_summary$coefficients)))] = "t/z value" # replace "t value" / "z value" with "t/z value"
      
      summary_df_tmp = as.data.frame(analysis_summary$coefficients,row.names = FALSE)
      if (measurement_i == 4) { summary_df_tmp$df = NA }
      summary_df_tmp$experiment_name = experiment_name
      summary_df_tmp$experiment_num = experiment_num
      summary_df_tmp$TrialType = TrialType_i
      summary_df_tmp$measurement = measurement_name
      summary_df_tmp$model = model_i
      summary_df_tmp$coefficient = rownames(analysis_summary$coefficients)
      summary_df_tmp = summary_df_tmp[,c(6:ncol(summary_df_tmp),1:5)] # order df with coefficients as last columns
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

View(summary_df)

# miss-match of subjetive ratings and manipulation:
missmatch_value = sum(BR_data$Delta_Val_rating[BR_data$TrialType<=2 & BR_data$ValidTrials==1]<=0) / sum(BR_data$TrialType<=2 & BR_data$ValidTrials==1)
missmatch_arousal = sum(BR_data$Delta_Aro_rating[BR_data$TrialType>=3 & BR_data$ValidTrials==1]<=0) / sum(BR_data$TrialType>=3 & BR_data$ValidTrials==1)

# Test prediction of condition 2 (Value | low arousal) which was not significant using the other measurement
options(warn=-1)
BR_data_agg=as.data.frame(aggregate(BR_data,by=list(BR_data$SubjectCode,BR_data$TrialType), mean, na.rm=TRUE))
Data_by_sub=melt(BR_data_agg,id = c("SubjectInd", "TrialType") ,measurement_names)
options(warn=0)
colnames(Data_by_sub)[ncol(Data_by_sub)-1] = "Measurement"
levels(Data_by_sub$Measurement) = c("Predominance Score", "Dominance Duration", "Percepts After Fusion", "Initial Percept")
PDS_data = Data_by_sub[Data_by_sub$Measurement=="Predominance Score",]
PDS_data = acast(PDS_data,formula = SubjectInd ~ TrialType)
PDS_data = as.data.frame(PDS_data)
colnames(PDS_data)= paste0("Cond",(1:ncol(PDS_data)))
my_model = lm(Cond2 ~ -1 + Cond1 + Cond3 + Cond4, data = PDS_data)
print(summary(lm.beta(my_model)))
PDS_data$prediction = my_model$fitted.values
dev.new()
ggplot (PDS_data, aes(x= prediction, y = Cond2)) +
  geom_point() + geom_smooth(method = "lm") +
  xlab("Model prediction") + ylab("Actual dominance") +
  ggtitle("Predicted Dominance in (Value | Low-Arousal) Trials")

# Visualize Experiment 2 correlation with arousal
n = length(unique(BR_data2$SubjectCode))
n_quant = 10
qvec = quantile(BR_data2$Delta_Aro_rating,probs=seq(0,1,1/n_quant),na.rm=TRUE, include.lowest = TRUE, labels = 1:n_quant)
BR_data2$Delta_Aro_rating_quantiles = cut(BR_data2$Delta_Aro_rating,breaks = qvec, include.lowest = TRUE)
BR_data2$Delta_Aro_rating_q_num = as.numeric(BR_data2$Delta_Aro_rating_quantiles )
BR_data2_by_quantiles = data.frame(row.names = 1:n_quant)
BR_data2_by_quantiles$Q = 1:n_quant
BR_data2_by_quantiles$DiffFraction.M = with(data =BR_data2,tapply(DiffFraction, Delta_Aro_rating_quantiles,mean, na.rm=TRUE))
BR_data2_by_quantiles$DiffFraction.SD = with(data =BR_data2,tapply(DiffFraction, Delta_Aro_rating_quantiles,sd, na.rm=TRUE))
BR_data2_by_quantiles$DiffFraction.SE =  BR_data2_by_quantiles$DiffFraction.SD/sqrt(n)
BR_data2_by_quantiles$lower = BR_data2_by_quantiles$DiffFraction.M - BR_data2_by_quantiles$DiffFraction.SE
BR_data2_by_quantiles$upper = BR_data2_by_quantiles$DiffFraction.M + BR_data2_by_quantiles$DiffFraction.SE


ggplot(data = BR_data2_by_quantiles, aes(x = Q, y = DiffFraction.M) )+
  geom_point(stat = 'identity') +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
  geom_smooth(data = subset(BR_data2,ValidTrials & !is.na(Delta_Aro_rating)), aes(x=Delta_Aro_rating_q_num, y = DiffFraction),method = "lm", color = "blue") +
  scale_x_continuous(breaks = 1:n_quant, name = "Decile") +
  labs(y = "High-Value Predominance Over Low-Value") +
  ggtitle("Predominance score - by arousal deciles") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

