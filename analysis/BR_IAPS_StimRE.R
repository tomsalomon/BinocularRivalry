library("rstudioapi") # to define current working directory
library("lme4") # for mixed effect models
library("lmerTest") # get p-values for mixed effect models

# clear workspace
rm(list=ls())

# Define the current script location as the working directory
pwd = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(pwd)
data_path=paste0(pwd,'/processed_data/')
experiment_num =3
experiment_name = 'BR_IAPS'

print(paste("analyzing:", experiment_name))
data_file = Sys.glob(paste(data_path, "/BR_data_",experiment_name,"*.txt",sep=""))
subjects_file = Sys.glob(paste(data_path, "/valid_subjects_*",experiment_name,"*.txt",sep=""))
subjects = unlist(read.table(subjects_file))
BR_data=read.table(data_file,header=T)
BR_data=BR_data[BR_data$SubjectCode %in% subjects,] # discard invalid subjects
BR_data$SubjectCode=as.factor(BR_data$SubjectCode)


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

BR_data$TrialType = 1 # High versus Low Value
BR_data$TrialType[BR_data$Delta_Aro==0 & BR_data$IsHighAro==1] = 1 # High Arousal - Different Value
BR_data$TrialType[BR_data$Delta_Aro==0 & BR_data$IsHighAro==0] = 2 # Low Arousal - Different Value
BR_data$TrialType[BR_data$Delta_Val==0 & BR_data$IsHighVal==1] = 3 # High Value - Different Arousal
BR_data$TrialType[BR_data$Delta_Val==0 & BR_data$IsHighVal==0] = 4 # Low Value - Different Arousal
BR_data$TrialType2=as.factor(BR_data$TrialType)
levels(BR_data$TrialType2)=c('High Arousal: Different Value','Low Arousal: Different Value','High Value: Different Arousal','Low Value: Different Arousal')

# Exclude invalid trials: participant pressed 2 keys simoultanously or did not percieve any stimulus (all fusion)
BR_data$ValidTrials=TRUE
BR_data$ValidTrials[BR_data$IsCorrupted | ((BR_data$Stim1Time+BR_data$Stim2Time) == 0)] = FALSE 

BR_data$DiffFraction = (BR_data$Stim1Fraction - 0.5) #   BR_data$DiffFraction = (BR_data$Stim1Fraction - BR_data$Stim2Fraction)
BR_data$DiffTime = (BR_data$Stim1Time - BR_data$Stim2Time)
BR_data$DiffQuantity = (BR_data$Stim1Quantity - BR_data$Stim2Quantity)
measurement_names = c('DiffFraction','DiffTime','DiffQuantity','InitialStim1')

model_i = 0
summary_df=as.data.frame(c())
models = list(c())
analysis_summaries = list(c())

for (TrialType_i in 1:max(BR_data$TrialType)){
  for (measurement_i in 1:4) {
    print(paste0("Condition = ",TrialType_i,", Measurement = ",measurement_i))
    model_i = model_i+1
    measurement_name = measurement_names [measurement_i]
    
    model_formula = formula(paste0(measurement_name, " ~ 1 + (1 + Stim1Name + Stim2Name | SubjectCode)"))
    if (measurement_i <= 3) {
      models[[model_i]] = lmer(model_formula, data=subset(BR_data,ValidTrials & TrialType==TrialType_i), na.action=na.omit)
    } else if (measurement_i ==4) { # Binomial dependent variable - is stim1 first percept
      models[[model_i]] = glmer(model_formula, data=subset(BR_data,ValidTrials & TrialType==TrialType_i), na.action=na.omit, family = binomial)
    }
    analysis_summary = summary(models[[model_i]])
    analysis_summaries[[model_i]] = analysis_summary
    colnames(analysis_summary$coefficients)[grep('Pr',(colnames(analysis_summary$coefficients)))] = "p" # replace Pr(>|z|) / Pr(>|t|) with p
    colnames(analysis_summary$coefficients)[grep('value',(colnames(analysis_summary$coefficients)))] = "t/z value" # replace "t value" / "z value" with "t/z value"
    
    summary_df_tmp = as.data.frame(analysis_summary$coefficients)
    
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
summary_df$asterisk = ""
summary_df$asterisk[summary_df$p < .1] = "#"
summary_df$asterisk[summary_df$p < .05] = "*"
summary_df$asterisk[summary_df$p < .01] = "**"
summary_df$asterisk[summary_df$p < .001] = "***"

View(summary_df)
