
# Load rquired R packages
library("rstudioapi") # to define current working directory
library("ggplot2") # plot package
library("dplyr")

library("lme4") # for mixed effect models
library("lmerTest") # get p-values for mixed effect models
library("Rcmdr") # 3d plot package
# clear workspace
rm(list=ls())

# Define the current script location as the working directory
pwd = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(pwd)
data_path=paste0(pwd,'/processed_data/')
experiment_names = c('BR_Celebrities','BR_Politicians','BR_IAPS') 

for (experiment_num in 1:length(experiment_names)) {
  # Read external data
  experiment_name = experiment_names[experiment_num]
  # File location
  data_file = Sys.glob(paste(data_path, "/BR_data_",experiment_name,"*.txt",sep=""))
  summary_file = Sys.glob(paste(data_path, "/SummaryTable_",experiment_name,"*.txt",sep=""))
  subjects_file = Sys.glob(paste(data_path, "/valid_subjects_*",experiment_name,"*.txt",sep=""))
  subjects = unlist(read.table(subjects_file)) # Valid subjects
  BR_data=read.table(data_file,header=T,sep = '\t') # Task data
  SummaryTable = read.table(summary_file,header=T,sep = '\t') # Statistical summary table
  
  # Remove invalid data (excluded subjects and trials)
  BR_data=BR_data[BR_data$SubjectCode %in% subjects,] # discard invalid subjects
  BR_data$ValidTrials=TRUE   # Exclude invalid trials: participant pressed 2 keys simoultanously or did not percieve any stimulus (all fusion)
  BR_data$ValidTrials[BR_data$IsCorrupted | ((BR_data$Stim1Time+BR_data$Stim2Time) == 0)] = FALSE 
  BR_data=BR_data[BR_data$ValidTrials ==1 ,] # discard invalid trials
  
  BR_data$SubjectCode=as.factor(BR_data$SubjectCode)
  BR_data$TrialType = 1 # High versus Low Value
  BR_data$TrialType[BR_data$Delta_Aro==0 & BR_data$IsHighAro==1] = 1 # High Arousal - Different Value (Exp 3)
  BR_data$TrialType[BR_data$Delta_Aro==0 & BR_data$IsHighAro==0] = 2 # Low Arousal - Different Value (Exp 3)
  BR_data$TrialType[BR_data$Delta_Val==0 & BR_data$IsHighVal==1] = 3 # High Value - Different Arousal (Exp 3)
  BR_data$TrialType[BR_data$Delta_Val==0 & BR_data$IsHighVal==0] = 4 # Low Value - Different Arousal (Exp 3)
  BR_data$TrialType2=as.factor(BR_data$TrialType)
  if (experiment_num ==3) {
    levels(BR_data$TrialType2)=c('High Arousal: Different Value','Low Arousal: Different Value','High Value: Different Arousal','Low Value: Different Arousal')
  } else { levels(BR_data$TrialType2)=c("Different Value") }
  
  if (experiment_num ==3) {
    # in Experiment 3, Trial type 2: Stim1 is the low value stimulus, while stim2 is the high-value. switch the stim in this condition to match with other trials (stim 1 is the high)
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
  # Dependent variable: High stimulus (stim1) dominance
  BR_data$DiffFraction = (BR_data$Stim1Fraction - 0.5)
  BR_data$DiffTime = (BR_data$Stim1Time - BR_data$Stim2Time)
  BR_data$DiffQuantity = (BR_data$Stim1Quantity - BR_data$Stim2Quantity)
  BR_data$DiffInitial = BR_data$InitialStim1 - 0.5
  measurement_names = c('DiffFraction','DiffTime','DiffQuantity','DiffInitial')
  
  BR_data_agg=as.data.frame(aggregate(BR_data,by=list(BR_data$SubjectCode,BR_data$TrialType), mean, na.rm=TRUE))
  Data_by_sub=melt(BR_data_agg,id = c("SubjectInd", "TrialType") ,measurement_names)
  Data_by_sub$TrialType2=as.factor(Data_by_sub$TrialType)
  if (experiment_num ==3) {
    levels(Data_by_sub$TrialType2)=c('Value |\nHigh Arousal','Value |\nLow Arousa','Arousal |\nHigh Value','Arousal |\nLow Value')
  } else { levels(Data_by_sub$TrialType2)=c("Different Value") }
  levels(Data_by_sub$variable) = c("Predominance Score", "Average Dominance Duration", "Percepts After Fusion", "Initial Percept")
  
  Data_by_sub$dummy_data = -1*Data_by_sub$value
  dummy_data=Data_by_sub
  dummy_data[,'value'] =-1*dummy_data[,'value']
  
  ggplot(Data_by_sub, aes(x=TrialType2, y=value, fill = TrialType2)) + 
    facet_wrap(~variable, scales = 'free') +
    geom_violin(trim=TRUE)+
    # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
    geom_boxplot(width=0.2, notch=TRUE, varwidth = TRUE, fill='white') +
    #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
    geom_hline(yintercept=0, linetype="dashed") +
    theme_bw() +
    labs(fill='Condition') +
    geom_blank(aes(y=dummy_data)) + # will set 0 at the middle with facet_wrap free scale
    theme(axis.title.x=element_blank(), axis.title.y=element_blank())
  
  plot = ggplot(Data_by_sub, aes(x=TrialType2, y=value, fill = TrialType2)) + 
    facet_wrap(~variable, scales = 'free') +
    # geom_violin(trim=FALSE)+
    # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
    geom_boxplot(notch=TRUE, position=position_dodge(.2)) +
    #stat_summary(fun.y=mean, geom="point", shape=16, size=2, color = 'red') +
    geom_hline(yintercept=0, linetype="dashed") +
    #  stat_compare_means(label = "p.signif", method = "t.test",ref.group =".all.") +
    theme_bw() +
    geom_blank(aes(y=dummy_data)) + # will set 0 at the middle with facet_wrap free scale
    theme(legend.position="none",axis.title.x=element_blank(), axis.title.y=element_blank()) 
  
  # Data frame containing asterisks representing statistical significance
  asterisk_data = distinct(Data_by_sub, variable, TrialType2) %>% arrange(variable, TrialType2)
  asterisk_data$yloc = NA
  variables = levels(asterisk_data$variable)
  for (var in variables) {
    asterisk_data$yloc [asterisk_data$variable == var] = max(Data_by_sub$value[Data_by_sub$variable == var]) *1.1
  }
  asterisk_data$label = c("a", NA, NA, NA, NA, "b", "b", NA, NA, NA, NA, NA, NA, NA, "***", "**")
  
  plot +geom_text(data = asterisk_data, aes(y = yloc, label = label), position = position_dodge(width = .75)) 
}


ggplot(Data_by_sub, aes(x=TrialType2, y=value, fill = TrialType2)) + 
  facet_wrap(~variable, scales = 'free',shrink = TRUE) +
  # geom_violin(trim=FALSE)+
  geom_boxplot(notch=TRUE, position=position_dodge(.2),outlier.size = 1) +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2, color = 'red') +
  geom_blank(data=dummy_data) + # will set 0 at the middle with facet_wrap free scale
  geom_jitter(size=.5,color='black',width=.05) +
  geom_hline(yintercept=0, linetype="dashed") + 
  theme_bw()

ggplot(Data_by_sub, aes(x=variable, y=value, fill = TrialType2)) + 
  facet_wrap(~variable, scales = 'free') +
  geom_boxplot(width=0.7, position = "dodge") +
  geom_hline(yintercept=0, linetype="dashed")



#geom_violin() +
geom_boxplot() + 
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  #geom_jitter(shape=16, position=position_jitter(0.2),  color="blue",  size=3)+
  stat_summary(fun.y=median, geom="point", size=5, color="red") + 
  geom_dotplot(binaxis='y', stackdir='center',position=position_dodge(.1))





