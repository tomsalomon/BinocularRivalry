
# Load rquired R packages
library("rstudioapi") # to define current working directory
library("ggplot2") # plot package
library("dplyr")
library("reshape")
library("gridExtra")

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
  experiment_name_full = paste0("Experiment ",experiment_num,": ",gsub("BR_","",experiment_name))
  
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
  BR_data$SubjectCode=as.factor(BR_data$SubjectCode)
  BR_data$TrialType = 1 # High versus Low Value
  BR_data$TrialType[BR_data$Delta_Aro==0 & BR_data$IsHighAro==1] = 1 # High Arousal - Different Value (Exp 3)
  BR_data$TrialType[BR_data$Delta_Aro==0 & BR_data$IsHighAro==0] = 2 # Low Arousal - Different Value (Exp 3)
  BR_data$TrialType[BR_data$Delta_Val==0 & BR_data$IsHighVal==1] = 3 # High Value - Different Arousal (Exp 3)
  BR_data$TrialType[BR_data$Delta_Val==0 & BR_data$IsHighVal==0] = 4 # Low Value - Different Arousal (Exp 3)
  BR_data$TrialType2=as.factor(BR_data$TrialType)
  if (experiment_num ==3) {
    levels(BR_data$TrialType2)=c('High Arousal: Different Value','Low Arousal: Different Value','High Value: Different Arousal','Low Value: Different Arousal')
  } else { levels(BR_data$TrialType2)=c("") }
  
  # Dependent variable: High stimulus (stim1) dominance
  BR_data$DiffFraction = (BR_data$Stim1Fraction - 0.5)
  BR_data$DiffTime = (BR_data$Stim1Time - BR_data$Stim2Time)
  BR_data$DiffQuantity = (BR_data$Stim1Quantity - BR_data$Stim2Quantity)
  BR_data$DiffInitial = BR_data$InitialStim1 - 0.5
  measurement_names = c('DiffFraction','DiffTime','DiffQuantity','DiffInitial')
  
  options(warn=-1)
  BR_data_agg=as.data.frame(aggregate(BR_data,by=list(BR_data$SubjectCode,BR_data$TrialType), mean, na.rm=TRUE))
  Data_by_sub=melt(BR_data_agg,id = c("SubjectInd", "TrialType") ,measurement_names)
  options(warn=0)
  colnames(Data_by_sub)[ncol(Data_by_sub)-1] = "Measurement"
  Data_by_sub$TrialType2=as.factor(Data_by_sub$TrialType)
  if (experiment_num ==3) {
    levels(Data_by_sub$TrialType2)=c('Value |\nHigh Aro.','Value |\nLow Aro.','Arousal |\nHigh Val.','Arousal |\nLow Val.')
    #levels(Data_by_sub$TrialType2)=c('Value |\nHigh Arousal','Value |\nLow Arousa','Arousal |\nHigh Value','Arousal |\nLow Value')
  } else { levels(Data_by_sub$TrialType2)=c("") }
  #levels(Data_by_sub$Measurement) = c("Predominance Score", "Average Dominance Duration", "Percepts After Fusion", "Initial Percept")
  levels(Data_by_sub$Measurement) = c("Predominance Score", "Dominance Duration", "Percepts After Fusion", "Initial Percept")
  
  Data_by_sub$y_min = NA
  Data_by_sub$y_max = NA
  
  # Data frame containing asterisks representing statistical significance
  levels(SummaryTable$Measurement) = levels(Data_by_sub$Measurement)[order(unique(SummaryTable$Measurement))]
  SummaryTable$TrialType2 = as.factor(SummaryTable$TrialType)
  levels(SummaryTable$TrialType2) = levels(Data_by_sub$TrialType2)[order(unique(SummaryTable$TrialType2))]
  SummaryTable$yloc = NA
  Measurements = levels(Data_by_sub$Measurement)
  for (Meas in Measurements) {
    y_max = max(abs(Data_by_sub$value[Data_by_sub$Measurement == Meas])) *1.2
    y_min = y_max*(-1)
    Data_by_sub$y_min[Data_by_sub$Measurement == Meas] = y_min
    Data_by_sub$y_max[Data_by_sub$Measurement == Meas] = y_max
    SummaryTable$yloc [SummaryTable$Measurement == Meas] = y_max * (1.1/1.2)
  }
  SummaryTable$non_significant = paste0 ("p = ",round(SummaryTable$p,digits = 2))
  SummaryTable$non_significant[SummaryTable$p < .05] = ""
  SummaryTable$asterisk = ""
  SummaryTable$asterisk[SummaryTable$p < .05] = "*"
  SummaryTable$asterisk[SummaryTable$p < .01] = "**"
  SummaryTable$asterisk[SummaryTable$p < .001] = "***"
  
  if (experiment_num<3) {
    y_lab = "High-Value Dominance"
    x_lab = ""
  } else if (experiment_num==3) {
    y_lab = "High-Value / High-Arousal Dominance"
    x_lab = "Manipulated Feature"
  }
  
  num_rows = ceiling(length(unique(Data_by_sub$TrialType2))/2)
  violin_plot = ggplot(Data_by_sub, aes(x=TrialType2, y=value, fill = TrialType2)) + 
    facet_wrap(~Measurement, scales = 'free', nrow = num_rows ) +
    geom_violin(trim=TRUE)+
    # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
    geom_boxplot(width=0.2, notch=TRUE, varwidth = TRUE, fill='white') +
    #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
    geom_hline(yintercept=0, linetype="dashed") +
    theme_bw() +
    labs(fill='Condition') +
    geom_blank(aes(y=y_min)) + # will set 0 at the middle with facet_wrap free scale
    geom_blank(aes(y=y_max)) + # will set 0 at the middle with facet_wrap free scale
    theme(aspect.ratio=num_rows*0.2 , axis.title.x=element_blank(), axis.title.y=element_blank()) +
    geom_text(data = SummaryTable, aes(y = yloc, label = asterisk), position = position_dodge(width = .75)) +
    ggtitle(gsub("_"," ",experiment_name))
  # dev.new()
  # print(violin_plot)
  
  box_plot = ggplot(Data_by_sub, aes(x=TrialType2, y=value, fill = TrialType2)) + 
    facet_wrap(~Measurement, scales = 'free', nrow = num_rows ) +
    # geom_violin(trim=FALSE)+
    # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
    geom_boxplot(notch=TRUE, position=position_dodge(.2), outlier.color = "gray30", outlier.size = 1) +
    geom_hline(yintercept=0, linetype="dashed") +
    theme_bw() +
    geom_blank(aes(y=y_min)) + # will set 0 at the middle with facet_wrap free scale
    geom_blank(aes(y=y_max)) + # will set 0 at the middle with facet_wrap free scale
    theme(aspect.ratio=2/num_rows, legend.position="none") + 
    geom_text(data = SummaryTable, aes(y = yloc, label = asterisk),size = 10) +
    geom_text(data = SummaryTable, aes(y = yloc*1.1, label = non_significant, fontface=3),size = 3) + 
    labs( x = x_lab, y = y_lab) +
    # theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + # remove x and y labs
    scale_fill_grey(start = 0.9, end = .5)
    ggtitle(experiment_name_full) + theme(plot.title = element_text(hjust = 0.5))
  
  dev.new()
  print(box_plot)
  # Save plot as pdf
  pdf(file=paste(data_path,'plot_',experiment_name,'.pdf',sep = ""), width=7, height=num_rows*7/2)
  print(box_plot)
  dev.off()
}

# Correlation matrix plot - Experiment 3, Predominance score
PDS_data = Data_by_sub[Data_by_sub$Measurement=="Predominance Score",]
PDS_data = cast(PDS_data,formula = SubjectInd ~ TrialType)
PDS_data = PDS_data[,2:5] # remove subject column
colnames(PDS_data) = levels(Data_by_sub$TrialType2)

source("ggcorplot.R")
dev.new()
ggcorplot(PDS_data)
pdf(file=paste0(data_path,'correaltion_matrix.pdf'), width=8.5,height=8)
ggcorplot(PDS_data)
dev.off()