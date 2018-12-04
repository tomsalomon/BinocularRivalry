
rm(list=ls())

library("lme4")
library("lmerTest")
library("ggplot2")

# Define these variable:
output_path="~/Drive/Experiment_Israel/Codes/Binocular_Rivalry/BR_Politicians/analysis/output/"
#output_path="C:/Users/Tom/Dropbox/Experiment_Israel/Codes/Binocular_Rivalry_IAPS/BR_hilla/analysis/output/"
subjects=c(35,39,44,45, 46,49,51,54,56,59,63,64,65,67,70,71,74,76,77,78)

data_file=Sys.glob(paste(output_path, "/BR_data_2018*.txt",sep=""))
BR_data=read.table(data_file,header=T,na.strings=c(999))
BR_data=BR_data[BR_data$SubjectCode %in% subjects,]
BR_data$SubjectCode=as.factor(BR_data$SubjectCode)

BR_data$TrialType[BR_data$Delta_Aro==0 & BR_data$IsHighAro==1] = 1 # High Arousal - Different Valence
BR_data$TrialType[BR_data$Delta_Aro==0 & BR_data$IsHighAro==0] = 2 # Low Arousal - Different Valence
BR_data$TrialType[BR_data$Delta_Val==0 & BR_data$IsHighVal==1] = 3 # High Valence - Different Arousal
BR_data$TrialType[BR_data$Delta_Val==0 & BR_data$IsHighVal==0] = 4 # Low Valence - Different Arousal
BR_data$TrialType2=as.factor(BR_data$TrialType)
levels(BR_data$TrialType2)=c("High Arousal - Different Valence","Low Arousal - Different Valence","High Valence - Different Arousal","Low Valence - Different Arousal")
BR_data$ValidTrials=TRUE
BR_data$ValidTrials[(BR_data$Stim1Time+BR_data$Stim2Time) == 0]= FALSE # All Fusion trials
BR_data$ValidTrials[(BR_data$IsCorrupted) == 1]= FALSE # pressed 2 keys

BR_data$Delta_Val2=BR_data$Delta_Val
BR_data$Delta_Val2[BR_data$Delta_Val2==0]=NA
BR_data$Delta_Val2=as.factor(BR_data$Delta_Val2)

BR_data$Delta_Aro2=BR_data$Delta_Aro
BR_data$Delta_Aro2[BR_data$Delta_Aro2==0]=NA
BR_data$Delta_Aro2=as.factor(BR_data$Delta_Aro2)

BR_data$Delta_Val_rating=BR_data$Val1_Ranking-BR_data$Val2_Ranking
BR_data$Delta_Aro_rating=BR_data$Aro1_Ranking-BR_data$Aro2_Ranking

BR_data$DiffTime = BR_data$Stim1Time - BR_data$Stim2Time
BR_data$DiffFraction = BR_data$Stim1Fraction - BR_data$Stim2Fraction
BR_data$DiffQuantity = BR_data$Stim1Quantity - BR_data$Stim2Quantity



# Delta_Aro_rating percentiles for visualization
percentile_groups=10;
BR_data$Delta_Aro_rating_percentile=(100/percentile_groups)*round(percentile_groups*ecdf(BR_data$Delta_Aro_rating)(BR_data$Delta_Aro_rating))
BR_data$DeltaArousalPercentile_factor=as.factor(BR_data$Delta_Aro_rating_percentile)
  
# IGNOR NA!
BR_data_Arousal_visualize=setNames(as.data.frame(seq(0,100,by=100/percentile_groups)),'DeltaArousalPercentile')
BR_data_Arousal_visualize$DeltaArousalPercentile_factor=as.factor(BR_data_Arousal_visualize$DeltaArousalPercentile)

BR_data_Arousal_visualize$Delta_Aro_rating=tapply(BR_data$Delta_Aro_rating, BR_data$Delta_Aro_rating_percentile,mean)

BR_data_Arousal_visualize$DiffTime=tapply(BR_data$DiffTime, BR_data$Delta_Aro_rating_percentile,mean)
BR_data_Arousal_visualize$DiffFraction=tapply(BR_data$DiffFraction, BR_data$Delta_Aro_rating_percentile,mean)
BR_data_Arousal_visualize$DiffQuantity=tapply(BR_data$DiffQuantity, BR_data$Delta_Aro_rating_percentile,mean)
BR_data_Arousal_visualize$FirstPercept=tapply(BR_data$InitialStim1, BR_data$Delta_Aro_rating_percentile,mean)

BR_data_Arousal_visualize$DiffTime_sd=tapply(BR_data$DiffTime, BR_data$Delta_Aro_rating_percentile,sd)
BR_data_Arousal_visualize$DiffFraction_sd=tapply(BR_data$DiffFraction, BR_data$Delta_Aro_rating_percentile,sd)
BR_data_Arousal_visualize$DiffQuantity_sd=tapply(BR_data$DiffQuantity, BR_data$Delta_Aro_rating_percentile,sd)
BR_data_Arousal_visualize$FirstPercept_sd=tapply(BR_data$InitialStim1, BR_data$Delta_Aro_rating_percentile,sd)


# Predominance score (PDS)
ggplot(data = BR_data_Arousal_visualize, aes(DeltaArousalPercentile_factor,DiffFraction, color=Delta_Aro_rating) ) + 
  geom_point() +    
  #geom_errorbar(aes(ymin=DiffFraction-DiffFraction_sd, ymax=DiffFraction+DiffFraction_sd), width=.1) +
  xlab(expression(paste(Delta," Arousal (percentile)"))) + 
  labs(color=expression(paste(Delta," Arousal (mean)"))) + 
  ylab(expression(paste(Delta," Predominance score (seconds)")))

# Average dominance duration (ADD)
ggplot(data = BR_data_Arousal_visualize, aes(DeltaArousalPercentile_factor,DiffTime, color=Delta_Aro_rating) ) + 
  geom_point() +    
  #geom_errorbar(aes(ymin=DiffTime-DiffTime_sd, ymax=DiffTime+DiffTime_sd), width=.1) +
  xlab(expression(paste(Delta," Arousal (percentile)"))) + 
  labs(color=expression(paste(Delta," Arousal (mean)"))) + 
  ylab(expression(paste(Delta," Average dominance duration  (seconds)")))

# Percepts after fusion (PAF)
ggplot(data = BR_data_Arousal_visualize, aes(DeltaArousalPercentile_factor,DiffQuantity, color=Delta_Aro_rating) ) + 
  geom_point() +    
  #geom_errorbar(aes(ymin=DiffQuantity-DiffQuantity_sd, ymax=DiffQuantity+DiffQuantity_sd), width=.1) +
  xlab(expression(paste(Delta," Arousal (percentile)"))) + 
  labs(color=expression(paste(Delta," Arousal (mean)"))) + 
  ylab(expression(paste(Delta," Percepets after Fusion")))


ggplot(data = BR_data, aes(DeltaArousalPercentile_factor,DiffFraction) ) + geom_boxplot()


###################
# DiffFraction
###################
# One Main effect
summary(lmer(DiffFraction ~ 1 + Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials)),na.action=na.omit))
summary(lmer(DiffTime ~ 1 + Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials)),na.action=na.omit))
summary(lmer(DiffQuantity ~ 1 + Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials)),na.action=na.omit))
summary(glmer(InitialStim1 ~ 1 + Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials)),na.action=na.omit,family=binomial))

BR_data$Delta_Aro_rating_percentile = p


summary(lmer(DiffFraction ~ 1 + Delta_Val2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==1)),na.action=na.omit))
summary(lmer(DiffFraction ~ 1 + Delta_Val2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==2)),na.action=na.omit))
summary(lmer(DiffFraction ~ 1 + Delta_Aro2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==3)),na.action=na.omit))
summary(lmer(DiffFraction ~ 1 + Delta_Aro2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==4)),na.action=na.omit))

# Two Main effects
summary(lmer(DiffFraction ~ 1 + Delta_Val2 + Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==1)),na.action=na.omit))
summary(lmer(DiffFraction ~ 1 + Delta_Val2 + Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==2)),na.action=na.omit))
summary(lmer(DiffFraction ~ 1 + Delta_Aro2 + Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==3)),na.action=na.omit))
summary(lmer(DiffFraction ~ 1 + Delta_Aro2 + Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==4)),na.action=na.omit))

# Two Main effects + interaction
summary(lmer(DiffFraction ~ 1 + Delta_Val2 * Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==1)),na.action=na.omit))
summary(lmer(DiffFraction ~ 1 + Delta_Val2 * Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==2)),na.action=na.omit))
summary(lmer(DiffFraction ~ 1 + Delta_Aro2 * Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==3)),na.action=na.omit))
summary(lmer(DiffFraction ~ 1 + Delta_Aro2 * Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==4)),na.action=na.omit))

summary(lmer(DiffFraction ~ 1 + Delta_Aro2 * Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType>=3)),na.action=na.omit))

###################
# DiffTime
###################
# One Main effect
summary(lmer(DiffTime ~ 1 + Delta_Val2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==1)),na.action=na.omit))
summary(lmer(DiffTime ~ 1 + Delta_Val2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==2)),na.action=na.omit))
summary(lmer(DiffTime ~ 1 + Delta_Aro2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==3)),na.action=na.omit))
summary(lmer(DiffTime ~ 1 + Delta_Aro2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==4)),na.action=na.omit))

# Two Main effects
summary(lmer(DiffTime ~ 1 + Delta_Val2 + Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==1)),na.action=na.omit))
summary(lmer(DiffTime ~ 1 + Delta_Val2 + Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==2)),na.action=na.omit))
summary(lmer(DiffTime ~ 1 + Delta_Aro2 + Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==3)),na.action=na.omit))
summary(lmer(DiffTime ~ 1 + Delta_Aro2 + Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==4)),na.action=na.omit))

# Two Main effects + interaction
summary(lmer(DiffTime ~ 1 + Delta_Val2 * Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==1)),na.action=na.omit))
summary(lmer(DiffTime ~ 1 + Delta_Val2 * Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==2)),na.action=na.omit))
summary(lmer(DiffTime ~ 1 + Delta_Aro2 * Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==3)),na.action=na.omit))
summary(lmer(DiffTime ~ 1 + Delta_Aro2 * Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==4)),na.action=na.omit))

###################
# DiffQuantity
###################
# One Main effect
summary(lmer(DiffQuantity ~ 1 + Delta_Val2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==1)),na.action=na.omit))
summary(lmer(DiffQuantity ~ 1 + Delta_Val2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==2)),na.action=na.omit))
summary(lmer(DiffQuantity ~ 1 + Delta_Aro2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==3)),na.action=na.omit))
summary(lmer(DiffQuantity ~ 1 + Delta_Aro2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==4)),na.action=na.omit))

# Two Main effects
summary(lmer(DiffQuantity ~ 1 + Delta_Val2 + Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==1)),na.action=na.omit))
summary(lmer(DiffQuantity ~ 1 + Delta_Val2 + Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==2)),na.action=na.omit))
summary(lmer(DiffQuantity ~ 1 + Delta_Aro2 + Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==3)),na.action=na.omit))
summary(lmer(DiffQuantity ~ 1 + Delta_Aro2 + Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==4)),na.action=na.omit))

# Two Main effects + interaction
summary(lmer(DiffQuantity ~ 1 + Delta_Val2 * Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==1)),na.action=na.omit))
summary(lmer(DiffQuantity ~ 1 + Delta_Val2 * Delta_Aro_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==2)),na.action=na.omit))
summary(lmer(DiffQuantity ~ 1 + Delta_Aro2 * Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==3)),na.action=na.omit))
summary(lmer(DiffQuantity ~ 1 + Delta_Aro2 * Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType==4)),na.action=na.omit))

summary(lmer(DiffQuantity ~ 1 + Delta_Aro_rating * Delta_Val_rating * TrialType+ (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials)),na.action=na.omit))


