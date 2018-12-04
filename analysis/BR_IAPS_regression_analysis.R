
rm(list=ls())

library("lme4")
library("lmerTest")
library("Rcmdr")

# Define these variable:
output_path="/Users/tomsalomon/Drive/Experiment_Israel/Codes/Binocular_Rivalry/BR_IAPS/analysis/output"
#output_path="C:/Users/Tom/Dropbox/Experiment_Israel/Codes/Binocular_Rivalry_IAPS/BR_hilla/analysis/output/"
subjects=c(1:3,5:7,9,11,13:19,21:23,25:39,41:42,44:45,47:49,51:52,54:56,58:63,65,67:75,77:81,83,85)

data_file=Sys.glob(paste(output_path, "/BR_data_20180102*.txt",sep=""))
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

BR_data$DiffTime = (BR_data$Stim1Time - BR_data$Stim2Time)
BR_data$DiffFraction = (BR_data$Stim1Fraction - BR_data$Stim2Fraction)
BR_data$DiffQuantity = (BR_data$Stim1Quantity - BR_data$Stim2Quantity)

###################
# DiffFraction
###################
# One Main effect - merged across trial types
summary(lmer(DiffFraction ~ 1 + Delta_Val2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType<=2)),na.action=na.omit))
summary(lmer(DiffFraction ~ 1 + Delta_Aro2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType>=3)),na.action=na.omit))

# One Main effect + interactio is high - merged across trial types
summary(lmer(DiffFraction ~ 1 + Delta_Val2 * TrialType2 + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType<=2)),na.action=na.omit))
summary(lmer(DiffFraction ~ 1 + Delta_Aro2 * TrialType2+ (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType>=3)),na.action=na.omit))

summary(lmer(DiffFraction ~ 1 + Delta_Aro_rating * TrialType2+ (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType>=3)),na.action=na.omit))


# One Main effect
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



summary(lmer(DiffFraction ~ 1 + Delta_Aro_rating * Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType>=1 & abs(BR_data$DiffFraction)<1)),na.action=na.omit))
summary(lmer(DiffTime ~ 1 + Delta_Aro_rating * Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType>=1)),na.action=na.omit))
summary(lmer(DiffQuantity ~ 1 + Delta_Aro_rating * Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType>=1)),na.action=na.omit))
summary(lmer(DiffFraction ~ 1 + Delta_Aro_rating * Delta_Val_rating + (1|SubjectCode),data=subset(BR_data,(BR_data$ValidTrials & BR_data$TrialType>=1)),na.action=na.omit))



BR_data2=subset(BR_data,(BR_data$ValidTrials))

scatter3d(DiffFraction ~ Delta_Aro_rating + Delta_Val_rating , BR_data2, 
          parallel=FALSE, fit="smooth",
          surface.col=list("black","blue") ,surface.alpha = list(0.3),axis.scales=FALSE,
          xlab = c("Preference Bias (rank)"), 
          ylab = c("PHQ (rank)"), 
          zlab = c("CAT effect (rank)"),
          neg.res.col = NA,
          pos.res.col = NA) 

