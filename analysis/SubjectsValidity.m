function [valid_subjects,invalid_subjects]=SubjectsValidity(data)

% Define these variables
analysis_path = pwd;
data_path = [analysis_path,'/output'];
Threshold_upper_Aro = 10;
Threshold_lower_Aro = 0;
Threshold_upper_Val = 10;
Threshold_lower_Val = 0;
MinimalProp = .75; % minimal valid trials required per condition
num_ok_corrupted_conditions = 0; % how many conditions can be corrupted befor a subject is excluded
visualize = 1;

% test if an input data was given. If not, prompt user to choose one
if exist('data','var')==0
    data_file_options = dir([data_path,'/BR_data*']);
    if length(data_file_options)>1
        data_file_selection = listdlg('PromptString','Select a data file:','SelectionMode','single','ListString',{data_file_options.name},'ListSize',[500,400]);
    else
        data_file_selection = 1;
    end
    data_file=data_file_options(data_file_selection).name;
    data=struct2table(tdfread([data_path,'/',data_file],'\t'));
end
data.InvalidRankLeft=zeros(size(data(:,1)));
data.InvalidRankRight=zeros(size(data(:,1)));

% Invalid trials due to inapropriate emotion ranking:
data.InvalidRankLeft(data.Delta_Aro==1 & data.Aro1_Ranking<Threshold_lower_Aro)=1;
data.InvalidRankLeft(data.Delta_Aro==-1 & data.Aro1_Ranking>Threshold_upper_Aro)=1;
data.InvalidRankLeft(data.Delta_Aro==0 & data.IsHighAro==0 & data.Aro1_Ranking>Threshold_upper_Aro)=1;
data.InvalidRankLeft(data.Delta_Aro==0 & data.IsHighAro==1 & data.Aro1_Ranking<Threshold_lower_Aro)=1;

data.InvalidRankRight(data.Delta_Aro==1 & data.Aro2_Ranking>Threshold_upper_Aro)=1;
data.InvalidRankRight(data.Delta_Aro==-1 & data.Aro2_Ranking<Threshold_lower_Aro)=1;
data.InvalidRankRight(data.Delta_Aro==0 & data.IsHighAro==0 & data.Aro2_Ranking>Threshold_upper_Aro)=1;
data.InvalidRankRight(data.Delta_Aro==0 & data.IsHighAro==1 & data.Aro2_Ranking<Threshold_lower_Aro)=1;

data.InvalidRankLeft(data.Delta_Val==1 & data.Val1_Ranking<Threshold_lower_Val)=1;
data.InvalidRankLeft(data.Delta_Val==-1 & data.Val1_Ranking>Threshold_upper_Val)=1;
data.InvalidRankLeft(data.Delta_Val==0 & data.IsHighVal==0 & data.Val1_Ranking>Threshold_upper_Val)=1;
data.InvalidRankLeft(data.Delta_Val==0 & data.IsHighVal==1 & data.Val1_Ranking<Threshold_lower_Val)=1;

data.InvalidRankRight(data.Delta_Val==1 & data.Val2_Ranking>Threshold_upper_Val)=1;
data.InvalidRankRight(data.Delta_Val==-1 & data.Val2_Ranking<Threshold_lower_Val)=1;
data.InvalidRankRight(data.Delta_Val==0 & data.IsHighVal==0 & data.Val2_Ranking>Threshold_upper_Val)=1;
data.InvalidRankRight(data.Delta_Val==0 & data.IsHighVal==1 & data.Val2_Ranking<Threshold_lower_Val)=1;

data.ValidTrial=ones(size(data(:,1)));
data.AllFusion=data.Stim1Time+data.Stim2Time==0;
% Don't exclude all fusion trials
% data.ValidTrial(data.IsCorrupted|data.InvalidRankLeft|data.InvalidRankRight)=0;
% Exclude all fusion trials
data.ValidTrial(data.IsCorrupted|data.AllFusion|data.InvalidRankLeft|data.InvalidRankRight)=0;

data.trialtype=zeros(size(data(:,1)));
data.trialtype(data.Delta_Aro==0 & data.IsHighAro==1) = 1; % High Arousal - Different Valence
data.trialtype(data.Delta_Aro==0 & data.IsHighAro==0) = 2; % Low Arousal - Different Valence
data.trialtype(data.Delta_Val==0 & data.IsHighVal==1) = 3; % High Valence - Different Arousal
data.trialtype(data.Delta_Val==0 & data.IsHighVal==0) = 4; % Low Valence - Different Arousal

summary_valid=zeros(max(data.SubjectInd),max(data.trialtype)+1);
for sub_ind=1:max(data.SubjectInd)
    summary_valid(sub_ind,1)=mean(data.SubjectCode(data.SubjectInd==sub_ind));
    for trialtype=1:max(data.trialtype)
        summary_valid(sub_ind,trialtype+1)=mean(data.ValidTrial(data.SubjectInd==sub_ind & data.trialtype==trialtype));
    end
end

summary_corrupted=zeros(max(data.SubjectInd),max(data.trialtype)+1);
for sub_ind=1:max(data.SubjectInd)
    summary_corrupted(sub_ind,1)=mean(data.SubjectCode(data.SubjectInd==sub_ind));
    for trialtype=1:max(data.trialtype)
        summary_corrupted(sub_ind,trialtype+1)=mean(data.IsCorrupted(data.SubjectInd==sub_ind & data.trialtype==trialtype));
    end
end

% summary_bad_ranking=zeros(max(data.SubjectInd),max(data.trialtype)+1);
% summary_bad_ranking(:,1)=summary_valid(:,1);
% summary_bad_ranking(:,2:end)=1-summary_valid(:,2:end)-summary_corrupted(:,2:end);

summary_valid(:,6)=(sum(summary_valid(:,2:5)<MinimalProp,2))<=num_ok_corrupted_conditions; % See if at least on condition has less tham the minimal proportion of valid trials
if sum(summary_valid(:,6))/length(summary_valid(:,1))>0.7
    smiley=':)';
else
    smiley=':(';
end

fprintf('\n%i out of %i Subjects should be excluded %s\n%i valid subjects left\n',sum(summary_valid(:,end)==0),length(summary_valid(:,1)),smiley,sum(summary_valid(:,end)));
if visualize
    figure
    summary_valid(:,7)=min(summary_valid(:,2:5),[],2); % minimal proportion of valid trials per condition
    histogram(summary_valid(summary_valid(:,6)==1,7)*16);
    hold on
    histogram(summary_valid(summary_valid(:,6)==0,7)*16);
    hold off
    ylabel('Number of participants')
    xlabel('Minimal number of valid trials per condition')
    xlim([-1,17])
    legend({'Included Participants','Excluded Participants'},'Location','NorthWest')
end
valid_subjects=summary_valid(summary_valid(:,6)==1,1);
invalid_subjects=summary_valid(summary_valid(:,6)==0,1);