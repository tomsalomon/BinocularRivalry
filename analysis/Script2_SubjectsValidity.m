% ~~~~~~~~~~~~~~~~~~~~~~~~ Written by Tom Salomon ~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ December, 2018 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

function [valid_subjects,invalid_subjects]=Script2_SubjectsValidity(data,visualize)

% Define these variables
analysis_path = pwd;
data_path = [analysis_path,'/processed_data'];
MinimalProp = .75; % minimal valid trials required per condition
num_ok_corrupted_conditions = 0; % how many conditions can be corrupted befor a subject is excluded

% test if an input data was given. If not, prompt user to choose one
if ~exist('data','var')
    data_file_options = dir([data_path,'/BR_data*']);
    if length(data_file_options)>1
        data_file_selection = listdlg('PromptString','Select a data file:','SelectionMode','single','ListString',{data_file_options.name},'ListSize',[500,400]);
    else
        data_file_selection = 1;
    end
    data_file=data_file_options(data_file_selection).name;
    data=readtable([data_path,'/',data_file]);
end
% optional input - visualize participants distribution
if ~exist('visualize','var')
    visualize = 1;
end

data.AllFusion=data.Stim1Time+data.Stim2Time==0; % Exclude all fusion trials
data.ValidTrial = ~(data.IsCorrupted|data.AllFusion);
% Define trial type for experiment 3 with IAPS
data.trialtype=ones(size(data(:,1))); % assign trial 1 for Exp. 1-2
data.trialtype(data.Delta_Aro==0 & data.IsHighAro==1) = 1; % High Arousal - Different Valence
data.trialtype(data.Delta_Aro==0 & data.IsHighAro==0) = 2; % Low Arousal - Different Valence
data.trialtype(data.Delta_Val==0 & data.IsHighVal==1) = 3; % High Valence - Different Arousal
data.trialtype(data.Delta_Val==0 & data.IsHighVal==0) = 4; % Low Valence - Different Arousal
num_trialtypes = max(data.trialtype);

summary_valid=zeros(max(data.SubjectInd),max(data.trialtype)+1);
for sub_ind=1:max(data.SubjectInd)
    summary_valid(sub_ind,1)=mean(data.SubjectCode(data.SubjectInd==sub_ind));
    for trialtype=1:num_trialtypes
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
num_trials_per_condition = sum(data.SubjectInd==sub_ind & data.trialtype==trialtype);

summary_valid(:,6)=(sum(summary_valid(:,2:num_trialtypes+1)<MinimalProp,2))<=num_ok_corrupted_conditions; % See if at least one condition has less than the minimal proportion of valid trials
fprintf('\n%i out of %i Subjects should be excluded\n%i valid subjects left\n',sum(summary_valid(:,end)==0),length(summary_valid(:,1)),sum(summary_valid(:,end)));
if visualize
    figure
    summary_valid(:,7)=min(summary_valid(:,2:num_trialtypes+1),[],2); % minimal proportion of valid trials per condition
    histogram(summary_valid(summary_valid(:,6)==1,7),num_trials_per_condition,'BinWidth',1/num_trials_per_condition);
    xlim([0,1])
    hold on
    histogram(summary_valid(summary_valid(:,6)==0,7),num_trials_per_condition,'BinWidth',1/num_trials_per_condition);
    hold off
    ylabel('Number of participants')
    xlabel('Minimal number of valid trials per condition')
    legend({'Included Participants','Excluded Participants'},'Location','NorthWest')
end
valid_subjects=summary_valid(summary_valid(:,6)==1,1);
invalid_subjects=summary_valid(summary_valid(:,6)==0,1);
if exist('data_file', 'var')
    output_file_path_valid = ['valid_subjects_',data_file(1:strfind(data_file,'.')-1),'.txt'];
    output_file_path_invalid = ['invalid_subjects_',data_file(1:strfind(data_file,'.')-1),'.txt'];
    csvwrite(fullfile(data_path,output_file_path_valid),valid_subjects)
    csvwrite(fullfile(data_path,output_file_path_invalid),invalid_subjects)
end

end