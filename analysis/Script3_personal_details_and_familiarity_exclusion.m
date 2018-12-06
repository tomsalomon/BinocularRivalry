% This script reads the raw data from Binocular Rivalry (BR) experiment
% and summerize measurements per each trial. In addition, the script looks
% for subjective measurements of value and arousal and adds them to the
% final outputted table.
%
% Important paths and info:
% Experiment/BR_Data/ - BR data is stored here. Each trial has an xlsx file.
% each participant has a file called 'BR_params_default.xlsx' where the
% data which stimuli appeared in each trial is stored.
% Experiment/Behavioral_Data/ - non BR data is stored here, namely:
% 	subjective value
%       'item ranking results' in Experiments 1-2. Results of Colley ranking algorithm (normal range: 0 - 1).
%       'Valence scale' in Experiment 3. continuous scale from 0 - 10
% 	Subjective Arousal
%       'Arousal' in Experiment 2. discrete scale from 0 - 10
%       'Arousal scale' in Experiment 3. continuous scale from 0 - 10
% output_path - where the processed merged data file will be saved
% experiment_num - the experiment to analyze: 1 - 'BR_Celebrities', 2 - 'BR_Politicians', 3 - 'BR_IAPS'

%% Initialize code - define paths and pre-allocation
clear;

warning('off');
experiment_names = {'BR_Celebrities','BR_Politicians','BR_IAPS'}; % One of three options
familiarity_thresh = [40, 30]; % minimal number of familiar faces to be included in the experiment
analysis_path=pwd;
processed_data_path = [analysis_path,'/processed_data'];
%%
for experiment_num = 1:2
    experiment_name = experiment_names{experiment_num};
    experiment_path = [analysis_path,'/../',experiment_name];
    
    valid_data_path=dir([processed_data_path,'/*',experiment_name,'*_valid*']);
    invalid_data_path=dir([processed_data_path,'/*',experiment_name,'*_invalid*']);
    valid_subject = csvread([valid_data_path.folder,'/',valid_data_path.name]);
    invalid_subject = csvread([invalid_data_path.folder,'/',invalid_data_path.name]);
    
    BR_raw_data_folder= ([experiment_path,'/BR_Data/']);
    behav_data_folder=([experiment_path,'/Behavioral_Data/']);
    
    personal_details_files = dir([behav_data_folder,'*personal*']);
    familiarity_files = dir([behav_data_folder,'*Familiarity*']);
    N = numel(personal_details_files);
    data=[];
    [familiarity, sub_code,is_tested, is_valid] = deal(nan(N,1));
    for sub_i = 1:N
        data = [data;readtable([behav_data_folder,personal_details_files(sub_i).name],'delimiter','\t')];
        familiarity_data = readtable([behav_data_folder,familiarity_files(sub_i).name],'delimiter','\t');
        familiarity(sub_i) = sum(table2array(familiarity_data(:,4))<=2);
        sub_code_str = cell2mat(familiarity_data{1,1});
        sub_code(sub_i)= str2double(sub_code_str(end-2:end));
        is_tested(sub_i) = ismember(sub_code(sub_i),[valid_subject;invalid_subject]);
        is_valid(sub_i) = ismember(sub_code(sub_i),valid_subject);
    end
    data.sub_code = sub_code;
    data.familiar_faces = familiarity;
    data.excluded_for_familiarity = data.familiar_faces<familiarity_thresh(experiment_num);
    data.excluded_for_corruption = is_tested&(~is_valid);
    data.is_valid = is_valid;
    
    fprintf('\n%s\n==================\n',experiment_name)
    fprintf('Valid n = %i; Total participated n = %i\n',sum(data.is_valid),numel(data.is_valid))
    fprintf('Excluded for familiarity n = %i; Excluded for under 75%% valid trials n = %i\n',sum(data.excluded_for_familiarity), sum(data.excluded_for_corruption))
end
warning('on');