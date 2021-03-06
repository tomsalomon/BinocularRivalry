% This script reads the data of all participants, counts how many were
% excluded according to 2 exclusion criteria:
%   1. In Experiments 1 and 2 subjects underwent scale ranking familiarity
% task where they were asked to indicate how familiar are they with the
% faces of familiar Celebrities (Exp. 1) or Politicians (Exp. 2). Only
% stimuli which were familiar to the participants ( ranked 1 - very
% familiar, or 2 - somwhat familiar) were used. If less than a
% predetermined number of faces was usable, the subject was excluded.
%   2. We excluded participants who  had less than 75% usuable trial in the
% bincoula rivalry task (i.e. - trials without simoultanous pressing or
% and trials where participant percieved at least one of the two stimuli)
%
% ~~~~~~~~~~~~~~~~~~~~~~~~ Written by Tom Salomon ~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ December, 2018 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%
%% Initialize code - define paths and pre-allocation
clear;
clc;
warning('off');
experiment_names = {'BR_Celebrities','BR_Politicians','BR_IAPS'}; % One of three options
familiarity_thresh = [40, 30,inf]; % minimal number of familiar faces to be included in the experiment
analysis_path=pwd;
processed_data_path = [analysis_path,'/processed_data'];
%% Run for each experiment
for experiment_num = 1:3
    experiment_name = experiment_names{experiment_num};
    experiment_path = [analysis_path,'/../',experiment_name];
    % load participants with BR data, use output of script2 - Subjects validity
    valid_data_path=dir([processed_data_path,'/valid_subjects_*',experiment_name,'*.txt']);
    invalid_data_path=dir([processed_data_path,'/invalid_subjects_*',experiment_name,'*.txt']);
    valid_subject = csvread([valid_data_path.folder,'/',valid_data_path.name]);
    invalid_subject = csvread([invalid_data_path.folder,'/',invalid_data_path.name]);
    % Define BR data and behavioral data location
    BR_raw_data_folder= ([experiment_path,'/BR_Data/']);
    behav_data_folder=([experiment_path,'/Behavioral_Data/']);
    % load files
    personal_details_files = dir([behav_data_folder,'*personal*']);
    familiarity_files = dir([behav_data_folder,'*Familiarity*']);
    N = numel(personal_details_files); % number of all participants (including excluded ones)
    data=[];
    if experiment_num ==3
        data = readtable([behav_data_folder,'/BR_personal_details.txt']);
        N = numel(data.age);
    end
    [familiarity, sub_code,is_tested, is_valid] = deal(nan(N,1));    % preallocation
    
    for sub_i = 1:N
        if experiment_num<=2
            data = [data;readtable([behav_data_folder,personal_details_files(sub_i).name],'delimiter','\t')];
            familiarity_data = readtable([behav_data_folder,familiarity_files(sub_i).name],'delimiter','\t');
            familiarity(sub_i) = sum(table2array(familiarity_data(:,4))<=2); % count how many faces were familiar to the subject in a familiarity ranking task
            sub_code_str = cell2mat(familiarity_data{1,1}); % subject code to match with lists of valid and invalid participants
            sub_code(sub_i)= str2double(sub_code_str(end-2:end));
        elseif experiment_num==3
            sub_code(sub_i)=data.subjectID(sub_i);
        end
        
        is_tested(sub_i) = ismember(sub_code(sub_i),[valid_subject;invalid_subject]);
        is_valid(sub_i) = ismember(sub_code(sub_i),valid_subject);
    end
    
    
    data.sub_code = sub_code;
    data.familiar_faces = familiarity;
    data.excluded_for_familiarity = data.familiar_faces<familiarity_thresh(experiment_num);
    data.excluded_for_corruption = is_tested&(~is_valid); % should be the same as 'invalid_subject' created by the previous script
    data.is_valid = is_valid;
    valid_subjects_data = data(data.is_valid==1,:);
    
    fprintf('\n%s\n==================\n',experiment_name)
    fprintf('Valid n = %i; Total participated n = %i\n',sum(data.is_valid),numel(data.is_valid))
    fprintf('Excluded for familiarity n = %i; Excluded for under 75%% valid trials n = %i\n',sum(data.excluded_for_familiarity), sum(data.excluded_for_corruption))
    fprintf('Age: Mean = %.2f, Range = %i - %i\n', nanmean(valid_subjects_data.age),  min(valid_subjects_data.age), max(valid_subjects_data.age))
    fprintf('Gender: n = %i females (%.2f%%)\n', sum(valid_subjects_data.gender_1_female_2_male_ ==1),  100*nanmean(valid_subjects_data.gender_1_female_2_male_ ==1))
    fprintf('Dominant hand: n = %i Right hand (%.2f%%)\n', sum(valid_subjects_data.dominantHand_1_right_2_left_ ==1),  100*nanmean(valid_subjects_data.dominantHand_1_right_2_left_ ==1))
end
warning('on');