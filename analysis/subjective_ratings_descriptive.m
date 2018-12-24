% This script reads the the subjective ratings of IAPS stimuli used in
% Experiment 3 and shows their descriptive statistics
% ~~~~~~~~~~~~~~~~~~~~~~~~ Written by Tom Salomon ~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ December, 2018 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% Initialize code - define paths and pre-allocation
clear;
% Define these variables
experiment_num = 3; % Experiment number to be analyzed: 1 - 'BR_Celebrities', 2 - 'BR_Politicians', 3 - 'BR_IAPS'
num_of_stimuli=16; % number of stimuli used in the experiment

experiment_names = {'BR_Celebrities','BR_Politicians','BR_IAPS'}; % One of three options
experiment_name = experiment_names{experiment_num};
analysis_path=pwd;
experiment_path = [analysis_path,'/../',experiment_name];
data_path = [analysis_path,'/processed_data'];
behav_data_folder=([experiment_path,'/Behavioral_Data/']);
data_file_options = dir(sprintf('%s/BR_data_%s*',data_path,experiment_name));
if length(data_file_options)>1
    data_file_selection = listdlg('PromptString','Select a data file:','SelectionMode','single','ListString',{data_file_options.name},'ListSize',[500,400]);
else
    data_file_selection = 1;
end
data_file=data_file_options(data_file_selection).name;

% remove invalid subject
valid_subjects = csvread(sprintf('%s/valid_subjects_%s',data_path,data_file));
invalid_subjects = csvread(sprintf('%s/invalid_subjects_%s',data_path,data_file));
num_of_subjects = length(valid_subjects);

% pre-allocation
Data_mat=nan(num_of_subjects,num_of_stimuli); % mat where numerical results will be written

%% Loading data from behavioral file to MATLAB cell variable
h = waitbar(0,'Reading raw data into MATLAB');
for sub_i= 1:num_of_subjects  % running subject. notice: subject's ID is not subject's index
    subject_code = valid_subjects(sub_i);
    data_file = dir(behav_data_folder,
    
    subject_dir=dir([BR_raw_data_folder,'/',subjects{sub_i},'/*Sub*']); %struct with dated folder name
    subject_with_date = subject_dir.name; %subject name and date for full path
    subject_path= [subject_dir.folder,'/',subject_with_date,'/'];
    [~,~,BR_params_default] = xlsread([subject_path,'from laptop/BR_params_default.xlsx'],'Pairs'); % read excel with info about the stimuli in each trial
    BR_params = cell2table(BR_params_default(2:end,:),'VariableName',BR_params_default(1,:));
    stimuli_pairs{sub_i} = BR_params;
    clear('subject_data');
    try
        load([tmp_data_path,subject_with_date,'.mat']); %try loading 'subject data' if data was already analyzed
        experiment_data(sub_i,:)=subject_data;
    catch % if subject was not previously analyzed, calculate and save for future use
        excel_files = dir([subject_path,'/from laptop/rivalry_pair_*.xlsx']); %lists files in subject's folder
        for excel_Ind=1:numel(excel_files) %going through the excel files (24/48/64...)
            excel_file = [excel_files(excel_Ind).folder,'/',excel_files(excel_Ind).name];
            % experiment_data{sub_i,excel_Ind} = xlsread(excel_file,'Default');
            experiment_data{sub_i,excel_Ind} = table2array(readtable(excel_file));
            experiment_data{sub_i,excel_Ind}(:,3)=[diff(experiment_data{sub_i,excel_Ind}(1:end,1));0]; % duration
            experiment_data{sub_i,excel_Ind}(:,4) = experiment_data{sub_i,excel_Ind}(:,2)==BR_event.stim1_start; % find stim1 perception onset
            experiment_data{sub_i,excel_Ind}(:,5) = experiment_data{sub_i,excel_Ind}(:,2)==BR_event.stim2_start; % find stim2 perception onset
        end
        subject_data=experiment_data(sub_i,:);
        save([tmp_data_path,subject_with_date],'subject_data');
    end %end of try-catch
    waitbar(sub_i/length(subjects),h);
end
close(h);

%% Processing Data
mat_ind=1; %from 1 to length of numel(subjects)*howmanytrials
h = waitbar(0,'Processing Data');
for sub_i= 1:length(subjects)
    % load non-BR task data - skip this phase for subjects or experiments
    % where the data was not collected (no arousal in experiment 1).
    [skip_subjective_arousal,skip_subjective_value]=deal(false);
    arousal_file=dir([behav_data_folder,subjects{sub_i},'_Arousal*.txt']);
    value_file=dir([behav_data_folder,subjects{sub_i},'_ItemRankingResults*.txt']);
    try % try rading the subjective arousal data files
        arousal_data=readtable([behav_data_folder,arousal_file.name],'delimiter','\t','HeaderLines',0,'ReadVariableNames',true);
        arousal_data.stimName=strrep(arousal_data.stimName,'.jpg',''); % remove '.jpg' extension from the stimuli names
    catch
        try % IAPS subjective ratings on a scale
            arousal_file=dir([behav_data_folder,subjects{sub_i},'*Arousal*.txt']);
            arousal_data=readtable([behav_data_folder,arousal_file.name],'delimiter','\t','HeaderLines',0,'ReadVariableNames',true);
            arousal_data.subjectAnswerArousal = arousal_data.Bid;
            arousal_data.stimName = strrep(arousal_data.Name,'.jpg','');
        catch % subject has no subjective arousal datafile
            skip_subjective_arousal=true;
        end
    end
    try % try rading the subjective value data files
        value_data=readtable([behav_data_folder,value_file.name],'delimiter','\t','HeaderLines',0,'ReadVariableNames',true);
        value_data.StimName=strrep(value_data.StimName,'.jpg','');
    catch
        try % IAPS subjective ratings on a scale
            value_file=dir([behav_data_folder,subjects{sub_i},'*Valence*.txt']);
            value_data=readtable([behav_data_folder,value_file.name],'delimiter','\t','HeaderLines',0,'ReadVariableNames',true);
            value_data.Rank = value_data.Bid;
            value_data.StimName = strrep(value_data.Name,'.jpg','');
        catch % subject has no subjective value datafile
            skip_subjective_value=true;
        end
    end
    
    try % objective value - only in the IAPS experiment
        delta_val = stimuli_pairs{sub_i}.Val_A - stimuli_pairs{sub_i}.Val_B;
        delta_aro = stimuli_pairs{sub_i}.Aro_A - stimuli_pairs{sub_i}.Aro_B;
        IsHighVal = mean([stimuli_pairs{sub_i}.Val_A , stimuli_pairs{sub_i}.Val_B],2);
        IsHighAro = mean([stimuli_pairs{sub_i}.Aro_A , stimuli_pairs{sub_i}.Aro_B],2);
        IsHighVal(IsHighVal==0.5) = nan;
        IsHighAro(IsHighAro==0.5) = nan;
    catch
        [delta_val,delta_aro, IsHighVal, IsHighAro] = deal(nan(num_of_stimuli,1));
    end
    
    for trial_i=1:num_of_stimuli % Go through all subject trials and calculate dependent variables
        % mat with trial info: 1 - onset,  2 - event, 3 - duration, 4 - isStim1event, 5 - isStim2event
        trial_data = experiment_data{sub_i,trial_i};
        
        stim1_time = sum(trial_data(trial_data(:,4)==1,3));
        stim2_time = sum(trial_data(trial_data(:,5)==1,3));
        fusion_time = sum(trial_data(trial_data(:,4)==0 & trial_data(:,5)==0,3));
        Stim1Quantity = sum(trial_data(:,4)); % Stim1Quantity
        Stim2Quantity = sum(trial_data(:,5)); % Stim2Quantity
        
        % first perceived stimulus
        first_stim1_loc = min([999,find(trial_data(:,2)==11, 1)]); %"1" indicates finding only the first index. 999 in case not percieved
        first_stim2_loc = min([999,find(trial_data(:,2)==12, 1)]);
        if first_stim1_loc < first_stim2_loc
            initial_stim_is_Stim1 = 1;
        elseif first_stim1_loc > first_stim2_loc
            initial_stim_is_Stim1 = 0;
        else % NaN in case no stim was percieved
            initial_stim_is_Stim1 = nan;
        end
        
        % find what event occured after stim 1 and stim 2 onset.
        events_after_stim1_onset = trial_data( [0;trial_data(1:end-1,4)]==1, 2);
        events_after_stim2_onset = trial_data( [0;trial_data(1:end-1,5)]==1, 2);
        %In all cases the event after stim onset is not its offset or the trial offset, this is a corrupted trial (simoultaneous pressing or unidentified keypress)
        valid_events = [ismember(events_after_stim1_onset,[BR_event.stim1_end,BR_event.trial_end]);...
            ismember(events_after_stim2_onset,[BR_event.stim2_end,BR_event.trial_end]);...
            ismember(trial_data(:,2),struct2array(BR_event))];
        is_corrupted_trial = ~all(valid_events);
        if is_corrupted_trial
            [stim1_time, stim2_time, fusion_time, initial_stim_is_Stim1, Stim1Quantity, Stim2Quantity] = deal(nan);
        end
        
        % Value and Arousal rankings
        stimuli1name = strrep(stimuli_pairs{sub_i}.StimA{trial_i},'.jpg','');
        stimuli2name = strrep(stimuli_pairs{sub_i}.StimB{trial_i},'.jpg','');
        [stimuli1_aro,stimuli2_aro]=deal(nan);
        if ~skip_subjective_arousal
            ind_arousal_1=find(strcmp(arousal_data.stimName,stimuli1name));
            ind_arousal_2=find(strcmp(arousal_data.stimName,stimuli2name));
            stimuli1_aro=arousal_data.subjectAnswerArousal(ind_arousal_1);
            stimuli2_aro=arousal_data.subjectAnswerArousal(ind_arousal_2);
        end
        [stimuli1_subjective_val,stimuli2_subjective_val]=deal(nan);
        if ~skip_subjective_value
            ind_subjective_value_1=find(strcmp(value_data.StimName,stimuli1name));
            ind_subjective_value_2=find(strcmp(value_data.StimName,stimuli2name));
            stimuli1_subjective_val=value_data.Rank(ind_subjective_value_1);
            stimuli2_subjective_val=value_data.Rank(ind_subjective_value_2);
        end
        
        % Add informarion to the summarizing table
        Data_mat(mat_ind,1) = stim1_time; %Stim1Time
        Data_mat(mat_ind,2) = stim2_time; %Stim2Time
        Data_mat(mat_ind,3) = stim1_time/(stim1_time+stim2_time); %Stim1Fraction
        Data_mat(mat_ind,4) = stim2_time/(stim1_time+stim2_time); %Stim2Fraction
        Data_mat(mat_ind,5) = fusion_time;
        Data_mat(mat_ind,6) = Stim1Quantity; % Stim1Quantity
        Data_mat(mat_ind,7) = Stim2Quantity; % Stim2Quantity
        Data_mat(mat_ind,8) = initial_stim_is_Stim1; % InitialStim1 - is stim 1 the initial
        Data_mat(mat_ind,9) = sub_i; % SubjectInd
        Data_mat(mat_ind,10) = str2double(subjects{sub_i}(end-1:end)); % SubjectCode
        Data_mat(mat_ind,11) = trial_i; %need to change to 64
        Data_mat(mat_ind,12) = delta_val(trial_i); % Delta_Val
        Data_mat(mat_ind,13) = delta_aro(trial_i); % Delta_Aro
        Data_mat(mat_ind,14) = IsHighVal(trial_i); % IsHighVal - in case that deltaval=0
        Data_mat(mat_ind,15) = IsHighAro(trial_i); % IsHighAro - in case that deltaaro=0
        Data_mat(mat_ind,16) = stimuli1_subjective_val; % Val1_Rankin
        Data_mat(mat_ind,17) = stimuli1_aro; % Aro1_Ranking
        Data_mat(mat_ind,18) = stimuli2_subjective_val; % Val2_Ranking
        Data_mat(mat_ind,19) = stimuli2_aro; % Aro2_Ranking
        Data_mat(mat_ind,20) = is_corrupted_trial; % IsCorrupted
        names1{mat_ind} = stimuli1name;
        names2{mat_ind} = stimuli2name;
        waitbar(mat_ind/(numel(subjects)*num_of_stimuli),h)
        mat_ind=mat_ind+1;
    end
end
close(h)

%% Turn matrix to table and save to output path
Headers_text={'Stim1Time','Stim2Time','Stim1Fraction','Stim2Fraction','Fusion','Stim1Quantity','Stim2Quantity', 'InitialStim1','SubjectInd','SubjectCode', 'Trial', 'Delta_Val', 'Delta_Aro', 'IsHighVal', 'IsHighAro', 'Val1_Ranking', 'Aro1_Ranking', 'Val2_Ranking' ,'Aro2_Ranking', 'IsCorrupted'};
Data_Table=mat2dataset(Data_mat,'VarNames',Headers_text);
% add non-numeric variable into the table
Data_Table.Stim1Name=names1;
Data_Table.Stim2Name=names2;
Data_Table=dataset2table(Data_Table);
% Save table
c=clock;
time_stamp=sprintf('%i%02.f%02.f_%02.fh_%02.fm',c(1),c(2),c(3),c(4),c(5));
writetable(Data_Table,[output_path,'BR_data_',experiment_name,'_',time_stamp,'.txt'],'Delimiter','\t','WriteRowNames',true);
