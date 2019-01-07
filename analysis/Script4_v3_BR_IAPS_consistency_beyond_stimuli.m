% Test the effect is consistent beyond any specific stimulus. The code runs
% the general analysis of Experiment 3, each time without all trials
% containing a specific stimulus.
% ~~~~~~~~~~~~~ Based on Script4_BR_IAPS_permutation_analysis ~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~ Written by Tom Salomon ~~~~~~~~~~~~~~~~~~~~~~~~
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ January, 2019 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

close all;
clear;
rng(1); % for reproducability

% Define these variables:
experiment_num = 3; % Experiment number to be analyzed: 1 - 'BR_Celebrities', 2 - 'BR_Politicians', 3 - 'BR_IAPS'
analysis_path = pwd;
data_path = [analysis_path,'/processed_data'];
experiment_names = {'BR_Celebrities','BR_Politicians','BR_IAPS'}; % One of three options

experiment_name = experiment_names{experiment_num};
data_file_options = dir(sprintf('%s/BR_data_%s*',data_path,experiment_name));
if length(data_file_options)>1
    data_file_selection = listdlg('PromptString','Select a data file:','SelectionMode','single','ListString',{data_file_options.name},'ListSize',[500,400]);
else
    data_file_selection = 1;
end
data_file=data_file_options(data_file_selection).name;
BR_data=struct2table(tdfread([data_path,'/',data_file],'\t'));

% remove invalid subject
valid_subjects = csvread(sprintf('%s/valid_subjects_%s',data_path,data_file));
invalid_subjects = csvread(sprintf('%s/invalid_subjects_%s',data_path,data_file));
BR_data(ismember(BR_data.SubjectCode,invalid_subjects),:)=[];
% define invalid trials: corrupted trial where participant pressed 2 keys simoultaneously or trials where participant did not percieve any stim.
names = (table2cell(BR_data));
names = names(:,end-1:end);
names_unique = unique(strrep(names(:),' ',''));
for name2test_i = 1:numel(names_unique)
    name2test = names_unique{name2test_i};
    invalid_trials=(BR_data.Stim1Time+BR_data.Stim2Time==0)|BR_data.IsCorrupted;
    invalid_trials = invalid_trials | (contains( names(:,1), name2test)) | (contains( names(:,2), name2test));
    % Define trial types (relevant for IAPS experiment only)
    BR_data.trialtype=ones(size(BR_data(:,1)));
    BR_data.trialtype(BR_data.Delta_Aro==0 & BR_data.IsHighAro==1) = 1; % High Arousal - Different Value
    BR_data.trialtype(BR_data.Delta_Aro==0 & BR_data.IsHighAro==0) = 2; % Low Arousal - Different Value
    BR_data.trialtype(BR_data.Delta_Val==0 & BR_data.IsHighVal==1) = 3; % High Value - Different Arousal
    BR_data.trialtype(BR_data.Delta_Val==0 & BR_data.IsHighVal==0) = 4; % Low Value - Different Arousal
    if experiment_num==3
        trialtypenames={'High Arousal: Different Value','Low Arousal: Different Value','High Value: Different Arousal','Low Value: Different Arousal'};
    else
        trialtypenames={[experiment_name,': Different Value']};
    end
    % Measurement to test: 1 - Fraction, 2 - Time, 3 - Quantity, 4 - First stim (under development)
    %measurement_type_names={'Predominance score','Average dominance duration','Percepts after fusion','Initial percept'};
    measurement_type_names={'Predominance score'};
    differences={BR_data.Stim1Fraction-0.5,...
        BR_data.Stim1Time-BR_data.Stim2Time,...
        BR_data.Stim1Quantity-BR_data.Stim2Quantity,...
        BR_data.InitialStim1-0.5}; % under H0, all diff should be 0 mean
    Stim1_measurement={BR_data.Stim1Fraction, BR_data.Stim1Time, BR_data.Stim1Quantity, BR_data.InitialStim1};
    Stim2_measurement={BR_data.Stim2Fraction, BR_data.Stim2Time, BR_data.Stim2Quantity, 1-BR_data.InitialStim1};
    
    num_measurements=length(measurement_type_names);
    num_TrialTypes=numel(trialtypenames);
    
    % pre allocation
    summary_table_col1=repmat(trialtypenames,num_measurements,1);
    summary_table_col1=summary_table_col1(:);
    summary_table_col2=repmat(measurement_type_names,1,num_TrialTypes);
    summary_table_col2=summary_table_col2(:);
    summary_table_col_nan=nan(size(summary_table_col1));
    summary_table=table(summary_table_col1,summary_table_col2,'variablenames',{'TrialType','Measurement'});
    % summary_table_col_nan,summary_table_col_nan,summary_table_col_nan,summary_table_col_nan);
    [summary_table.Mean_Stim1, summary_table.Mean_Stim2, summary_table.Mean_difference, summary_table.CI_lower, summary_table.CI_upper, ...
        summary_table.p] = deal(summary_table_col_nan);
    [diff_means,Stim1_means,Stim2_means] = deal(cell(num_TrialTypes,num_measurements));
    ind=0;
    figure('Name',['results excluding stimulus: ',name2test],'units','normalized','position',[0.1,0.1,length(trialtypenames)*0.2,0.2]);
    for trialtype=1:length(trialtypenames)
        subplot(1,length(trialtypenames),trialtype)
        
        for measurement_type=1:length(measurement_type_names)
            ind=(trialtype-1)*num_measurements+measurement_type; % table index
            test_diff=differences{measurement_type}; % Select the measurement you want to test: 1,2,3 or 4.
            test_Stim1 = Stim1_measurement{measurement_type};
            test_Stim2 = Stim2_measurement{measurement_type};
            test_diff(invalid_trials)=nan;
            test_Stim1(invalid_trials)=nan;
            test_Stim2(invalid_trials)=nan;
            
            % use the correct variable to indicat if stim1 is high on the varying emotion (1) or stim2 is the higher (-1)
            if experiment_num==3 && trialtype<=2
                high_emotion_stim=BR_data.Delta_Val;
            elseif experiment_num==3 && trialtype>2
                high_emotion_stim=BR_data.Delta_Aro;
            elseif experiment_num~=3
                high_emotion_stim=ones(size(test_diff));
            end
            % in case stim1 was the low value / low arousal, flip between stim1 and stim 2 labels
            if mean(high_emotion_stim(BR_data.trialtype==trialtype)) == -1
                test_diff = -1*test_diff;
                test_Stim1_measurement_tmp = test_Stim1;
                test_Stim1 = test_Stim2;
                test_Stim2 = test_Stim1_measurement_tmp;
            end
            test_diff=test_diff(BR_data.trialtype==trialtype);
            test_Stim1=test_Stim1(BR_data.trialtype==trialtype);
            test_Stim2=test_Stim2(BR_data.trialtype==trialtype);
            
            N=length(unique(BR_data.SubjectCode));
            % time_diff_mat=mean(reshape(time_diff,[length(time_diff)/N],N));
            diff_mat=reshape(test_diff,[length(test_diff)/N,N]);
            Stim1_mat=reshape(test_Stim1,[length(test_diff)/N,N]);
            Stim2_mat=reshape(test_Stim2,[length(test_diff)/N,N]);
            
            diff_means{trialtype,measurement_type}=nanmean(diff_mat);
            Stim1_means{trialtype,measurement_type}=nanmean(Stim1_mat);
            Stim2_means{trialtype,measurement_type}=nanmean(Stim2_mat);
            
            summary_table.p(ind)=sign_flip_permutation_test(diff_means{trialtype,measurement_type},1,0,1,20000);
            
            xlabel(sprintf('%s',trialtypenames{trialtype}))
            pause(0.01);
            summary_table.Mean_difference(ind)=mean(diff_means{trialtype,measurement_type});
            summary_table.Mean_Stim1(ind)=mean(Stim1_means{trialtype,measurement_type});
            summary_table.Mean_Stim2(ind)=mean(Stim2_means{trialtype,measurement_type});
            CI=bootci(20000,@(x)mean(x),diff_means{trialtype,measurement_type});
            summary_table.CI_lower(ind)=CI(1);
            summary_table.CI_upper(ind)=CI(2);
            title(measurement_type_names(measurement_type));
        end
    end
end
