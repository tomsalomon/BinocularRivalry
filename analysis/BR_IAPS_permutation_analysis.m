
close all;
clear;
rng(1); % for reproducability

% Define these variables:
analysis_path = pwd;
data_path = [analysis_path,'/output'];

data_file_options = dir([data_path,'/BR_data*']);
if length(data_file_options)>1
    data_file_selection = listdlg('PromptString','Select a data file:','SelectionMode','single','ListString',{data_file_options.name},'ListSize',[500,400]);
else
    data_file_selection = 1;
end
data_file=data_file_options(data_file_selection).name;
BR_data=struct2table(tdfread([data_path,'/',data_file],'\t'));

% remove invalid subject
[valid_subjects,invalid_subjects]=SubjectsValidity(BR_data);
BR_data(ismember(BR_data.SubjectCode,invalid_subjects),:)=[];

% BR_data.InitialStim1(BR_data.Stim1Quantity>0 & BR_data.Stim2Quantity==0)=1;
invalid_trials=(BR_data.Stim1Time+BR_data.Stim2Time==0)|BR_data.IsCorrupted;

% Define trial types
BR_data.trialtype=ones(size(BR_data(:,1)));
BR_data.trialtype(BR_data.Delta_Aro==0 & BR_data.IsHighAro==1) = 1; % High Arousal - Different Valence
BR_data.trialtype(BR_data.Delta_Aro==0 & BR_data.IsHighAro==0) = 2; % Low Arousal - Different Valence
BR_data.trialtype(BR_data.Delta_Val==0 & BR_data.IsHighVal==1) = 3; % High Valence - Different Arousal
BR_data.trialtype(BR_data.Delta_Val==0 & BR_data.IsHighVal==0) = 4; % Low Valence - Different Arousal
trialtypenames={'High Arousal: Different Valence','Low Arousal: Different Valence','High Valence: Different Arousal','Low Valence: Different Arousal'};
% trialtypenames={'Celebrities by Value'};
% Measurement to test: 1 - Fraction, 2 - Time, 3 - Quantity, 4 - First stim (under development)
measurement_type_names={'Predominance score','Average dominance duration','Percepts after fusion','Initial percept'};
diff={BR_data.Stim1Fraction-BR_data.Stim2Fraction,...
    BR_data.Stim1Time-BR_data.Stim2Time,...
    BR_data.Stim1Quantity-BR_data.Stim2Quantity,...
    BR_data.InitialStim1-0.5};
num_measurements=length(measurement_type_names);
num_TrialTypes=length(trialtypenames);

my_p=nan(length(measurement_type_names),length(num_TrialTypes),1);
my_mean=my_p;

summary_table_col1=repmat(trialtypenames,num_measurements,1);

summary_table_col1=summary_table_col1(:);

summary_table_col2=repmat(measurement_type_names,1,num_TrialTypes);

summary_table_col2=summary_table_col2(:);
summary_table_col_nan=nan(size(summary_table_col1));
summary_table=table(summary_table_col1,summary_table_col2,summary_table_col_nan,summary_table_col_nan,summary_table_col_nan,summary_table_col_nan);
summary_table.Properties.VariableNames = {'TrialType','Measurement','Mean','ci_lower','ci_upper','p'};

ind=0;
for trialtype=1:length(trialtypenames)
    figure('Name',trialtypenames{trialtype},'units','normalized','position',[0.1,0.1,length(measurement_type_names)*0.2,0.2]);
    for measurement_type=1:length(measurement_type_names)
        test_diff=diff{measurement_type}; % Select the measurement you want to test: 1,2,3 or 4.
        test_diff(invalid_trials)=nan;
        ind=ind+1;
        % use the correct variable to indicat if stim1 is high on the varying
        % emotion (1) or stim2 is the higher (-1)
        if trialtype<=2
            high_emotion_stim=BR_data.Delta_Val;
        else
            high_emotion_stim=BR_data.Delta_Aro;
        end
        % high_emotion_stim=ones(size(BR_data.Delta_Val));
        test_data=test_diff(BR_data.trialtype==trialtype).*high_emotion_stim(BR_data.trialtype==trialtype);
        
        N=length(unique(BR_data.SubjectCode));
        % time_diff_mat=mean(reshape(time_diff,[length(time_diff)/N],N));
        diff_mat=reshape(test_data,[length(test_data)/N,N]);
        diff_means{trialtype,measurement_type}=nanmean(diff_mat);
        
        subplot(1,length(measurement_type_names),measurement_type)
        summary_table.Mean(ind)=mean(diff_means{trialtype,measurement_type});
        summary_table.p(ind)=dependent_samples_permutation_mean(diff_means{trialtype,measurement_type});
        ci=bootci(1000,@(x)mean(x),diff_means{trialtype,measurement_type});
        summary_table.ci_lower(ind)=ci(1);
        summary_table.ci_upper(ind)=ci(2);
        title(measurement_type_names(measurement_type));
        %         fprintf('Permutation test results %s: mean = %.2f, p = %.3f\n',trialtypenames{trialtype},my_mean(trialtype),my_p(trialtype));
        
    end
end

% Interaction
figure('Name','Interaction: High vs. Low Valence','units','normalized','position',[0.1,0.1,length(measurement_type_names)*0.2,0.2]);
for measurement_type=1:length(measurement_type_names)
interaction_diff=diff_means{3,measurement_type}-diff_means{4,measurement_type};
subplot(1,length(measurement_type_names),measurement_type);
dependent_samples_permutation_mean(interaction_diff);
title(measurement_type_names(measurement_type));
end

% Main Effects
figure('Name','Main Effect: High vs. Low Valence','units','normalized','position',[0.1,0.1,length(measurement_type_names)*0.2,0.2]);
for measurement_type=1:length(measurement_type_names)
main_effect=(diff_means{3,measurement_type}+diff_means{4,measurement_type})/2;
subplot(1,length(measurement_type_names),measurement_type);
dependent_samples_permutation_mean(main_effect);
title(measurement_type_names(measurement_type));
end

for measurement_type=1:length(measurement_type_names)
        figure('Name',measurement_type_names{measurement_type});
        means=summary_table.Mean(strcmp(summary_table.Measurement,measurement_type_names{measurement_type}));
        ci_lower=summary_table.ci_lower(strcmp(summary_table.Measurement,measurement_type_names{measurement_type}));
        ci_upper=summary_table.ci_upper(strcmp(summary_table.Measurement,measurement_type_names{measurement_type}));
        bar(diag(means),'stacked')
        set(gca,'XtickLabel',strrep(trialtypenames,': ',':\newline'))
        hold on
        errorbar(1:length(trialtypenames),means,means-ci_lower,ci_upper-means,'k*');
        ylim([min(ci_lower),max(ci_upper)]*1.2)
end