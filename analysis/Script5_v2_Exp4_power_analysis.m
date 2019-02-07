
close all;
clear;

% Define these variables:
experiment_num = 3; % Experiment number to be analyzed: 1 - 'BR_Celebrities', 2 - 'BR_Politicians', 3 - 'BR_IAPS'
measurement2test = 1; % 1 - 'Predominance score',2 - 'Average dominance duration',3 - 'Percepts after fusion', 4 - 'Initial percept'
trialtype2test=4;
stop_cond=5; % number of time sufficient power will be reached before stopping, minimum 1
power_iterations=1000; % number of random permutation to test each sample size.
max_N=100; % Stop at this N the simulation even if power was not reached
alpha=0.05/4;
beta=0.05;
power_target=1-beta;
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
invalid_trials=(BR_data.Stim1Time+BR_data.Stim2Time==0)|BR_data.IsCorrupted;

measurement_type_names={'Predominance score','Average dominance duration','Percepts after fusion','Initial percept'};

Stim1_measurement={BR_data.Stim1Fraction, BR_data.Stim1Time./BR_data.Stim1Quantity, BR_data.Stim1Quantity, BR_data.InitialStim1};
Stim2_measurement={BR_data.Stim2Fraction, BR_data.Stim2Time./BR_data.Stim2Quantity, BR_data.Stim2Quantity, 1-BR_data.InitialStim1};

BR_data.trialtype=ones(size(BR_data(:,1)));
BR_data.trialtype(BR_data.Delta_Aro==0 & BR_data.IsHighAro==1) = 1; % High Arousal - Different Value
BR_data.trialtype(BR_data.Delta_Aro==0 & BR_data.IsHighAro==0) = 2; % Low Arousal - Different Value
BR_data.trialtype(BR_data.Delta_Val==0 & BR_data.IsHighVal==1) = 3; % High Value - Different Arousal
BR_data.trialtype(BR_data.Delta_Val==0 & BR_data.IsHighVal==0) = 4; % Low Value - Different Arousal
if experiment_num==3
    trialtypenames={'High Arousal: Different Value','Low Arousal: Different Value','High Value: Different Arousal','Low Value: Different Arousal'};
else
    trialtypenames={[strrep(experiment_name,'_',' '),': Different Value']};
end
subs = unique(BR_data.SubjectCode);
N=numel(subs);
N_trialtypes = numel(trialtypenames);

differences_mat = nan(N,N_trialtypes);
for trialtype=1:N_trialtypes
    for sub_i=1:N
        ind = BR_data.SubjectCode == subs(sub_i) & BR_data.trialtype == trialtype;
        differences_mat(sub_i,trialtype) = nanmean(BR_data.Stim1Fraction(ind)-0.5);
    end
end

visualize=false;
onetailed=false;
p_values=zeros(power_iterations,max_N,N_trialtypes);
required_n=[];
update_rm_str=sprintf('\nPower analysis Update:\n=====================\n');
n=0;
while (stop_cond > 0) && (n < max_N)
    n=n+1;
    parfor itr=1:power_iterations % parallel loop to go through iterations. Chage to regular "for-loop" if there is a problem with parallel toolbox
        permutation_seed = itr;
        rng(permutation_seed); % for reproducability
        data = differences_mat; % copy variable here to aviod communication overhead warning
        rand_sample=randi(N,[n,1]);
        rand_sample_data=data(rand_sample,:);
        for trialtype=1:N_trialtypes
        p_values(itr,n,trialtype)=sign_flip_permutation_test(rand_sample_data(:,trialtype),visualize,onetailed,permutation_seed);
        end
    end
    % Count significant results proportion
    if mean(p_values(:,n,trialtype2test) < alpha) > power_target
        stop_cond=stop_cond-1;
        if isempty(required_n)
            required_n=n;
        end
    end
    % Prinf the latest n and power
    fprintf(update_rm_str); % remove previous printed results
    update_str = sprintf('n=%i, power=%.2f',n,mean(p_values(:,n,trialtype2test)<alpha));
    fprintf(update_str); % print updated results
    pause(0.1)
    update_rm_str = repmat('\b',1,numel(update_str));
end
fprintf('\n')
p_values(:,n+1:max_N,:)=[];

figure
hold on
for trialtype = 1:N_trialtypes
plot(mean(p_values(:,:,trialtype)<alpha));
end
legend(trialtypenames,'location','southeast')
xlim([0,n]);
ylim([0,1]);
set(gca,'XTick', 0:2:(floor(n/2)*2));
xlabel('Sample size (N)')
ylabel(sprintf('Power (1 - \\beta)'))
plot(xlim,[power_target,power_target],'b--');
plot([required_n,required_n],ylim,'r--');
hold off
fprintf('\nRequired n=%i, power=%.2f\n',required_n,mean(p_values(:,required_n)<alpha))
