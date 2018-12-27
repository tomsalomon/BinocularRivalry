
function [p_value,random_distribution_means]=dependent_samples_permutation_mean(data,visualize,onetailed,seed)
% This functio  will perform a permutation test for dependent samples
% means.
% Input may be: 1 vector of the two group differences, or the two groups.

% for replicability
% rng('Shuffle');

if ~exist('data','var')
    error('An input data must be supplied');
end

if ~exist('onesided','var')
    onetailed=false;
end

if ~exist('visualize','var')
    visualize=true;
end

if ~exist('seed','var')
    seed=1;
end

rng(seed) % set randomisation seed for reproducibility
random_sample_n=20000;
% Data is assumed to be organized as Column per subject ans row per trial
[n_trials,N]=size(data);

if n_trials>1
    group_means=nanmean(data);
else
    group_means=data;
end

my_mean=mean(group_means);

direction=sign(my_mean);

% duplicate results, each column is a random sample
random_samples=repmat(group_means',[1,random_sample_n]);

% randomly flip signs
flip=sign(1-2*rand(size(random_samples)));
flip(1:N,1)=1; % first sample is the original sample
random_samples=random_samples.*flip;
random_distribution_means=mean(random_samples,1);

% calculate p-value as number of means more extreme than my_mean
if direction==1
    more_extreme_samples=random_distribution_means>=my_mean;
else
    more_extreme_samples=random_distribution_means<=my_mean;
end

p_value_onesided=sum(more_extreme_samples)/random_sample_n;
if onetailed
    p_value=p_value_onesided;
    p_string = ['p (one-sided): ',num2str(p_value)];
else
    p_value=p_value_onesided*2;
    p_string = ['p (two-sided): ',num2str(p_value)];
end

if visualize
    bin_width=(max(random_distribution_means)-min(random_distribution_means))/100;
    %figure
    histogram(random_distribution_means(:),'BinWidth',bin_width,'EdgeAlpha',0.5,'EdgeColor','none');
    hold on
    histogram(random_distribution_means(~more_extreme_samples),'BinWidth',bin_width,'FaceColor','k','EdgeColor','none');
    plot([random_distribution_means(1),random_distribution_means(1)],ylim,'b--','LineWidth',2)
    ylim(ylim*1.2); % add some space above
    northwest_pos=[min(xlim)+(max(xlim)-min(xlim))*0.05 ,min(ylim)+(max(ylim)-min(ylim))*0.9];
    text(northwest_pos(1),northwest_pos(2),p_string);
    hold off
end

end
