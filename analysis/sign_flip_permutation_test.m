
function [p_value,random_distribution_means]=sign_flip_permutation_test(data, visualize, onetailed, seed, permutations_n)
% This function  will perform a permutation test for one sample mean via
% sign flipping. In case of dependent sample test, give as input (1 X N)
% difference vector or (trial_n X N) matrix where each row is a trial, and
% each column is a subject.
% The function will average results per subject (columns), and perform
% random sign flipping for the subjects' values.
%
% Additional optional inputs
% visualize (default = True) - plot hitogram of permutation results
% onetailed (default = True) - two-sided hypothesis (multiply p-value by 2)
% seed (default = 1) - seed for reproducibility
% permutations_n (default = 20,000) - number of random permutations

if ~exist('data','var')
    error('An input data must be supplied');
end

if ~exist('onetailed','var')
    onetailed=false;
end

if ~exist('visualize','var')
    visualize=true;
end

if ~exist('seed','var')
    seed=1;
end

if ~exist('permutations_n','var')
permutations_n=20000;
end

rng(seed) % set randomisation seed for reproducibility
% Data is assumed to be organized as Column per subject ans row per trial
[n_trials,N]=size(data);
if N==1 % reorient vectors
    if n_trials >1
    data=data';
    [n_trials,N]=size(data);
    end
end
if n_trials>1
    group_means=nanmean(data);
else
    group_means=data;
end

my_mean=mean(group_means);

direction=sign(my_mean);

% duplicate results, each column is a random sample
random_samples=repmat(group_means',[1,permutations_n]);

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

p_value_onesided=sum(more_extreme_samples)/permutations_n;
if onetailed
    p_value=p_value_onesided;
    p_string = ['p (one-sided): ',num2str(p_value)];
else
    p_value=p_value_onesided*2;
    p_string = ['p (two-sided): ',num2str(p_value)];
end

if visualize
    n_bins = 100;
    bin_width=(max(random_distribution_means)-min(random_distribution_means))/n_bins;
    histogram(random_distribution_means(:),'BinWidth',bin_width,'EdgeAlpha',0.5,'EdgeColor','none');
    hold on
    histogram(random_distribution_means(~more_extreme_samples),'BinWidth',bin_width,'FaceColor','k','EdgeColor','none');
    plot([random_distribution_means(1),random_distribution_means(1)],ylim,'b--','LineWidth',2) % original effect
    ylim(ylim*1.2); % add some space above
    northwest_pos=[min(xlim)+(max(xlim)-min(xlim))*0.05 ,min(ylim)+(max(ylim)-min(ylim))*0.9];
    text(northwest_pos(1),northwest_pos(2),p_string);
    hold off
end

end
