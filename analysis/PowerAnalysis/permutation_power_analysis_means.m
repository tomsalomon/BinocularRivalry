% data=[10;10;10;10;30;-30];
% lable1=[1,1,1,0,0,0]
% lable2=1-lable1
% lable1*data(:,1)/sum(lable1)-lable2*data(:,1)/sum(lable2)
%
%
% a=[1:4,11:14,21:24]
% a_2=repmat(a,[1,3])'
% n=4
% b=reshape(a_2,[n,length(a_2)/n])
% c=Shuffle(b)
% d=c(:)
%clear;
close all;
clear;

BR_data=readtable('data_BR_Exp2.txt','delimiter','\t');
PerCorrupted=zeros(max(BR_data.Subject),1);
for sub=1:max(BR_data.Subject)
   PerCorrupted(sub)=mean(BR_data.IsCorrupted(BR_data.Subject==sub));
end
remove_subjects=find(PerCorrupted>=0.5)
BR_data(ismember(BR_data.Subject,remove_subjects),:)=[];

BR_data.Initial_face(BR_data.Quantity_face1>0 & BR_data.Quantity_face2==0)=1;
invalid_trials=(BR_data.face1Time+BR_data.face2Time==0)|BR_data.IsCorrupted;

diff={BR_data.face1Fraction-BR_data.face2Fraction,...
    BR_data.face1Time-BR_data.face2Time,...
    BR_data.Initial_face-0.5,...
    BR_data.Quantity_face1-BR_data.Quantity_face2};

test_diff=diff{4}; % Select the measurement you want to test: 1,2, or 3.
test_diff(invalid_trials)=nan;
N=length(unique(BR_data.Subject));
% time_diff_mat=mean(reshape(time_diff,[length(time_diff)/N],N));
time_diff_mat=reshape(test_diff,[length(test_diff)/N,N]);
time_diff_means=nanmean(time_diff_mat);


my_p=dependent_samples_permutation_mean(time_diff_means);
disp(['Permutation test results: ',num2str(round(my_p,3))])
pause(0.1);
direction=sign(nanmean(test_diff));
visualize=false;
onetailed=false;
n=0;
stop_cond=10; % test stop_cond n's after reaching required power
power_iterations=1000;
max_N=100;
p_values=zeros(power_iterations,max_N);
alpha=0.05;
beta=0.2;
power_target=1-beta;
required_n=[];
while stop_cond>0
    n=n+1;
    for itr=1:power_iterations
        rand_sample=randi(N,[n,1]);
        rand_sample_data=time_diff_means(:,rand_sample);
        p_values(itr,n)=dependent_samples_permutation_mean(rand_sample_data,visualize,onetailed);
    end
    % Count significant results proportion
    if mean(p_values(:,n)<alpha)>power_target
        stop_cond=stop_cond-1;
        if isempty(required_n)
            required_n=n;
        end
    end
    fprintf('n=%i, power=%.2f\n',n,mean(p_values(:,n)<alpha));
end
p_values(:,n+1:max_N)=[];

figure
plot(mean(p_values<alpha),'k');
xlim([0,n]);
ylim([0,1]);
set(gca,'XTick', 0:2:(floor(n/2)*2));
xlabel('Sample size (N)')
ylabel('Power')
hold on
plot(xlim,[power_target,power_target],'b--');
plot([required_n,required_n],ylim,'r--');
hold off
% fprintf('Required n=%i, power=%.2f',n,