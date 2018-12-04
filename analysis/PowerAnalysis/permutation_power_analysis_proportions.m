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
clear;
close all;

BR_data=readtable('data_BR_politicians.txt','delimiter','\t');

time_diff=BR_data.face1Fraction-0.5;
%time_diff(BR_data.face1Time+BR_data.face2Time==0)=nan;
N=length(unique(BR_data.Subject));
% time_diff_mat=mean(reshape(time_diff,[length(time_diff)/N],N));
time_diff_mat=reshape(time_diff,[length(time_diff)/N,N]);
time_diff_mean=mean(time_diff_mat);

direction=sign(mean(time_diff));
visualize=false;
onetailed=false;
n=0;
stop_cond=10; % test stop_cond n's after reaching required power
power_iterations=1000;
max_N=70;
p_values=zeros(power_iterations,max_N);
alpha=0.05;
beta=0.2;
power_target=1-beta;
required_n=[];
while stop_cond>0
    n=n+1;
    for itr=1:power_iterations
        rand_sample=randi(N,[n,1]);
        rand_sample_data=time_diff_mean(:,rand_sample);
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