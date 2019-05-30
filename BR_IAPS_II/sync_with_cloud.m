function sync_with_cloud(subjectID,export)
% this function copy data from the local experiment path to the cloud
% directory (export = 1, default) or vice versa (import)

% define these paths

% export path - where data is saved in the experiment computer
export_path_behav = './Output/';
export_path_BR = './logs/';
% cloud path - where data is copied to/from on the cloud
cloud_path = '~/Dropbox/experimentsOutput/TomSalomon/BR_IAPS';
cloud_path_behav = [cloud_path,'/Behavioral_Data/'];
cloud_path_BR = [cloud_path,'/BR_Data/'];
% import path - where data is copied to in the analysis computer
import_path = '.';
import_path_behav = [import_path,'/Behavioral_Data/'];
import_path_BR = [import_path,'/BR_Data/'];


if ~exist('export','var')
    export = 2;
end

if ~exist('subjectID','var')
    subjectID = 'Sub';
end

if isnumeric(subjectID)
    subjectID = sprintf('Sub_%03i',subjectID);
end

switch export
    case 1 % export from experimental computer to cloud
        origin_dir_behave = export_path_behav;
        origin_dir_BR = export_path_BR;
        target_dir_behave = cloud_path_behav;
        target_dir_BR = cloud_path_BR;
    case 2 % import from cloud to analysis computer
        origin_dir_behave = cloud_path_behav;
        origin_dir_BR = cloud_path_BR;
        target_dir_behave = import_path_behav;
        target_dir_BR = import_path_BR;
end

sub_data_behave = dir([origin_dir_behave,'/*',subjectID,'*']);
sub_data_BR = dir([origin_dir_BR,'/*',subjectID,'*']);

for file_i = 1:numel(sub_data_behave)
    file_name = sub_data_behave(file_i).name;
    copyfile([origin_dir_behave,'/',file_name],[target_dir_behave,'/',file_name]);
end

for file_i = 1:numel(sub_data_BR)
    file_name = sub_data_BR(file_i).name;
    copyfile([origin_dir_BR,'/',file_name],[target_dir_BR,'/',file_name]);
end