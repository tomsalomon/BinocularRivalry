addpath(genpath('./../'))
images = dir('./S_IAPS*');
%images = dir('/Users/tomsalomon/Drive/Experiment_Israel/Codes/Binocular_Rivalry/EXP/stimuli/pairsTom/*.jpg')
for i = 1:numel(images)
    image = imread([images(i).folder,'/',images(i).name]);
    try
        image_gray{i} = imresize(rgb2gray(image),[500 500]);
        
    catch
        image_gray{i} = image;
    end
end
a= histMatch(image_gray,1);

for i = 1:numel(image_gray)
    try
        figure;
        imshowpair(a{i}, image_gray{i}, 'montage');
        new_name = ['Shined_',images(i).name];
        imwrite(a{i},new_name)
    catch
    end
end

