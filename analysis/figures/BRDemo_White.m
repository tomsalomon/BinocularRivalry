% This script create a visualization of BR transition 

clear all
close all

% Define These variables:
% which images you want to visualize
im2show_red = './Shined_S_IAPS2.jpg';
im2show_blue = './Shined_S_IAPS1.jpg';
percent_gradient = 1/4; % percent cange between each image

image1 = double(imread(im2show_red))/255;
image2 = double(imread(im2show_blue))/255;

[im_size] = size(image1);
[image_R,image_B] = deal(ones(im_size(1),im_size(2),3));
image_R(:,:,2) = image1;
image_R(:,:,3) = image1;
image_B(:,:,1) = image2;
image_B(:,:,2) = image2;
for Percent_blue = 0:percent_gradient:1
   figure('name',sprintf('%.2f',Percent_blue*100));
   imshow(image_R*(1-Percent_blue) + image_B*(Percent_blue))
end
