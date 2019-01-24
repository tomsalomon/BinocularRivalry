clear all
close all
% define these:
gradient_n = 2;
image1 = double(imread('./Shined_S_IAPS2.jpg'))/255;
image2 = double(imread('./Shined_S_IAPS4.jpg'))/255;

[im_size] = size(image1);
[image_R,image_B] = deal(ones(im_size(1),im_size(2),3));
image_R(:,:,2) = image1;
image_R(:,:,3) = image1;
image_B(:,:,1) = image2;
image_B(:,:,2) = image2;
for Percent_b = 0:1/gradient_n:1
   figure('name',sprintf('%.2f',Percent_b*100));
   imshow(image_R*(1-Percent_b) + image_B*(Percent_b))
end
