clear all
close all

image1 = double(imread('./Shined_S_IAPS2.jpg'))/255;
image2 = double(imread('./Shined_S_IAPS1.jpg'))/255;
[im_size] = size(image1);
[image_R,image_B] = deal(zeros(im_size(1),im_size(2),3));
image_R(:,:,1) = image1 * 1.2;
image_B(:,:,3) = image2 * 1.2;

gradient_n = 4;

for Percent_b = 0:1/gradient_n:1

   figure('name',sprintf('%.2f',Percent_b));
   imshow(image_R*(1-Percent_b) + image_B*(Percent_b))
 
end