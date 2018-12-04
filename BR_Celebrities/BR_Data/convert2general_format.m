clear

subs = dir([pwd,'/Sub*']);

for i = 1:numel(subs)
    folder = [pwd,'/',subs(i).name,'/Sub*'];
    name2change = dir(folder);
    new_data_path = [name2change.folder,'/',name2change.name,'/from laptop'];
  
    for j = 1:9
        old_file = dir(sprintf('%s/riv*_%i_*',new_data_path,j));
        try
            new_file = strrep(old_file.name,sprintf('_%i_',j),sprintf('_0%i_',j));
            movefile([new_data_path,'/',old_file.name],[new_data_path,'/',new_file])
        catch
        end
    end
end