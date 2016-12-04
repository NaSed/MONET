function Main(x)

 % NO = 1;
% load(strcat('C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Data/in', num2str(NO), '.mat'));
load(x);
% NO = strrep(strrep(exp, 'C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Data/in', ''), '.mat','');
NO = strrep(strrep(x, 'C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Data/in', ''), '.mat','');

[res totaliter maxiter]= berge(mat);
save(strcat('C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Data/BERGEout', NO, '.mat'), 'res', 'totaliter', 'maxiter')
exit;
end