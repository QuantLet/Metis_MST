%% MST clustering for figure
clc,clear

% =========================================================================

rng default
r=1/(2+2*cos(pi/12));
p=1-r-2*r*sin(pi/12);
P11 = rand([6,2]).*[r*1/4 1/20]*[1 2.8;0 1]+[1/3*r p-0.4*r];
P12 = rand([6,2]).*[r*1/4 1/20]*[1 -2.8;0 1]+[2/3*r p+0.4*r];
P13 = rand([6,2]).*[r*1/4 1/20]*[1 2.8;0 1]+[r p-0.4*r];
P14 = rand([6,2]).*[r*1/4 1/20]*[1 -2.8;0 1]+[4/3*r p+0.4*r];
P1=[P11;P12;P13;P14];
P21 = rand([10,2]).*[r*4/3 1/20]+[p-2/3*r 4/3*r];
P22 = rand([10,2]).*[1/20 r*4/3]+[p r/6];
P2=[P21;P22];
P31 = rand([10,2]).*[1/30 5/4*pi]+[r*1/3 pi*1/4];P31 = [cos(P31(:,2)) sin(P31(:,2))].*P31(:,1)+[1-r 1-r+1/3*r];
P32 = rand([10,2]).*[1/30 5/4*pi]+[r*1/3 pi*5/4];P32 = [cos(P32(:,2)) sin(P32(:,2))].*P32(:,1)+[1-r 1-r-1/3*r];
P3=[P31;P32];

P=[P1;P2;P3];
DM = pdist2(P,P);
G=graph(DM);
T=minspantree(G,'Method','sparse');

% Get the distance matrix from the MST
sparseDM = full(adjacency(T, 'weighted')); % Distance matrix from MST
sparseDM(sparseDM == 0) = Inf; % Replace zero values with Inf to avoid confusion with actual zero distances

% Extract the upper triangular part of both distance matrices
upperTriangularIndices = triu(true(size(DM)), 1); % Get logical indices for upper triangular part excluding diagonal
originalDistances = DM(upperTriangularIndices); % Extract the upper triangular elements
mstDistances = sparseDM(upperTriangularIndices); % Extract the upper triangular elements


% Create a figure to compare the distance matrices
figure;
subplot(1, 2, 1);
boxplot(originalDistances);
title('Box Plot of Original Distances');
xlabel('Original Distances');
ylabel('Edge Length');
ylim([-0.01,0.84])
set(gca, 'XTickLabel', []) % Remove x-ticker "1"
set(gca, 'FontSize', 15); % Set the font size for the axis

subplot(1, 2, 2);
boxplot(mstDistances(isfinite(mstDistances))); % Only consider finite values (distances) in the MST
title('Box Plot of MST Distances');
xlabel('MST Distances');
ylabel('Edge Length');
ylim([-0.01,0.84])
set(gca, 'XTickLabel', []) % Remove x-ticker "1"
set(gca, 'FontSize', 15); % Set the font size for the axis

saveas(gcf,'MST_boxplot','png')