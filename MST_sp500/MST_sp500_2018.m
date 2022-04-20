%% load SP500 TiME SERIES in 2008
clc,clear
load("sp500.mat");
logret=sp500.Return(and(sp500.Time>=datenum([2008,01,01]),sp500.Time<=datenum([2008,12,31])),:);
timeline=sp500.Time(and(sp500.Time>=datenum([2008,01,01]),sp500.Time<=datenum([2008,12,31])),:);
price=sp500.IndexPrice(and(sp500.Time>=datenum([2008,01,01]),sp500.Time<=datenum([2008,12,31])),:);
%% generate minimal spanning tree
DM=pdist2(logret,logret);
for i=1:length(DM)-2
    for j=i+2:length(DM)
        DM(i,j)=inf;
    end
end
for j=1:length(DM)-2
    for i=j+2:length(DM)
        DM(i,j)=inf;
    end
end
G=graph(DM);
T=minspantree(G,'Method','sparse');
P=[timeline,logret];
T_Edges=sortrows(T.Edges,"Weight");
%% plot png of SP500 MST
figure;
for i = 1:length(T_Edges.Weight)
    plot([P(T_Edges.EndNodes(i,1),1),P(T_Edges.EndNodes(i,2),1)]...
        ,[P(T_Edges.EndNodes(i,1),2),P(T_Edges.EndNodes(i,2),2)],'r',"LineWidth",0.5);hold on
end
dateaxis('x',12);
xlim([timeline(1),timeline(end)]);
set(gca,'color','none','YColor','none')
%% save png of SP500 MST
saveas(gcf,'MST_sp500_2018','png');
