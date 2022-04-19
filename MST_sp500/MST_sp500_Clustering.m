%% TiME SERIES by Kruskal
clc,clear
load("sp500.mat");
logret=sample(:,4);
price=sample(:,5);
timeline=datenum(sample(:,1:3));
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
%% color
cmap = hsv(20);
cmap([1,2,3,4,18,5,13],:) = cmap([5,18,13,3,4,1,2],:);
COLOR=cmap;
%% plot K=20 clustering png
K=20;
figure;
T_K=T_Edges(1:end-K+1,:);
T_K=sortrows(T_K,"EndNodes");
Cluster=ones(size(T_K.Weight) );
for i=1:length(T_K.Weight)
    Cluster(i)=T_K.EndNodes(i,1)-i;
end
T_K=[T_K table(Cluster)];
for i = 1:length(T_K.Weight)
    plot([P(T_K.EndNodes(i,1),1),P(T_K.EndNodes(i,2),1)]...
        ,[P(T_K.EndNodes(i,1),2),P(T_K.EndNodes(i,2),2)],"Color",COLOR(T_K.Cluster(i)+1,:),"LineWidth",0.5);
    hold on
end
dateaxis('x',12);
yticks([])
xticks([])
xlim([timeline(1),timeline(end)]);
ylim([-0.1,0.15]);
set(gca,'color' ,'none','XColor','none','YColor','none')
set(gcf,'Color','none')
%% save K=20 cluster png
saveas(gcf,'SP500_Clustering_K20','png');
%% plot gif of SP500 Clustering given K=2:20
figure;
ax=gca;
f=gcf;
ax.XColor='none';
ax.YColor='none';
hold(ax,'on')
DelayTime=.8;
plot(timeline,logret,'r');
xlim([timeline(1),timeline(end)]);
pause(DelayTime)
F=getframe(ax);
[imind,cm]=rgb2ind(F.cdata,256);
imwrite(imind,cm,'SP500_Clustering.gif','gif','Loopcount',inf,'DelayTime',DelayTime);
% given K
for K=2:20
    clf(f);
    ax=gca;
    f=gcf;
    ax.XColor='none';
    ax.YColor='none';
    scatter(timeline,logret,0.3)

    T_K=T_Edges(1:end-K+1,:);
    T_K=sortrows(T_K,"EndNodes");
    Cluster=ones(size(T_K.Weight));
    for i=1:length(T_K.Weight)
        Cluster(i)=T_K.EndNodes(i,1)-i;
    end
    T_K=[T_K table(Cluster)];
    for i = 1:length(T_K.Weight)
        plot([P(T_K.EndNodes(i,1),1),P(T_K.EndNodes(i,2),1)]...
            ,[P(T_K.EndNodes(i,1),2),P(T_K.EndNodes(i,2),2)],"Color",COLOR(T_K.Cluster(i)+1,:),"LineWidth",0.5);
        hold on
    end
    dateaxis('x',12);
    yticks([])
    xticks([])
    xlim([timeline(1),timeline(end)]);
    ylim([-0.1,0.15]);
    t=['K=' num2str(K)];
    title(t,Color='red',FontSize=16,position=[mean(P(:,1)),prctile(P(:,2),100)]);
    %     set(ax,'color' ,'none')
    pause(DelayTime);saveFrame(ax,DelayTime);
end


function saveFrame(ax,DelayTime)
F=getframe(ax);
[imind,cm]=rgb2ind(F.cdata,256);
imwrite(imind,cm,'SP500_Clustering.gif','gif','WriteMode','append','DelayTime',DelayTime,"TransparentColor",0);
end