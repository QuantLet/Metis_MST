%% MST clustering for gif by Kruskal
clc,clear
ax=gca;
ax.DataAspectRatio=[1 1 1];
ax.XLim=[0 1];
ax.YLim=[0 1];
ax.XColor='none';
ax.YColor='none';
hold(ax,'on')
DelayTime=.2;

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
scatter(P1(:,1),P1(:,2),100,'k','LineWidth',2,'MarkerFaceColor','auto');
scatter(P2(:,1),P2(:,2),100,'k','LineWidth',2,'MarkerFaceColor','auto');
scatter(P3(:,1),P3(:,2),100,'k','LineWidth',2,'MarkerFaceColor','auto');
pause(.5)
F=getframe(ax);
[imind,cm]=rgb2ind(F.cdata,256);
imwrite(imind,cm,'Kruskal_Clustering.gif','gif','Loopcount',inf,'DelayTime',DelayTime);

P=[P1;P2;P3];
DM = pdist2(P,P);
G=graph(DM);
T=minspantree(G,'Method','sparse');

% figure
F=getframe(ax);
[imind,cm]=rgb2ind(F.cdata,256);

T1.Edges=sortrows(T.Edges,2);

j=0.98;
for i = 1:floor(length(T1.Edges.Weight)*j)
    plot([P(T1.Edges.EndNodes(i,1),1),P(T1.Edges.EndNodes(i,2),1)]...
        ,[P(T1.Edges.EndNodes(i,1),2),P(T1.Edges.EndNodes(i,2),2)],'r',"LineWidth",3);
    pause(.2);saveFrame(ax,DelayTime)
end
pause(2);saveFrame(ax,2)

[X,Y]=getEllipse([r,p],[1,0;0,0.7],r^2,200);
plot(X,Y,'Color',[250,250,0]./255,'LineWidth',5);
[X,Y]=getEllipse([p,r],[0.8,0;0,0.7],r^2,200);
plot(X,Y,'Color',[0,250,0]./255,'LineWidth',5);
[X,Y]=getEllipse([1-r,1-r],[0.7,0;0,0.8],r^2,200);
plot(X,Y,'Color',[0,0,250]./255,'LineWidth',5);
pause(2);saveFrame(ax,2)
%% save the final figure as a single
% saveas(gcf,'MST_Clustering','png')
%% 椭圆数据计算函数，输入协方差矩阵、中心点、半径生成椭圆数据
function [X,Y]=getEllipse(Mu,Sigma,S,pntNum)
% (X-Mu)*inv(Sigma)*(X-Mu)=S
invSig=inv(Sigma);

[V,D]=eig(invSig);
aa=sqrt(S/D(1));
bb=sqrt(S/D(4));

t=linspace(0,2*pi,pntNum);
XY=V*[aa*cos(t);bb*sin(t)];
X=(XY(1,:)+Mu(1))';
Y=(XY(2,:)+Mu(2))';
end

%% save gif 
function saveFrame(ax,DelayTime)
F=getframe(ax);
[imind,cm]=rgb2ind(F.cdata,256);
imwrite(imind,cm,'kruskal_Clustering.gif','gif','WriteMode','append','DelayTime',DelayTime);
end


