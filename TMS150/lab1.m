clc;clf;clear
N=100;
X = normrnd(0,1,[1,N]);
hist(X)

X=sort(X);

phat=mle(X);

x=zeros(1,N);
y=zeros(1,N);
for i=1:N
    x(1,i)=(i-0.5)/N;
    y(i,1)=normcdf(X(1,i),phat(1),phat(2));
end

plot(x,y)

hold on;

plot(x,x)

% vi får då att P( D <= 0.136) = 0.95.

D=max(abs(normcdf(X,phat(1),phat(2))-(1:N)./N))
a=0.136

%%
clc;clf;clear
N=100;
X=gammarnd(2,2,[1,N])