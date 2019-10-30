%% all stocks
clc;clf;clear 

S = load('stockdata.tsv');
hold on
time = 1:length(S(:,2));
for i = 1:7
    plot(time,S(:,i+1));
end
xlabel('time');
ylabel('stock price');

legend('AstraZenica','Electroluc','Ericsson','Gambio','Nokia','Swedish Match','Svenska Handelsbanken');



%% stock 1

clc;clf;clear 
S = load('stockdata.tsv');
S1 = S(:,2);
time = 1:length(S1);
subplot(3,2,1);
plot(time,S1);
X1 = logreturn(S1);
subplot(3,2,2)
plot(time,X1);
subplot(3,2,3);
hist(X1,100);
subplot(3,2,4);
qqplot(X1);

[h,p] = chi2gof(X1);
p

subplot(3,2,[5 6]);
autocorr(X1);

mean1 = mean(X1)
stdDev1 = sqrt(var(X1))


%% stock 2

clc;clf;clear 
S = load('stockdata.tsv');
S2 = S(:,3);
time = 1:length(S2);
subplot(3,2,1);
plot(time,S2);
X2 = logreturn(S2);
subplot(3,2,2)
plot(time,X2);
subplot(3,2,3);
hist(X2,100);
subplot(3,2,4);
qqplot(X2);

[h,p] = chi2gof(X2);
p

subplot(3,2,[5 6]);
autocorr(X2);

mean1 = mean(X2)
stdDev1 = sqrt(var(X2))

%% stock 3

clc;clf;clear 
S = load('stockdata.tsv');
S3 = S(:,4);
time = 1:length(S3);
subplot(3,2,1);
plot(time,S3);
X3 = logreturn(S3);
subplot(3,2,2)
plot(time,X3);
subplot(3,2,3);
hist(X3,100);
subplot(3,2,4);
qqplot(X3);

[h,p] = chi2gof(X3);
p

subplot(3,2,[5 6]);
autocorr(X3);

mean1 = mean(X3)
stdDev1 = sqrt(var(X3))



%% stock 4 

clc;clf;clear 
S = load('stockdata.tsv');
S4 = S(:,5);
time = 1:length(S4);
subplot(3,2,1);
plot(time,S4);
X4 = logreturn(S4);
subplot(3,2,2)
plot(time,X4);
subplot(3,2,3);
hist(X4,100);
subplot(3,2,4);
qqplot(X4);

[h,p] = chi2gof(X4);
p

subplot(3,2,[5 6]);
autocorr(X4);

mean1 = mean(X4)
stdDev1 = sqrt(var(X4))
%% stock 5

clc;clf;clear 
S = load('stockdata.tsv');
S5 = S(:,6);
time = 1:length(S5);
subplot(3,2,1);
plot(time,S5);
X5 = logreturn(S5);
subplot(3,2,2)
plot(time,X5);
subplot(3,2,3);
hist(X5,100);
subplot(3,2,4);
qqplot(X5);

[h,p] = chi2gof(X5);
p

subplot(3,2,[5 6]);
autocorr(X5);

mean1 = mean(X5)
stdDev1 = sqrt(var(X5))
%% stock 6

clc;clf;clear 
S = load('stockdata.tsv');
S6 = S(:,7);
time = 1:length(S6);
subplot(3,2,1);
plot(time,S6);
X6 = logreturn(S6);
subplot(3,2,2)
plot(time,X6);
subplot(3,2,3);
hist(X6,100);
subplot(3,2,4);
qqplot(X6);

[h,p] = chi2gof(X6);
p

subplot(3,2,[5 6]);
autocorr(X6);

mean1 = mean(X6)
stdDev1 = sqrt(var(X6))
%% stock 7

clc;clf;clear 
S = load('stockdata.tsv');
S7 = S(:,8);
time = 1:length(S7);
subplot(3,2,1);
plot(time,S7);
X7 = logreturn(S7);
subplot(3,2,2)
plot(time,X7);
subplot(3,2,3);
hist(X7,100);
subplot(3,2,4);
qqplot(X7);

[h,p] = chi2gof(X7);
p

subplot(3,2,[5 6]);
autocorr(X7);

mean1 = mean(X7)
stdDev1 = sqrt(var(X7))
%% correlation 

clc;clf;clear 
S = load('stockdata.tsv');

X=zeros(1006,7);
for i = 1:7
    X(:,i) = logreturn(S(:,i+1));
end

correlation=corrcoef(X)

%% utility function

clc;clf;clear
x=linspace(-0.1,1,1000);
y1=@(x,k) util(x,k);
K=[1 4 7 0.5]
hold on
for k=1:4
    plot(x,y1(x,K(k)))
end
xlabel('x');
ylabel('utility')
legend('k=1','k=4','k=7','k=0.5');
axis([-0.1 1 -1 1])

%% stock 1
format short
clc;clf;clear 

S = load('stockdata.tsv');
S1 = S(:,2);
X1 = logreturn(S1);
mean = mean(X1);
std = sqrt(var(X1));

f = @(x,k) util(x,k).*normpdf(x,mean,std);

k1 = integral(@(x)f(x,1),-0.3,0.3)
k4 = integral(@(x)f(x,4),-0.3,0.3)
k7 = integral(@(x)f(x,7),-0.3,0.3)
k12 = integral(@(x)f(x,0.5),-0.3,0.3)

%% stock 2
format short
clc;clf;clear 

S = load('stockdata.tsv');
S2 = S(:,3);
X2 = logreturn(S2);
mean = mean(X2);
std = sqrt(var(X2));

f = @(x,k) util(x,k).*normpdf(x,mean,std);

k1 = integral(@(x)f(x,1),-0.3,0.3)
k4 = integral(@(x)f(x,4),-0.3,0.3)
k7 = integral(@(x)f(x,7),-0.3,0.3)
k12 = integral(@(x)f(x,0.5),-0.3,0.3)

%% stock 3
format short
clc;clf;clear 

S = load('stockdata.tsv');
S3 = S(:,4);
X3 = logreturn(S3);
mean = mean(X3);
std = sqrt(var(X3));

f = @(x,k) util(x,k).*normpdf(x,mean,std);

k1 = integral(@(x)f(x,1),-0.3,0.3)
k4 = integral(@(x)f(x,4),-0.3,0.3)
k7 = integral(@(x)f(x,7),-0.3,0.3)
k12 = integral(@(x)f(x,0.5),-0.3,0.3)

%% stock 4
format short
clc;clf;clear 

S = load('stockdata.tsv');
S4 = S(:,5);
X4 = logreturn(S4);
mean = mean(X4);
std = sqrt(var(X4));

f = @(x,k) util(x,k).*normpdf(x,mean,std);

k1 = integral(@(x)f(x,1),-0.3,0.3)
k4 = integral(@(x)f(x,4),-0.3,0.3)
k7 = integral(@(x)f(x,7),-0.3,0.3)
k12 = integral(@(x)f(x,0.5),-0.3,0.3)

%% stock 5
format short
clc;clf;clear 

S = load('stockdata.tsv');
S5 = S(:,6);
X5 = logreturn(S5);
mean = mean(X5);
std = sqrt(var(X5));

f = @(x,k) util(x,k).*normpdf(x,mean,std);

k1 = integral(@(x)f(x,1),-0.3,0.3)
k4 = integral(@(x)f(x,4),-0.3,0.3)
k7 = integral(@(x)f(x,7),-0.3,0.3)
k12 = integral(@(x)f(x,0.5),-0.3,0.3)
%% stock 6
format short
clc;clf;clear 

S = load('stockdata.tsv');
S6 = S(:,7);
X6 = logreturn(S6);
mean = mean(X6);
std = sqrt(var(X6));

f = @(x,k) util(x,k).*normpdf(x,mean,std);

k1 = integral(@(x)f(x,1),-0.3,0.3)
k4 = integral(@(x)f(x,4),-0.3,0.3)
k7 = integral(@(x)f(x,7),-0.3,0.3)
k12 = integral(@(x)f(x,0.5),-0.3,0.3)

%% stock 7
format short
clc;clf;clear 

S = load('stockdata.tsv');
S7 = S(:,8);
X7 = logreturn(S7);
mean = mean(X7);
std = sqrt(var(X7));

f = @(x,k) util(x,k).*normpdf(x,mean,std);

k1 = integral(@(x)f(x,1),-0.3,0.3)
k4 = integral(@(x)f(x,4),-0.3,0.3)
k7 = integral(@(x)f(x,7),-0.3,0.3)
k12 = integral(@(x)f(x,0.5),-0.3,0.3)

%% 2 stocks
clc;clf;clear
format short
S = load('stockdata.tsv');
SE = S(:,4);
SG = S(:,5);

XE = logreturn(SE);
XG = logreturn(SG);
mu = [mean(XE),mean(XG)]';

Cov = cov(XE,XG);
U = @(w1,k) 1-exp(-k*(mu'*[w1;1-w1]-(k/2)*[w1;1-w1]'*Cov*[w1;1-w1]));
K = [1 4 7 0.5];

w1 = linspace(0,1,500);
V = zeros(1,500);
for i = 1:500
    V(i) = U(w1(i),1);
end
plot(w1,V);
xlabel('w1');
ylabel('expected utility');

options = optimset('TolFun', 1e-15);

maxW = zeros(1,length(K));
expUtil = zeros(1,length(K));
for k = 1:length(K)
    W = @(w1) -U(w1,K(k));
    maxW(k) = fmincon(W,1/2,[],[],[],[],0,1,[],options);
    expUtil(k) = U(maxW(k),K(k));
end
%% 7 stocks

clc;clf;clear
format short
S = load('stockdata.tsv');
C = zeros(length(S(:,2)),7);
mu = zeros(1,7);
for i = 1:7
    X = logreturn(S(:,i+1));
    mu(i) = mean(X);
    C(:,i) = X;  
end
mu = mu';
Cov = cov(C);

U = @(w,k) 1-exp(-k*(mu'*w-(k/2)*w'*Cov*w));
K = [1 4 7 0.5];

options = optimset('TolFun', 1e-15);
V = zeros(9,4); %% matrix containing all information
lb = zeros(7,1);
ub = ones(7,1);
for k = 1:length(K)
    W = @(w) -U(w,K(k));
    V(1:7,k) = fmincon(W,(1/7)*ones(7,1),[],[],ones(1,7),1,lb,ub,[],options);
    V(8,k) = U(V(1:7,k),K(k));
    V(9,k) = U((1/7)*ones(7,1),K(k));
end

%%













