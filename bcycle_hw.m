clc
clear all
close all

c = fred
startdate = '01/01/1996';
enddate = '01/01/2022';

%real GDP for Japan and Brazil (each in domestic currencies) 
d1 = fetch(c,'NGDPRSAXDCBRQ',startdate,enddate)   
d2 = fetch(c,'JPNRGDPEXP',startdate,enddate) 

q = d1.Data(:,1);
%時系列

Ybra = d1.Data(:,2);
%GDP of Brazil
Yjpn = d2.Data(:,2);
%GDP of Japan



%[trend, cycle] = hpfilter(log(y), 1600);
[cycleBRA, trendBRA] = qmacro_hpfilter(log(Ybra), 1600);
[cycleJPN, trendJPN] = qmacro_hpfilter(log(Yjpn), 1600);

% compute sd(y) (from detrended series)
ysdBRA = std(cycleBRA)*100;
ysdJPN = std(cycleJPN)*100;

disp(['Percent standard deviation of detrended log real GDP for Brazil: ', num2str(ysdBRA),'.']); disp(' ')
disp(['Percent standard deviation of detrended log real GDP for Japan: ', num2str(ysdJPN),'.']); disp(' ')

%相関係数
coefficient = corr(cycleBRA, cycleJPN);
disp(['Correlation coefficient: ' num2str(coefficient)]);



%Charts 
%Brazil
figure
subplot(2,1,2);
plot(q, cycleBRA,'r')
datetick('x', 'yyyy')
xlabel('Time')
title('Cyclical components for Brazil')
grid on;

%Japan
figure
subplot(2,1,2);
plot(q, cycleJPN,'r')
datetick('x', 'yyyy')
xlabel('Time')
title('Cyclical components for Japan')
grid on;


