#生成随机游动序列
Rwalk <- vector()
Rwalk[1] <- rnorm(1)
for (i in 2:60) { 
  Rwalk[i] <- Rwalk[i-1]+rnorm(1)
}
ts_Rwalk <- ts(Rwalk)
win.graph(width = 4.8, height = 2.5, pointsize = 8)
plot(ts_Rwalk,  ylab = 'Random Walk', xlab = 'Time', type = 'l')
points(ts_Rwalk, col = 'blue')  

#错误的将随机游动用OLS线性拟�?
library(TSA)
data(rwalk)
lm_rwalk <- lm(rwalk ~ time(rwalk))
summary(lm_rwalk)
win.graph(width = 4.8, height = 2.5,pointsize = 8)
plot(rwalk,  ylab = 'Random Walk', xlab = 'Time', type = 'l')
points(rwalk, col = 'blue')  
abline(lm_rwalk,col = 'red')

#�����������
library(TSA)
data(wages)
lm2_wages = lm(wages ~ time(wages) + I(time(wages)^2))
summary(lm2_wages)
xfit <- time(wages)
yfit_2 <- fitted(lm2_wages)
win.graph(width = 4.8, height = 2.5, pointsize = 8)
plot(wages,ylab = 'Minthly Wages', xlab = 'Time',type = 'l')
points(wages,col = 'blue')
lines(as.vector(xfit),as.vector(yfit_2),col = 'red')

#���Ƽ�����ͼ
data(tempdub)
temper_month <- season(tempdub)
lm_temper_1 <- lm(tempdub ~ temper_month -1)#-1�����ؾ���
summary(lm_temper_1)
#���нؾ�������
lm_temper_2 <- lm(tempdub ~ temper_month)
summary(lm_temper_2)
#���������ƽ��й���
har_temper <- harmonic(tempdub)
lm_temper_3 <- lm(tempdub ~ har_temper)
summary(lm_temper_3)
#�в����
xfit <- time(tempdub)
yfit <- fitted(lm_temper_1)
res_temper <- rstudent(lm_temper_1)
win.graph(width = 4.8,height = 2.5,pointsize = 8)
plot(y = res_temper,x = as.vector(xfit),xlab = 'Time',ylab = 'standardized Residuals',type = 'l')
points(res_temper,x = as.vector(xfit),pch = as.vector(temper_month), col = 'blue')

win.graph(width = 4.8,height = 2.5,pointsize = 8)
plot(y = res_temper,x = as.vector(yfit), xlab = 'Fitted Trend Values',ylab = 'Standard Residuals',type = 'n')
points(res_temper,x = as.vector(yfit), pch = as.vector(temper_month),col = 'blue')
#QQͼ
win.graph(width = 4.8,height = 2.5,pointsize = 8)
qqnorm(res_temper, main = '',col = 'blue')
#ֱ��ͼ
win.graph(width = 4.8,height = 2.5,pointsize = 8)
hist(res_temper,xlab = 'Standard Residuals',main = '',col = 'blue')
#�����ͼ
win.graph(width = 4.8,height = 2.5,pointsize = 8)
acf(res_temper,main = '',col = 'blue')
#t���飬�������ֵ���裬SW���飬������̬�ԣ��γ̼��飬���������
t.test(as.vector(res_temper))
shapiro.test(as.vector(res_temper))
runs(as.vector(res_temper))