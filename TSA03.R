#鐢熸垚闅忔満娓稿姩搴忓垪
Rwalk <- vector()
Rwalk[1] <- rnorm(1)
for (i in 2:60) { 
  Rwalk[i] <- Rwalk[i-1]+rnorm(1)
}
ts_Rwalk <- ts(Rwalk)
win.graph(width = 4.8, height = 2.5, pointsize = 8)
plot(ts_Rwalk,  ylab = 'Random Walk', xlab = 'Time', type = 'l')
points(ts_Rwalk, col = 'blue')  

#閿欒鐨勫皢闅忔満娓稿姩鐢∣LS绾挎�ф嫙鍚?
library(TSA)
data(rwalk)
lm_rwalk <- lm(rwalk ~ time(rwalk))
summary(lm_rwalk)
win.graph(width = 4.8, height = 2.5,pointsize = 8)
plot(rwalk,  ylab = 'Random Walk', xlab = 'Time', type = 'l')
points(rwalk, col = 'blue')  
abline(lm_rwalk,col = 'red')

#二次拟合趋势
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

#绘制季节性图
data(tempdub)
temper_month <- season(tempdub)
lm_temper_1 <- lm(tempdub ~ temper_month -1)#-1不含截距项
summary(lm_temper_1)
#含有截距项如下
lm_temper_2 <- lm(tempdub ~ temper_month)
summary(lm_temper_2)
#用余弦趋势进行估计
har_temper <- harmonic(tempdub)
lm_temper_3 <- lm(tempdub ~ har_temper)
summary(lm_temper_3)
#残差分析
xfit <- time(tempdub)
yfit <- fitted(lm_temper_1)
res_temper <- rstudent(lm_temper_1)
win.graph(width = 4.8,height = 2.5,pointsize = 8)
plot(y = res_temper,x = as.vector(xfit),xlab = 'Time',ylab = 'standardized Residuals',type = 'l')
points(res_temper,x = as.vector(xfit),pch = as.vector(temper_month), col = 'blue')

win.graph(width = 4.8,height = 2.5,pointsize = 8)
plot(y = res_temper,x = as.vector(yfit), xlab = 'Fitted Trend Values',ylab = 'Standard Residuals',type = 'n')
points(res_temper,x = as.vector(yfit), pch = as.vector(temper_month),col = 'blue')
#QQ图
win.graph(width = 4.8,height = 2.5,pointsize = 8)
qqnorm(res_temper, main = '',col = 'blue')
#直方图
win.graph(width = 4.8,height = 2.5,pointsize = 8)
hist(res_temper,xlab = 'Standard Residuals',main = '',col = 'blue')
#自相关图
win.graph(width = 4.8,height = 2.5,pointsize = 8)
acf(res_temper,main = '',col = 'blue')
#t检验，检验零均值假设，SW检验，检验正态性；游程检验，检验独立性
t.test(as.vector(res_temper))
shapiro.test(as.vector(res_temper))
runs(as.vector(res_temper))