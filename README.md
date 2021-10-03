# CH.01
cor.test(x,y,alternative = "two.side",method = "pearson",conf.level = 0.95) #计算X与Y的相关性系数并检验显著性
lm2.1 <- lm(y~x) #求线性回归函数 
summary(lm2.1) #输出回归分析的结果
anova(lm2.1) #输出方差分析表 
e <- resid(lm2.1,degits = 5) #残差赋值给e，保留小数点后5位
plot(x,e) #绘制残差图，横坐标为X
sre <- rstandard(lm2.1) #计算学生化残差
confint(lm2.1) #计算回归系数95%置信区间
ypred <- predict(lm2.1 ,new,interval = "prediction",level = 0.95) #计算预测区间并赋值给ypred
yconf <- predict(lm2.1,new,interval = "confidence",level = 0.95) #计算预测值及置信区间并赋值给yconf
