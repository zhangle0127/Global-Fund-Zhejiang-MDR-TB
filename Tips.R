TIPS
Surv：用于创建生存数据对象
survfit：创建KM生存曲线或是Cox调整生存曲线
survdiff：用于不同组的统计检验
coxph：构建COX回归模型
cox.zph：检验PH假设是否成立
survreg：构建参数模型

install.packages("C:/reshape_0.8.6.zip", repos = NULL, type = "win.binary")

#读入后,缺失值是空格
read.csv("xx.csv",header=TRUE)
#读入后,缺失值表示成 NA
read.csv("xx.csv",header=TRUE,na="")