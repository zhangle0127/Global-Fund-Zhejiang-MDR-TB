stset time, failure(status==2) scale(365.25)
stsum, by(regimenIS) /*计算中位生存时间*/
stci, rmean by(regimenIS)/*计算平均生存时间及其可信区间*/
sts list
sts graph
sts graph, by(regimenIS)
sts graph if regimenIS==1,gwood /*95%CI*/
sts graph if regimenIS==2,gwood

sts test regimenIS,logrank /* 进行Log-Rank 检验*/
sts test regimenIS,wilcoxon
sts test regimenIS,trend /*检验死亡(生存)率是否随分组变量取值水平的增高而上升或下降*/
