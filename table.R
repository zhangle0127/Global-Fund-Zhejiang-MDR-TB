
library(gmodels)
library(tableone)
library(ReporteRsjars)
library(ReporteRs)
library(magrittr)
# CrossTable(regimen,outcome5,prop.t=F,prop.c=F,prop.chisq=F,chisq=T,fisher=T)
# CrossTable(regimeSD,outcome4,prop.t=F,prop.c=F,prop.chisq=F,chisq=T,fisher=T)
use(mdr)
CrossTable(outcome5,regimenshort,prop.c=F,prop.t=F,prop.chisq=F,fisher=T)
# outcome5
t_outcome5<-CreateTableOne("outcome5",strata=c("regimenshort"),mdr,"outcome5")
print(table_outcome5,quote=TRUE,noSpaces=TRUE)
# successpoor
t_sp<-CreateTableOne("successpoor",strata=c("regimenshort"),mdr,"successpoor")
print(t_sp,quote=TRUE,noSpaces=TRUE)