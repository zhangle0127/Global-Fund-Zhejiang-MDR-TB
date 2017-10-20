library(gmodels)
library(tableone)
# library(ReporteRsjars)
# library(ReporteRs)
library(magrittr)

# pattern ---------------------------------------------------------------------
tab1(mdr163$pattern)
t=CreateCatTable("pattern",data=mdr163)
print(t,noSpaces = T,quote = T)

# CrossTable(regimen,outcome5,prop.t=F,prop.c=F,prop.chisq=F,chisq=T,fisher=T)

use(mdr)
# Table outcome5 vs tbregister ------------------------------------------------
# CrossTable(outcome5,tbregister,prop.c=F,prop.t=F,prop.chisq=F,fisher=T)
t_outcome5<-CreateTableOne("outcome5",data=mdr537)
print(t_outcome5,quote=TRUE,noSpaces=TRUE)

t_outcome5<-CreateTableOne("outcome5",strata=c("tbregister"),data=mdr537)
print(t_outcome5,quote=TRUE,noSpaces=TRUE)

# Table outcome4 vs tbregister  
t_outcome4<-CreateTableOne("outcome4",data=mdr537)
print(t_outcome4,quote=TRUE,noSpaces=TRUE)

t_outcome4<-CreateTableOne("outcome4",strata=c("tbregister"),data=mdr537)
print(t_outcome4,quote=TRUE,noSpaces=TRUE)

# Table successpoor vs tbregister
t_sp<-CreateTableOne("successpoor",strata=c("tbregister"),data=mdr537)
print(t_sp,quote=TRUE,noSpaces=TRUE,showAllLevels=T)


#listvars---------------------------------------------------------------------
dput(names(mdr537))
# all describe vars
desVars<-c("agecat", "sex", "occupation", "household", "tbregister",
"history", "hemosputum", "cavity", "adverse", "treatmentmode", "city","EMB","SM","KM","OFX","pattern3","secondline","queshi","successpoor","outcome4","outcome5")
# 不分层 Overall
table1<-CreateTableOne(vars = desVars, data = mdr537)
print(table1,quote=TRUE,showAllLevels=TRUE,noSpaces=TRUE)
# 分层 city
table1<-CreateTableOne(vars = desVars, data = mdr537, strata="city")
print(table1,quote=TRUE,showAllLevels=TRUE,noSpaces=TRUE)

# temp-------------------------------------------------------------------------
library(tableone)
tableSEX<-CreateTableOne(vars=desVars,strata="sex",data=mdr,catVars)
print(tableSEX,showAllLevels=TRUE,noSpaces=TRUE,quote=TRUE)

t=CreateCatTable(vars=c("agecat", "sex", "occupation", "household", "tbregister","history", "hemosputum", "cavity", "adverse", "treatmentmode", "pattern3","outcome4","outcome5"),data=subset(mdr517,successpoor=="Poor"))
print(t,showAllLevels=T)

t=CreateCatTable(vars=c("agecat", "sex", "occupation", "household", "tbregister","history", "treatmentmode","regimenIS","pattern3","hemosputum", "cavity", "adverse"),
	data=mdr415)
print(t,showAllLevels=T,quote=T)