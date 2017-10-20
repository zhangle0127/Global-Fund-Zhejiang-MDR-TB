# source('G:/work/MDR/code/code20161026/cleandata.R', encoding='UTF-8')
# poor2: means death and failure are defined as poor outcomes
# poor3: means death, failure and failure are defined as poor outcomes
# echo=TRUE -----------------------------------------------------------
library(epicalc)
library(dplyr)
library(lubridate)
# working dir
remove(list=ls())
setwd("~/work/浙江全球基金MDR-TB项目/MDR 治疗")
# 读入后缺失值表示为 NA
mdr537<-read.csv("mdr537.csv",header=TRUE,na=c("","污染"))

# 筛选adverse变量 -------------------------------------------------------------
adverse537<-select(mdr537,不良反应:是否因不良反应而中断过化疗)
adverse186<-na.omit(adverse537)

rf=function(x) {recode(x,"无"="No","有"="aYes","未知"="Unknown")}
adverse537=sapply(adverse537,rf)
adverse537[is.na(adverse537)]<-"Unknown"
adverse537=as.data.frame(adverse537)
# adverse537<-sapply(adverse537,addNA,simplify = FALSE)

# 生成 adverse 表格
library(tableone)
tadverse=CreateCatTable(vars=dput(names(adverse537)),data=adverse537)
print(tadverse,showAllLevels = T,quote = T,noSpaces = T)

# rename var ------------------------------------------------------------------
library(reshape)
mdr537<-rename(mdr537,c(性别='sex',年龄='age',地市名称='city',登记分类.allmdr='tbregister',有无空洞='cavity',现详细地址类型='addresstype',	既往抗结核治疗史='history',治疗模式='treatmentmode',停止治疗时间='enddate',停止治疗原因='outcome',户籍类型.allmdr='household',职业.allmdr='occupation',有无咯血或血痰='hemosputum',异烟肼='INH',利福平='RFP',乙胺丁醇='EMB',链霉素='SM',卡那霉素='KM',氧氟沙星='OFX',开始治疗日期='startdate',涂片报告日期='smeardate',实际治疗的化疗方案='regimen',不良反应='adverse',X6月末痰涂片结果='month6smear',X6月末痰培养结果='month6culture',X3月末X线结果='month3xray'))

# occupation有95个缺失值，已填补过--------------------------------------------

# generate var ---------------------------------------------------------------
#secondline apply对row进行操作(margin=1),应用fun为any,任一is.na=TRUE,any为true
library(lubridate)
mdr537<-within(mdr537,{
	#secondline
	secondline=apply(data.frame(is.na(KM),is.na(OFX)),1,any)
	queshi=apply(data.frame(is.na(SM),is.na(EMB),is.na(KM),is.na(OFX)),1,any)
	#INH RFP EMB SM KM OFX
	INHtemp<-INH	
	RFPtemp<-RFP	
	EMBtemp<-EMB	
	SMtemp<-SM	
	KMtemp<-KM
	OFXtemp<-OFX
    status=outcome
	outcome5=outcome
	outcome4=outcome
	successpoor=outcome
	id=1:nrow(mdr537)
})

#time
mdr537<-within(mdr537,{
	startdate<-ymd(startdate)
	enddate<-ymd(enddate)
	timedays=difftime(enddate,startdate,units="days")
	timemonths=round(timedays/30.4375,1)
	time=as.numeric(timedays)
	# status
})  
# 查看耐药谱缺失情况
table(mdr537$timedays)
table(mdr537$queshi)
# table(mdr537$secondline)

# pattern 将Susceptible替换为空白
s<-function(x) sub("S","",x)
mdr537[,134:139]<-sapply(mdr537[,134:139],as.character)
mdr537[,134:139]<-sapply(mdr537[,134:139],s)

# pattern 将Resistnce替换为相应药物
mdr537$INHtemp<-sub("R","H",mdr537$INHtemp)
mdr537$RFPtemp<-sub("R","/R",mdr537$RFPtemp)
mdr537$EMBtemp<-sub("R","/E",mdr537$EMBtemp)
mdr537$SMtemp <-sub("R","/S",mdr537$SMtemp)
mdr537$KMtemp <-sub("R","/Km",mdr537$KMtemp)
mdr537$OFXtemp<-sub("R","/Ofx",mdr537$OFXtemp)

#recode.is.na
use(mdr537)
recode.is.na(vars=OFXtemp:INHtemp,"")
mdr537=.data

#paste 得到耐药谱
mdr537$pattern<-with(mdr537,paste0(INHtemp,RFPtemp,EMBtemp,SMtemp,KMtemp,OFXtemp))
mdr537$pattern4=mdr537$pattern
mdr537$pattern3=mdr537$pattern
mdr537$pattern2=mdr537$pattern
mdr537$XDR=mdr537$pattern
mdr537$addresstype2=mdr537$addresstype
mdr537$regimenshort=mdr537$regimen
mdr537$year=year(mdr537$startdate)

# recode agecat -------------------------------------------------------------
mdr537<-within(mdr537,{
	# agecat
	agecat=NA
	agecat[age>60]="60yrs"
	agecat[age>30 & age<=60]="30-60yrs"
	agecat[age<=30]="<=30yrs"
 })	

# mdr537<-within(mdr537,{
# 	# agecat
# 	agecat=NA
# 	agecat[age<15]="10-15"
# 	agecat[age>=15 & age<20]="15-20"
# 	agecat[age>=20 & age<25]="20-25"
# 	agecat[age>=25 & age<30]="25-30"
# 	agecat[age>=30 & age<35]="30-35"
# 	agecat[age>=35 & age<40]="35-40"
# 	agecat[age>=40 & age<45]="40-45"
# 	agecat[age>=45 & age<50]="45-50"
# 	agecat[age>=50 & age<55]="50-55"
# 	agecat[age>=55 & age<60]="55-60"
# 	agecat[age>=60 & age<65]="60-65"
# 	agecat[age>=65 & age<70]="65-70"
# 	agecat[age>=70 & age<75]="70-75"
# 	agecat[age>=75 & age<80]="75-80"
# 	agecat[age>=80]="80-85"
#  })	


mdr537<-within(mdr537,{
	# timemonthscat
	timemonthscat=NA
	timemonthscat[timemonths>30]="30"
	timemonthscat[timemonths>20 & timemonths<=30]="20-30"
	timemonthscat[timemonths>10 & timemonths<=20]="10-20"
	timemonthscat[timemonths<=10]="0-10"
 })	
#recode regimenshort -------------------------------------------------------
# \escape 将冒号后的全替换成空白，例如 SR4：6Z Cm Lfx PAS Pto/60Z Lfx Cs Pto
# s=function(x) {sub('\\：.*','',x)}
# a=mdr537$regimenshort
# b=s(a)
# table(b)

# mdr537$regimenshort=s(mdr537$regimenshort)
# b=sub('\\：.*','',a)

mdr537<-within(mdr537,{
	regimenshort=sub('\\：.*','',regimenshort)
	regimenshort=as.factor(regimenshort)
})

mdr537$regimenIS=mdr537$regimenshort
mdr537$regimenlong=mdr537$regimenshort
# recode var ---------------------------------------------------------------
#重新编码 空洞

use(mdr537)
recode.is.na(vars=cavity,"Unknown")
recode.is.na(vars=month6smear,"未查")
recode.is.na(vars=month6culture,"未查")
recode.is.na(vars=month3xray,"未查")
# recode status(status=1结局，status=0删失)
mdr537=.data

# poor3 死亡 失败  丢失-------------------------------------------------------
library(dplyr)
require(dplyr)
mdr537=within(mdr537,{
status=recode(status,"不良反应导致失败"="1","不良反应停止治疗"="1","其他原因失败"="1","失败"="1","死亡"="1","转入XDR治疗"="1","丢失"="1","完成治疗"="0","治愈"="0")
# poor2 死亡 失败------------------------------------------------------------
# recode(status,"不良反应导致失败"="1","不良反应停止治疗"="1","其他原因失败"="1","失败"="1","死亡"="1","转入XDR治疗"="1","丢失"="0","完成治疗"="0","治愈"="0")

# recode outcome5
outcome5=recode(outcome5,"治愈"="Cure","完成治疗"="Completion","不良反应导致失败"="Failure","不良反应停止治疗"="Failure","其他原因失败"="Failure","失败"="Failure","转入XDR治疗"="Failure","死亡"="Death","丢失"="Default")
outcome5=factor(outcome5,levels=c("Cure","Completion","Failure","Death","Default"))
# recode outcome4
outcome4=recode(outcome4,"治愈"="Success","完成治疗"="Success","不良反应导致失败"="Failure","不良反应停止治疗"="Failure","其他原因失败"="Failure","失败"="Failure","转入XDR治疗"="Failure","死亡"="Death","丢失"="Default")
outcome4=factor(outcome4,levels=c("Success","Failure","Death","Default"))
# recode successpoor
successpoor=recode(successpoor,"治愈"="Success","完成治疗"="Success","不良反应导致失败"="Poor","不良反应停止治疗"="Poor","其他原因失败"="Poor","失败"="Poor","转入XDR治疗"="Poor","死亡"="Poor","丢失"="Poor")
successpoor=factor(successpoor,levels=c("Success","Poor"))

#### recode resistant pattern "MDR-FL","Pre-XDR","XDR"
pattern3=recode(pattern3,"H/R"="MDR-FL","H/R/E"="MDR-FL","H/R/S"="MDR-FL","H/R/E/S"="MDR-FL","H/R/S/Km"="Pre-XDR","H/R/Ofx"="Pre-XDR","H/R/E/Ofx"="Pre-XDR","H/R/S/Ofx"="Pre-XDR","H/R/E/S/Ofx"="Pre-XDR","H/R/Km/Ofx"="XDR","H/R/E/Km/Ofx"="XDR","H/R/S/Km/Ofx"="XDR","H/R/E/S/Km/Ofx"="XDR")
pattern3=as.character(pattern3)
pattern3[secondline==TRUE]="Unknown"

#### recode resistant pattern "MDR-FL","MDR-SL"
pattern2=recode(pattern2,"H/R"="MDR-FL","H/R/E"="MDR-FL","H/R/S"="MDR-FL","H/R/E/S"="MDR-FL","H/R/S/Km"="MDR-SL","H/R/Ofx"="MDR-SL","H/R/E/Ofx"="MDR-SL","H/R/S/Ofx"="MDR-SL","H/R/E/S/Ofx"="MDR-SL","H/R/Km/Ofx"="MDR-SL","H/R/E/Km/Ofx"="MDR-SL","H/R/S/Km/Ofx"="MDR-SL","H/R/E/S/Km/Ofx"="MDR-SL")
pattern2=as.character(pattern2)
pattern2[secondline==TRUE]="Unknown"

#### recode XDR
XDR=recode(XDR,"H/R"="0","H/R/E"="0","H/R/S"="0","H/R/E/S"="0","H/R/S/Km"="0","H/R/Ofx"="0","H/R/E/Ofx"="0","H/R/S/Ofx"="0","H/R/E/S/Ofx"="0","H/R/Km/Ofx"="1","H/R/E/Km/Ofx"="1","H/R/S/Km/Ofx"="1","H/R/E/S/Km/Ofx"="1")
XDR=as.character(XDR)
XDR[secondline==TRUE]="Unknown"

SM=as.character(SM)
SM[is.na(SM)]="Unknown"

EMB=as.character(EMB)
EMB[is.na(EMB)]="Unknown"

KM=as.character(KM)
KM[is.na(KM)]="Unknown"

OFX=as.character(OFX)
OFX[is.na(OFX)]="Unknown"

regimenshort=as.character(regimenshort)
regimenshort[is.na(regimenshort)]="Unknown"

#### recode tbregister
tbregister=recode(tbregister,"2和3月末痰涂片阳性"="New","新患者"="New","初治失败"="T.A.F","复治失败"="T.A.F","复发"="Relapse","返回"="T.A.D","其他"="Other")
tbregister=factor(tbregister,levels=c("New", "Relapse", "T.A.F", "T.A.D", "Other"))
####recode 现详细地址类型 addresstype (tips:其它 not其他)
addresstype2=recode(addresstype2,"本省其它地市"="在外地","本市其它县区"="在外地","其他省"="在外地","本县区"="本县区")
### recode regimen 治疗方案
regimenlong=recode(regimenlong,"IR"="IR: Individualized Regimen","SR1"="SR1: 6Z Am Lfx Cs Pto/18Z Lfx Cs Pto","SR2"="SR2: 6Z Am Lfx PAS Pto/18Z Lfx PAS Pto","SR3"="SR3: 6Z Cm Lfx Cs Pto/18Z Lfx Cs Pto", "SR4"="SR4: 6Z Cm Lfx PAS Pto/18Z Lfx Cs Pt","SR5"="SR5: 12Cm Mfx PAS Cs Clr Amx/Clv /24Mfx PAS Cs Clr Amx/Clv")
regimenIS=recode(regimenshort,"SR1"="SR","SR2"="SR","SR3"="SR","SR4"="SR","SR5"="SR")
sex=recode(sex,"男"="Male","女"="Female")
adverse=recode(adverse,"未知"="Unknown","无"="No","有"="Yes")
cavity=recode(cavity,"正常"="No","无"="No","有"="Yes")
household=recode(household,"本地"="Local","外地"="Float")
hemosputum=recode(hemosputum,"无"="No","有"="Yes")
history=recode(history,'仅使用过一线药'='First-line drugs only','使用过一线和二线药'='Second-line drugs','无'='None')
city=recode(city,'杭州市'='Hangzhou','湖州市'='Huzhou','嘉兴市'='Jiaxing','丽水市'='Lishui','衢州市'='Quzhou','绍兴市'='Shaoxing')
treatmentmode=recode(treatmentmode,"门诊治疗"="Outpatient treatment","住院治疗"="Hospitalization")
month6smear=recode(month6smear,'未查'='Unknown','阳性'='Positive','阴性'='Negative')
month6culture=recode(month6culture,'未查'='Unknown','阳性'='Positive','阴性'='Negative')
month3xray=recode(month3xray,'不变'='Unchanged','全吸'='Absorbed','未查'='Unknown','吸收'='Absorbed','显吸'='Absorbed')
})

# occupation
mdr537$occupation=as.character(mdr537$occupation)
mdr537$occupation[mdr537$occupation!="农民"]="Other"
mdr537$occupation[mdr537$occupation=="农民"]="Farmer"

# 选择需要分析的变量 -----------------------------------------------------
mdr537<-mdr537 %>%
select(id,secondline,queshi,agecat,sex,tbregister,household,occupation,year,city,pattern,pattern2,pattern3,pattern4,history,cavity,adverse,hemosputum,treatmentmode,month6smear,month6culture,month3xray,regimen,regimenshort,regimenlong,regimenIS,outcome,successpoor,outcome4,outcome5,status,XDR,addresstype,addresstype2,timemonthscat,INH,RFP,EMB,SM,KM,OFX,time,timemonths,age,startdate,enddate)
mdr537[,2:41]<-as.data.frame(sapply(mdr537[,2:41],addNA,simplify = FALSE))
# nf=2:41  # 分类变量下标
# ni=41:43 # 数值变量下标
mdr537[,2:41]<-lapply(mdr537[,2:41],as.factor)
mdr537[,42:44]<-lapply(mdr537[,42:44],as.numeric)

# set reference groups -------------------------------------------------------
library(doBy)
mdr537=within(mdr537,{
		agecat=relevel(agecat,ref="<=30yrs")
        sex=relevel(sex,ref="Male")
        tbregister=relevel(tbregister, ref="New")
        household=relevel(household,ref="Local")
        occupation=relevel(occupation,ref="Other")
        year=relevel(year,ref="2009")
		pattern3=relevel(pattern3,ref="MDR-FL")
		pattern2=relevel(pattern2,ref="MDR-FL")
		history=relevel(history,ref="Second-line drugs")
		cavity=relevel(cavity,ref="No")
		adverse=relevel(adverse,ref="No")
		hemosputum=relevel(hemosputum,ref="No")
        treatmentmode=relevel(treatmentmode,ref="Outpatient treatment")
        month6smear=relevel(month6smear,ref="Negative")        
        month6culture=relevel(month6culture,ref="Negative")
        month3xray=relevel(month3xray,ref="Absorbed")        
        regimenIS=relevel(regimenIS,ref="SR")
        addresstype=relevel(addresstype,ref="本县区")
        addresstype2=relevel(addresstype2,ref="本县区")
        timemonthscat=relevel(timemonthscat,ref="0-10")
        EMB=relevel(EMB,ref="S")
        SM=relevel(SM,ref="S")
        KM=relevel(KM,ref="S")
        OFX=relevel(OFX,ref="S")
        SurvObj=Surv(time, status==1)
})

        mdr537$EMB=relevel(mdr537$EMB,ref="S")
        mdr537$SM=relevel(mdr537$SM,ref="S")
        mdr537$KM=relevel(mdr537$KM,ref="S")
        mdr537$OFX=relevel(mdr537$OFX,ref="S")
# 将treatment 4 个缺失值改为 "Outpatient treatment" ------------------------
mdr537[treatmentmode=="NA","treatmentmode"]="Outpatient treatment"
mdr537[,2:41]<-as.data.frame(sapply(mdr537[,2:41],droplevels))

# mdr517--------------------------------------------------------------------
mdr536=mdr537[!is.na(mdr537$startdate),] # missing 1
mdr519=mdr536[!is.na(mdr536$enddate),] # missing 17
mdr517=subset(mdr519,time>=0) # missing 2
remove(mdr536)
remove(mdr519)
mdr517[,2:41]<-as.data.frame(sapply(mdr517[,2:41],droplevels))
table(mdr517$time)

# # SurvObj <- Surv(timeday,outcomepoor)
# mdr$SurvObj <- with(mdr, Surv(time, status==1)) 
mdr157<-subset(mdr517,queshi=="FALSE")
mdr157[,2:41]<-as.data.frame(sapply(mdr157[,2:41],droplevels))
aggregate(age~sex,data=mdr537,mean)

sd(subset(mdr537,sex=="Male")$age)
[1] 16.02198
sd(subset(mdr537,sex=="Female")$age)
[1] 15.07918

mdr517[mdr517$time==0,"time"]=0.5

# write.dta(mdr517[,1:46],"mdr517.dta")

mdr537<-within(mdr537,{
	# agecat
	agecat=NA
	# agecat[age<15]="10-15"
	agecat[age>=10 & age<=20]="10-20"
	agecat[age>20 & age<=25]="20-25"
	agecat[age>25 & age<=30]="25-30"
	agecat[age>30 & age<=35]="30-35"
	agecat[age>35 & age<=40]="35-40"
	agecat[age>40 & age<=45]="40-45"
	agecat[age>45 & age<=50]="45-50"
	agecat[age>50 & age<=55]="50-55"
	agecat[age>55 & age<=60]="55-60"
	agecat[age>60 & age<=65]="60-65"
	agecat[age>65 & age<=70]="65-70"
	agecat[age>70 & age<=75]="70-75"
	agecat[age>75 & age<=85]="75-85"
	# agecat[age>=80]="80-85"
 })	

library(lattice)
t=xtabs(~agecat+successpoor,data=mdr537)
t=as.data.frame(prop.table(t,1))
dotplot(Freq~agecat|successpoor,data=t,layout=c(1,2))

t=xtabs(~agecat+outcome4,data=mdr537)
t=as.data.frame(prop.table(t,1))
names(t)[3]="Proportion"
dotplot(Proportion~agecat|outcome4,data=t,layout=c(2,2),xlab=" Age groups",ylab="Proportion of treatment outcome")

# 延迟  --------------------------------------------------------------------------------

t_ss=difftime(mdr537$startdate,mdr537$smeardate,units = "day")
mean(t_ss,na.rm=T)
# Time difference of 115.6729 days
# 痰涂片到治疗
sd(t_ss,na.rm = T)
[1] 139.6818
# 四分位数
quantile(t_ss,na.rm = T)
Time differences in days
        0%        25%        50%        75%       100% 
-438.66667   17.33333   88.33333  162.58333 1038.33333 



t_cs=difftime(mdr537$startdate,mdr537$培养结果报告日期,units = "day")
mean(t_cs,na.rm=T)
# Time difference of 91.13371 days
# 痰培养到治疗

t_dsts=difftime(mdr537$startdate,mdr537$药敏结果报告日期,units = "day")
mean(t_dsts,na.rm=T)
# Time difference of 39.12399 days
# 药敏到治疗
sd(t_dsts,na.rm=T)
# 125.8908

# 四分位数
quantile(t_dsts,na.rm = T)
Time differences in days
          0%          25%          50%          75%         100% 
-754.6666667    0.3333333   11.3333333   44.8333333  933.3333333 
