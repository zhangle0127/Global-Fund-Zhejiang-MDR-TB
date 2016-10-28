# source('G:/work/MDR/code/code20161026/cleandata.R', encoding='UTF-8')
# echo=TRUE -----------------------------------------------------------
library(epicalc)
library(dplyr)
library(lubridate)
# working dir
setwd("G:/work/MDR")
# 读入后缺失值表示为 NA
mdr417<-read.csv("mdr417.csv",header=TRUE,na=c("","污染"))
# rename var ---------------------------------------------------------
library(reshape)
mdr417<-rename(mdr417,c(性别='sex',年龄='age',地市名称='city',登记分类.allmdr='tbregister',有无空洞='cavity',现详细地址类型='addresstype',
	既往抗结核治疗史='history',治疗模式='treatmentmode',停止治疗时间='enddate',停止治疗原因='outcome',户籍类型.allmdr='household',有无症状='symptom',有无咯血或血痰='hemosputum',异烟肼='INH',利福平='RFP',乙胺丁醇='EMB',链霉素='SM',卡那霉素='KM',氧氟沙星='OFX',开始治疗日期='startdate',涂片报告日期='smeardate',实际治疗的化疗方案='regimen',不良反应='adverse',X2月末痰涂片结果='month2smear',X2月末痰培养结果='month2culture',X3月末X线结果='month3xray'))

# generate var ---------------------------------------------------------------
#queshi apply对row进行操作(margin=1),应用fun为any,任一is.na=TRUE,any为true
mdr417<-within(mdr417,{
	#queshi
	queshi=apply(data.frame(is.na(INH),is.na(RFP),is.na(EMB),is.na(SM),is.na(KM),is.na(OFX)),1,any)
	#INH RFP EMB SM KM OFX
	INHtemp<-INH	
	RFPtemp<-RFP	
	EMBtemp<-EMB	
	SMtemp<-SM	
	KMtemp<-KM
	OFXtemp<-OFX
	#time
	startdate<-ymd(startdate)
	enddate<-ymd(enddate)
	smeardate<-ymd(smeardate)
	timedays=difftime(enddate,startdate,units="days")
	time=as.numeric(timedays)
	timediff=as.numeric(difftime(startdate,smeardate,units="days"))
	# status
	status=outcome
	outcome5=outcome
	outcome4=outcome
	successpoor=outcome
})  
# 查看耐药谱缺失情况
table(mdr417$queshi)
# pattern 将Susceptible替换为空白
s<-function(x) sub("S","",x)
mdr417[,136:141]<-sapply(mdr417[,136:141],as.character)
mdr417[,136:141]<-sapply(mdr417[,136:141],s)
# pattern 将Resistnce替换为相应药物
mdr417$INHtemp<-sub("R","H",mdr417$INHtemp)
mdr417$RFPtemp<-sub("R","/R",mdr417$RFPtemp)
mdr417$EMBtemp<-sub("R","/E",mdr417$EMBtemp)
mdr417$SMtemp <-sub("R","/S",mdr417$SMtemp)
mdr417$KMtemp <-sub("R","/Km",mdr417$KMtemp)
mdr417$OFXtemp<-sub("R","/Ofx",mdr417$OFXtemp)
#recode.is.na
use(mdr417)
recode.is.na(vars=OFXtemp:INHtemp,"")
mdr417=.data
#paste 得到耐药谱
mdr417$pattern<-with(mdr417,paste0(INHtemp,RFPtemp,EMBtemp,SMtemp,KMtemp,OFXtemp))
mdr417$pattern4=mdr417$pattern
mdr417$pattern3=mdr417$pattern
mdr417$pattern2=mdr417$pattern
mdr417$XDR=mdr417$pattern
mdr417$addresstype2=mdr417$addresstype
mdr417$regimenshort=mdr417$regimen
mdr417$year=year(mdr417$startdate)

# recode agecat -------------------------------------------------------------
mdr417<-within(mdr417,{
	# agecat
	agecat=NA
	agecat[age>65]="65yrs"
	agecat[age>45 & age<=65]="46-65yrs"
	agecat[age>25 & age<=45]="26-45yrs"
	agecat[age<=25]="<=25yrs"
 })	

#recode regimenshort -------------------------------------------------------
mdr417<-within(mdr417,{
	b=gregexpr('\\w+：',regimenshort)
	c=regmatches(x=regimenshort, b)
	d=unlist(c)
	regimenshort=as.factor(d)
})
mdr417$regimenIS=mdr417$regimenshort
mdr417$regimenlong=mdr417$regimenshort
# recode var ---------------------------------------------------------------
#重新编码 空洞
use(mdr417)
recode.is.na(vars=cavity,"Unknown")
recode.is.na(vars=month2smear,"未查")
recode.is.na(vars=month2culture,"未查")
recode.is.na(vars=month3xray,"未查")
# recode status(status=1结局，status=0删失)
recode(vars=status,
	c("不良反应导致失败","不良反应停止治疗","其他原因失败","失败","死亡","转入XDR治疗","丢失","完成治疗","治愈"),
	c("1","1","1","1","1","1","0","0","0"))
# recode outcome5
recode(vars=outcome5 ,
	c("治愈","完成治疗","不良反应导致失败","不良反应停止治疗","其他原因失败","失败","转入XDR治疗","死亡","丢失"),c("Cure","Completion","Failure","Failure","Failure","Failure","Failure","Death","Default"))
# recode outcome4
recode(vars=outcome4 ,
	c("治愈","完成治疗","不良反应导致失败","不良反应停止治疗","其他原因失败","失败","转入XDR治疗","死亡","丢失"),c("Success","Success","Failure","Failure","Failure","Failure","Failure","Death","Default"))
# recode successpoor
recode(vars=successpoor ,
	c("治愈","完成治疗","不良反应导致失败","不良反应停止治疗","其他原因失败","失败","转入XDR治疗","死亡","丢失"),c("Success","Success","Poor","Poor","Poor","Poor","Poor","Poor","Default"))
#### recode resistant pattern "MDR only","MDR+1st","MDR+Ofx/Km","XDR"
recode(vars =pattern4,
       c("H/R","H/R/E","H/R/S","H/R/E/S",
       	 "H/R/S/Km","H/R/Ofx","H/R/E/Ofx","H/R/S/Ofx","H/R/E/S/Ofx",
         "H/R/Km/Ofx","H/R/E/Km/Ofx","H/R/S/Km/Ofx","H/R/E/S/Km/Ofx"),
       c("MDR only","MDR_FL","MDR_FL","MDR_FL",
         "MDR_SL","MDR_SL","MDR_SL","MDR_SL","MDR_SL",
         "XDR","XDR","XDR","XDR"))
#### recode resistant pattern "MDR_FL","MDR_SL","XDR"
recode(vars =pattern3,
       c("H/R","H/R/E","H/R/S","H/R/E/S",
       	 "H/R/S/Km","H/R/Ofx","H/R/E/Ofx","H/R/S/Ofx","H/R/E/S/Ofx",
         "H/R/Km/Ofx","H/R/E/Km/Ofx","H/R/S/Km/Ofx","H/R/E/S/Km/Ofx"),
       c("MDR_FL","MDR_FL","MDR_FL","MDR_FL",
         "MDR_SL","MDR_SL","MDR_SL","MDR_SL","MDR_SL",
         "XDR","XDR","XDR","XDR"))
#### recode resistant pattern "MDR_FL","MDR_SL"
recode(vars =pattern2,
       c("H/R","H/R/E","H/R/S","H/R/E/S",
       	 "H/R/S/Km","H/R/Ofx","H/R/E/Ofx","H/R/S/Ofx","H/R/E/S/Ofx",
         "H/R/Km/Ofx","H/R/E/Km/Ofx","H/R/S/Km/Ofx","H/R/E/S/Km/Ofx"),
       c("MDR_FL","MDR_FL","MDR_FL","MDR_FL",
         "MDR_SL","MDR_SL","MDR_SL","MDR_SL","MDR_SL",
         "MDR_SL","MDR_SL","MDR_SL","MDR_SL"))
#### recode MDR
recode(vars =XDR,
       c("H/R","H/R/E","H/R/S","H/R/E/S",
       	 "H/R/S/Km","H/R/Ofx","H/R/E/Ofx","H/R/S/Ofx","H/R/E/S/Ofx",
         "H/R/Km/Ofx","H/R/E/Km/Ofx","H/R/S/Km/Ofx","H/R/E/S/Km/Ofx"),
       c("0","0","0","0",
         "0","0","0","0","0",
         "1","1","1","1"))
#### recode tbregister
recode(vars=tbregister,c("2和3月末痰涂片阳性","新患者","初治失败","复治失败","复发","返回","其他"),c("New","New","T.A.F1","T.A.F2","Relapse","T.A.D","Other"))
####recode 现详细地址类型 addresstype (tips:其它 not其他)
recode(vars=addresstype2,c("本省其它地市","本市其它县区","其他省","本县区"),
       c("在外地","在外地","在外地","本县区"))          
### recode regimen 治疗方案
recode(vars=regimenlong,
  	   c("IR：",  "SR1：", "SR2：", "SR3：", "SR4：", "SR5："),
  	   c("IR：个体化疗方案",
  	     "SR1：6Z Am Lfx Cs Pto/18Z Lfx Cs Pto",
  	     "SR2：6Z Am Lfx PAS Pto/18Z Lfx PAS Pto",
         "SR3：6Z Cm Lfx Cs Pto/18Z Lfx Cs Pto",
         "SR4：6Z Cm Lfx PAS Pto/18Z Lfx Cs Pt",
         "SR5：12Cm Mfx PAS Cs Clr Amx/Clv /24Mfx PAS Cs Clr Amx/Clv"))
recode(vars=regimenIS,
		c("IR：",  "SR1：", "SR2：", "SR3：", "SR4：", "SR5："),
		c("IR", "SR", "SR", "SR", "SR", "SR"))
recode(vars=sex,c("男","女"),c("Male","Female"))
recode(vars=adverse,c("未知","无","有"),c("Unknown","No","Yes"))
recode(vars=cavity,c("正常","无","有"),c("No","No","Yes"))
recode(vars=household,c("本地","外地"),c("Local","Float"))
recode(vars=hemosputum,c("无","有"),c("No","Yes"))
recode(vars=history,c('仅使用过一线药','使用过一线和二线药','无' ),
				 c('First-line drugs only','Second-line drugs','None'))
recode(vars=city,c('杭州市','湖州市','嘉兴市','丽水市','衢州市','绍兴市'),
				 c('Hangzhou','Huzhou','Jiaxing','Lishui','Quzhou','Shaoxing'))
recode(vars=treatmentmode,c("门诊治疗","住院治疗"),
				 c("Outpatient treatment","Hospitalization"))
recode(vars=month2smear,c('未查','阳性','阴性'),
				 c('Unknown','Positive','Negative'))
recode(vars=month2culture,c('未查','阳性','阴性'),
				 c('Unknown','Positive','Negative'))
recode(vars=month3xray,c('不变','全吸','未查','吸收','显吸'),
				 c('Unchanged','Absorbed','Unknown','Absorbed','Absorbed'))
mdr417=.data
mdr415=subset(mdr417,time>=0)

# 选择需要分析的变量 -------------------------------------------------------
# 一行5个
mdr415<-mdr415 %>%
select(queshi,agecat,sex,tbregister,household,
		year,city,pattern2,pattern3,pattern4,
		history,cavity,adverse,hemosputum,treatmentmode,
		month2smear,month2culture,month3xray,regimen,regimenshort,
		regimenlong,regimenIS,outcome,successpoor,outcome4,
		outcome5,status,XDR,addresstype,addresstype2,
        time,timediff)
# n=32      # 全部变量下标
# nf=1:30   # 分类变量下标
# ni=31:32  # 数值变量下标
mdr415[,1:30]<-lapply(mdr415[,1:30],as.factor)
mdr415[,31:32]<-lapply(mdr415[,31:32],as.numeric)

# set reference groups -------------------------------------------------------
library(doBy)
mdr=within(mdr415,{
		agecat=relevel(agecat,ref="26-45yrs")
        sex=relevel(sex,ref="Female")
        tbregister=relevel(tbregister, ref="New")
        household=relevel(household,ref="Local")
        year=relevel(year,ref="2009")
        pattern4=relevel(pattern4,ref="MDR only")
		pattern3=relevel(pattern4,ref="MDR_FL")
		pattern2=relevel(pattern4,ref="MDR_FL")
		history=relevel(history,ref="Second-line drugs")
		cavity=relevel(cavity,ref="No")
		adverse=relevel(adverse,ref="No")
		hemosputum=relevel(hemosputum,ref="No")
        treatmentmode=relevel(treatmentmode,ref="Outpatient treatment")
        month2smear=relevel(month2smear,ref="Negative")        
        month2culture=relevel(month2culture,ref="Negative")
        month3xray=relevel(month3xray,ref="Absorbed")        
        regimenIS=relevel(regimenIS,ref="SR")
        addresstype=relevel(addresstype,ref="本县区")
        addresstype2=relevel(addresstype2,ref="本县区")
        SurvObj=Surv(time, status==1)
})
# # SurvObj <- Surv(timeday,outcomepoor)
# mdr$SurvObj <- with(mdr, Surv(time, status==1)) 