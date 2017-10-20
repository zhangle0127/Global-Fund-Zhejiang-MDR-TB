# PPT上的图，要大一些 --- Resisitance Pattern ---------------------------------
tiff(file="KM_pattern3_days.tiff", width=9, height=8, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.7, cex.axis=1.5)
par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))
p3=subset(mdr517,secondline==FALSE) # mdr159，queshi：mdr157
levels(p3$pattern3)
p3$SurvObj <- with(p3, Surv(time, status==1))
fit=survfit(SurvObj~pattern3,data=p3)
plot(fit,col=c("blue3","firebrick3","black"),lty=c(1,2,3),ylab="Cumulative survival function",xlab="Days from MDR-TB treatment initiation to poor outcome",lwd=2,
     mark.time=F,xlim=c(0, 1500), yaxs="r",cex.lab=1.7)
legend("topright",title="Resisitance Pattern",c("MDR-FL","Pre-XDR","XDR"),bty="n", col=c("blue3","firebrick3","black"),lty=c(1,2,3),cex=1.4)
dev.off()
survdiff(p3$SurvObj~p3$pattern3) 

#同上 jpeg--------------Resisitance Pattern -----------------------------------
jpeg(file="KM_pattern3_days.jpg", width=700, height=600, units="px", quality=1000)
par(tcl=-0.25,lwd=1.7, cex.axis=1.5)
par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))
p3=subset(mdr517,secondline==FALSE)
levels(p3$pattern3)
p3$SurvObj <- with(p3, Surv(time, status==1))
fit=survfit(SurvObj~pattern3,data=p3)
plot(fit,col=c("blue3","firebrick3","black"),lty=1,ylab="Cumulative survival function",xlab="Days from MDR-TB treatment initiation to poor outcome",lwd=2,mark.time=F,xlim=c(0, 1500), yaxs="r",cex.lab=1.7)
legend("topright",title="Resisitance Pattern",c("MDR","Pre-XDR","XDR"),bty="n",
       col=c("blue3","firebrick3","darkgreen","black"),lwd=2,lty=1,cex=1.6)
dev.off()
survdiff(p3$SurvObj~p3$pattern3) 

# ----------- Registration Type--------------------------------------------
tiff(file="KM_Registration.tiff", width=9, height=8, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.7, cex.axis=1.5)
par(pin=c(4,3),mar=c(6,6,3,3)+0.1,mgp=c(3.7,1,0))
mdr517$SurvObj <- with(mdr517, Surv(time, status==1))
levels(mdr517$tbregister)
fit=survfit(SurvObj~tbregister,data=mdr517)
plot(fit,col=c("blue3","firebrick3","black","green","gray"),lty=1,ylab="Cumulative survival function",xlab="Days from MDR-TB treatment initiation to poor outcome",lwd=2,
     mark.time=F,xlim=c(0, 1500), yaxs="r",cex.lab=1.7)
legend("topright",title="Registration Type",c("New", "Other", "Relapse", "T.A.D", "T.A.F"),bty="n",col=c("blue3","firebrick3","black","green","gray"),lty=1,cex=1.4)
dev.off()
survdiff(mdr517$SurvObj~mdr517$tbregister) 

# -----------------------------------------------------------------------------
setwd("G:/work/浙江全球基金MDR-TB项目/MDR 治疗")
tiff(file="successful_regimen.tiff", width=9, height=8, units="in", compression="lzw", res=600)
par(tcl=-0.25,lwd=1.7, cex.axis=1.5)
par(pin=c(4,3),mar=c(3,7,4,5)+0.1,mgp=c(3.7,1,0))

se=c(0,0,0,0,0,0)
p=c(0.652,0.683,0.685,0.842,0.643,1.000)
n=c(92,41,219,19,42,2)
for (i in 1:6)
    se[i]=sqrt(p[i]*(1-p[i])/n[i])
x=1:6
plot(x,p,
     ylim=c(0.4,1.0),pch=16,cex=1.5,xlab=NA,xaxt="n",bty="n",
     ylab="Treatment success proportion (%)",cex.lab=1.8)
text(1.26,0.652,"IR",cex=1.4)
text(2.29,0.683,"SR1",cex=1.4)
text(3.29,0.685,"SR2",cex=1.4)
text(4.29,0.842,"SR3",cex=1.4)
text(5.29,0.643,"SR4",cex=1.4)
text(5.71,1.000,"SR5",cex=1.4)
# xaxt="n" # no x axis
# bty="n"  # no 四周边框
regimenlable=c("IR", "SR1","SR2","SR3","SR4","SR5")
# 做标准差的线
arrows(x,p-se,x,p+se,length=0.05,angle=90,code=3)
legend(0.8,0.58,c("IR：Individualized Regimen", 
                  "SR1：6Z Am Lfx Cs Pto/18Z Lfx Cs Pto",
                  "SR2：6Z Am Lfx PAS Pto/18Z Lfx PAS Pto",
                  "SR3：6Z Cm Lfx Cs Pto/18Z Lfx Cs Pto",
                  "SR4：6Z Cm Lfx PAS Pto/18Z Lfx Cs Pt",
                  "SR5：12Cm Mfx PAS Cs Clr Amx/Clv /24Mfx PAS Cs Clr Amx/Clv"),
       bty="n",cex=1.2)
# axis(1,at=seq(1,6,1),label=regimenlable)  
dev.off()
