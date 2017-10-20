# allmdr处理
allmdr=read.csv("G:/work/MDR/数据/allmdr824.csv")
names(allmdr)
names(allmdr)[35]<-"是否纳入GFP治疗" #GFP global fund project全球基金
names(allmdr)[36]<-"未纳入GFP治疗原因"

#----------------------------------------------------------------------------
# 杨都云无治疗信息，无转归信息，无是否纳入GF信息
对原始数据allmdr的修改
allmdr$是否纳入GFP治疗[allmdr$姓名=="杨都云"]<-"否"

# 停止治疗时间 (df20092013)其他 49个
# 用 停止治疗日期(allmdr) 填补16个后，
# 还有33个不知道转归