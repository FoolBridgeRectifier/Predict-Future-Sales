library(ggplot2)
library(dplyr)
# rm(list = ls())
shop = read.csv("shops.csv")
item = read.csv("items.csv")
sales = read.csv("sales_train_v2.csv")
cat = read.csv("item_categories.csv")
# Merging Datasets
out=merge(sales,item,by="item_id")
out=select(out,-c(item_name))

# Eliminating Missing Values
out=out[out$item_cnt_day>=0,]
out=out[out$item_price!=-1,]
out2=out


# Eliminating Extreme Outliers.
out=out[out$item_cnt_day<200,]

# Plotting Outliers
plot(out$item_cnt_day,out$date_block_num)
plot(out2$item_cnt_day,out2$date_block_num)

# Getting Date and Day values
out$date=as.character(out$date)
month = substr(out$date, start = 4, stop = 5)
month=as.numeric(month)
out=cbind(out,month)

# Aggregating per month
outmon=aggregate(out$item_cnt_day, list(out$item_id,out$shop_id,out$date_block_num,out$item_category_id,out$month,out$item_price), sum)
colnames(outmon)=c("item_id","shop_id","date_block_num","item_category_id","month","item_price","item_cnt_month")
train=outmon[outmon$date_block_num!=33,]
test=outmon[outmon$date_block_num==33,]
outmonstor=outmon

# Creating Features
# Average according to respective features
outmon=outmonstor
itemavg = aggregate(outmon[outmon$date_block_num!=33,]$item_cnt_month,list(outmon[outmon$date_block_num!=33,]$item_id),mean)
colnames(itemavg)=c("item_id","itemavg")
outmon=merge(outmon,itemavg,by="item_id")

len=length(unique(out$item_id))
monavg = aggregate(outmon[outmon$date_block_num!=33,]$item_cnt_month,list(outmon[outmon$date_block_num!=33,]$month),sum)
colnames(monavg)=c("month","monavg")
monavg$monavg=monavg$monavg/len
outmon=merge(outmon,monavg,by="month")


len=length(unique(out$item_id))
shopavg = aggregate(outmon[outmon$date_block_num!=33,]$item_cnt_month,list(outmon[outmon$date_block_num!=33,]$shop_id),sum)
colnames(shopavg)=c("shop_id","shopavg")
shopavg$shopavg=shopavg$shopavg/len
outmon=merge(outmon,shopavg,by="shop_id")

len=length(unique(out$item_id))
costavg = aggregate(outmon[outmon$date_block_num!=33,]$item_cnt_month,list(outmon[outmon$date_block_num!=33,]$item_price),sum)
colnames(costavg)=c("item_price","costavg")
costavg$costavg=costavg$costavg/len
outmon=merge(outmon,costavg,by="item_price")


catavg = aggregate(outmon[outmon$date_block_num!=33,]$item_cnt_month,list(outmon[outmon$date_block_num!=33,]$item_category_id),sum)
colnames(catavg)=c("item_category_id","catavg")
for(i in nrow(catavg)){
  
  catavg$catavg=catavg$catavg/length(unique(out[out$item_category_id==catavg[i,]$item_category_id,]$item_id))
}
outmon=merge(outmon,catavg,by="item_category_id")

# Decide the order based on above averages
itemavg = aggregate(outmon[outmon$date_block_num!=33,]$item_cnt_month,list(outmon[outmon$date_block_num!=33,]$item_id),mean)
colnames(itemavg)=c("item_id","itemord")
itemavg=itemavg[order(itemavg$itemord) , ]
itemavg$itemord=c(1:nrow(itemavg))
outmon=merge(outmon,itemavg,by="item_id")

len=length(unique(out$item_id))
monavg = aggregate(outmon[outmon$date_block_num!=33,]$item_cnt_month,list(outmon[outmon$date_block_num!=33,]$month),sum)
colnames(monavg)=c("month","monord")
monavg$monord=monavg$monord/len
monavg=monavg[order(monavg$monord) , ]
monavg$monord=c(1:nrow(monavg))
outmon=merge(outmon,monavg,by="month")


len=length(unique(out$item_id))
shopavg = aggregate(outmon[outmon$date_block_num!=33,]$item_cnt_month,list(outmon[outmon$date_block_num!=33,]$shop_id),sum)
colnames(shopavg)=c("shop_id","shopord")
shopavg$shopord=shopavg$shopord/len
shopavg=shopavg[order(shopavg$shopord) , ]
shopavg$shopord=c(1:nrow(shopavg))
outmon=merge(outmon,shopavg,by="shop_id")

len=length(unique(out$item_id))
costavg = aggregate(outmon[outmon$date_block_num!=33,]$item_cnt_month,list(outmon[outmon$date_block_num!=33,]$item_price),sum)
colnames(costavg)=c("item_price","costord")
costavg$costord=costavg$costord/len
costavg=costavg[order(costavg$costord) , ]
costavg$costord=c(1:nrow(costavg))
outmon=merge(outmon,costavg,by="item_price")


catavg = aggregate(outmon[outmon$date_block_num!=33,]$item_cnt_month,list(outmon[outmon$date_block_num!=33,]$item_category_id),sum)
colnames(catavg)=c("item_category_id","catord")
for(i in nrow(catavg)){
  
  catavg$catord=catavg$catord/length(unique(out[out$item_category_id==catavg[i,]$item_category_id,]$item_id))
}
catavg=catavg[order(catavg$catord) , ]
catavg$catord=c(1:nrow(catavg))
outmon=merge(outmon,catavg,by="item_category_id")

# Normalizing values
outnorm=outmon
outnorm$count_1=as.integer(outnorm$item_cnt_month%%10) # Seperated to 100s, 10s and 1s for easier classification
outnorm$count_10=as.integer(outnorm$item_cnt_month/10)%%10
outnorm$count_100=as.integer(outnorm$item_cnt_month/100)
outnorm$shopavg=(outnorm$shopavg*100000)/max(outnorm$shopavg)
outnorm$catavg=(outnorm$catavg*100000)/max(outnorm$catavg)
outnorm$monavg=(outnorm$monavg*100000)/max(outnorm$monavg)
outnorm$costavg=(outnorm$costavg*100000)/max(outnorm$costavg)
outnorm$itemavg=(outnorm$itemavg*100000)/max(outnorm$itemavg)
trainnorm=outnorm[outnorm$date_block_num!=33,]
testnorm=outnorm[outnorm$date_block_num==33,]
testnormstor=testnorm

# Model 1 - Random Forest
library(randomForest)
require(caTools)
r1=proc.time()
dectrain=trainnorm[trainnorm$date_block_num>30,] # For best results we took from month 30 to 32
dectrain=select(dectrain,-c(item_cnt_month,count_1,count_100,itemord,catord,monord,shopord,costord,item_category_id,item_price,shop_id,month,item_id,date_block_num))
rf2 <- randomForest(
  count_10 ~ .,
  data=dectrain
)
dectrain=trainnorm[trainnorm$date_block_num>30,]
dectrain=select(dectrain,-c(item_cnt_month,count_1,count_10,itemord,catord,monord,shopord,costord,item_category_id,item_price,shop_id,month,item_id,date_block_num))
rf3 <- randomForest(
  count_100 ~ .,
  data=dectrain
)

dectrain=trainnorm[trainnorm$date_block_num>30,]
dectrain=select(dectrain,-c(item_cnt_month,count_10,count_100,count_100,itemord,catord,monord,shopord,costord,item_category_id,item_price,shop_id,month,item_id,date_block_num))
rf5 <- randomForest(
  count_1 ~ .,
  data=dectrain
)
r2=proc.time()


output=testnorm
dectest=testnorm
dectest=select(dectest,-c(item_cnt_month,count_1,count_10,count_100,item_category_id,item_price,shop_id,month,item_id,date_block_num))
pred100 = round(predict(rf3, newdata=dectest))
output=cbind(output,pred100)

dectest=testnorm
dectest=select(dectest,-c(item_cnt_month,count_1,count_10,count_100,item_category_id,item_price,shop_id,month,item_id,date_block_num))
pred10 = round(predict(rf2, newdata=dectest))
output=cbind(output,pred10)

dectest=testnorm
dectest=select(dectest,-c(item_cnt_month,count_1,count_10,count_100,item_category_id,item_price,shop_id,month,item_id,date_block_num))
pred1 = round(predict(rf5, newdata=dectest))
output=cbind(output,pred1)


output$pred100=output$pred100*100
output$pred10=output$pred10*10
pred=output$pred1+output$pred10+output$pred100
output=cbind(output,pred)

# random forest output
x=aggregate(output$item_cnt_month, list(output$item_id,output$shop_id), sum)
y=aggregate(output$pred, list(output$item_id,output$shop_id), sum)
plot(x[,3],type = 'l',col="red",xlab="item_id",ylab="count",main="Random Forest Actual Output")
plot(y[,3],type = 'l',col="blue",xlab="item_id",ylab="count",main="Random Forest Predicted Output")
# MSE and RMSE
mse1=x[,3]-y[,3]
derr=mse1
dmeanerr=mse1
mse1=mse1^2
len=length(mse1)
dmeanerr=sum(dmeanerr)/len
mse1=sum(mse1)/len
rmse=sqrt(mse1)



# Model 2 - KNN
library(class)
rmsestor=10
r3=proc.time()
month=22 # For best results we took from month 22 to 32
knnout=outnorm
knntrain=knnout[knnout$date_block_num!=33&knnout$date_block_num>month,13:17]
knncl1=knnout[knnout$date_block_num!=33&knnout$date_block_num>month,18]
knntest=knnout[knnout$date_block_num==33,13:17]
kpred1=knn(knntrain,knntest,cl=knncl1,k=length(unique(knncl1)))

knntrain=knnout[knnout$date_block_num!=33&knnout$date_block_num>month,13:17]
knncl10=knnout[knnout$date_block_num!=33&knnout$date_block_num>month,19]
knntest=knnout[knnout$date_block_num==33,13:17]
kpred10=knn(knntrain,knntest,cl=knncl10,k=length(unique(knncl10)))

knntrain=knnout[knnout$date_block_num!=33&knnout$date_block_num>month,13:17]
knncl100=knnout[knnout$date_block_num!=33&knnout$date_block_num>month,20]
knntest=knnout[knnout$date_block_num==33,13:17]
kpred100=knn(knntrain,knntest,cl=knncl100,k=length(unique(knncl100)))
output=knnout[knnout$date_block_num==33,]
kpred=as.numeric(as.character(kpred1))+(as.numeric(as.character(kpred10))*10)+(as.numeric(as.character(kpred100))*100)
koutput=cbind(output,kpred1,kpred10,pred100,kpred)
r4=proc.time()

# Output KNN
kx=aggregate(koutput$kpred, list(koutput$item_id,koutput$shop_id), sum)
ky=aggregate(koutput$item_cnt_month, list(koutput$item_id,koutput$shop_id), sum)
plot(ky[,3],type = 'l',col="red",xlab="item_id",ylab="count",main="KNN Actual Output")
plot(kx[,3],type = 'l',col="blue",xlab="item_id",ylab="count",main="KNN Predicted Output")
#  MSE and RMSE
kmse=kx[,3]-ky[,3]
kerr=kmse
kmeanerr=kmse
kmse=kmse^2
klen=length(kmse)
kmeanerr=sum(kmeanerr)/klen
kmse=sum(kmse)/klen
krmse=sqrt(kmse)


# Random Forest MSE
print(mse1)
# KNN MSE
print(kmse)
# Random Forest RMSE
print(rmse)
# KNN RMSE
print(krmse)
# Random Forest Time taken in seconds
print((r2-r1)[3])
# KNN Time taken in seconds
print((r4-r3)[3])
# T value
var=sum(((kerr-derr)-kmeanerr-dmeanerr)^2)/klen
t = (dmeanerr-kmeanerr)/sqrt((var)/klen)
print(t)
# T table value for 394 dof and 95 percent is 1.984
# by table p value is 1.645 for 90 percent
# 1.282 for 80 percent
t.test(kerr, derr, paired = TRUE, alternative = "two.sided",conf.level = 0.95,var.equal = TRUE)
# by R t.test p is 2.2e-16
# T value is greater
# Hence we ignore null hypothesis
# MODEL 1 and 2 are significantly different
