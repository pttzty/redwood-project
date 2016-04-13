setwd('../data/')
library(dplyr)
library(ggplot2)
data_all<-read.csv("sonoma-data-all.csv",header=TRUE,stringsAsFactors=FALSE)
data_location=read.csv('mote-location-data.txt',header=TRUE,stringsAsFactors=FALSE,sep='')
data_all$Tree<-as.factor(data_all$Tree)
data_all$Direc<-as.factor(data_all$Direc)
## remove strange nodeid values
data_all<-data_all[data_all$nodeid!=135 & data_all$nodeid!=65535 & data_all$nodeid!=100,]
data_comb_all<-merge(data_all,data_location,by.x='nodeid',by.y='ID')
###remove distant nodes
### remove NA because all NAs in one row
## remove after june 2, 
## first remove humidity out of range (100 and 0), unreasonable percentage values


## unreasonable humd_adj
data_comb_all<-filter(data_comb_all,epoch<=10288 & Dist<=1 & humid_adj!="NA" & humid_adj<=100 & humid_adj>=0)

### How to remove outliers, ggplot
abnormal_voltage_id<-filter(data_comb_all,voltage<2.4)[,c('nodeid','epoch')]
ggplot(data_comb_all) + geom_line(mapping=aes(x=epoch,y=humid_adj,color=as.factor(nodeid)))+ylim(-5,105)

tmp<-filter(data_comb_all,voltage<100)
ggplot(tmp) + geom_line(mapping=aes(x=epoch,y=humid_temp,color=as.factor(nodeid)))

