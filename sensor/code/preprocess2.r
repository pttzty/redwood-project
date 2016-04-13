setwd('../data/')
library(dplyr)
library(ggplot2)

## combine two datasets together, using log data if available net otherwise.
data_net<-read.csv("sonoma-data-net.csv",header=TRUE)
data_log<-read.csv("sonoma-data-log.csv",header=TRUE)
data_all<-rbind(data_log,data_net)
bool_vec<-duplicated(data_all[,c(2,3)])
data_all<-data_all[!bool_vec,]


data_location=read.csv('mote-location-data.txt',header=TRUE,sep='')



data_comb_all<-merge(data_all,data_location,by.x='nodeid',by.y='ID')

## conversion of voltage
vol_net<-data_net[,c('epoch','nodeid','voltage')]
vol_log<-data_log[,c('epoch','nodeid','voltage')]
### 1000 is definitely an outlier.
vol_all<-unique(merge(vol_log,vol_net,by=c('epoch','nodeid')))
colnames(vol_all)<-c('epoch','nodeid','voltage_log','voltage_net')
vol_all<-vol_all[vol_all$voltage_net<1000,]
vol_conversion=lm(vol_all$voltage_log~vol_all$voltage_net)


ggplot(vol_all,aes(y=jitter(voltage_log),
                   x=jitter(voltage_net)))+geom_point(shape=1,color='blue')+geom_smooth(method= lm, color = "red")+
  theme(title=element_text(size = 20),
        axis.title = element_text(size = 19),
        axis.text = element_text(size = 14))+annotate('text',x = 245,y=2.8,label='R Squared: 0.9961',size=8)+labs(x='net voltage',y='log voltage')


## focus only on the interior tree
data_interior<-data_comb_all[data_comb_all$Tree=='interior',]

## transform vol
data_interior$voltage[data_interior$voltage>150]= data_interior$voltage[data_interior$voltage>150]*vol_conversion$coefficients[2]+vol_conversion$coefficients[1]

## Remove missing values
data_interior<-data_interior[is.na(data_interior$humid_temp)==FALSE,]%>%arrange(epoch,nodeid)

## remove unreasonable distance and humidity values, distant values, is the last net value recorded.
data_interior<-filter(data_interior,epoch<=max(data_net$epoch) & Dist<=1 & humid_adj<=100 & humid_adj>=0)


##
conversion_correct<-function(epoch){
  origin<-strptime('2004-04-27 17:05:00',format = "%Y-%m-%d %H:%M:%S")
  result<-origin+5*60*epoch
  return(result)
}
data_interior$actual_time<-conversion_correct(data_interior$epoch)

## remove temperature outliers, referring to the voltage values, seems like should 
## only use the log data for these two nodes.
ggplot(data_interior) + geom_line(mapping=aes(x=actual_time,y=humid_temp,color=as.factor(nodeid)))+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  labs(y='temperature',x='Date',color='nodeid')+annotate('text',x = as.POSIXct("2004-05-10 20:00:00"),y=100,label='node 78',size=5)+
  annotate('text',x = as.POSIXct("2004-06-02 00:00:00"),y=100,label='node 141',size=5)



ggplot(data_interior) + geom_line(mapping=aes(x=actual_time,y=voltage,color=as.factor(nodeid)))+labs(x='Date',color='nodeid')+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  annotate('text',x = as.POSIXct("2004-05-09 00:00:00"),y=1.7,label='node 78',size=5)+
  annotate('text',x = as.POSIXct("2004-05-22 00:00:00"),y=2,label='node 138',size=5)+
  annotate('text',x = as.POSIXct("2004-05-20 00:00:00"),y=-1,label='node 134 and node 141',size=5)


##2
ggplot(data_interior) + geom_point(mapping=aes(x=actual_time,y=as.factor(nodeid),color=voltage<2.4))+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  labs(y='nodeid',x='Date',color='voltage<2.4')
  

ggplot(data_interior) + geom_point(mapping=aes(x=actual_time,y=as.factor(nodeid),color=voltage<2.4))+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  labs(y='nodeid',x='Date',color='voltage<2.4')


tmp<-filter(data_interior,nodeid==78 | nodeid==134 | nodeid==141 |nodeid==138)
### show temp abnormal, select susicpicious ones, only remove 78 and 141 with abnormal values
ggplot(tmp,aes(x=actual_time,y=humid_temp,color=as.factor(nodeid)))+
  geom_line()+labs(color='nodeid')+
  scale_color_manual(breaks=c('78','134','138','141'),values=c('green','blue','black','deeppink'))+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  annotate('text',x = as.POSIXct("2004-06-02 00:00:00"),y=100,label='node 141',size=6)+annotate('text',x = as.POSIXct("2004-05-10 20:00:00"),y=100,label='node 78',size=5)+
  labs(y='temperature',x='Date',color='nodeid')







tmp<-filter(data_interior,nodeid==78 | nodeid==134 | nodeid==141 |nodeid==138)
### show temp abnormal, select susicpicious ones, only remove 78 and 141 with abnormal values
ggplot(NULL,aes(x=actual_time,y=humid_temp))+
  geom_line(data=data_interior[!data_interior$nodeid %in% c(78,134,141,138),],aes(color='other node'),size=3)+
  geom_line(data = tmp,aes(color=as.factor(nodeid)))+
  scale_color_manual(breaks=c('other node','78','134','138','141'),values=c('black','blue','deeppink','green','lightblue'))+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  annotate('text',x = as.POSIXct("2004-06-02 00:00:00"),y=100,label='node 141',size=5)+annotate('text',x = as.POSIXct("2004-05-10 20:00:00"),y=100,label='node 78',size=5)+
  labs(y='temperature',x='Date',color='nodeid')+
  geom_vline(xintercept=as.numeric(conversion_correct(max_78)),linetype=2)+
  geom_vline(xintercept=as.numeric(conversion_correct(max_141)),linetype=2)+
  annotate('text',x=conversion_correct(max_78)-38000,y=117,label='node78 cutoff',size=5,color='red')+
  annotate('text',x=conversion_correct(max_141)-50000,y=117,label='node141 cutoff',size=5,color='red')





### fancy plot
ggplot(tmp) + geom_line(mapping=aes(x=epoch,y=voltage,color=as.factor(nodeid)))+scale_color_manual(breaks=c('78','134','138','141'),values=c('red','blue','green','black'))+labs(color='nodeid')


ggplot(tmp)+geom_point(aes(x=epoch,y=humid_temp,color=voltage,shape=as.factor(nodeid)),size=0.7,alpha=0.7)


max_78=max(data_log[data_log$nodeid==78,]$epoch)
max_141=max(data_log[data_log$nodeid==141,]$epoch)
data_interior=filter(data_interior,nodeid!=78 | epoch<=max_78)
data_interior=filter(data_interior,nodeid!=141 | epoch<=max_141)

ggplot(data_interior) + geom_line(mapping=aes(x=actual_time,y=humid_temp,color=as.factor(nodeid)))+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  labs(y='temperature',x='Date',color='nodeid')




## Conversion of time to the right thing
conversion_time<-function(epoch){
  origin<-strptime('2004-04-28 00:05:00',format = "%Y-%m-%d %H:%M:%S")
  result<-origin+5*60*epoch
  return(result)
}
data_interior$actual_time<-conversion_time(data_interior$epoch)

## correct conversion
conversion_correct<-function(epoch){
  origin<-strptime('2004-04-27 17:05:00',format = "%Y-%m-%d %H:%M:%S")
  result<-origin+5*60*epoch
  return(result)
}
data_interior$actual_time<-conversion_correct(data_interior$epoch)

mean_tmp<-data_interior[data_interior$actual_time>=as.POSIXct("2004-05-01 00:00:00") & data_interior$actual_time<=as.POSIXct("2004-05-02 00:00:00"),]%>%select(hamatop,epoch)%>%group_by(epoch)%>%summarise(Average=mean(hamatop))
ggplot(data_interior) + geom_point(mapping = aes(x = actual_time, y = hamatop, color = as.factor(nodeid))) +
  xlim(as.POSIXct("2004-05-01 00:00:00"),as.POSIXct("2004-05-02 00:00:00")) +
  scale_colour_discrete()+labs(color='nodeid')+  theme(title=element_text(size = 20),axis.title = element_text(size = 17),axis.text = element_text(size = 14))+
  geom_line(mean_tmp,mapping=aes(x=conversion_time(epoch),y=Average),size=2)


###
write.csv(data_interior,file = 'interior_data.csv',row.names = FALSE,quote = FALSE)


#### reread
data_interior<-read.csv('interior_data.csv',header=TRUE)
data_interior$actual_time<-as.POSIXct(data_interior$actual_time)


### different height
mean_tmp<-data_interior[data_interior$actual_time>=as.POSIXct("2004-05-01 00:00:00") & data_interior$actual_time<=as.POSIXct("2004-05-02 00:00:00"),]%>%select(hamatop,epoch)%>%group_by(epoch)%>%summarise(Average=mean(hamatop))
ggplot(data_interior) + geom_point(mapping = aes(x = actual_time, y = hamatop, color = Height)) +
  xlim(as.POSIXct("2004-05-01 00:00:00"),as.POSIXct("2004-05-02 00:00:00")) +
  scale_colour_gradient(high='red',low='green')+labs(color='height')+  theme(title=element_text(size = 20),axis.title = element_text(size = 17),axis.text = element_text(size = 14))+
  geom_line(mean_tmp,mapping=aes(x=conversion_correct(epoch),y=Average),size=2)

#RPAR
mean_tmp_rpar<-data_interior[data_interior$actual_time>=as.POSIXct("2004-05-01 00:00:00") & data_interior$actual_time<=as.POSIXct("2004-05-02 00:00:00"),]%>%select(hamabot,epoch)%>%group_by(epoch)%>%summarise(Average=mean(hamabot))
ggplot(data_interior) + geom_point(mapping = aes(x = actual_time, y = hamabot, color = Height)) +
  xlim(as.POSIXct("2004-05-01 00:00:00"),as.POSIXct("2004-05-02 00:00:00")) +
  scale_colour_gradient(high='red',low='green')+labs(color='height')+  theme(title=element_text(size = 20),axis.title = element_text(size = 17),axis.text = element_text(size = 14))+
  geom_line(mean_tmp_rpar,mapping=aes(x=conversion_correct(epoch),y=Average),size=2)

## temp
ggplot(data_interior) + geom_point(mapping = aes(x = actual_time, y = humid_temp, color = Height)) +
  xlim(as.POSIXct("2004-05-01 00:00:00"),as.POSIXct("2004-05-02 00:00:00")) +
  scale_colour_gradient(high='red',low='green')+labs(color='height')+ labs(y='temperature') +theme(title=element_text(size = 20),axis.title = element_text(size = 17),axis.text = element_text(size = 14))

## humid
ggplot(data_interior) + geom_point(mapping = aes(x = actual_time, y = humid_adj, color = Height)) +
  xlim(as.POSIXct("2004-05-01 00:00:00"),as.POSIXct("2004-05-02 00:00:00")) +
  scale_colour_gradient(high='red',low='green')+labs(color='height')+ labs(y='adjusted humidity') +theme(title=element_text(size = 20),axis.title = element_text(size = 17),axis.text = element_text(size = 14))



#### data exploration
library(reshape2)
agg_interior <- data_interior %>% 
  mutate(daytime = format(actual_time, "%H:%M:%S")) %>%
  select(humid_temp, humid_adj, hamatop, hamabot, Height, daytime) %>%
  melt(id = c("Height", "daytime"))

interior_mean <- dcast(agg_interior %>% filter(variable == "humid_temp"), Height + daytime ~ variable,
                            fun.aggregate = mean)
ggplot(interior_mean) + 
  geom_line(mapping = aes(x = strptime(daytime,format='%H:%M:%S'), y = hamatop,color =Height)) + 
  scale_colour_gradient(high='red',low='green')


#ggplot(interior_mean) + 
#  geom_tile(mapping = aes(x = strptime(daytime,format='%H:%M:%S'), y = as.factor(Height),fill =humid_temp)) + 
#  scale_fill_gradient(high='darked',low='red')