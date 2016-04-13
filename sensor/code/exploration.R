setwd('../data/')
library(dplyr)
library(ggplot2)
data_interior<-read.csv('interior_data.csv',header=TRUE)
data_interior$actual_time<-as.POSIXct(data_interior$actual_time)
library(reshape2)
library(Rmisc)
library(scales)


##histogram for counts
ggplot(data_interior,aes(actual_time,fill=as.factor(Height),order = -as.numeric(Height)))+
  geom_histogram(binwidth=86400*2)+scale_fill_discrete(name="Height")+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  labs(y='Valid Readings Count',x='Date',color='Height')+geom_vline(xintercept=1000000,linetype=2)

#ggplot(data_interior,aes(actual_time,fill=..Height..))
ggplot(data_interior,aes(as.factor(Height)))+
  geom_bar(fill='deeppink')+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  labs(y='Valid Readings Count',x='Height')








conversion_correct<-function(epoch){
  origin<-strptime('2004-04-27 17:05:00',format = "%Y-%m-%d %H:%M:%S")
  result<-origin+5*60*epoch
  return(result)
}

######51
mean_tmp<-data_interior[data_interior$actual_time>=as.POSIXct("2004-05-01 00:00:00") & data_interior$actual_time<=as.POSIXct("2004-05-02 00:00:00"),]%>%select(hamatop,epoch)%>%group_by(epoch)%>%summarise(Average=mean(hamatop))
ggplot(data_interior) + geom_point(mapping = aes(x = actual_time, y = hamatop, color = Height)) +
  xlim(as.POSIXct("2004-05-01 00:00:00"),as.POSIXct("2004-05-02 00:00:00")) +
  scale_colour_gradient(high='red',low='green')+labs(color='height')+  
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  geom_line(mean_tmp,mapping=aes(x=conversion_correct(epoch),y=Average),size=2)+
  labs(x='Hours',y='Incident PAR',color='Height')

#RPAR
mean_tmp_rpar<-data_interior[data_interior$actual_time>=as.POSIXct("2004-05-01 00:00:00") & data_interior$actual_time<=as.POSIXct("2004-05-02 00:00:00"),]%>%select(hamabot,epoch)%>%group_by(epoch)%>%summarise(Average=mean(hamabot))
ggplot(data_interior) + geom_point(mapping = aes(x = actual_time, y = hamabot, color = Height)) +
  xlim(as.POSIXct("2004-05-01 00:00:00"),as.POSIXct("2004-05-02 00:00:00")) +
  scale_colour_gradient(high='red',low='green')+labs(color='height')+ 
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  geom_line(mean_tmp_rpar,mapping=aes(x=conversion_correct(epoch),y=Average),size=2)+
  labs(x='Hours',y='Reflected PAR',color='Height')

## temp
ggplot(data_interior) + geom_point(mapping = aes(x = actual_time, y = humid_temp, color = Height)) +
  xlim(as.POSIXct("2004-05-01 00:00:00"),as.POSIXct("2004-05-02 00:00:00")) +
  scale_colour_gradient(high='red',low='green')+labs(color='Height')+ labs(x='Hours',y='Temperature')+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))
  
## humid
ggplot(data_interior) + geom_point(mapping = aes(x = actual_time, y = humid_adj, color = Height)) +
  xlim(as.POSIXct("2004-05-01 00:00:00"),as.POSIXct("2004-05-02 00:00:00")) +
  scale_colour_gradient(high='red',low='green')+labs(color='Height')+ labs(x='Hours',y='Adjusted humidity')+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))







###neg correlation humid temp; 22.9:node 80; 65.5: node 113
ggplot(data_interior%>%filter(nodeid==80))+
  geom_point(mapping = aes(x = actual_time, y = humid_temp, color = humid_adj))+
  scale_colour_gradient(high='yellow',low='blue')+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  labs(x='Date',y="Temperature",color='Humidity')
  
ggplot(data_interior%>%filter(nodeid==113))+
  geom_point(mapping = aes(x = actual_time, y = humid_temp, color = humid_adj))+
  scale_colour_gradient(high='yellow',low='blue')+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  labs(x='Date',y="Temperature",color='Humidity')

##nodeid 5
ggplot(data_interior%>%filter(Height==52.1))+
  geom_point(mapping = aes(x = actual_time, y = humid_temp, color = humid_adj))+
  scale_colour_gradient(high='yellow',low='blue')+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  labs(x='Date',y="Temperature",color='Humidity')

#### neg correlation scatter plot
p11<-ggplot(data_interior%>%filter(nodeid==80),mapping=aes(x=humid_adj,y=humid_temp,color=hamatop))+
  geom_point(shape=16)+scale_colour_gradient(high='blue',low='orange')+
  geom_smooth(method= lm, color = "red")+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  labs(x='Adjusted Humidity',y="Temperature",color='PAR')
  
p12<-ggplot(data_interior%>%filter(nodeid==113),mapping=aes(x=humid_adj,y=humid_temp,color=hamatop))+
  geom_point(shape=16)+scale_colour_gradient(high='blue',low='orange')+
  geom_smooth(method= lm, color = "red")+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  labs(x='Adjusted Humidity',y="Temperature",color='PAR')

multiplot(p11,p12,cols=2)

ggplot(data_interior%>%filter(nodeid==5),mapping=aes(x=humid_adj,y=humid_temp,color=hamatop))+
  geom_point(shape=16)+scale_colour_gradient(high='blue',low='orange')+
  geom_smooth(method= lm, color = "red")+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  labs(x='Adjusted Humidity',y="Temperature",color='PAR')



### random sample 10000
interior_sample<-sample_n(data_interior,10000)

p13<-ggplot(interior_sample,mapping=aes(x=humid_adj,y=humid_temp,color=hamatop))+
  geom_point(shape=16,size=1.2,alpha=0.8)+scale_colour_gradient(high='blue',low='orange')+
  geom_smooth(method= lm, color = "red")+
  theme(title=element_text(size = 20),strip.text.x=element_text(size=14),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.title=element_text(size=14),legend.text = element_text(size = 14))+
  labs(x='Adjusted Humidity',y="Temperature",color='Incident PAR')



##### multiplot
ggplot(data_interior%>%filter(nodeid==80 | nodeid==113),mapping=aes(x=humid_adj,y=humid_temp,color=hamatop))+
  geom_point(shape=16,size=1.2,alpha=0.8)+facet_grid(.~nodeid)+
  scale_colour_gradient(high='blue',low='orange')+
  geom_smooth(method= lm, color = "red")+
  theme(title=element_text(size = 20),strip.text.x=element_text(size=14),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.title=element_text(size=14),legend.text = element_text(size = 14))+
  labs(x='Adjusted Humidity',y="Temperature",color='Incident PAR')
  
  
p14<-ggplot(interior_sample)+
  geom_point(mapping = aes(x = actual_time, y = humid_temp, color = humid_adj))+
  scale_colour_gradient(high='yellow',low='blue')+
  theme(title=element_text(size = 20),
        axis.title = element_text(size = 19),
        axis.text = element_text(size = 14),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 14),
        legend.margin=unit(0,'cm'))+
  labs(x='Date',y="Temperature",color='Humidity')

multiplot(p14,p13,cols=2)

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_int<-get_upper_tri(cor(data_interior %>% select(Height,humid_temp,humid_adj,hamatop,hamabot)))
melted_cormat <- melt(upper_int, na.rm = TRUE)
levels(melted_cormat$Var1)<-list("Height"="Height",
                                 "Temperature"="humid_temp",
                                 "Adjusted Humidity"="humid_adj",
                                 "Incident PAR"="hamatop",
                                  "Reflected PAR"="hamabot")
levels(melted_cormat$Var2)<-list("Height"="Height",
                                 "Temperature"="humid_temp",
                                 "Adjusted Humidity"="humid_adj",
                                 "Incident PAR"="hamatop",
                                 "Reflected PAR"="hamabot")
## heatmap
qplot(x=Var2, y=Var1, data=melted_cormat, fill=value, geom="tile")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  geom_text(aes(Var2, Var1, label = round(value,digits=3),size=18), color = "black", size = 7)+
  theme(
    axis.title = element_text(size = 19),axis.text = element_text(size = 18),legend.text = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x=element_text(angle=45,vjust=1,hjust=1),
    legend.justification = c(1, 0),
    legend.position = c(0.48, 0.75),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 11, barheight = 2,
                               title.position = "top", title.hjust = 0.5))


### daily cycle
#mean_temp<-data_interior%>%select(humid_temp,epoch)%>%group_by(epoch)%>%summarise(Average=mean(humid_temp))
p1<-ggplot(data_interior) + geom_point(mapping = aes(x = actual_time, y = humid_temp, color = Height),size=0.5) +
  scale_colour_gradient(high='red',low='green')+labs(color='height')+  
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
#  geom_line(mean_temp,mapping=aes(x=conversion_correct(epoch),y=Average),size=0.5)+
  labs(x='Date',y='Temperature',color='Height')+guides(color=FALSE)

#mean_humid<-data_interior%>%select(humid_adj,epoch)%>%group_by(epoch)%>%summarise(Average=mean(humid_adj))
p2<-ggplot(data_interior) + geom_point(mapping = aes(x = actual_time, y = humid_adj, color = Height),size=0.5) +
  scale_colour_gradient(high='red',low='green')+labs(color='height')+  
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  #  geom_line(mean_temp,mapping=aes(x=conversion_correct(epoch),y=Average),size=0.5)+
  labs(x='Date',y='Adjusted humidity',color='Height')

#mean_hama<-data_interior%>%select(hamatop,epoch)%>%group_by(epoch)%>%summarise(Average=mean(hamatop))
p3<-ggplot(data_interior) + geom_point(mapping = aes(x = actual_time, y = hamatop, color = Height),size=0.5) +
  scale_colour_gradient(high='red',low='green')+labs(color='height')+  
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
#  geom_line(mean_hama,mapping=aes(x=conversion_correct(epoch),y=Average),size=0.5)+
  labs(x='Date',y='Incident PAR',color='Height')+guides(color=FALSE)

p4<-ggplot(data_interior) + geom_point(mapping = aes(x = actual_time, y = hamabot, color = Height),size=0.5) +
  scale_colour_gradient(high='red',low='green')+labs(color='height')+  
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  #  geom_line(mean_hama,mapping=aes(x=conversion_correct(epoch),y=Average),size=0.5)+
  labs(x='Date',y='Reflected PAR',color='Height')

multiplot(p1, p3, p2, p4, cols=2)


#####

interior_sub<-data_interior %>% 
  mutate(daytime = format(actual_time, "%H:%M:%S")) %>%
  select(nodeid,humid_temp, humid_adj, hamatop, hamabot, Height,Direc, daytime,actual_time)

day_sub<-interior_sub%>%filter(daytime<='20:00',daytime>='06:00')



ggplot(day_sub%>%filter(nodeid==113),mapping=aes(x=humid_adj,y=humid_temp,color=hamatop))+
  geom_point(shape=16)+scale_colour_gradient(high='blue',low='orange')+
  geom_smooth(method= lm, color = "red")+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  labs(x='Adjusted Humidity',y="Temperature",color='PAR')





cor(interior_sub%>%filter(nodeid==113)[,'hamatop'],interior_sub%>%filter(nodeid==113)[,'humid_adj'])

cor(interior_sub$hamabot,interior_sub$hamatop)


####

mean_tmp<-data_interior%>%select(Height,Direc)%>%group_by(Direc)%>%summarise(Average=mean(Height))

summary(data_interior)
p5<-ggplot(day_sub,aes(x=Direc,y=log(hamatop+1)))+
  geom_boxplot(outlier.size = 0.5, outlier.colour = "#003366",fill='deeppink')+
  labs(x='Direction',y="log(Incdient PAR)")+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))

p6<-ggplot(day_sub,aes(x=Direc,y=Height))+
  geom_boxplot(outlier.size = 0.5, outlier.colour = "#003366",fill='deeppink')+
  labs(x='Direction',y="Height")+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))
multiplot(p5,p6,cols=2)


#####

####
ggplot(interior_sub) + geom_point(mapping=aes(x=actual_time,y=hamabot,color=Height),size=0.4)+scale_colour_gradient(high='red',low='green')+
  theme(title=element_text(size = 20),axis.title = element_text(size = 19),axis.text = element_text(size = 14),legend.text = element_text(size = 14))+
  labs(x='Date',color='nodeid')

##### findings
agg_interior <- data_interior %>% 
  mutate(daytime = format(actual_time, "%H:%M:%S")) %>%
  select(humid_temp, humid_adj, hamatop, hamabot, Height, daytime) %>%
  melt(id = c("Height", "daytime"))



interior_mean <- dcast(agg_interior %>% filter(variable == "hamatop"), Height + daytime ~ variable,
                       fun.aggregate = mean)
p7<-ggplot(interior_mean) + 
  geom_tile(mapping = aes(x = strptime(daytime,format='%H:%M:%S',tz='PST'), fill = hamatop,y =as.factor(Height))) + 
  scale_fill_gradient(low='orange',high='blue') +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST'))+
  labs(x='Hours',y='Height',fill='Incident PAR')+
  theme(title=element_text(size = 20),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title=element_text(size=19),
        axis.title.x=element_blank())





# dcast(interior_mean, daytime ~ hamatop,
#       fun.aggregate = mean)


tmp_mean_1<-data_interior %>% 
  mutate(daytime = format(actual_time, "%H:%M:%S")) %>%
  select(humid_temp, humid_adj, hamatop, hamabot, Height, daytime)%>%
  group_by(daytime)%>%summarise(Average=mean(hamatop))

interior_mean <- dcast(agg_interior %>% filter(variable == "hamatop"), Height + daytime ~ variable,
                       fun.aggregate = mean)
p8<-ggplot(interior_mean) + 
  geom_point(mapping = aes(x = strptime(daytime,format='%H:%M:%S',tz='PST'), color = Height,y =hamatop),size=0.7) + 
  scale_color_gradient(low='green',high='red') +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST'))+
  labs(x='Hours',y='Incident PAR',color='Height')+
  theme(title=element_text(size = 20),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title=element_text(size=20),
        axis.title.x=element_blank())+
  geom_line(tmp_mean_1,mapping=aes(x=strptime(daytime,format='%H:%M:%S',tz='PST'),y=Average),size=1.5)


### hamabot
interior_mean <- dcast(agg_interior %>% filter(variable == "hamabot"), Height + daytime ~ variable,
                       fun.aggregate = mean)
p9<-ggplot(interior_mean) + 
  geom_tile(mapping = aes(x = strptime(daytime,format='%H:%M:%S',tz='PST'), fill = hamabot,y =as.factor(Height))) + 
  scale_fill_gradient(high='blue',low='orange') +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST'))+
  labs(x='Hours',y='Height',fill='Reflected PAR')+
  theme(title=element_text(size = 20),axis.title = element_text(size = 22),axis.text = element_text(size = 16),legend.text = element_text(size = 16),legend.title=element_text(size=19))

tmp_mean_2<-data_interior %>% 
  mutate(daytime = format(actual_time, "%H:%M:%S")) %>%
  select(humid_temp, humid_adj, hamatop, hamabot, Height, daytime)%>%
  group_by(daytime)%>%summarise(Average=mean(hamabot))

interior_mean <- dcast(agg_interior %>% filter(variable == "hamabot"), Height + daytime ~ variable,
                       fun.aggregate = mean)



p10<-ggplot(interior_mean) + 
  geom_point(mapping = aes(x = strptime(daytime,format='%H:%M:%S',tz='PST'), color = Height,y =hamabot),size=0.7) + 
  scale_color_gradient(low='green',high='red') +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST'))+
  labs(x='Hours',y='Reflected PAR',color='Height')+
  theme(title=element_text(size = 20),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title=element_text(size=20))+
  geom_line(tmp_mean_2,mapping=aes(x=strptime(daytime,format='%H:%M:%S',tz='PST'),y=Average),size=1.5)

multiplot(p7,p9,cols=1)
multiplot(p8,p10,cols=1)



###finding 2: divide into three periods
get_period <- function(epoch){
  if(epoch<1811){return('04/27 - 05/03')}
  else{
    if(epoch<8147){return('05/03 - 05/25')}
    else{return('05/26 - 06/02')}
  }
}
period_data<-data_interior%>%
  mutate(daytime = format(actual_time, "%H:%M:%S"),period=sapply(epoch,get_period))%>%
  select(humid_temp, humid_adj, hamatop, hamabot, Height, daytime, period)%>%
  melt(id = c("Height", "period", "daytime"))


period_temp_mean<-dcast(period_data%>% filter(variable == "humid_temp"), 
                        Height + period + daytime ~ variable,
                        fun.aggregate = mean)

p15<-ggplot(period_temp_mean) + 
  geom_point(mapping = aes(x = strptime(daytime,format='%H:%M:%S',tz='PST'), y = humid_temp,color=Height),size=0.5) + 
  scale_color_gradient(high='red',low='green')+
  facet_grid(.~period)+scale_x_datetime(breaks=date_breaks('2 hour'),
                                        labels=date_format('%H:%M', tz = 'PST'))+
  labs(x='Hours',y='Temperature',color='Height')+
  theme(title=element_text(size = 20),
        axis.title = element_text(size = 19),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(size=16),
        strip.text.x=element_text(size=16),
        axis.text.x=element_text(angle = 90),
        axis.title.x=element_blank())

period_humid_mean<-dcast(period_data%>% filter(variable == "humid_adj"), 
                        Height + period + daytime ~ variable,
                        fun.aggregate = mean)
p16<-ggplot(period_humid_mean) + 
  geom_point(mapping = aes(x = strptime(daytime,format='%H:%M:%S',tz='PST'), y = humid_adj,color=Height),size=0.5) + 
  scale_color_gradient(high='red',low='green')+
  facet_grid(.~period)+scale_x_datetime(breaks=date_breaks('2 hour'),
                                        labels=date_format('%H:%M', tz = 'PST'))+
  labs(x='Hours',y='Adjusted Humidity',color='Height')+
  theme(title=element_text(size = 20),
        axis.title = element_text(size = 19),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(size=16),
        strip.text.x=element_text(size=16),
        axis.text.x=element_text(angle = 90))

multiplot(p15,p16,cols=1)


####app
interior_mean <- dcast(agg_interior %>% filter(variable == "humid_temp"), Height + daytime ~ variable,
                       fun.aggregate = mean)
p20<-ggplot(interior_mean) + 
  geom_tile(mapping = aes(x = strptime(daytime,format='%H:%M:%S',tz='PST'), fill = humid_temp,y =as.factor(Height))) + 
  scale_fill_gradient(low='brown',high='gold1') +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST'))+
  labs(x='Hours',y='Height',fill='Temperature')+
  theme(title=element_text(size = 20),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(size=17),
        axis.text.x=element_text(angle = 90),
        legend.margin=unit(0,'cm'))
interior_mean <- dcast(agg_interior %>% filter(variable == "humid_adj"), Height + daytime ~ variable,
                       fun.aggregate = mean)
p21<-ggplot(interior_mean) + 
  geom_tile(mapping = aes(x = strptime(daytime,format='%H:%M:%S',tz='PST'), fill = humid_adj,y =as.factor(Height))) + 
  scale_fill_gradient(low='blue',high='yellow') +
  scale_x_datetime(breaks=date_breaks('2 hour'),
                   labels=date_format('%H:%M', tz = 'PST'))+
  labs(x='Hours',y='Height',fill='Humidity')+
  theme(title=element_text(size = 20),
        axis.text.x=element_text(angle = 90),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(size=17),
        legend.margin=unit(0,'cm'))
multiplot(p20,p21,cols=2)
