##The following code conducts a GAM analysis depth and activity of yelloweye rockfish monitored in Hood Canal##

## The following code adds physical covariates (DO) to depth and activity GAM analyses ##


####################################################################
################## GAM ANALYSIS of TELEMETRY DATA ##################
####################################################################
library(ggplot2)
library(qpcR)
library(mgcv)
library(voxel)
library(tidyr)

# Creating SeqD
YE.combined.dat$decimal.time <- decimal_date(YE.combined.dat$Date.and.Time..PST.PDT.)
YE.combined.dat$Seq_DecD <- YE.combined.dat$decimal.time - min(YE.combined.dat$decimal.time)

# Renaming dataframes
Depth <- YE.combined.dat %>%
  filter(Sensor.Unit == "m")

Accel <- YE.combined.dat %>%
  filter(Sensor.Unit == "m/s²")

# renaming column headings
Depth$Depth <- Depth$Sensor.Value
Depth$Month <- month(Depth$Date.and.Time..PST.PDT.)
Depth$FISHID <- Depth$Transmitter
Depth$HR <- hour(Depth$Date.and.Time..PST.PDT.)

Accel$Month <- month(Accel$Date.and.Time..PST.PDT.)
Accel$Accel <- Accel$Sensor.Value
Accel$FISHID <- Accel$Transmitter
Accel$HR <- hour(Accel$Date.and.Time..PST.PDT.)

# Big Datset
Depth$Depth=abs(Depth$Depth)*(-1)
Depth$Month=as.factor(Depth$Month)
Depth$FISHID=as.factor(Depth$FISHID)

Accel$Month=as.factor(Accel$Month)
Accel$FISHID=as.factor(Accel$FISHID)



#Depth Analysis
hist(Depth$Depth)
# add site in
# remove hour- add day/night variable
rel=glmr(Depth~ Month+(1 | FISHID), data=Depth)

re0=gam(Depth~s(HR,by=Month,bs="cc")+Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)
re1i=gam(Depth~s(HR,bs="cc")+Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)
re1m=gam(Depth~s(HR,by=Month,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)
re2h=gam(Depth~s(HR,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)
re2m=gam(Depth~Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)
re3=gam(Depth~s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)

re4=gam(Depth~s(HR,by=Month,bs="cc")+Site+Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)

x=cbind(AIC(re0),AIC(re1i),AIC(re1m),AIC(re2h),AIC(re2m),AIC(re3))
akaike.weights(x)

r0=gam(Depth~s(HR,by=Month,bs="cc")+Month,correlation=corAR1(form = Seq_DecD | 1), data=Depth)
r1i=gam(Depth~s(HR,bs="cc")+Month,correlation=corAR1(form = Seq_DecD | 1), data=Depth)
r1m=gam(Depth~s(HR,by=Month,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)
r2h=gam(Depth~s(HR,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Depth)
r2m=gam(Depth~Month,correlation=corAR1(form = Seq_DecD | 1), data=Depth)
r3=gam(Depth~1,correlation=corAR1(form = Seq_DecD | 1), data=Depth)

x=cbind(x,AIC(r0),AIC(r1i),AIC(r1m),AIC(r2h),AIC(r2m),AIC(r3))
akaike.weights(x)


# plotting 
summary(re0)
par(mfrow=c(2,2))
plotGAM(gamFit=re0,smooth.cov="HR",groupCovs="O2",rawOrFitted = "FALSE",color="Black")+
  facet_wrap(~O2)+xlab('Hour of Day')+
  ylab('Depth (m)')+ggtitle('')
validgam(model=re0,count=Depth$Depth)

resids<-resid(re0,"pearson")
d1=data.frame(cbind(resids,Depth$HR))
colnames(d1)=c('Residuals','Hour')
ggplot(data=d1,aes(x=Hour,y=Residuals))+geom_point()


#Acceleration Analysis
hist(Accel$Sensor.Value)
Accel$LogAccel <- log(Accel$Sensor.Value+1)
hist(Accel$LogAccel)
# think about zero inflated models for accel data


re0=gam(log(Accel+1)~s(HR,by=Month,bs="cc")+Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
re1i=gam(log(Accel+1)~s(HR,bs="cc")+Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
re1m=gam(log(Accel+1)~s(HR,by=Month,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
re2h=gam(log(Accel+1)~s(HR,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
re2m=gam(log(Accel+1)~Month+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
re3=gam(log(Accel+1)~s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)

x=cbind(AIC(re0),AIC(re1i),AIC(re1m),AIC(re2h),AIC(re2m),AIC(re3))
akaike.weights(x)

r0=gam(log(Accel+1)~s(HR,by=Month,bs="cc")+Month,correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r1i=gam(log(Accel+1)~s(HR,bs="cc")+Month,correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r1m=gam(log(Accel+1)~s(HR,by=Month,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r2h=gam(log(Accel+1)~s(HR,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r2m=gam(log(Accel+1)~Month,correlation=corAR1(form = Seq_DecD | 1), data=Accel)
r3=gam(log(Accel+1)~1,correlation=corAR1(form = Seq_DecD | 1), data=Accel)

x=cbind(x,AIC(r0),AIC(r1i),AIC(r1m),AIC(r2h),AIC(r2m),AIC(r3))
a=akaike.weights(x)



#Depth Analysis O2

q$Depth <- q$Sensor.Value
q$Month <- month(q$Date.and.Time..PST.PDT.)
q$FISHID <- q$Transmitter
q$HR <- hour(q$Date.and.Time..PST.PDT.)
q$O2 <- as.factor(q$O2)
q$Depth=abs(q$Depth)*(-1)
q$Month=as.factor(q$Month)
q$FISHID=as.factor(q$FISHID)

q1=subset(q,O2!="NA")
q1=subset(q,O2=="Normoxia" | O2=="Hypoxia")
hist(q$Depth)

re0=gam(Depth~s(HR,by=O2,bs="cc")+O2+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=q)
re1i=gam(Depth~s(HR,bs="cc")+O2+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=q)
re1m=gam(Depth~s(HR,by=O2,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=q)
re2h=gam(Depth~s(HR,bs="cc")+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=q)
re2m=gam(Depth~O2+s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=q)
re3=gam(Depth~s(FISHID,bs="re"),correlation=corAR1(form = Seq_DecD | 1), data=q)

x=cbind(AIC(re0),AIC(re1i),AIC(re1m),AIC(re2h),AIC(re2m),AIC(re3))
akaike.weights(x)

r0=gam(Depth~s(HR,by=O2,bs="cc")+O2,correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
r1i=gam(Depth~s(HR,bs="cc")+O2,correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
r1m=gam(Depth~s(HR,by=O2,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
r2h=gam(Depth~s(HR,bs="cc"),correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
r2m=gam(Depth~O2,correlation=corAR1(form = Seq_DecD | 1), data=Depth1)
r3=gam(Depth~1,correlation=corAR1(form = Seq_DecD | 1), data=Depth1)


