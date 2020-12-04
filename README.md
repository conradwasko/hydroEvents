# hydroEvents


# EXAMPLE 1

```R
######### Aim: to see how different event methods affect C-Q relationship #######################
############# best illustrated with continuous water quality data ###############################

#### site that has >5y recent continuous data in both EC and flow - one used for illustration ###
######### raw data - selected a site from VIC DELWP https://data.water.vic.gov.au/ ##############
#### 232210 ####

EC = read.csv("232210.Conductivity.csv",skip=2)
Q = read.csv("232210.MeanWaterFlow.csv",skip=2)

head(EC)
head(Q)

EC$Mean[which(EC$Qual>152)] = NA
Q$Mean[which(Q$Qual>152)] = NA

ECzoo = zoo(EC$Mean,as.Date(strptime(substr(EC$Date,10,20),"%d/%m/%Y")))
Qzoo = zoo(Q$Mean/83,as.Date(strptime(substr(Q$Date,10,20),"%d/%m/%Y")))

library(hydroEvents)
library(zoo)
source("R/newfunctions_DG_20201123.R")

###### select a one-year period to illustrate - based on water year??? (I only did eyeballing) #####
selstart=as.Date("2018-05-01");selend=as.Date("2019-04-30")
ECzoo = ECzoo[which(time(ECzoo)>=selstart&time(ECzoo)>=selend)]
Qzoo = Qzoo[which(time(Qzoo)>=selstart&time(Qzoo)>=selend)]

plot(ECzoo)
plot(Qzoo)

plot(ECzoo~Qzoo)

CQ = merge.zoo(ECzoo,Qzoo)
Qvec = as.vector(CQ$Qzoo)

###### compare CQ-event partition with different event apporoaches - BF and Minima tested
# a function to plot CQ with different colours by event periods (rising, falling and BF)
CQ_event = function(C,Q,event_method,methodname) {
  risfal_res = limbs(data=as.vector(Q),events=event_method)
  RL_ind = FL_ind = ev_ind = NULL
  for(i in 1:nrow(risfal_res)) {
    RL_ind = c(RL_ind,risfal_res$ris.srt[i]:risfal_res$ris.end[i])
    FL_ind = c(FL_ind,risfal_res$fal.srt[i]:risfal_res$fal.end[i])
    ev_ind = c(ev_ind,risfal_res$srt[i]:risfal_res$end[i])
  }

  BF_ind = as.vector(1:length(as.vector(Q)))[-ev_ind]
  plot(C~Q,xlab="Q (mm/d)",ylab="C (mg/L)",main = paste("C-Q relationship -",methodname),pch=20)
  points(C[RL_ind]~Q[RL_ind],col="blue",pch=20)
  points(C[FL_ind]~Q[FL_ind],col="green",pch=20)
  points(C[BF_ind]~Q[BF_ind],col="red",pch=20)
  legend("topright",legend=c("rising limb","falling limb","baseflow"),pch=20,col=c("blue","green","red"))

  CQ = merge.zoo(ECzoo,Qzoo)
  res = list(event=ev_ind,base=BF_ind,rising=RL_ind,falling=FL_ind,
             eventCQ=CQ[ev_ind,],baseCQ=CQ[BF_ind,],
             risingCQ=CQ[RL_ind,],fallingCQ=CQ[FL_ind,])

}

# test with different event methods: BF and Min
BF_res = eventBaseflow(Qvec)
plotevents(data=Qvec,events=BF_res)
limbs(data=Qvec,events=BF_res)

Min_res = eventMinima(Qvec, delta.x = 20, delta.y = 100)
plotevents(data=Qvec,events=Min_res)
limbs(data=Qvec,events=Min_res) # We used different indexing approaches which is why the peaks looks funny here - need to discuss

# Final plot on CQ comparison from two event approaches
par(mfrow=c(2,2))
par(mar=c(2,2,2,1))
CQ_event(ECzoo,Qzoo,BF_res,"Baseflow method")
CQ_event(ECzoo,Qzoo,Min_res,"LocalMinima method")

```
## EXAMPLE 2
