# hydroEvents
Insert description of package here

## Example 1
Aim: To see how different event methods affect C-Q relationship 

```R
# Demonstration of two different methods
srt = as.Date("2018-05-01")
end = as.Date("2019-04-30")
EC = data232210$EC$Mean[which(data232210$EC$Date >= srt & data232210$EC$Date <= end)]
Q  = data232210$Q$Mean[which(data232210$Q$Date >= srt & data232210$Q$Date <= end)]

BF_res = eventBaseflow(Q)
plotEvents(data = Q, events = BF_res)
limbs(data = Q, events = BF_res)

MIN_res = eventMinima(Q, delta.x = 20, delta.y = 100)
plotEvents(data = Q, events = MIN_res)
limbs(data = Q, events = MIN_res) #
```

```R

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


# Final plot on CQ comparison from two event approaches
par(mfrow=c(2,2))
par(mar=c(2,2,2,1))
CQ_event(ECzoo,Qzoo,BF_res,"Baseflow method")
CQ_event(ECzoo,Qzoo,Min_res,"LocalMinima method")

```
## EXAMPLE 2

```R
# TESTS
library(hydroEvents)
library(zoo)
data("dataBassRiver")
length(dataBassRiver)
fakedate = seq(as.Date("2000-01-01"),as.Date("2000-01-01")+66,by="days")
Basszoo = zoo(dataBassRiver,fakedate)
plot(dataBassRiver)
plot(Basszoo)

source("R/newfunctions_DG_20201027.R")
#source("C:/Users/danlug/OneDrive - The University of Melbourne/Potential/EventPackage/hydroEvents001/R/newfunctions_DG_20201027.R")
# ALL EVENT METHODS SO FAR
Max_res = eventMaxima(dataBassRiver, delta.y = 200, delta.x = 1)
Min_res = eventMinima(dataBassRiver, delta.x = 5, delta.y = 20)
PoT_res = eventPOT(dataBassRiver, threshold = 0, min.diff = 1)
BF_res = eventBaseflow(dataBassRiver, BFI_Th = 0.5, min.diff = 1, threshold = 0)


# added function - smoothing
# flexible parameters are nsteps to smooth over, and weighting for the middle value (with all other weightings to be 1)
# DEMO
plot(dataBassRiver,type="o")
lines(smoothing(dataBassRiver,nstep=1),type="o",col="RED") # 1+1+1 = 3d MA
lines(smoothing(dataBassRiver,nstep=2),type="o",col="blue") # 2+1+2 = 5d MA

# added function - plotevents
# plot events in two diff ways - line over or shaded
# DEMO
plotevents(data=dataBassRiver,events=Min_res)
plotevents(data=dataBassRiver,events=Min_res,type="bound")

# updated function - baseFlow_DG with BFI
# DEMO
plot(dataBassRiver,type="o")
baseFlow_DG(dataBassRiver)$bfi

# added function - eventBaseflow
# DEMO
BF_res = eventBaseflow(dataBassRiver)
plotevents(data=dataBassRiver,events=BF_res)
# added argument to extract raw ev with event.style = "raw"
eventBaseflow(dataBassRiver,out.style="raw")
eventBaseflow(dataBassRiver,out.style="summary") # default

# added function - limbs (use identified events to do R/F limb separation)
# DEMO
BF_risfal_res = limbs(data=dataBassRiver,events=BF_res)
Max_risfal_res = limbs(data=dataBassRiver,events=Max_res)

# argument "filter" in limbs indicates whether rate of change filter is used
# argument "min.rates" specifies min rate of increase for identifying rising and decrease for falling
limbs(data=dataBassRiver,events=BF_res,filter=F)
limbs(data=dataBassRiver,events=BF_res,filter=T,min.rates=c(0,0)) # this has same effects as no filter line above
limbs(data=dataBassRiver,events=BF_res,filter=T,plot=T,min.rates=c(10,-10))
limbs(data=dataBassRiver,events=BF_res,filter=T,plot=T,min.rates=c(200,-200))

#########################################################
# FINAL DEMO - method comparison
Max_res = eventMaxima(dataBassRiver, delta.y = 200, delta.x = 1)
Min_res = eventMinima(dataBassRiver, delta.x = 5, delta.y = 20)
PoT_res = eventPOT(dataBassRiver, threshold = 0, min.diff = 1)
BF_res = eventBaseflow(dataBassRiver, BFI_Th = 0.5, min.diff = 1, threshold = 0)

par(mfrow=c(4,1))
par(mar=c(2,2,2,2))
plotevents(data=dataBassRiver,events=Max_res,main="eventMaxima")
plotevents(data=dataBassRiver,events=Min_res,main="eventMinima")
plotevents(data=dataBassRiver,events=PoT_res,main="eventPOT")
plotevents(data=dataBassRiver,events=BF_res,main="eventBaseflow")

```
# hydroEvents
Insert description of package here

## Example 3
Aim: Compare different methods of choosing flow events visually 

```R
library(hydroEvents)
data("dataBassRiver")

bf = baseFlow(dataBassRiver)
Max_res = eventMaxima(dataBassRiver-bf, delta.y = 200, delta.x = 1, thresh = 0)
Min_res = eventMinima(dataBassRiver-bf, delta.x = 5, delta.y = 20)
PoT_res = eventPOT(dataBassRiver-bf, threshold = 0, min.diff = 1)
BFI_res = eventBaseflow(dataBassRiver, BFI_Th = 0.5, min.diff = 1, threshold = 0)

par(mfrow=c(4,1))
par(mar=c(2,2,2,2))
plotEvents(data=dataBassRiver-bf,events=Max_res,main="eventMaxima")
plotEvents(data=dataBassRiver-bf,events=Min_res,main="eventMinima")
plotEvents(data=dataBassRiver-bf,events=PoT_res,main="eventPOT")
plotEvents(data=dataBassRiver-bf,events=BFI_res,main="eventBaseflow")
```
![example02](https://user-images.githubusercontent.com/39328041/108943839-aaafcd80-76ad-11eb-8d33-438f5c99c560.png)
