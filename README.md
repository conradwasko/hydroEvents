# hydroEvents
Insert description of package here

## Example 1
Aim: Present baseflow filter

```R
# Implementation from hydroEvents
library(hydroEvents)
bf.1 = baseflowB(dataBassRiver, alpha = 0.925)
bf.2 = baseflowB(dataBassRiver, alpha = 0.980)
BFI.1 = sum(bf.1$bf)/sum(dataBassRiver)
BFI.2 = sum(bf.2$bf)/sum(dataBassRiver)
print(BFI.1) # 0.39
print(BFI.2) # 0.20
plot(1:length(dataBassRiver), dataBassRiver, type = "l", lwd = 2, col = "steelblue",
     ylab = "Flow (ML/d)", xlab = "Time index", mgp = c(2, 0.6, 0))
lines(1:length(dataBassRiver), bf.1$bf, lwd = 2, lty = 2, col = "darkgreen")
lines(1:length(dataBassRiver), bf.2$bf, lwd = 2, lty = 2, col = "darkorange")
legend("topright", legend = c("Flow", "Baseflow (0.925)", "Baseflow (0.980)"), cex = 0.8,
       lwd = 2, col = c("steelblue", "darkgreen", "darkorange"), bty = "n")
```
![baseflow01](https://user-images.githubusercontent.com/39328041/120128453-59abfa00-c205-11eb-825a-4eb59b25f188.jpg)

## Example 2
Aim: Extract precipitation events

```R
events = eventPOT(dataLoch, threshold = 0, min.diff = 1)
plotEvents(dataLoch, dates = NULL, events = events, type = "hyet", main = "Rainfall Events (threshold = 0, min.diff = 1)")
```
![precip01](https://user-images.githubusercontent.com/39328041/120242717-ef4c9580-c2a8-11eb-99cb-210f625aa4f6.jpg)

## Example 3.1
Aim: Extract flow events - sensitivity to parameter choice. All the choices are reasonable but result in very different event choice

```R
bf = baseflowB(dataBassRiver, alpha = 0.925)
qf = dataBassRiver - bf$bf
events = eventMaxima(qf, delta.y = 200, delta.x = 1, threshold = 0)
plotEvents(qf, dates = NULL, events = events, type = "lineover", main = "delta.y = 200, delta.x = 1")
events = eventMaxima(qf, delta.y = 500, delta.x = 1, threshold = 0)
plotEvents(qf, dates = NULL, events = events, type = "lineover", main = "delta.y = 500, delta.x = 1")
events = eventMaxima(qf, delta.y = 10, delta.x = 7, threshold = 0)
plotEvents(qf, dates = NULL, events = events, type = "lineover", main = "delta.y = 10, delta.x = 7")
```
![flow_maxima01](https://user-images.githubusercontent.com/39328041/120246157-bfa28b00-c2b2-11eb-9f4b-e51c9e1c3a28.jpg)
![flow_maxima02](https://user-images.githubusercontent.com/39328041/120246153-be715e00-c2b2-11eb-91fb-daf31f4aa0e3.jpg)
![flow_maxima03](https://user-images.githubusercontent.com/39328041/120246156-bfa28b00-c2b2-11eb-94df-64f64cb3d615.jpg)

## Example 3.2
Aim: Extract flow events - sensitivity to method choice.

```R
library(hydroEvents)

bf = baseflowB(dataBassRiver)
Max_res = eventMaxima(dataBassRiver-bf, delta.y = 200, delta.x = 1, threshold = 0)
Min_res = eventMinima(dataBassRiver-bf, delta.x = 5, delta.y = 20, threshold = 0)
PoT_res = eventPOT(dataBassRiver-bf, threshold = 0, min.diff = 1)
BFI_res = eventBaseflow(dataBassRiver, BFI_Th = 0.5, min.diff = 1, threshold = 0)

par(mfrow=c(4,1))
par(mar=c(2,2,2,2))
plotEvents(data=dataBassRiver-bf,events=Max_res,main="eventMaxima")
plotEvents(data=dataBassRiver-bf,events=Min_res,main="eventMinima")
plotEvents(data=dataBassRiver-bf,events=PoT_res,main="eventPOT")
plotEvents(data=dataBassRiver-bf,events=BFI_res,main="eventBaseflow")
```
![example_02](https://user-images.githubusercontent.com/39328041/109441738-364ca400-7a8a-11eb-81da-0e5a5ac313d2.jpeg)

## Example 5
Aim: To see how different event methods affect rising/falling limbs identified

```R
library(hydroEvents)
data("dataWQQ")

# streamflow and water quality data for 410073 TUMUT RIVER AT ODDYS BRIDGE
qdata=qdata[[1]]

# identify streamflow events - two ways
Q = as.vector(qdata$Q_cumecs)
BF_res = eventBaseflow(Q)
plotEvents(data = Q, events = BF_res)

bf = baseFlow(Q)
MAX_res = eventMaxima(Q-bf$bf, delta.x =3, delta.y = 0.5, threshold = 0.1)
plotEvents(data = Q, events = MAX_res)

# compare limbs identified from two event methods
par(mfrow=c(2,1))
par(mar=c(2,2,2,2))
limbs(data = Q, dates=qdata$time, events = BF_res, main="with 'eventBaseflow'")
limbs(data = Q, dates=qdata$time, events = MAX_res, main="with 'eventMaxima', delta.x = 3, delta.y = 0.5, threshold = 0.1") 
```
![Example5](https://user-images.githubusercontent.com/29298475/111926773-4ba17500-8b02-11eb-9a19-873f38295747.jpeg)

## Example 6
Aim: Derive event-based C-Q relationship (DG)

```R
# a function to plot CQ with different colours by event periods (rising, falling limbs and baseflow)
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
![Example6](https://user-images.githubusercontent.com/29298475/111926779-4cd2a200-8b02-11eb-9d3a-f2c8131117b0.jpeg)

## Example 8
Aim: Demonstrate matching of rainfall and water level surge (residuals)

```R
library(hydroEvents)
# rainfall (P) and water level surge (WL) at Burnie, Tasmania (Pluvio 91009; Tide gauge: IDO71005)
data("dataPWL") 

# find events in P and WL data
events.P = eventPOT(Psel, threshold = 0, min.diff = 3) # 3-h no rain

bf = baseFlow(WLsel)
events.Q1 = eventMaxima(WLsel, delta.y = 0.05, delta.x = 3, thresh = 0.01) # min of 3-h event
par(mfrow=c(2,1))
par(mar=c(2,2,2,2))
plotEvents(data = Psel, events = events.P, main = "Hourly precipitation (mm)", type = "hyet")
plotEvents(data = WLsel, events = events.Q1, main = "Hourly water level surge (m)", type = "lineover")
```
![Example8](https://user-images.githubusercontent.com/29298475/111944391-a4d1ce80-8b2b-11eb-87ec-32b5aa26fea7.jpeg)

```R
# match events
matched.1 = pairEvents(events.P, events.Q1, lag = 12,  type = 1)
matched.2 = pairEvents(events.P, events.Q1, lag = 12, type = 2)
matched.3 = pairEvents(events.P, events.Q1, lag = 12,  type = 3)
matched.4 = pairEvents(events.P, events.Q1, lag = 12, type = 4)

par(mfrow=c(4,1))
par(mar=c(2,2,2,2))
plotPairedEvents(data.1 = Psel, data.2 = WLsel, events = matched.1, type = "hyet", color.list=rainbow(nrow(matched.1)))
plotPairedEvents(data.1 = Psel, data.2 = WLsel, events = matched.2, type = "hyet", color.list=rainbow(nrow(matched.1)))
plotPairedEvents(data.1 = Psel, data.2 = WLsel, events = matched.3, type = "hyet", color.list=rainbow(nrow(matched.1)))
plotPairedEvents(data.1 = Psel, data.2 = WLsel, events = matched.4, type = "hyet", color.list=rainbow(nrow(matched.1)))
```
![Example8b](https://user-images.githubusercontent.com/29298475/111944220-3d1b8380-8b2b-11eb-8e91-d500b56433e5.jpeg)

##################### only add above this line - below are codes prior Mar 22. 2021 ######################
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
## Example
Aim: Demonstrate sampling functions on multiple data sets
```R
library(hydroEvents)
data("dataBassRiver")
data("dataLoch")
bf = baseFlow(dataBassRiver)
PoT_res_P = eventPOT(dataLoch, threshold = 1, min.diff = 1)
PoT_res_Q = eventPOT(dataBassRiver-bf, threshold = 1, min.diff = 1)

par(mfrow=c(2,1))
par(mar=c(2,2,2,2))
plotEvents(data=dataLoch,events=PoT_res_P,main="Loch", type = "hyet")
plotEvents(data=dataBassRiver-bf,events=PoT_res_Q,main="Bass River", type = "lineover")
```
![Example01](https://user-images.githubusercontent.com/39328041/109442598-c7bd1580-7a8c-11eb-8956-e891c162f630.jpeg)

## Example 3
Aim: Compare different methods of choosing flow events visually 

```R
library(hydroEvents)
data("dataBassRiver")

bf = baseFlow(dataBassRiver)
Max_res = eventMaxima(dataBassRiver-bf, delta.y = 200, delta.x = 1, threshold = 0)
Min_res = eventMinima(dataBassRiver-bf, delta.x = 5, delta.y = 20, threshold = 0)
PoT_res = eventPOT(dataBassRiver-bf, threshold = 0, min.diff = 1)
BFI_res = eventBaseflow(dataBassRiver, BFI_Th = 0.5, min.diff = 1, threshold = 0)

par(mfrow=c(4,1))
par(mar=c(2,2,2,2))
plotEvents(data=dataBassRiver-bf,events=Max_res,main="eventMaxima")
plotEvents(data=dataBassRiver-bf,events=Min_res,main="eventMinima")
plotEvents(data=dataBassRiver-bf,events=PoT_res,main="eventPOT")
plotEvents(data=dataBassRiver-bf,events=BFI_res,main="eventBaseflow")
```
![example_02](https://user-images.githubusercontent.com/39328041/109441738-364ca400-7a8a-11eb-81da-0e5a5ac313d2.jpeg)

## Example 4 
Aim: Demonstrate matching of events and slight pitfalls
```R
library(hydroEvents)
data("dataBassRiver")

library(hydroEvents)
srt = as.Date("2011-05-01")
end = as.Date("2011-08-30")
data = data314213[which(data314213$Date >= srt & data314213$Date <= end),]

events.P = eventPOT(data$precip_mm, threshold = 1, min.diff = 2)
plotEvents(data = data$precip_mm, events = events.P, main = "Precip", type = "hyet")
```
We have identified rainfall events as follows

![Example04_1](https://user-images.githubusercontent.com/39328041/109444083-7b73d480-7a90-11eb-9be0-404f8acd30c3.jpeg)

```R
bf = baseFlow(data$Flow_ML, alpha = 0.925)
events.Q1 = eventMaxima(data$Flow_ML-bf, delta.y = 1,   delta.x = 1, thresh = 0)
events.Q2 = eventMaxima(data$Flow_ML-bf, delta.y = 1,   delta.x = 1, thresh = 20)
events.Q3 = eventMaxima(data$Flow_ML-bf, delta.y = 500, delta.x = 1, thresh = 100)
events.Q4 = eventMaxima(data$Flow_ML-bf, delta.y = 500, delta.x = 7, thresh = 100)
plotEvents(data = data$Flow_ML-bf, events = events.Q1, main = "Flow", type = "lineover")
```
We use four different methods to identify flow events. It is hard to argue that events (8) and (9) are the same event. Though (7) and (8) maybe.
![Example04_2](https://user-images.githubusercontent.com/39328041/109444245-ea512d80-7a90-11eb-9497-ced93bf773ca.jpeg)

```R
matched.1 = pairEvents(events.P, events.Q1, lag = 5,  type = 1)
matched.2 = pairEvents(events.P, events.Q2, lag = 10, type = 2)
matched.3 = pairEvents(events.P, events.Q3, lag = 5,  type = 3)
matched.4 = pairEvents(events.P, events.Q4, lag = 10, type = 4)
par(mfrow=c(4,1))
par(mar=c(2,2,2,2))
plotPairedEvents(data.1 = data$precip_mm, data.2 = data$Flow_ML-bf, events = matched.1, type = "hyet", col = rainbow)
plotPairedEvents(data.1 = data$precip_mm, data.2 = data$Flow_ML-bf, events = matched.2, type = "hyet", col = rainbow)
plotPairedEvents(data.1 = data$precip_mm, data.2 = data$Flow_ML-bf, events = matched.3, type = "hyet", col = rainbow)
plotPairedEvents(data.1 = data$precip_mm, data.2 = data$Flow_ML-bf, events = matched.4, type = "hyet", col = rainbow)
```
![Example04_3](https://user-images.githubusercontent.com/39328041/109444625-ca6e3980-7a91-11eb-900b-b1eda14300b5.jpeg)

There are a few things to note here. If you match from flow events (type 3) to rainfall events you are constrained to often misclassify peaks. Also note that if you match from rainfall events you can easily merge flow events thereby choosing events more physically realistically. 

Another point is that you need to pick large thresholds to join events together - and you miss the smaller rainfall events.

## Example 5

```R
# Aim:An example with rainfall runoff ratios
library(hydroEvents)
data("dataEvap")

#a.p = c(892, 1182, 1979, 344, 799)
#a.q = c(133, 550, 777, 56.2, 195)

#a.q/a.p
#rank(a.q/a.p)
#rank(rr)
#cbind(rr, a.q/a.p)

n = length(dataEvap)
rr.1 = rr.2 = rr.3 = rep(NA_real_, n)
par(mfrow = c(5, 1))
par(mar=c(2,2,2,2))
for (i in 1:n) {
  print(dataEvap[[i]]$site)
  eventP = eventPOT(dataEvap[[i]]$P, threshold = 1, min.diff = 2)
  bf = baseFlow(dataEvap[[i]]$Q)
  eventQ = eventMinima(dataEvap[[i]]$Q-bf, delta.y = 0.1, delta.x =  1, threshold = 0)
  pairs = pairEvents(eventP, eventQ, lag = 5, type = 1)
  plotPairedEvents(data.1 = dataEvap[[i]]$P[1:200], data.2 = (dataEvap[[i]]$Q-bf)[1:200], events = pairs[1:20,])

  stats.P = calcStats(pairs$srt, pairs$end, data = dataEvap[[i]]$P, f.vec = c("sum"))
  stats.Q = calcStats(pairs$matched.srt, pairs$matched.end, data = dataEvap[[i]]$Q-bf, f.vec = c("sum"))
  stats.Q[is.na(stats.Q)] = 0

 
  rr.1[i] = mean(stats.Q$sum/stats.P$sum)
  rr.2[i] = sum(dataEvap[[i]]$Q)/sum(dataEvap[[i]]$P)
  
  pairs = pairEvents(eventP, eventQ, lag = 5, type = 4)
  pairs = pairs[!is.na(pairs$matched.srt),]
  stats.P = calcStats(pairs$matched.srt, pairs$matched.end, data = dataEvap[[i]]$P, f.vec = c("sum"))
  stats.Q = calcStats(pairs$srt, pairs$end, data = dataEvap[[i]]$Q-bf, f.vec = c("sum"))

   rr.3[i] = mean(stats.Q$sum/stats.P$sum)
  
    #mean(rr)

  #nrow(eventP)
  #nrow(eventQ)
}

cbind(rr.1, rr.3, rr.2)

#           rr.1       rr.3       rr.2
#[1,] 0.07649349 0.04133459 0.14410396
#[2,] 0.20968947 0.17203738 0.47116525
#[3,] 0.08769046 0.10874407 0.38265599
#[4,] 0.01712640 0.03161799 0.06927159
#[5,] 0.10045657 0.09122927 0.24661891

```
