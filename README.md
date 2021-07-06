# hydroEvents
Events from individual hydrologic time series are extracted, and events from multiple time series can be matched to each other.

## Example 1
Aim: Present baseflow filter

```R
# Implementation from hydroEvents
library(hydroEvents)
data(dataBassRiver)
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
data(dataLoch)
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
events.1 = eventMaxima(qf, delta.y = 200, delta.x = 1, threshold = 0)
events.2 = eventMaxima(qf, delta.y = 500, delta.x = 1, threshold = 0)
events.3 = eventMaxima(qf, delta.y = 10,  delta.x = 7, threshold = 0)

oldpar <- par(mfrow = c(3, 1), mar = c(2, 2, 2, 2))
plotEvents(qf, dates = NULL, events = events.1, type = "lineover", main = "delta.y = 200, delta.x = 1")
plotEvents(qf, dates = NULL, events = events.2, type = "lineover", main = "delta.y = 500, delta.x = 1")
plotEvents(qf, dates = NULL, events = events.3, type = "lineover", main = "delta.y = 10, delta.x = 7")
par(oldpar)
```
![flow_maxima01](https://user-images.githubusercontent.com/39328041/120246780-9e429e80-c2b4-11eb-8d05-8f2a1d1ca7d3.jpg)

## Example 3.2
Aim: Extract flow events - sensitivity to method choice.

```R
library(hydroEvents)

bf = baseflowB(dataBassRiver)
Max_res = eventMaxima(dataBassRiver-bf$bf, delta.y = 200, delta.x = 1, threshold = 0)
Min_res = eventMinima(dataBassRiver-bf$bf, delta.x = 5, delta.y = 20, threshold = 0)
PoT_res = eventPOT(dataBassRiver-bf$bf, threshold = 0, min.diff = 1)
BFI_res = eventBaseflow(dataBassRiver, BFI_Th = 0.5, min.diff = 1, threshold = 0)

oldpar <- par(mfrow = c(4, 1), mar = c(2, 2, 2, 2))
plotEvents(data = dataBassRiver-bf$bf, events = Max_res, main = "eventMaxima")
plotEvents(data = dataBassRiver-bf$bf, events = Min_res, main = "eventMinima")
plotEvents(data = dataBassRiver-bf$bf, events = PoT_res, main = "eventPOT")
plotEvents(data = dataBassRiver-bf$bf, events = BFI_res, main = "eventBaseflow")
par(oldpar)
```
![example_02](https://user-images.githubusercontent.com/39328041/109441738-364ca400-7a8a-11eb-81da-0e5a5ac313d2.jpeg)

## Example 4
Aim: To see how different event methods affect rising/falling limbs identified

```R
library(hydroEvents)
data(WQ_Q)

# streamflow and water quality data for 410073 TUMUT RIVER AT ODDYS BRIDGE
qdata=qdata[[1]]

# identify streamflow events - two ways
Q = as.vector(qdata$Q_cumecs)
BF_res = eventBaseflow(Q)
plotEvents(data = Q, events = BF_res)

bf = baseflowB(Q)
MAX_res = eventMaxima(Q-bf$bf, delta.x =3, delta.y = 0.5, threshold = 0.1)
plotEvents(data = Q, events = MAX_res)

# compare rising/falling limbs identified from two event methods
oldpar <- par(mfrow=c(2,1), mar=c(2,2,2,2))
limbs(data = Q, dates=qdata$time, events = BF_res, main="with 'eventBaseflow'")
limbs(data = Q, dates=qdata$time, events = MAX_res, main="with 'eventMaxima', delta.x = 3, delta.y = 0.5, threshold = 0.1") 
par(oldpar)
```
![Example5](https://user-images.githubusercontent.com/29298475/111926773-4ba17500-8b02-11eb-9a19-873f38295747.jpeg)

## Example 5
Aim: Derive event-based concentration-discharge (C-Q) relationships, explore influces of baseflow 

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

  BF_ind = as.vector(1:length(as.vector(Q)))[-ev_ind] # extract data index under baseflow conditions (i.e. not part of an event)
  plot(C~Q,xlab="Q (mm/d)",ylab="C (mg/L)",main = paste("C-Q relationship -",methodname),pch=20) # plot C-Q relationships, coloured by rising/falling limbs and baseflow
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
oldpar <- par(mfrow=c(2,2), mar=c(2,2,2,1))
CQ_event(ECzoo,Qzoo,BF_res,"Baseflow method")
CQ_event(ECzoo,Qzoo,Min_res,"LocalMinima method")
par(oldpar)
```
![Example6](https://user-images.githubusercontent.com/29298475/111926779-4cd2a200-8b02-11eb-9d3a-f2c8131117b0.jpeg)

## Example 6
Aim: Demonstrate matching rainfall to runoff

```R
library(hydroEvents)

srt = as.Date("2015-02-05")
end = as.Date("2015-04-01")
dat = dataCatchment$`105105A`[which(dataCatchment$`105105A`$Date >= srt & dataCatchment$`105105A`$Date <= end),]

events.P = eventPOT(dat$Precip_mm, threshold = 1, min.diff = 2)
events.Q = eventMaxima(dat$Flow_ML, delta.y = 2, delta.x = 1, thresh = 70)

olpar <- par(mfrow = c(2, 1), mar = c(3, 2.7, 2, 1))
plotEvents(dat$Precip_mm, events = events.P, type = "hyet", colpnt = "#E41A1C", colline = "#E41A1C", ylab = "Precipitation (mm)", xlab = "Index", main = "2015")
plotEvents(dat$Flow_ML, events = events.Q, type = "lineover", colpnt = "#E41A1C", colline = "#377EB8", ylab = "Flow (ML/day)", xlab = "Index", main = "")
par(oldpar)
```

![Example6](https://user-images.githubusercontent.com/29298475/111926779-4cd2a200-8b02-11eb-9d3a-f2c8131117b0.jpeg)

```R
matched.1 = pairEvents(events.P, events.Q, lag = 5,  type = 1)
matched.2 = pairEvents(events.P, events.Q, lag = 5,  type = 2)
matched.3 = pairEvents(events.P, events.Q, lag = 3,  type = 3)
matched.4 = pairEvents(events.P, events.Q, lag = 7, type = 4)
matched.5 = pairEvents(events.P, events.Q, lag = 5, type = 5)

oldpar <- par(mfrow = c(5, 1), mar = c(2, 3, 2, 3))
plotPairs(data.1 = dat$Precip_mm, data.2 = dat$Flow_ML, events = matched.1, col = rainbow(nrow(events.P)), 
ylab.1 = "P (mm)", ylab.2 = "Q (ML/day)", cex.2 = 0.66)
plotPairs(data.1 = dat$Precip_mm, data.2 = dat$Flow_ML, events = matched.2, col = rainbow(nrow(events.P)), 
ylab.1 = "P (mm)", ylab.2 = "Q (ML/day)", cex.2 = 0.66)
plotPairs(data.1 = dat$Precip_mm, data.2 = dat$Flow_ML, events = matched.3, col = rainbow(nrow(events.P)), 
ylab.1 = "Q (ML/day)", ylab.2 = "P (mm)", cex.2 = 0.66)
plotPairs(data.1 = dat$Precip_mm, data.2 = dat$Flow_ML, events = matched.4, col = rainbow(nrow(events.P)), 
ylab.1 = "Q (ML/day)", ylab.2 = "P (mm)", cex.2 = 0.66)
plotPairs(data.1 = dat$Precip_mm, data.2 = dat$Flow_ML, events = matched.5, col = rainbow(nrow(events.P)), 
ylab.1 = "P (mm)", ylab.2 = "Q ML/day)", cex.2 = 0.66)
par(oldpar)
```
<img src="https://user-images.githubusercontent.com/39328041/124549966-4a0e7980-de73-11eb-8e10-c18de4da922f.jpg" width="400">

## Example 7
Aim: Demonstrate matching of rainfall and water level surge (residuals)

```R
library(hydroEvents)
# rainfall (P) and water level surge (WL) at Burnie, Tasmania (Pluvio 91009; Tide gauge: IDO71005)
data(data_P_WL) 

# find events in P and WL data
events.P = eventPOT(Psel, threshold = 4, min.diff = 3) # Rain over 4mm is considered an event; events over 3 hrs apart are considered as separate

bf = baseflowB(WLsel)
events.Q1 = eventMaxima(WLsel, delta.y = 0.05, delta.x = 3, thresh = 0.05) # WL surge residual over 0.05m is considered an event; events over 3 hrs apart are considered as separate
oldpar <- par(mfrow=c(2,1), mar=c(2,2,2,2))
plotEvents(data = Psel, events = events.P, main = "Hourly precipitation (mm)", type = "hyet")
plotEvents(data = WLsel, events = events.Q1, main = "Hourly water level surge (m)", type = "lineover")
par(oldpar)
```
![Example7a](https://user-images.githubusercontent.com/29298475/122487933-ba1da280-d01f-11eb-8522-74816f76fc73.jpeg)
```R
# pairning events - use type = 5 to search both ways for the pairing, try two values for the lag (search radius)
matched.1 = pairEvents(events.P, events.Q1, lag = 12, type = 5) 
matched.2 = pairEvents(events.P, events.Q1, lag = 24, type = 5)

plotPairs(data.1 = Psel, data.2 = WLsel, events = matched.1, type = "hyet", color.list=rainbow(nrow(matched.1)))
plotPairs(data.1 = Psel, data.2 = WLsel, events = matched.2, type = "hyet", color.list=rainbow(nrow(matched.2)))
```
![Example7b](https://user-images.githubusercontent.com/29298475/122487944-c275dd80-d01f-11eb-8e3d-63b26fa733fa.jpeg)


