calcREIC= function(rainfall, quickflow, events, n_rainfall, area)
{
  events = na.omit(events)
  
  vol_rainfall = vol_runoff = rc = c()
  for(ev in 1 : nrow(events))
  {
    vol_rainfall[ev] = sum(rainfall[events[ev,1] : events[ev,2]])
    vol_runoff[ev] = sum(quickflow[events[ev,3] : events[ev,4]])
    rc[ev] = vol_runoff[ev] / (area * vol_rainfall[ev])
  }
  
  rc_valid = length(which(rc <= 1))
  n_events = nrow(events)
  
  reic= 1 - sqrt((1 - (rc_valid / n_events)) ^ 2 + (1 - (n_events / n_rainfall)) ^ 2)
  return(reic)
}
