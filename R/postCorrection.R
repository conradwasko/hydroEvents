postCorrection = function(events,method = "remove duplicates") {
  if(nrow(na.omit(events))>1) {
    if (method == "remove duplicates") {
      na_idx = which(rowSums(is.na(events)) > 0)
      events_hydro_1 = na.omit(events)  
      #remove_idx = c()
      rain_cols = paste(events_hydro_1$srt, events_hydro_1$end, sep="-")
      runoff_cols = paste(events_hydro_1$matched.srt, events_hydro_1$matched.end, sep="-")
      remove_idx = #c(remove_idx, 
        c(which(duplicated(rain_cols) == TRUE),
          which(duplicated(runoff_cols) == TRUE))
      if(length(remove_idx) != 0)
      {
        events_hydro_1 = events_hydro_1[-remove_idx,]
      }
      for(i in 2:nrow(events_hydro_1))
      {
        # overlaps of runoff
        if(events_hydro_1[i,3] < events_hydro_1[i-1,4]) # I used numbers of 3 and 4 instead of name of columns because the name of columns will differ based on the pairing type.
        {
          events_hydro_1[i,3] = events_hydro_1[i-1,4] 
        }
        # overlaps of rainfall
        if(events_hydro_1[i,1] < events_hydro_1[i-1,2])
        {
          events_hydro_1[i,1] = events_hydro_1[i-1,2] 
        }
      }
      events_hydro_1 = rbind(events_hydro_1, events[na_idx,]) # I have added this line and the next one as well.
      events_hydro_1 = events_hydro_1[order(events_hydro_1[,1]),] # This is because the user can still have access to NA values after correction.
    } else if (method == "shift duplicates") {
      na_idx = which(rowSums(is.na(events)) > 0)
      events_hydro_1 = na.omit(events)
      remove_idx = c()
      for(ev in 2:nrow(events_hydro_1))
      {
        if(events_hydro_1[ev,4] <= events_hydro_1[ev-1,4])
        {
          remove_idx = c(remove_idx,ev)
        }else if(events_hydro_1[ev-1,3] <= events_hydro_1[ev,3] & events_hydro_1[ev,3]<=events_hydro_1[ev-1,4])
        {
          events_hydro_1[ev,3]= events_hydro_1[ev-1,4]+1
        }
      }
      if(length(remove_idx) != 0)
      {
        events_hydro_1 = events_hydro_1[-remove_idx,]
      }
      events_hydro_1 = rbind(events_hydro_1, events[na_idx,]) # I have added this line and the next one as well.
      events_hydro_1 = events_hydro_1[order(events_hydro_1[,1]),] # This is because the user can still have access to NA values after correction.
    } else {
      warning("No valid method selected. Please refer to function manual.")
    }
    
    return(events_hydro_1)
  } else {
    events_hydro_1 = events
  }

}

