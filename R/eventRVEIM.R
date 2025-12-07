eventRVEIM= function(rainfall,streamflow, dvar=3, alpha=0.925, 
  #################### DG. 20251019. Added Section to return consistent format with original event functions e.g., eventMaxima #######
                     out.style = "summary") 
  #################### DG. 20251019. Added Section to return consistent format with original event functions e.g., eventMaxima #######

{  
  # cHECKING DATA:
  if (length(rainfall) != length(streamflow))
  {
    stop("Rainfall and streamflow data should have the same length!")
  }
  
  # FIND THE BEST LAG:
  correl = c()
  correl[1] = cor(streamflow, rainfall)
  for (lag in 1:10)
  {
    str = streamflow[(lag + 1) : length(streamflow)]
    prp = rainfall[1 : (length(rainfall) - lag)]
    correl[lag + 1]= cor(str, prp)
  }
  best_lag= which.max(correl)
  
  # CAPTURING THE NATURAL VARIABILITY OF BASEFLOW:
  bfA = baseflowA(streamflow, alpha = alpha)$bf
  MAD = median(abs(bfA - median(bfA)))
  limit = median(bfA) + (3* MAD)
  var_lim = var(bfA[bfA <= limit])
  
  # CALCULATE THE VARIANCE:
  QD = var_dvar = c()
  QD[1] = 0
  var_dvar[1] = 0
  for (j in 2:length(streamflow))
  {
    QD[j] = streamflow[j] - streamflow[j - 1]
    if (j <= dvar)
    {
      var_dvar[j] = var(streamflow[1 : j])
    }else
    {
      var_dvar[j] = var(streamflow[(j - dvar + 1) : j])
    }
  }
  
  # RAINFALL EVENTS:
  prep_events = eventPOT(rainfall, threshold = 1, min.diff = 1)
  
  # SEARCH FOR STREAMFLOW EVENTS FOR EACH RAINFALL EVENT:
  str_rain = end_rain = l_rain = str_flw = end_flw = c()
  previous_end_flow = RC = 0
  for (j in 1 : nrow(prep_events))
  {
    # CALCULATE RAINFALL EVENT CHARACTERISTICS:
    str_rain[j] = prep_events$srt[j]
    end_rain[j] = prep_events$end[j]
    l_rain[j] = end_rain[j] - str_rain[j] + 1
    end_of_data = FALSE
    
    # INDICES THAT STREAMFLOW EVENT SHOULD BE OCCURED IN:
    idx_candidate = (str_rain[j]) : (end_rain[j] + best_lag)
    if(tail(idx_candidate, 1) >= length(streamflow))
    {
      idx_candidate =(str_rain[j]) : length(streamflow)
      str_flw[j] = NA
      end_flw[j] = NA 
      break
    }
    any_move = var_dvar[idx_candidate] > var_lim
    
    # CHECK THE STREAMFLOW OCCURANCE:
    if(length(which(any_move == TRUE)) < l_rain[j] | length(idx_candidate[which(any_move == TRUE & QD[idx_candidate] > 0)]) == 0)
    {
      # No streamflow event:
      streamflow_event = FALSE
    }else
    {
      # Streamflow event:
      streamflow_event = TRUE
    }
    if(j != 1)
    {
      if(tail(idx_candidate,1) <= previous_end_flow)
      {
        # This rainfall event is within the previous streamflow event. So:
        streamflow_event = FALSE
        next
      }
    }
    
    # FINDING THE START AND THE END OF STREAMFLOW EVENTS: 
    if(streamflow_event == FALSE)
    {
      str_flw[j] = NA
      end_flw[j] = NA
    }else
    {
      # THE START OF TEH EVENT:
      str_flw[j] = idx_candidate[(which(any_move == TRUE & QD[idx_candidate] >= 0))[1]]
      if(j!=1)
      {
        if(is.na(str_flw[j-1]) == FALSE)
        {
          if(str_flw[j] < end_flw[j - 1])
          {
            str_flw[j] = end_flw[j - 1]
            idx_candidate = str_flw[j] : tail(idx_candidate, 1)
          }
        }
      }
      
      # THE END OF THE EVENT:
      if(tail(QD[idx_candidate], 1) > 0)#(tail(which(streamflow[idx_candidate]==max(streamflow[idx_candidate])),1)==length(idx_candidate))
      {
        # We are in the rising limb, so we should find the falling limb:
        initial_guess_ind = tail(idx_candidate, 1)
        meet = FALSE
        t = 1
        while(meet == FALSE)
        {
          if((initial_guess_ind + t) >= length(streamflow))
          {
            end_of_data = TRUE
            end_flw[j] = initial_guess_ind + t
            meet = TRUE
          }
          else if(QD[initial_guess_ind + t] < 0)
          {
            falling_limb_start = initial_guess_ind + t
            meet = TRUE
          }else
          {
            t = t + 1
          }
        }
        # We should find the end of the falling limb:
        if(end_of_data == FALSE)
        {
          initial_guess_ind = falling_limb_start
          meet = FALSE
          t = 1
          while(meet == FALSE)
          {
            if((initial_guess_ind + t) >= length(streamflow))
            {
              end_of_data = TRUE
              end_flw[j] = initial_guess_ind+t
              meet = TRUE
            }
            else if(QD[initial_guess_ind + t] >= 0)
            {
              end_flw[j] = initial_guess_ind + t - 1
              meet = TRUE
            }else
            {
              t = t + 1
            }
          }
        }
      }else
      {
        initial_guess_ind = tail(idx_candidate[QD[idx_candidate] < 0], 1)
        if(length(initial_guess_ind) == 0)
        {
          end_flw[j] = tail(idx_candidate, 1)
        }
        else
        {
          end_flw[j] = initial_guess_ind
          if(initial_guess_ind == tail(idx_candidate, 1))
          {
            meet = FALSE
            t = 1
            while(meet == FALSE)
            {
              if((initial_guess_ind + t) >= length(streamflow))
              {
                end_of_data = TRUE
                end_flw[j] = initial_guess_ind + t
                meet = TRUE
              }
              if(QD[initial_guess_ind + t] >= 0)
              {
                end_flw[j] = initial_guess_ind + t - 1
                meet = TRUE
              }else
              {
                t = t + 1
              }
            }
          }
        }
      }
    }
    if(is.na(end_flw[j]) == FALSE)
    {
      previous_end_flow = end_flw[j]
    }
  }
  
  # checking if the length of streamflow events equals to rainfall events
  # In some cases, at the end of streamflow data, because there is no further data,
  # we have to consider NA streamflow events for last rainfall events manually.
  if(length(str_flw) != length(str_rain))
  {
    beg_na = length(str_flw) + 1
    end_na = nrow(prep_events)
    
    str_flw[beg_na : end_na] = NA
    end_flw[beg_na : end_na] = NA
  }
  
  #################### DG. 20251019. Added Section to return consistent format with original event functions e.g., eventMaxima #######
  if (out.style == "summary") {
    event.stats = calcStats(str_flw, end_flw, streamflow, 
                            f.vec = c("which.max", "max", "sum"))
    return(data.frame(srt = str_rain, 
                      end = end_rain,
                      matched.srt = str_flw,
                      matched.end = end_flw, 
                      event.stats))
  }
  else {
    return(data.frame(srt = str_rain,
                      end = end_rain,
                      matched.srt = str_flw,
                      matched.end = end_flw))
  }
  #################### DG. 20251019. Added Section to return consistent format with original event functions e.g., eventMaxima #######
  
  #events = data.frame(srt = str_rain, 
   #                   end = end_rain,
    #                  matched.srt = str_flw,
     #                 matched.end = end_flw)
  #eturn(events)
}
