# Supplementary script for ERL manuscript: doi: XXXXXXXX (to be updated)
# rainfall-runoff event identification and runoff coefficients for 
# large-sample analyses
# 
# This script identifies rainfall-runoff events using two contrasting methods 
# and calculate all event runoff coefficients. The two methods are:
#   1. eventRVEIM: dvar = 10 and alpha = 0.975
#   2. eventMaxima: delta.y = -0.75, delta.x = 1, threshold = 0, alpha = 0.925
#
# For each catchment, the script retains the table containing paired 
# rainfall-runoff event and calculates the event runoff coefficient of 
# every paired event:
#
#                    event quickflow
#       RC = -----------------------------------
#                    event rainfall
# When no runoff event is paired to a rainfall event i.e., a rainfall event does 
# not generate measurable runoff, RC = 0 
#
# Rainfall and streamflow inputs must therefore use equivalent depth units 
# (e.g. mm per time step). 
#
# Required objects in the R workspace
# -----------------------------------
# Loaded variables:
# Psel          A list of rainfall time series, one numeric vector per catchment.
# Qsel_filled   A list of gap-filled streamflow time series, in the same order
#               and with the same lengths as Psel.
# sel.area      Numeric vector of catchment areas used to estimate the pairing
#               lag for eventMaxima. Its units must match the empirical formula
#               used below.
#
# Required package: hydroEvents: https://cran.r-project.org/package=hydroEvents
# Required functions (load the package/source files that provide them before
# running this script): eventPOT(), eventRVEIM(), eventMaxima(), pairEvents(),
# and baseflowA().


# -----------------------------------------------------------------------------
# Settings
# -----------------------------------------------------------------------------
library(hydroEvents)

rainfall_threshold <- 1 # use 1mm daily rainfall as threshold to identify rainfall event
baseflow_alpha_RVEIM <- 0.975
dvar_RVEIM <- 10

baseflow_alpha_evMax <- 0.925
maxima_delta_y_evMax <- -0.75
maxima_delta_x_evMax <- 1
maxima_threshold_evMax <- 0
maxima_pairing_type_evMax <- 1

baseflow_passes <- 3
# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

# Calculate event runoff coefficients from a matched-event table.
#
# eventRVEIM and pairEvents may return their rainfall and runoff indices in
# different column orders. A pairEvents-style table begins with "matched.srt"
# and stores runoff start/end in columns 1:2 and rainfall start/end in columns
# 3:4. Otherwise, rainfall indices are assumed to occupy columns 1:2 and runoff
# indices columns 3:4, matching the eventRVEIM summary output.

calculate_event_rc <- function(events, rainfall, quickflow) {
  if (is.null(events) || nrow(events) == 0L) {
    return(numeric(0))
  }

  if (ncol(events) < 4L) {
    stop("The matched-event table must contain at least four index columns.")
  }

  pair_events_style <- identical(names(events)[1], "matched.srt")

  if (pair_events_style) {
    runoff_columns <- 1:2
    rainfall_columns <- 3:4
  } else {
    rainfall_columns <- 1:2
    runoff_columns <- 3:4
  }

  vapply(seq_len(nrow(events)), function(i) {
    rainfall_start <- as.integer(events[i, rainfall_columns[1]])
    rainfall_end <- as.integer(events[i, rainfall_columns[2]])
    runoff_start <- as.integer(events[i, runoff_columns[1]])
    runoff_end <- as.integer(events[i, runoff_columns[2]])

    rainfall_volume <- sum(
      rainfall[rainfall_start:rainfall_end],
      na.rm = TRUE
    )
    if (is.na(runoff_start) & is.na(runoff_end)) {
      runoff_volume <- 0
    } else {
      runoff_volume <- sum(
        quickflow[runoff_start:runoff_end]
      )
    }


    if (!is.finite(rainfall_volume) || rainfall_volume <= 0) {
      return(NA_real_)
    }

    runoff_volume / rainfall_volume
  }, numeric(1))
}

# -----------------------------------------------------------------------------
# Check inputs and initialise outputs
# -----------------------------------------------------------------------------

if (!is.list(Psel) || !is.list(Qsel_filled)) {
  stop("Psel and Qsel_filled must both be lists.")
}

n_sites <- length(Psel)

if (length(Qsel_filled) != n_sites || length(sel.area) != n_sites) {
  stop("Psel, Qsel_filled, and sel.area must contain the same number of sites.")
}

event_tables_rveim <- vector("list", n_sites)
event_tables_maxima <- vector("list", n_sites)
event_rc_rveim <- vector("list", n_sites)
event_rc_maxima <- vector("list", n_sites)


# -----------------------------------------------------------------------------
# Identify events and calculate event runoff coefficients
# -----------------------------------------------------------------------------

for (site in seq_len(n_sites)) {
  rainfall <- Psel[[site]]
  streamflow <- Qsel_filled[[site]]

  if (length(rainfall) != length(streamflow)) {
    stop(sprintf("Rainfall and streamflow lengths differ at site %d.", site))
  }

  # Lyne-Hollick-style baseflow separation. The resulting quickflow series is
  # used by both methods, ensuring that their RC estimates are comparable.

  # Method 1: eventRVEIM ------------------------------------------------------
  baseflow_RVEIM <- baseflowA(
    streamflow,
    alpha = baseflow_alpha_RVEIM,
    passes = baseflow_passes
  )$bf
  quickflow_RVEIM <- streamflow - baseflow_RVEIM
  
  rveim_events <- eventRVEIM(
    rainfall,
    streamflow,
    dvar = dvar_RVEIM,
    alpha = baseflow_alpha_RVEIM,
    out.style = "summary"
  )

  event_tables_rveim[[site]] <- rveim_events
  event_rc_rveim[[site]] <- calculate_event_rc(
    events = rveim_events,
    rainfall = rainfall,
    quickflow = quickflow_RVEIM 
  )

  # Method 2: eventMaxima -----------------------------------------------------
  rainfall_events <- eventPOT(
    data = rainfall,
    threshold = rainfall_threshold
  )
  
  baseflow_evMax <- baseflowA(
    streamflow,
    alpha = baseflow_alpha_evMax,
    passes = baseflow_passes
  )$bf
  quickflow_evMax <- streamflow - baseflow_evMax
  
  runoff_events <- eventMaxima(
    quickflow_evMax,
    delta.y = maxima_delta_y_evMax,
    delta.x = maxima_delta_x_evMax,
    threshold = maxima_threshold_evMax
  )

  # Estimated catchment response time, converted to daily time steps. The
  # ceiling ensures a minimum whole-number lag accepted by pairEvents().
  pairing_lag <- ceiling(0.76 * sel.area[site]^0.38 / 24)

  maxima_events <- pairEvents(
    rainfall_events,
    runoff_events,
    lag = pairing_lag,
    type = maxima_pairing_type_evMax
  )

  event_tables_maxima[[site]] <- maxima_events
  event_rc_maxima[[site]] <- calculate_event_rc(
    events = maxima_events,
    rainfall = rainfall,
    quickflow = quickflow_evMax
  )
}

# -----------------------------------------------------------------------------
# Outputs
# -----------------------------------------------------------------------------
# event_tables_rveim  Matched-event tables returned by eventRVEIM.
# event_tables_maxima Matched-event tables returned by pairEvents for the
#                     eventMaxima method.
# event_rc_rveim      Event runoff coefficients from eventRVEIM matches.
# event_rc_maxima     Event runoff coefficients from eventMaxima matches.