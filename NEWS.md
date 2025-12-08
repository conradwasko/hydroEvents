# hydroEvents 0.13.0

* Fixed minor bug in plotEvents with colouring
* Added new functions:
* 1. eventRVEIM, the Robust Variance-based Event Identification Method (RVEIM), which identifies rainfall-runoff events given rainfall and runoff time series.
* 2. postCorrection, which process/Correct the paired rainfall-runoff events identified based on two different methods (see .Rd file for details)
* 3. calcREIC, which calculates the REIC value for a set of rainfall–runoff events given rainfall, quickflow, 
and event data, following the approach introduced in Mohammadpour Khoie et al. (2025). https://doi.org/10.1016/j.envsoft.2025.106521. REIC is a simple objective metric to assess the plausibility of the rainfall-runoff events chosen.
* Added new dataset:
* 1. hourlyQ, and hourly streamflow dataset to demonstrate event identification with hourly data.