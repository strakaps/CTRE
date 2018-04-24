### Thresholding Bursty Time Series

The given datasets exhibit "bursty" behaviour:

* immediately after an even occurs, it's likely that there are more events
* sometimes there are extended periods in which no large events occur.

Try:

* varying the threshold height with the slider
* for simulated data, vary the `Tail parameter`. It governs the 
  strength of burstiness (the closer to 0, the burstier; hit "Re-simulate"
  after you change it). 
* for magnitudes with a power-law distribution, such as the Solar Flare
  data, a log scale is more appropriate
