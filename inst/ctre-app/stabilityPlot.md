### Stability Plots for Interarrival Times

Asymptotically (for high enough thresholds), the interarrival times 
(i.e. times between threshold crossings) follow a Mittag-Leffler
distribution. 
The slider now chooses the maximum number of exceedances 
(the minimum height of the threshold). 
The estimates of the tail and (rescaled) scale parameters of the 
Mittag-Leffler distribution are plotted as the threshold varies from 
the minimum height to the 10th largest data point. 
To the left (high threshold), data are scarce and variance is high; 
to the right (low threshold), the asymptotics are off and bias is high. 
A region of 'stability' in the middle is choisen for a parameter estimate. 

1. Adjust the `Tail parameter` to your choice of estimate
2. Then read off estimates of the scale parameter. 

For instance, `tail=0.85` and `scale=3000` (days) seem to be a good fit 
for the Bitcoin trade dataset. 
This means that if the threshold is set to the k-th largest magnitude, 
then the interarrival times are Mittag-Leffler distributed with parameters
`tail = 0.85` and `scale = 3000 / k^(1/tail)`.
