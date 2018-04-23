The Mittag-Leffler distribution has a `tail` and a `scale` parameter, 
and setting `tail=1` recovers the exponential distribution.
Varying `tail` away from 1 (note that `tail` needs to be from the interval (0,1]) has two effects:

1. It models occasional very long quiet periods
2. It increases the frequency of events outside of quiet periods (makes the dynamics more "bursty").

The empirical datasets seem to prefer `tail < 1` mostly because of the second effect.  Repeated simulations show that the CTRM model predicts longer rests than are usually seen in the data.
**Hence the strength of the CTRM model lies in the modelling on short to medium scale event rather than long time scales.**
