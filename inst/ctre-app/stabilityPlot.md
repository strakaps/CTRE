### Inference on the Exceedance time distribution

A Mittag-Leffler distribution is fitted to the arrival times of the threshold crossings.

* From left to right, the threshold is lowered; the x-axis shows the number `k` of exceedances.
* At each threshold height, a Mittag-Leffler distribution is fitted to the `k-1` inter-exceedance times.

**Move the `tail parameter` so it best coincides with the tail parameter estimate.** The tail parameter is used in rescaling the scale parameter (scroll down), hence it changes.

For instance, `tail=0.9` and `scale=13` (days) seem to be a good fit for the Bitcoin trade dataset. This means that if the threshold is set to a height that is only crossed once every `n` observations, then the threshold crossing times have a Mittag-Leffler inter-arrival distribution with `tail=0.9` and `scale=13 * n^(1/tail)`.

Dashed lines are 95% confidence intervals.
