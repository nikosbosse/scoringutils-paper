scores_table <- data.frame(
  
)

wis <- list(
  Metric = "WIS (Weighted) interval score", 
  `Formula` = r"(For a single interval, the score is computed as $IS_\alpha(F,y) = (u-l) + \frac{2}{\alpha} \cdot (l-y) \cdot 1(y \leq l) + \frac{2}{\alpha} \cdot (y-u) \cdot 1(y \geq u)$, where 1() is the indicator function, y is the true value, and $l$ and $u$ are the $\frac{\alpha}{2}$ and $1 - \frac{\alpha}{2}$ quantile of $F$, i.e. the lower and upper bound of a single prediction interval. For a set of $K$ prediction intervals and the median $m$, the score is computed as a weighted sum, $WIS = \frac{1}{K + 0.5} \cdot (w_0 \cdot |y - m| + \sum_{k = 1}^{K} w_k \cdot IS_{\alpha}(F, y))$. $w_k$ is a weight for every interval. Usually, $w_k = \frac{\alpha_k}{2}$ and $w_0 = 0.5$)", 
  `Application and Explanation` = "proper scoring rule for quantile forecasts. converges to crps for increasing number of interval. The score can be decomposed into a sharpness contribution and penalties for over- and underprediction.", 
  `Caveats` = "The wis is based on measures of absolute error. When averaging across multiple targets, it will therefore be dominated by targets with higher absolute values."
)

library(magrittr)
as.data.frame(wis) %>%
  kableExtra::kable()
