---
author:
- |
  Nikos I. Bosse\
  London School of Hygiene and Tropical Medicine Second Author\
  Plus Affiliation
bibliography:
- references.bib
title: Evaluating Covid-19 Short-Term Forecasts using in
---

# Introduction

Good forecasts are of great interest to decision makers in various
fields like finance (), weather predictions or infectious disease
modeling [@funkShorttermForecastsInform2020]. An integral part of
assessing and improving their usefulness is forecast evaluation. For
decades, researchers have developed and refined an arsenal of techniques
not only to forecast, but also to evaluate these forecasts (see e.g.
[@bracherEvaluatingEpidemicForecasts2020a],
[@funkAssessingPerformanceRealtime2019],
[@gneitingProbabilisticForecastsCalibration2007], and
[@gneitingStrictlyProperScoring2007]). Yet even with this rich body of
research available, implementing a complete forecast evaluation in is
not trivial.

Some packages exist that bundle different scoring metrics together, but
none offer the user a standalone solution to forecast evaluation. The
package \[CITATION\] offers a very extensive collection of proper
scoring rules. Its exclusive focus on proper scoring rules and the fact
that all functions are implemented using vectors and matrices instead of
data.frames, however, make it more suitable for experienced users or as
a building block in larger application. It also lacks features like
pairwise comparisons between forecast models and plotting functionality
that are invaluable in the evaluation process. Other packages like
\[https://cran.r-project.org/web/packages/Metrics/Metrics.pdf\]
\[https://cran.r-project.org/web/packages/MLmetrics/MLmetrics.pdf\] are
geared towards machine learning problems and don't implement the set of
metrics and scoring rules desired for forecast evaluation. The package
aims to bring forth a standardised and tested toolkit. It offers
convenient automated forecast evaluation in a data.table format, but
also provides experienced users with a set of reliable lower-level
scoring metrics they can build upon in other applications. In addition
it implements a wide range of flexible plots that are able to cover most
day-to-day use cases.

This paper provides an overview of the fundamental ideas behind forecast
evaluation, gives a detailed explanation of the evaluation metrics in
and discusses what to think about when applying them in practice. It
then presents a case study based on the evaluation of Covid-19 related
short-term forecasts in the UK [@funkShorttermForecastsInform2020].

## Forecast types and forecast formats

In its most general sense, a forecast is the forecaster's stated belief
about the future [@gneitingStrictlyProperScoring2007] that can come in
many different forms. Quantitative forecasts are either point forecasts
or probabilistic in nature and can make statements about continuous,
discrete or binary outcome variables. Point forecasts only give one
single number for the most likely outcome, but do not quantify the
forecaster's uncertainty. This limits their usefulness, as a very
certain forecast may, for example, warrant a very different course of
actions than does a very uncertain one. Probabilistic forecasts, in
contrast, by definition provide a full predictive distribution. This
makes them much more useful in any applied setting, as we learn about
the forecaster's uncertainty and their belief about all aspects of the
underlying data-generating distribution (including e.g. skewness or the
width of its tails). Probabilistic forecasts are therefore the focus of
this paper as well as the package.

The predictive distribution of a probabilistic forecast can be
represented in different ways with implications for the appropriate
evaluation approach. For most forecasting problems, predictive
distributions are not readily available in a closed form (and the
package therefore does not support scoring them directly). Instead,
predictive distributions are often represented by a set of quantiles or
predictive samples. Predictive samples require a lot of storage space
and also come with a loss of precision that is especially pronounced in
the tails of the predictive distribution, where quite a lot of samples
are needed to accurately characterise the distribution. For that reason,
often quantiles or central prediction intervals are reported instead
\[citation FORECAST HUBS\]. For binary or multinomial prediction
targets, common in many classification problems, a probabilistic
forecasts is represented by the probability that an outcome will come
true. Table [1](#tab:forecast-types){reference-type="ref"
reference="tab:forecast-types"} summarises the different forecast types
and formats. While specific metrics may differ depending on the forecast
type or format, the general forecasting paradigm
[@gneitingProbabilisticForecastsCalibration2007] that guides the
evaluation process is the same.

::: {#tab:forecast-types}
  **Forecast type**   **Target type**             **Representation of the predictive distribution**
  ------------------- --------------------------- -----------------------------------------------------
  Point forecast      continuous integer binary   one single number for the predicted outcome
                      continuous integer          predictive samples quantiles closed analytical form
                      binary                      binary probabilities

  : [\[tab:forecast-types\]]{#tab:forecast-types
  label="tab:forecast-types"} Forecasts can be distinguished by whether
  they are probabilistic in nature, or a point forecast only. Depending
  on the type of the target (discrete, continuous or binary) different
  representation of the predictive distribution are possible.
:::

## The forecasting paradigm

Any forecaster should aim to minimise the difference between the
(cumulative) predictive distribution $F$ and the unknown true
data-generating distribution $G$
[@gneitingProbabilisticForecastsCalibration2007]. For an ideal forecast,
we therefore have $$F = G,$$ where $F$ and $G$ are both cumulative
distribution functions. As we don't know the true data-generating
distribution, we cannot assess the difference between the two
distributions directly. [@gneitingProbabilisticForecastsCalibration2007]
instead suggest to focus on two central aspects of the predictive
distribution, calibration and sharpness (illustrated in Figure
[1](#fig:forecast-paradigm){reference-type="ref"
reference="fig:forecast-paradigm"}. Calibration refers to the
statistical consistency (i.e. absence of systematic deviations) between
the predictive distribution and the observations. Sharpness is a feature
of the forecast only and describes how concentrated the predictive
distribution is, i.e. how precise the forecasts are. The general
forecasting paradigm states that we should maximise sharpness of the
predictive distribution subject to calibration. A model that made very
precise forecasts would at best be useless if the forecasts were wrong
most of the time. On the other hand, a model may be well calibrated, but
not sharp enough to be useful. Take a weather forecast that would assign
30 percent rain probability for every single day. It may be (marginally)
calibrated when looking at the average rainfall over the course of a
year, but it doesn't give much guidance on a day to day basis.
[@gneitingProbabilisticForecastsCalibration2007] discuss different forms
of calibration in more detail.

![[\[fig:forecast-paradigm\]]{#fig:forecast-paradigm
label="fig:forecast-paradigm"} Schematic illustration of calibration and
sharpness. True value are represented in red, the predictive
distribution is shown in
black](plots/forecast-paradigm.png){#fig:forecast-paradigm}

# Scoring metrics implemented in 

Some of the metrics in focus only on sharpness or on calibration.
Others, called proper scoring rules, combine both aspects into a single
number. The former can be helpful to learn about specific model aspects
and improve them, the latter are especially useful to assess and rank
predictive performance of a forecaster. The following gives an
introduction to how these metrics can be used to evaluate forecasts.
Table
[\[tab:table-summary-scores\]](#tab:table-summary-scores){reference-type="ref"
reference="tab:table-summary-scores"} shows an overview of the metrics
implemented in . Table
[\[tab:score-table\]](#tab:score-table){reference-type="ref"
reference="tab:score-table"} in the Appendix gives a mathematical
definition and thorough explanation of all the metrics. Figure
[\[fig:calibration-plots\]](#fig:calibration-plots){reference-type="ref"
reference="fig:calibration-plots"} gives an applied example of different
scoring metrics and visualisations for simple toy data.

::: landscape
::: {#tab:table-summary-scores2}
  Metric                                 Discrete         Continuous     Binary            Closed-form    Samples (approx.)   Quantiles      Properties
  -------------------------------------- ---------------- -------------- ----------------- -------------- ------------------- -------------- ----------------------------------------------------------------------------
                                                                                                                                             
  Metric                                 Discrete         Continuous     Binary            Closed-form    Samples (approx.)   Quantiles      Properties
                                                                                                                                             
  Log score (logS)                       ($\checkmark$)   $\checkmark$                     $\checkmark$   $\checkmark$                       proper scoring rule, local, unstable for outliers
                                                                                                                                             
  Dawid-Sebastiani score (DSS)           $\checkmark$     $\checkmark$                     $\checkmark$   $\checkmark$                       proper scoring rule, somewhat global, somewhat stable handling of outliers
                                                                                                                                             
  Interval coverage                      $\checkmark$     $\checkmark$                                    THINK AGAIN         $\checkmark$   measure for calibration
                                                                                                                                             
  Probability integral transform (PIT)   $\checkmark$     $\checkmark$                     $\checkmark$   $\checkmark$        $\checkmark$   assesses calibration
                                                                                                                                             
  Bias                                   $\checkmark$     $\checkmark$   $\checkmark$???   $\checkmark$   $\checkmark$        $\checkmark$   captures tendency to over-or underpredict (aspect of calibration)
                                                                                                                                             
  Relative skill                         depends          depends        depends           depends        depends             depends        Ranks models based on pairwise comparisons
                                                                                                                                             

  : [\[tab:table-summary-scores2\]]{#tab:table-summary-scores2
  label="tab:table-summary-scores2"} Summary table of scores available
  in scoringutils
:::
:::

## Evaluating calibration and sharpness independently

Evaluating calibration and sharpness independently is helpful for model
diagnostics. To that end makes numerous metrics available that aim to
capture different aspects of sharpness and calibration.

### Assessing calibration

Calibration means consistency between forecasts and observed values, but
there are various ways in which a forecast can systematically deviate
from the observations (see
[@gneitingProbabilisticForecastsCalibration2007] for a discussion of
different forms of calibration MAYBE IT WOULD BE A GOOD IDEA TO INCLUDE
THESE DIFFERENT FORMS OF CALIBRATION HERE). allows the user to examine
three different sub-aspects of calibration: bias, empirical coverage,
and the probability integral transform (PIT).

Bias, i.e. systematic over- or underprediction, is a very common form of
miscalibration which therefore deserves separate attention. The bias
metric (with slightly different versions for the various forecast types
and formats) captures a general tendency to over- and underpredict that
is bound to be between minus one (underpredicton) and one
(overprediction), where zero is ideal. It is derived by looking at how
much of the probability mass of the predictive distribution is below or
above the true observed value. For quantile forecasts we have second
alternative approach available to assess over- and underprediction - by
simply looking at the corresponding components of the weighted interval
score. What is different between the over- and underprediction
components and bias as described above is its sensitivity to outliers.
The former are derived from absolute differences, while the latter is
bound and rather captures a general tendency to be biased.

Another way to look at calibration (precisely: probabilistic calibration
in [@gneitingProbabilisticForecastsCalibration2007]) is to compare the
proportion of observed values covered by different parts of the
predictive distribution with the nominal coverage implied by the CDF of
the distribution. This is most easily understood in the context of
quantile forecasts, but can easily be transferred to sample-based
continuous and discrete forecasts as well. To assess empirical coverage
at a certain interval range, we simply measure the proportion of true
observed values that fall into corresponding range of the predictive
distribution. If the 0.05, 0.25, 0.75, and 0.95 quantiles are given,
then 50% of the true values should fall between the 0.25 and 0.75
quantiles and 90% should fall between the 0.05 and 0.95 quantiles. We
can calculate and plot these values to inspect how well different parts
of the forecast distribution are calibrated. To get an even more precise
picture, we can also look at the percentage of true values below every
single quantile of the predictive distribution. This allows to diagnose
issues in the lower and upper tails of the prediction intervals
separately. A similar way to visualise the same information is a PIT
histogram. In order to conveniently assess deviations between the
predictive distribution and the true data-generating distribution we can
transform the observed values using the probability integral
transformation (PIT) [@dawidPresentPositionPotential1984] (see more
details in Table
[\[tab:score-table\]](#tab:score-table){reference-type="ref"
reference="tab:score-table"}). If both distributions are equal, the
transformed values will follow a uniform distribution. A histogram of
the transformed values can help to diagnose systematic differences
between the predictions and the observed values. Figure
[\[fig:calibration-plots\]](#fig:calibration-plots){reference-type="ref"
reference="fig:calibration-plots"} exemplifies the characteristic shape
of certain systematic deviations of the predicitve distribution from the
true data-generating distribution. In the PIT histograms, bias leads to
a triangular shape, overdispersion results in a hump shaped form and
underdispersion in a U-shape. ADD INTERPRETATION FOR QUANTILE AND
INTERVAL COVERAGE PLOTS HERE.

### Assessing sharpness

Sharpness is the ability to produce narrow forecasts. It does not depend
on the actual observations and is a quality of the forecast only
[\[gneitingProbabilisticForecastsCalibration2007\]](#gneitingProbabilisticForecastsCalibration2007){reference-type="ref"
reference="gneitingProbabilisticForecastsCalibration2007"}. Sharpness is
therefore only useful subject to calibration, as exemplified above in
Figure
[\[fig:calibration-example\]](#fig:calibration-example){reference-type="ref"
reference="fig:calibration-example"}. We may be willing to trade off a
little calibration for a lot more sharpness, but usually not much. For
sample-based forecasts, calculates sharpness as the normalised median
absolute deviation about the median (MADN)
\[funkAssessingPerformanceRealtime2019\] (for details see Table
[\[tab:score-overview\]](#tab:score-overview){reference-type="ref"
reference="tab:score-overview"}). For quantile forecasts, we take the
sharpness component of the WIS which corresponds to a weighted average
of the individual interval widths.

WHAT WOULD BE REALLY COOL IS TO DETERMINE A WAY TO FIND THE OPTIMAL
SHARPNESS OF A FORECAST. AS IS, I FEEL THIS PARAGRAPH IS SOMEWHAT
USELESS.

## Proper scoring rules

Proper scoring rules [@gneitingStrictlyProperScoring2007] jointly assess
sharpness and calibration and assign a single numeric value to a
forecast. A scoring rule is proper if a perfect forecaster (the
predictive distribution equals the data-generating distribution)
receives the lowest score on average. This makes sure that a forecaster
evaluated by a proper scoring rule is always incentivised to state their
best estimate. As summarised in Table
[\[tab:table-summary-scores\]](#tab:table-summary-scores){reference-type="ref"
reference="tab:table-summary-scores"}, not all proper scoring rules are
suitable for all forecast formats.

## Proper scoring rules for sample-based forecasts (CRPS, logS and DSS)

For sample-based forecasts, the provides the following proper scoring
rules: the (continuous) ranked probability score (CRPS) \[CITATION\],
the log score (logS) \[CITATION\], and the Dawid-Sebastiani-score (DSS)
\[CITATION\] (formal definitions are given in Table
[\[tab:score-table\]](#tab:score-table){reference-type="ref"
reference="tab:score-table"}). The corresponding functions are imported
from the package and exposed to the user through a slightly adapted
interface. Other, non-sample-based variants of the CRPS, logS and DSS
are available directly in the package, but not in . All three scores are
in principle applicable to continuous as well as discrete forecasts. The
implementation of the log score, however, requires a kernel density
estimation that may be inappropriate for discrete values (see also Table
[\[tab:table-summary-scores\]](#tab:table-summary-scores){reference-type="ref"
reference="tab:table-summary-scores"}). The logS is therefore not
computed for discrete predictions in .

When scoring forecasts in a sample-based format, the choice is usually
between the logS and the CRPS. The DSS is much less commonly used. It is
easier to compute, but apart from that does not have immediate
advantages over the former two. CRPS and logS differ in three important
aspects: sensitivity to distance [@winklerScoringRulesEvaluation1996],
sensitivity to outlier predictions, and sensitivity to the order of
magnitude of the forecasted quantity.

The CRPS is a so-called global scoring rule, which means that the entire
predictive distribution is taken into account when scoring a single
forecast. The log score, on the other hand is local. The resulting score
does not depend on the overall distance between the observed value and
the distribution, but only on the probability density assigned to the
actual outcome. Imagine two forecasters, A and B, who forecast the
number of goals scored by a team in a football match. If both
forecasters assigned the same probability to the true outcome (e.g. 2
points), but A assigned higher probability to extreme outcomes far away
from the actually observed outcome (e.g. A stated 10 goals may be quite
likely, while B's predictions are concentrated around 1, 2 or 3 points),
then A will receive a worse score than B. The log score, in contrast, is
a local scoring rule that only scores the probability assigned to the
actual outcome and ignores the rest of the predictive distribution.
Judged by the log score, A and B would receive exactly the same score.
Sensitivity to distance (taking the entire predictive distribution into
account) may be an advantage in most settings that involve decision
making. Forecaster A's prediction that assigns high probability to
results far away from the observed value is arguably less useful than
B's forecast that assigns higher probability to values closer to it (the
probability assigned to the actual outcome being equal for both
forecasts). The log score is only implicitly sensitive to distance if we
assume that values close to the observed value are actually more likely
to occur. The logS may, however, be more appropriate for inferential
purposes (see [@winklerScoringRulesEvaluation1996]) and is commonly used
in Bayesian statistics \[CITATION\].

A second important difference is how forecasts are treated that deviate
strongly from the observed outcome. The CRPS can be thought of as a
generalisation of the absolute error to a predictive distribution. It
therefore scales linearly with the distance between forecast
distribution and true value. The log score, however, is the log of the
predictive density evaluated at the observed value. It can quickly go to
negative infinity if the probability assigned to the observed outcome is
close to zero. The CRPS is therefore considered more stable than the log
score. The behaviour of the DSS is in between the two. Whether or not
harsh punishment of bad predictions is desirable or not depends of
course on the setting. [@bracherEvaluatingEpidemicForecasts2020a]
exemplify that in practice there may indeed be substantial differences
between how the CRPS and log score judge the same forecast.

As the CRPS is a generalisation of the absolute value, overall scores
depend on the order of magnitude of the quantity we try to forecast.
This makes it harder to compare forecasts for very different targets, or
assess average performance if the quantity of interest varies
substantially over time. Average scores are dominated by forecasts for
targets with high absolute numbers. This may be desirable,if we care
most about forecasts in situations where numbers are high, but usually
it is not. LogS and DSS are more robust against this effect. Another way
to address this issue by using pairwise comparisons will be introduced
later.

## Proper scoring rules for non-sample-based forecasts (WIS and BS)

For forecasts in an interval or quantile format, offers the weighted
interval score (WIS) [@bracherEvaluatingEpidemicForecasts2020a]. The WIS
has very similar properties to the CRPS and can be thought of as a
quantile-based approximation. For an increasing number of equally-spaced
prediction intervals the WIS converges to the CRPS. One additional
benefit of the WIS is that it can easily be decomposed into three
additive components: an uncertainty penalty (sharpness) for the width of
a prediction interval and penalties for over- and underprediction (if a
value falls outside of a prediction interval). This can be very helpful
in diagnosing model problems. It may even be useful to convert samples
into quantiles and use the WIS instead of the CRPS to make use of this
decomposition for the purpose of model diagnostics. Binary forecasts can
be scored using the Brier score (BS) \[CITATION\], which corresponds to
the squared difference between the given probability and the outcome
(either 0 or 1).

## Pairwise comparisons

If what we care about is to determine which model performs best,
pairwise comparisons between models are a suitable approach \[CITATION
CRAMER et al.\]. In turn, each pair of models is evaluated based on the
targets that both models have predicted. The mean score by one model is
divided by the mean score of the other model to obtain the mean score
ratio (see Table
[\[tab:score-overview\]](#tab:score-overview){reference-type="ref"
reference="tab:score-overview"}, a measure of relative performance. To
obtain an overall relative skill score for a model, we take the
geomatric mean of all mean score ratios that involve that model
(omitting comparisons where there is no overlapping set of forecasts).
This gives us an indicator of performance relative to all other models.
The orientation depends on the score used. For the proper scoring rules
described above, smaller is better and a relative skill score smaller
than 1 indicates that a model is performing better than the average
model. We can obtain a scaled relative skill score by dividing a model's
relative skill by the relative skill of a baseline model. A scaled
relative skill smaller than one then means that the model in question
performed better than the baseline.

It is in principle possible to obtain p-values that help determine
whether two models perform significantly differently. allows to compute
these using eitab:score-tablether the Wilcoxon rank sum test or a
permutation test. In practice, this is slightly complicated by the fact
that both tests assume independent observations. In reality, however,
forecasts by a model may be correlated across time or another dimension
(e.g. if a forecaster has a bad day, they will likely perform badly
across different targets for a given forecast date). P-values may
therefore be too quick to suggest significant differences where there
aren't any. One way to mitigate this is to aggregate observations over a
category where one suspects correlation. A test that is performed on
aggregate scores will likely be more conservative.

# Evaluating UK short-term forecasts

The following section shows an example evaluation of short-term
predictions of four different Covid-19 related targets in the UK made
between March 31 and July 13 2020. Forecasts were produced by six
research groups in the UK, and submitted to the Scientific Pandemic
Influenza Group on Modelling (SPI-M). The forecasts aimed to assess the
likely future burden the UK healthcare system would face from the
Covid-19 pandemic. Predictions submitted to SPI-M were aggregated and
used to inform UK government health policy through the Strategic
Advisory Group of Experts (SAGE). All predictions as well as most of the
observed values are now publicly available and discussed in more depth
in \[FUNK ET AL\].

The evaluation process looks as follows: First we load, prepare and
visualise (some of) the data. Then we obtain forecast scores and a model
ranking based on pairwise-comparisons, followed by a more detailed
analysis of calibration and sharpness.

## Preparing and visualising the data

We first need to obtain the data by installing and loading the using the
following commands:

::: Schunk
::: Sinput
R> \# install and load data pacakge from external repository R> \#
remotes::install_github(\"sbfnk/covid19.forecasts.uk\") R> R> \# load
packages R> library(covid19.forecasts.uk) R> library(dplyr) R>
library(scoringutils) R> \# load truth data R> data(covid_uk_data) R> \#
head(covid_uk_data) R> R> \# load forecasts R> data(uk_forecasts) R> \#
head(uk_forecasts)
:::
:::

Let us take a first look a the data:

::: Schunk
::: Sinput
R> glimpse(covid_uk_data)
:::

::: Soutput
Rows: 4,174 Columns: 6
$geography  <fct> London, London, London, London, London, London, L…$
value_type \<fct> hospital_inc, hospital_inc, hospital_inc, hospita...
$value_desc <fct> Hospital admissions, Hospital admissions, Hospita…$
truncation \<dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
$value_date <date> 2020-03-20, 2020-03-21, 2020-03-22, 2020-03-23, …$
value \<dbl> 18, 232, 280, 241, 313, 353, 516, 637, 677, 546, ...
:::

::: Sinput
R> glimpse(uk_forecasts)
:::

::: Soutput
Rows: 1,254,513 Columns: 8
$model         <fct> EpiSoon, EpiSoon, EpiSoon, EpiSoon, EpiSoon, E…$
geography \<chr> \"East of England\", \"East of England\", \"East of...
$value_type    <fct> hospital_inc, hospital_inc, hospital_inc, hosp…$
creation_date \<date> 2020-03-31, 2020-03-31, 2020-03-31, 2020-03-3...
$value_date    <date> 2020-04-01, 2020-04-02, 2020-04-03, 2020-04-0…$
quantile \<dbl> 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05...
$value         <dbl> 70, 64, 53, 42, 30, 19, 4, 0, 0, 0, 0, 0, 0, 0…$
value_desc \<fct> Hospital admissions, Hospital admissions, Hosp...
:::
:::

Some minor changes need to be made to the data before we can evaluate
the forecasts. The names of the columns that hold the forecasts and the
true observed values need to be changed to and . While we could also
proceed with separate data sets for the evaluation, we merge the two
data sets in order to remove all instances where the forecasts, but not
the true observations were made public. The package provides a function
that attempts to merge the data sets in a sensible way.

::: Schunk
::: Sinput
R> uk_forecasts \<- rename(uk_forecasts, prediction = value) R>
covid_uk_data \<-rename(covid_uk_data, true_value = value) R> combined
\<- merge_pred_and_obs(uk_forecasts, covid_uk_data)
:::
:::

It is helpful to start the evaluation process by visualising the
available data, as missing forecasts can impact the evaluation if
missingness is not random, but instead correlates with performance. The
function returns a heatmap with the number of available forecasts. By
default, the function treats a set of different quantiles or samples as
one forecast. However, the user can specify manually which elements to
treat as one forecast and which categories to sum over to count the
number of available forecasts.

::: Schunk
::: Sinput
R> show_avail_forecasts(data = combined, + x = \"creation_date\", +
show_numbers = FALSE, + legend_position = \"bottom\", + facet_formula =
  value_desc)
:::
:::

![[\[fig:avail-forecasts\]]{#fig:avail-forecasts
label="fig:avail-forecasts"} Overview of the number of available
forecasts](plots/plot-show-availability.pdf){#fig:avail-forecasts}

The forecasts and observed values themselves can be visualised using the
function. Data visualisation is of course highly context-dependent, but
the function tries to accommodate most practical use-cases. In addition
to basic plotting functionality it offers the user an optional ad hoc
way to filter both forecasts and observed values. This makes it possible
to tweak the plot in a beginner-friendly way without having to
manipulate the data separately. Forecasts and observed values can be
passed in separately (and are merged internally) or as a single
data.frame. Conditions to filter on need to be provided as a list of
strings, where each of the strings represents an expression that can be
evaluated to filter the data. To display, for example, death forecasts
from the SIRCOVID model made on the 22nd July 2020 for the four UK
countries and add three weeks of prior observations to the plot, we can
call:

::: Schunk
::: Sinput
R> locations \<- 'c(\"Scotland\", \"Wales\", \"Northern Ireland\",
\"England\")' R> plot_predictions(truth = covid_uk_data, + forecasts =
uk_forecasts, + filter_both = list(paste(\"geography + 'value_type ==
\"death_inc_line\"'), + filter_truth = list('value_date \<=
\"2020-06-22\"', + 'value_date \> \"2020-06-01\"'), + filter_forecasts =
list('model == \"SIRCOVID\"', + 'creation_date == \"2020-06-22\"'), + x
= \"value_date\", + facet_formula =   geography) + +
ggplot2::theme(legend.position = \"bottom\")
:::
:::

The output is shown in Figure
[\[fig:forecast-visualisation\]](#fig:forecast-visualisation){reference-type="ref"
reference="fig:forecast-visualisation"}.

## Scoring forecasts with 

The actual scoring of forecasts based on observed values can be
performed using the function . The function automatically detects the
forecast type and format, applies the appropriate scoring metrics and
aggregates results as desired by the user. Internally, operations are
handled using to allow for fast and efficient computation. The function
requires a or similar with some pre-specified columns (e.g. a column
called \"prediction\", one called \"true_value\" and more depending on
the forecast format). Additional columns may be present to indicate a
grouping of forecasts, for example forecasts made in different locations
or over different forecast horizons. To illustrate the requirements,
provides example data for all possible forecast formats. It also offers
functions to to transform between various formats, e.g. from a sample
based format to a quantile format.

When conducting an evaluation, we need to decide on the level of
grouping over which we want to aggregate scores. For the UK short-term
forecasts it makes sense to look at the scores achieved by every model
separate for all prediction targets. This can be achieved by calling

::: Schunk
::: Sinput
R> scores \<- eval_forecasts(combined, + summarise_by = c(\"model\",
\"value_desc\")) R> glimpse(scores)
:::
:::

aggregates scores by taking the mean over according to the grouping
specified in . In the above example, if , then scores would be averaged
over all creation dates, forecast horizons (as represented by the the
value dates), locations and quantiles to yield one score per model and
forecast target. For a more detailed analysis we could for example
specifiy to additionally stratify by location. If we wanted to have one
score per quantile or one per prediction interval range, we could use
something like . This would allow us to examine interval or quantile
coverage or makes it possible to analyse the accuracy of the tails of
the forecasts. In addition to the mean, we can also obtain the standard
deviation of the scores over which we average, as well as any desired
quantile, by specifying and for example for the median.

The user must, however, exercise some caution when aggregating scores.
As explained above, many of the metrics are absolute and scale with the
magnitude of the quantity to forecast. This makes it sometimes
ill-advised to average over them. In the given example, looking at one
score per model (i.e. specifying ) is problematic, as overall aggregate
scores would be dominated by hospital admissions, while errors on death
forecasts would have little influence. Similarly, aggregating over
different forecast horizons is often not a good idea as the mean will be
dominated by further ahead forecast horizons.

The second important argument of the function is the argument that
denotes the unit of a single forecast. In the above example, the unit of
a single forecast would be as there is one distinct forecast for each
combination of these categories. Quantiles do not need to be included,
as several quantiles make up one forecast (and similarly for samples).
By default, if , will automatically use all present columns to determine
the unit of a single forecast. In the above example, we did not have to
explicitly specify the argument, as all available columns were relevant
to the unit of a single forecast. If there are additional columns that
do not correspond to the unit of a single forecast, like for example the
observed temperature in a given location, must be specified and these
columns must be excluded.

In order to obtain a model ranking, we recommend to look at the relative
skill in terms of an appropriate proper scoring rule instead of the raw
score. Relative skill scores can be aggregated more easily across
different forecast targets as they are less influenced by the order of
magnitude of the quantity to forecast than e.g. the WIS or the CRPS.

Pairwise comparisons between models \[CITATION\] can be obtained in two
different ways. First, relative skill scores based on pairwise
comparisons are by default returned from . These will be computed
separately for the categories defined in the argument (excluding the
category 'model'). Alternatively, a set of scores can be post-processed
using the separate function . This approach is to be used for
visualisation and if p-values for the pairwise comparisons are needed,
as those are not returned from . Usually, one would compute scores
without specifying a argument, but sometimes it may be sensible to
average over certain scores, for example for predictions generated at a
certain date. This allows to reduce the correlation between observations
that enter the computation of p-values, which in turn makes the test
less liberal. Using the function we can visualise the mean score ratios
between all models as well as the

::: Schunk
::: Sinput
R> \# unsummarised scores R> unsummarised_scores \<-
eval_forecasts(combined) R> pairwise \<-
pairwise_comparison(unsummarised_scores, + summarise_by =
\"value_desc\")
:::
:::

The result is a with different scores and metrics in a tidy format that
can easily be used for further manipulation and plotting.

# Visualisation and interpretation of evaluation results

## Visualising aggregate scores and rankings

A good starting point for an evaluation is the following score table
that visusalises the scores we produced above. We can facet the table to
account for the different forecast targets:

::: Schunk
::: Sinput
R> score_table(scores, y = \"model\", facet_formula =   value_desc)
:::
:::

![[\[fig:score-table\]]{#fig:score-table label="fig:score-table"}
Coloured table to visualise the computed
scores](plots/plot-score-table.pdf){#fig:score-table}

The most informative metric in terms of model ranking is the
relative_skill. However, interpretation is not always straightforward
and has to be done carefully. We can see that performance varied quite a
bit across different metrics, where some models did well on one target,
but poorly on another. Especially the Exponential growth/decline model
stands out as it received the lowest relative skill score for hospital
admissions, but the highest for the total number of beds occupied.
Looking back at Figure [2](#fig:avail-forecasts){reference-type="ref"
reference="fig:avail-forecasts"}, we see that the model has only
submitted very few forecasts over all. It may therefore be sensible to
require all models to have submitted forecasts for at least 50% of all
forecast targets in order to enter the pairwise comparisons. For similar
reasons, the interval score may be deceiving if looked at in isolation.
As can be seen, the DetSEIRwithNB MLE model received a lower relative
skill score, but a higher interval score than the DetSEIRwithNB MCMC
model. This, again, can be explained by the fact that they forecasted
different targets. The interval score, as an absolute metric, is highly
influenced by the absolute value of the quantity that is forecasted. For
the same reason, one should be careful when summarising interval scores
from different locations or forecast targets, as the average score will
be dominated by outliers as well as differences in the absolute level.
Assuming a large enough set of available overlapping forecasts, the
relative skill score is more robust. It therefore is reasonable to
assume that the DetSEIRwithNB MLE forecasted quantities with a higher
absolute value, but tended to perform worse than the DetSEIRwithNB MCMC
model as far as we can tell based on the set of all pariwise
comparisons. This can be confirmed for the direct comparison between the
two by looking at the mean score ratios from the pairwise comparisons.
These can be obtained by calling

::: Schunk
::: Sinput
R> plot_pairwise_comparison(pairwise) + + ggplot2::facet_wrap( 
value_desc, scales = \"free_x\")
:::
:::

![[\[fig:pairwise-comparison\]]{#fig:pairwise-comparison
label="fig:pairwise-comparison"} Ratios of mean scores based on
overlapping forecast sets. If a tile is blue, then the model on the
y-axis performed better. If it is red, the model on the x-axis performed
better in direct comparison.
](plots/plot-pairwise-plot.pdf){#fig:pairwise-comparison}

In terms of actually understanding *why* one model performs well or
badly, the other metrics shown in Figure
[3](#fig:score-table){reference-type="ref" reference="fig:score-table"}
provide additional insight. We turn to them in the following.

## Visual model diagnostics

For forecasts in an interval format, looking at the components of the
weighted interval score separately is a natural next step. We can see in
Figure [5](#fig:wis-components){reference-type="ref"
reference="fig:wis-components"} that the majority of penalties come from
over-and underprediction, instead of the sharpness component. We also
see that most models tended to either over- or underpredict actual
numbers.

::: Schunk
::: Sinput
R> wis_components(scores, + facet_formula =   value_desc, + scales =
\"free_x\", + relative_contributions = TRUE, + x_text_angle = 0) + +
ggplot2::coord_flip() + + ggplot2::theme(legend.position = \"bottom\")
:::
:::

![[\[fig:wis-components\]]{#fig:wis-components
label="fig:wis-components"} p-values and ratios
together.](plots/plot-WIS-components.pdf){#fig:wis-components}

We can have a closer look at calibration using the functions and . The
interval coverage plot shows the proportion of all true values that fall
within all the different prediction intervals. This gives a visual
impression of probabilistic calibration
[\[gneitingProbabilisticForecastsCalibration2007\]](#gneitingProbabilisticForecastsCalibration2007){reference-type="ref"
reference="gneitingProbabilisticForecastsCalibration2007"}. Ideally, $x$
percent of true values should be covered by the $x$%-prediction
intervals, resulting in a 45° line. Areas shaded in green indicate that
the model is covering more true values than it actually should, while
areas in white indicate that the model fails to cover the desired
proportion of true values with its prediction intervals. The majority of
the models were too confident in their predictions, while some showed
showed good calibration. The quantile coverage plot shows the proportion
of all true values below certain predictive quantiles. While this plot
is slightly harder to interpret, it also includes information about bias
as and allows to separate the lower and upper boundaries of the
prediction intervals. We can see, for example, that the Exponential
growth/decline model was consistently biased downwards. Figure
[6](#fig:coverage){reference-type="ref" reference="fig:coverage"}

::: Schunk
::: Sinput
R> cov_scores \<- eval_forecasts(combined, + summarise_by = c(\"model\",
+ \"range\", \"quantile\")) R>
scoringutils::interval_coverage(cov_scores) R>
scoringutils::quantile_coverage(cov_scores)
:::
:::

![[\[fig:coverage\]]{#fig:coverage label="fig:coverage"} quantile and
interval coverage](plots/plot-coverage.pdf){#fig:coverage}

DO I WANT TO INCLUDE PIT PLOTS AS WELL? I GUESS? NEED TO LOOK AT THE
IMPLEMENTATION FOR QUANTILE FORECASTS

Look at e.g. bias by location? Figure
[7](#fig:bias-heatmap){reference-type="ref"
reference="fig:bias-heatmap"}

::: Schunk
::: Sinput
R> scores \<- eval_forecasts(combined, + summarise_by = c(\"model\", +
\"value_desc\", + \"geography\"), + compute_relative_skill = FALSE) R>
scoringutils::score_heatmap(scores, metric = \"bias\", + x =
\"geography\", facet_formula =   value_desc, + scale = \"free_x\")
:::
:::

![[\[fig:bias-heatmap\]]{#fig:bias-heatmap label="fig:bias-heatmap"}
bias by location](plots/plot-calibration.pdf){#fig:bias-heatmap}

WHAT IS NEEDED HERE IS A BIT OF THINKING WITH REGARDS TO WHAT
VISUALISATION I WANT TO SHOW AND IN HOW MUCH DETAIL I WANT TO ANALYSE
THE MODELS.

::: CodeInput
.
:::

# Summary and discussion {#sec:summary}

COMING SOON.

# Acknowledgments {#acknowledgments .unnumbered}

::: appendix
# Appendix section {#app:technical}
:::
