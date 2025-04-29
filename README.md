
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tailor

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tailor)](https://CRAN.R-project.org/package=tailor)
[![R-CMD-check](https://github.com/tidymodels/tailor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/tailor/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/tailor/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/tailor)
<!-- badges: end -->

Postprocessors refine predictions outputted from machine learning models
to improve predictive performance or better satisfy distributional
limitations. This package introduces ‘tailor’ objects, which compose
iterative adjustments to model predictions. In addition to utilities to
create new adjustments, the package provides a number of pre-written
ones:

- For probability distributions: calibration
- For transformation of probabilities to hard class predictions:
  thresholds, equivocal zones
- For numeric distributions: calibration, range

Tailors are tightly integrated with the
[tidymodels](https://tidymodels.org) framework. For greatest ease of
use, situate tailors in model
[workflows](https://workflows.tidymodels.org) with `add_tailor()`.

The package is under active development; please treat it as experimental
and don’t depend on the syntax staying the same.

## Installation

You can install the development version of tailor like so:

``` r
pak::pak("tidymodels/tailor")
```

## Example

``` r
library(tailor)
```

The `two_class_example` dataset from modeldata gives the true value of
an outcome variable `truth` as well as predicted probabilities (`Class1`
and `Class2`). The hard class predictions, in `predicted`, are
`"Class1"` if the probability assigned to `"Class1"` is above `.5`, and
`"Class2"` otherwise.

``` r
library(modeldata)
#> 
#> Attaching package: 'modeldata'
#> The following object is masked from 'package:datasets':
#> 
#>     penguins
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

head(two_class_example)
#>    truth      Class1       Class2 predicted
#> 1 Class2 0.003589243 0.9964107574    Class2
#> 2 Class1 0.678621054 0.3213789460    Class1
#> 3 Class2 0.110893522 0.8891064779    Class2
#> 4 Class1 0.735161703 0.2648382969    Class1
#> 5 Class2 0.016239960 0.9837600397    Class2
#> 6 Class1 0.999275071 0.0007249286    Class1
```

The model predicts `"Class1"` more often than it does `"Class2"`.

``` r
two_class_example |> count(predicted)
#>   predicted   n
#> 1    Class1 277
#> 2    Class2 223
```

If we wanted the model to predict `"Class2"` more often, we could
increase the probability threshold assigned to `"Class1"` above which
the hard class prediction will be `"Class1"`. In the tailor package,
this adjustment is implemented in `adjust_probability_threshold()`,
which can be situated in a tailor object.

``` r
post_obj <-
  tailor() |>
  adjust_probability_threshold(threshold = .9)

post_obj
#> 
#> ── tailor ──────────────────────────────────────────────────────────────────────
#> A binary postprocessor with 1 adjustment:
#> 
#> • Adjust probability threshold to 0.9.
```

tailors must be fitted before they can predict on new data. For
adjustments like `adjust_probability_threshold()`, there’s no training
that actually happens at the `fit()` step besides recording the name and
type of relevant variables. For other adjustments, like probability
calibration with `adjust_probability_calibration()`, parameters are
actually estimated at the `fit()` step and separate data should be used
to train the postprocessor and evaluate its performance.

In this case, though, we can `fit()` on the whole dataset. The resulting
object is still a tailor, but is now flagged as trained.

``` r
post_res <- fit(
  post_obj,
  two_class_example,
  outcome = c(truth),
  estimate = c(predicted),
  probabilities = c(Class1, Class2)
)

post_res
#> 
#> ── tailor ──────────────────────────────────────────────────────────────────────
#> A binary postprocessor with 1 adjustment:
#> 
#> • Adjust probability threshold to 0.9. [trained]
```

When used with a model [workflow](https://workflows.tidymodels.org) via
`add_tailor()`, the arguments to `fit()` a tailor will be set
automatically (in addition to the data splitting needed for
postprocessors that require training).

Now, when passed new data, the trained tailor will determine the
outputted class based on whether the probability assigned to the level
`"Class1"` is above `.9`, resulting in more predictions of `"Class2"`
than before.

``` r
predict(post_res, two_class_example) |> count(predicted)
#> # A tibble: 2 × 2
#>   predicted     n
#>   <fct>     <int>
#> 1 Class1      180
#> 2 Class2      320
```

Tailors compose adjustments; when several `adjust_*()` functions are
called iteratively, tailors will apply them in order at `fit()` and
`predict()` time.
