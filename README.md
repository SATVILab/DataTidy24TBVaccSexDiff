
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DataTidy24TBVaccSexDiff

<!-- badges: start -->
<!-- badges: end -->

The purpose of `DataTidy24TBVaccSexDiff` is to generate analysis-ready
data for the assessment of sex-associated differences in TB
vaccine-induced antigen-specific CD4 T cell responses.

## Installation

To install `DataTidy24TBVaccSexDiff`, run:

``` r
remotes::install_github(
  "SATVILab/DataTidy24TBVaccSexDiff"
)
```

## Usage

`DataTidy24TBVaccSexDiff` provides processed data, as well as
convenience functions for calculating transformations of the response
(summed and profile; see Rodo (2019) for details) and extracting
relevant timepoints (baseline, peak and memory; again, see Rodo (2019)
for details).

### Loading data

To load the data, run:

``` r
library(DataTidy24TBVaccSexDiff)
data("data_tidy_vacc_freq")
data_tidy_vacc_freq
#> # A tibble: 7,763 × 9
#>    vaccine prid  ptid           sex   infxn  timepoint subset cyt_combn response
#>    <chr>   <chr> <chr>          <chr> <chr>      <dbl> <chr>  <chr>        <dbl>
#>  1 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g+2+t+      0.0988
#>  2 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g+2+t-      0.0096
#>  3 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g+2-t+      0.0068
#>  4 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g+2-t-      0.0084
#>  5 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g-2+t+      0.0236
#>  6 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g-2+t-      0     
#>  7 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g-2-t+      0.0129
#>  8 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd8    g+2+t+      0     
#>  9 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd8    g+2+t-      0     
#> 10 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd8    g+2-t+      0.013 
#> # ℹ 7,753 more rows
```

## Calculating summed and profile responses

To calculate the summed response, run:

``` r
vacc_calc_response_summed(
  data_tidy_vacc_freq, "response"
)
#> # A tibble: 1,109 × 7
#>    vaccine prid  timepoint infxn      ptid       subset response
#>    <chr>   <chr>     <dbl> <chr>      <chr>      <chr>     <dbl>
#>  1 bcg     040           0 uninfected bcg-040-1  cd4     0.213  
#>  2 bcg     040           0 uninfected bcg-040-1  cd8     0.00718
#>  3 bcg     040           0 uninfected bcg-040-10 cd4     0.0348 
#>  4 bcg     040           0 uninfected bcg-040-10 cd8     0.00284
#>  5 bcg     040           0 uninfected bcg-040-11 cd4     0.0443 
#>  6 bcg     040           0 uninfected bcg-040-11 cd8     0.0192 
#>  7 bcg     040           0 uninfected bcg-040-12 cd4     0.233  
#>  8 bcg     040           0 uninfected bcg-040-12 cd8     0.0431 
#>  9 bcg     040           0 uninfected bcg-040-13 cd4     0.178  
#> 10 bcg     040           0 uninfected bcg-040-13 cd8     0.0278 
#> # ℹ 1,099 more rows
```

To calculate the summed profile, run:

``` r
vacc_calc_response_profile(
  data_tidy_vacc_freq, "response"
)
#> # A tibble: 7,763 × 9
#>    vaccine prid  ptid           sex   infxn  timepoint subset cyt_combn response
#>    <chr>   <chr> <chr>          <chr> <chr>      <dbl> <chr>  <chr>        <dbl>
#>  1 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g+2+t+      0.0988
#>  2 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g+2+t-      0.0096
#>  3 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g+2-t+      0.0068
#>  4 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g+2-t-      0.0084
#>  5 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g-2+t+      0.0236
#>  6 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g-2+t-      0     
#>  7 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g-2-t+      0.0129
#>  8 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd8    g+2+t+      0     
#>  9 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd8    g+2+t-      0     
#> 10 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd8    g+2-t+      0.013 
#> # ℹ 7,753 more rows
```

## Extracting timepoints

To extract the baseline response, run:

``` r
vacc_extract_baseline(data_tidy_vacc_freq)
#> # A tibble: 1,806 × 9
#>    vaccine prid  ptid           sex   infxn  timepoint subset cyt_combn response
#>    <chr>   <chr> <chr>          <chr> <chr>      <dbl> <chr>  <chr>        <dbl>
#>  1 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd4    g+2+t+      0     
#>  2 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd4    g+2+t-      0     
#>  3 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd4    g+2-t+      0     
#>  4 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd4    g+2-t-      0     
#>  5 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd4    g-2+t+      0.0006
#>  6 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd4    g-2+t-      0.0009
#>  7 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd4    g-2-t+      0.0009
#>  8 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd8    g+2+t+      0     
#>  9 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd8    g+2+t-      0     
#> 10 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd8    g+2-t+      0     
#> # ℹ 1,796 more rows
```

To extract the peak response, run:

``` r
vacc_extract_peak(data_tidy_vacc_freq)
#> # A tibble: 1,785 × 9
#>    vaccine prid  ptid           sex   infxn  timepoint subset cyt_combn response
#>    <chr>   <chr> <chr>          <chr> <chr>      <dbl> <chr>  <chr>        <dbl>
#>  1 mva85a  tb008 mva85a-tb008-8 male  uninf…         7 cd4    g+2+t+      0.0289
#>  2 mva85a  tb008 mva85a-tb008-8 male  uninf…         7 cd4    g+2+t-      0.0105
#>  3 mva85a  tb008 mva85a-tb008-8 male  uninf…         7 cd4    g+2-t+      0.0068
#>  4 mva85a  tb008 mva85a-tb008-8 male  uninf…         7 cd4    g+2-t-      0     
#>  5 mva85a  tb008 mva85a-tb008-8 male  uninf…         7 cd4    g-2+t+      0.007 
#>  6 mva85a  tb008 mva85a-tb008-8 male  uninf…         7 cd4    g-2+t-      0.0052
#>  7 mva85a  tb008 mva85a-tb008-8 male  uninf…         7 cd4    g-2-t+      0.0132
#>  8 mva85a  tb008 mva85a-tb008-8 male  uninf…         7 cd8    g+2+t+      0.0011
#>  9 mva85a  tb008 mva85a-tb008-8 male  uninf…         7 cd8    g+2+t-      0     
#> 10 mva85a  tb008 mva85a-tb008-8 male  uninf…         7 cd8    g+2-t+      0     
#> # ℹ 1,775 more rows
```

To extract the memory response, run:

``` r
vacc_extract_memory(data_tidy_vacc_freq)
#> # A tibble: 1,456 × 9
#>    vaccine prid  ptid           sex   infxn  timepoint subset cyt_combn response
#>    <chr>   <chr> <chr>          <chr> <chr>      <dbl> <chr>  <chr>        <dbl>
#>  1 mva85a  tb008 mva85a-tb008-5 male  uninf…       168 cd4    g+2+t+      0.0393
#>  2 mva85a  tb008 mva85a-tb008-5 male  uninf…       168 cd4    g+2+t-      0.0035
#>  3 mva85a  tb008 mva85a-tb008-5 male  uninf…       168 cd4    g+2-t+      0.0015
#>  4 mva85a  tb008 mva85a-tb008-5 male  uninf…       168 cd4    g+2-t-      0.0006
#>  5 mva85a  tb008 mva85a-tb008-5 male  uninf…       168 cd4    g-2+t+      0.0063
#>  6 mva85a  tb008 mva85a-tb008-5 male  uninf…       168 cd4    g-2+t-      0.002 
#>  7 mva85a  tb008 mva85a-tb008-5 male  uninf…       168 cd4    g-2-t+      0     
#>  8 mva85a  tb008 mva85a-tb008-5 male  uninf…       168 cd8    g+2+t+      0     
#>  9 mva85a  tb008 mva85a-tb008-5 male  uninf…       168 cd8    g+2+t-      0     
#> 10 mva85a  tb008 mva85a-tb008-5 male  uninf…       168 cd8    g+2-t+      0.014 
#> # ℹ 1,446 more rows
```

Note that these work automatically after the calculation of summed or
profile responses (and the order is irrelevant), for example:

``` r
data_tidy_vacc_freq |>
  vacc_calc_response_summed("response") |>
  vacc_extract_peak()
#> # A tibble: 255 × 7
#>    vaccine prid  timepoint infxn      ptid       subset response
#>    <chr>   <chr>     <dbl> <chr>      <chr>      <chr>     <dbl>
#>  1 bcg     040          70 uninfected bcg-040-1  cd4     0.418  
#>  2 bcg     040          70 uninfected bcg-040-1  cd8     0.00475
#>  3 bcg     040          70 uninfected bcg-040-10 cd4     0.317  
#>  4 bcg     040          70 uninfected bcg-040-10 cd8     0.0182 
#>  5 bcg     040          70 uninfected bcg-040-11 cd4     0.0566 
#>  6 bcg     040          70 uninfected bcg-040-11 cd8     0.0148 
#>  7 bcg     040          70 uninfected bcg-040-12 cd4     0.422  
#>  8 bcg     040          70 uninfected bcg-040-12 cd8     0.00383
#>  9 bcg     040          70 uninfected bcg-040-13 cd4     0.195  
#> 10 bcg     040          70 uninfected bcg-040-13 cd8     0.0302 
#> # ℹ 245 more rows
data_tidy_vacc_freq |>
  vacc_extract_baseline() |>
  vacc_calc_response_profile("response")
#> # A tibble: 1,806 × 9
#>    vaccine prid  ptid           sex   infxn  timepoint subset cyt_combn response
#>    <chr>   <chr> <chr>          <chr> <chr>      <dbl> <chr>  <chr>        <dbl>
#>  1 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd4    g+2+t+      0     
#>  2 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd4    g+2+t-      0     
#>  3 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd4    g+2-t+      0     
#>  4 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd4    g+2-t-      0     
#>  5 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd4    g-2+t+      0.0006
#>  6 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd4    g-2+t-      0.0009
#>  7 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd4    g-2-t+      0.0009
#>  8 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd8    g+2+t+      0     
#>  9 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd8    g+2+t-      0     
#> 10 mva85a  tb008 mva85a-tb008-8 male  uninf…         0 cd8    g+2-t+      0     
#> # ℹ 1,796 more rows
```

In addition, a convenience function for setting negative responses to
zero is provided:

``` r
vacc_set_neg_to_zero(data_tidy_vacc_freq, "response")
#> # A tibble: 7,763 × 9
#>    vaccine prid  ptid           sex   infxn  timepoint subset cyt_combn response
#>    <chr>   <chr> <chr>          <chr> <chr>      <dbl> <chr>  <chr>        <dbl>
#>  1 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g+2+t+      0.0988
#>  2 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g+2+t-      0.0096
#>  3 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g+2-t+      0.0068
#>  4 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g+2-t-      0.0084
#>  5 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g-2+t+      0.0236
#>  6 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g-2+t-      0     
#>  7 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd4    g-2-t+      0.0129
#>  8 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd8    g+2+t+      0     
#>  9 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd8    g+2+t-      0     
#> 10 mva85a  tb008 mva85a-tb008-5 male  uninf…        28 cd8    g+2-t+      0.013 
#> # ℹ 7,753 more rows
```

## Reproduction

*To be completed*
