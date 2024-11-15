<!-- README.md is generated from README.Rmd. Please edit that file -->

# DataTidy24TBVaccSexDiff

<!-- badges: start -->
<!-- badges: end -->

The purpose of `DataTidy24TBVaccSexDiff` is to generate analysis-ready
data for the assessment of sex-associated differences in TB
vaccine-induced antigen-specific CD4 T cell responses.

## Installation

To install `DataTidy24TBVaccSexDiff`, run:

    remotes::install_github(
      "SATVILab/DataTidy24TBVaccSexDiff"
    )

## Usage

`DataTidy24TBVaccSexDiff` provides processed data, as well as
convenience functions for calculating transformations of the response
(summed and profile; see Rodo (2019) for details) and extracting
relevant timepoints (baseline, peak and memory; again, see Rodo (2019)
for details).

### Loading data

To load the data, run:

    library(DataTidy24TBVaccSexDiff)
    data("data_tidy_vacc_freq")
    data_tidy_vacc_freq
    #> # A tibble: 11,361 × 9
    #>    vaccine prid  ptid           sex   infxn      timepoint subset cyt_combn response
    #>    <chr>   <chr> <chr>          <chr> <chr>          <dbl> <chr>  <chr>        <dbl>
    #>  1 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g+2+t+      0.0988
    #>  2 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g+2+t-      0.0096
    #>  3 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g+2-t+      0.0068
    #>  4 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g+2-t-      0.0084
    #>  5 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g-2+t+      0.0236
    #>  6 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g-2+t-      0     
    #>  7 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g-2-t+      0.0129
    #>  8 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd8    g+2+t+      0     
    #>  9 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd8    g+2+t-      0     
    #> 10 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd8    g+2-t+      0.013 
    #> # ℹ 11,351 more rows

### Calculating summed and profile responses

To calculate the summed response, run:

    vacc_calc_response_summed(
      data_tidy_vacc_freq, "response"
    )
    #> # A tibble: 1,623 × 8
    #>    vaccine prid  timepoint infxn      sex    ptid       subset response
    #>    <chr>   <chr>     <dbl> <chr>      <chr>  <chr>      <chr>     <dbl>
    #>  1 bcg     040           0 uninfected female bcg-040-11 cd4     0.0443 
    #>  2 bcg     040           0 uninfected female bcg-040-11 cd8     0.0192 
    #>  3 bcg     040           0 uninfected female bcg-040-15 cd4     0.954  
    #>  4 bcg     040           0 uninfected female bcg-040-15 cd8     0.187  
    #>  5 bcg     040           0 uninfected female bcg-040-16 cd4     0.0496 
    #>  6 bcg     040           0 uninfected female bcg-040-16 cd8     0.00202
    #>  7 bcg     040           0 uninfected female bcg-040-2  cd4     0.175  
    #>  8 bcg     040           0 uninfected female bcg-040-2  cd8     0.0254 
    #>  9 bcg     040           0 uninfected female bcg-040-21 cd4     0.103  
    #> 10 bcg     040           0 uninfected female bcg-040-21 cd8     0.0294 
    #> # ℹ 1,613 more rows

To calculate the summed profile, run:

    vacc_calc_response_profile(
      data_tidy_vacc_freq, "response"
    )
    #> # A tibble: 11,361 × 9
    #>    vaccine prid  ptid           sex   infxn      timepoint subset cyt_combn response
    #>    <chr>   <chr> <chr>          <chr> <chr>          <dbl> <chr>  <chr>        <dbl>
    #>  1 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g+2+t+      0.0988
    #>  2 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g+2+t-      0.0096
    #>  3 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g+2-t+      0.0068
    #>  4 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g+2-t-      0.0084
    #>  5 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g-2+t+      0.0236
    #>  6 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g-2+t-      0     
    #>  7 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g-2-t+      0.0129
    #>  8 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd8    g+2+t+      0     
    #>  9 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd8    g+2+t-      0     
    #> 10 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd8    g+2-t+      0.013 
    #> # ℹ 11,351 more rows

### Extracting timepoints

To extract the baseline response, run:

    vacc_extract_baseline(data_tidy_vacc_freq)
    #> # A tibble: 2,394 × 9
    #>    vaccine prid  ptid           sex   infxn      timepoint subset cyt_combn response
    #>    <chr>   <chr> <chr>          <chr> <chr>          <dbl> <chr>  <chr>        <dbl>
    #>  1 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd4    g+2+t+      0     
    #>  2 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd4    g+2+t-      0     
    #>  3 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd4    g+2-t+      0     
    #>  4 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd4    g+2-t-      0     
    #>  5 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd4    g-2+t+      0.0006
    #>  6 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd4    g-2+t-      0.0009
    #>  7 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd4    g-2-t+      0.0009
    #>  8 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd8    g+2+t+      0     
    #>  9 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd8    g+2+t-      0     
    #> 10 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd8    g+2-t+      0     
    #> # ℹ 2,384 more rows

To extract the peak response, run:

    vacc_extract_peak(data_tidy_vacc_freq)
    #> # A tibble: 2,387 × 9
    #>    vaccine prid  ptid           sex   infxn      timepoint subset cyt_combn response
    #>    <chr>   <chr> <chr>          <chr> <chr>          <dbl> <chr>  <chr>        <dbl>
    #>  1 mva85a  tb008 mva85a-tb008-8 male  uninfected         7 cd4    g+2+t+      0.0289
    #>  2 mva85a  tb008 mva85a-tb008-8 male  uninfected         7 cd4    g+2+t-      0.0105
    #>  3 mva85a  tb008 mva85a-tb008-8 male  uninfected         7 cd4    g+2-t+      0.0068
    #>  4 mva85a  tb008 mva85a-tb008-8 male  uninfected         7 cd4    g+2-t-      0     
    #>  5 mva85a  tb008 mva85a-tb008-8 male  uninfected         7 cd4    g-2+t+      0.007 
    #>  6 mva85a  tb008 mva85a-tb008-8 male  uninfected         7 cd4    g-2+t-      0.0052
    #>  7 mva85a  tb008 mva85a-tb008-8 male  uninfected         7 cd4    g-2-t+      0.0132
    #>  8 mva85a  tb008 mva85a-tb008-8 male  uninfected         7 cd8    g+2+t+      0.0011
    #>  9 mva85a  tb008 mva85a-tb008-8 male  uninfected         7 cd8    g+2+t-      0     
    #> 10 mva85a  tb008 mva85a-tb008-8 male  uninfected         7 cd8    g+2-t+      0     
    #> # ℹ 2,377 more rows

To extract the memory response, run:

    vacc_extract_memory(data_tidy_vacc_freq)
    #> # A tibble: 2,058 × 9
    #>    vaccine prid  ptid           sex   infxn      timepoint subset cyt_combn response
    #>    <chr>   <chr> <chr>          <chr> <chr>          <dbl> <chr>  <chr>        <dbl>
    #>  1 mva85a  tb008 mva85a-tb008-5 male  uninfected       168 cd4    g+2+t+      0.0393
    #>  2 mva85a  tb008 mva85a-tb008-5 male  uninfected       168 cd4    g+2+t-      0.0035
    #>  3 mva85a  tb008 mva85a-tb008-5 male  uninfected       168 cd4    g+2-t+      0.0015
    #>  4 mva85a  tb008 mva85a-tb008-5 male  uninfected       168 cd4    g+2-t-      0.0006
    #>  5 mva85a  tb008 mva85a-tb008-5 male  uninfected       168 cd4    g-2+t+      0.0063
    #>  6 mva85a  tb008 mva85a-tb008-5 male  uninfected       168 cd4    g-2+t-      0.002 
    #>  7 mva85a  tb008 mva85a-tb008-5 male  uninfected       168 cd4    g-2-t+      0     
    #>  8 mva85a  tb008 mva85a-tb008-5 male  uninfected       168 cd8    g+2+t+      0     
    #>  9 mva85a  tb008 mva85a-tb008-5 male  uninfected       168 cd8    g+2+t-      0     
    #> 10 mva85a  tb008 mva85a-tb008-5 male  uninfected       168 cd8    g+2-t+      0.014 
    #> # ℹ 2,048 more rows

Note that these work automatically after the calculation of summed or
profile responses (and the order is irrelevant), for example:

    data_tidy_vacc_freq |>
      vacc_calc_response_summed("response") |>
      vacc_extract_peak()
    #> # A tibble: 341 × 8
    #>    vaccine prid  timepoint infxn      sex    ptid       subset response
    #>    <chr>   <chr>     <dbl> <chr>      <chr>  <chr>      <chr>     <dbl>
    #>  1 bcg     040          70 uninfected female bcg-040-11 cd4     0.0566 
    #>  2 bcg     040          70 uninfected female bcg-040-11 cd8     0.0148 
    #>  3 bcg     040          70 uninfected female bcg-040-15 cd4     0.105  
    #>  4 bcg     040          70 uninfected female bcg-040-15 cd8     0.00132
    #>  5 bcg     040          70 uninfected female bcg-040-16 cd4     0.00594
    #>  6 bcg     040          70 uninfected female bcg-040-16 cd8     0.0243 
    #>  7 bcg     040          70 uninfected female bcg-040-2  cd4     0.440  
    #>  8 bcg     040          70 uninfected female bcg-040-2  cd8     0.00312
    #>  9 bcg     040          70 uninfected female bcg-040-21 cd4     0.280  
    #> 10 bcg     040          70 uninfected female bcg-040-21 cd8     0.0176 
    #> # ℹ 331 more rows
    data_tidy_vacc_freq |>
      vacc_extract_baseline() |>
      vacc_calc_response_profile("response")
    #> # A tibble: 2,394 × 9
    #>    vaccine prid  ptid           sex   infxn      timepoint subset cyt_combn response
    #>    <chr>   <chr> <chr>          <chr> <chr>          <dbl> <chr>  <chr>        <dbl>
    #>  1 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd4    g+2+t+      0     
    #>  2 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd4    g+2+t-      0     
    #>  3 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd4    g+2-t+      0     
    #>  4 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd4    g+2-t-      0     
    #>  5 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd4    g-2+t+      0.0006
    #>  6 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd4    g-2+t-      0.0009
    #>  7 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd4    g-2-t+      0.0009
    #>  8 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd8    g+2+t+      0     
    #>  9 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd8    g+2+t-      0     
    #> 10 mva85a  tb008 mva85a-tb008-8 male  uninfected         0 cd8    g+2-t+      0     
    #> # ℹ 2,384 more rows

In addition, a convenience function for setting negative responses to
zero is provided:

    vacc_set_neg_to_zero(data_tidy_vacc_freq, "response")
    #> # A tibble: 11,361 × 9
    #>    vaccine prid  ptid           sex   infxn      timepoint subset cyt_combn response
    #>    <chr>   <chr> <chr>          <chr> <chr>          <dbl> <chr>  <chr>        <dbl>
    #>  1 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g+2+t+      0.0988
    #>  2 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g+2+t-      0.0096
    #>  3 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g+2-t+      0.0068
    #>  4 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g+2-t-      0.0084
    #>  5 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g-2+t+      0.0236
    #>  6 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g-2+t-      0     
    #>  7 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd4    g-2-t+      0.0129
    #>  8 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd8    g+2+t+      0     
    #>  9 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd8    g+2+t-      0     
    #> 10 mva85a  tb008 mva85a-tb008-5 male  uninfected        28 cd8    g+2-t+      0.013 
    #> # ℹ 11,351 more rows

## Reproduction

*To be completed*
