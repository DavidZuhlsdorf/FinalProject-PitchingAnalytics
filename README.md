# Untitled


## Data

This project uses pitch-level Statcast data from the **2025 World
Series**.

``` r
library(tidyverse)
```

    Warning: package 'purrr' was built under R version 4.5.3

    Warning: package 'dplyr' was built under R version 4.5.3

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
    ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ✔ purrr     1.2.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(plotly)
```


    Attaching package: 'plotly'

    The following object is masked from 'package:ggplot2':

        last_plot

    The following object is masked from 'package:stats':

        filter

    The following object is masked from 'package:graphics':

        layout

``` r
library(here)
```

    here() starts at C:/Users/023dz/OneDrive - St. Lawrence University/Stat334/Stat334/FinalProject-PitchingAnalytics

``` r
library(viridis)
```

    Loading required package: viridisLite

``` r
data <- read_excel(here::here("data/2025WS_data.xlsx"))
```

## Questions

## Graph Title
