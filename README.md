
<!-- README.md is generated from README.Rmd. Please edit that file -->
gen5helper
==========

A Collection of Functions for Processing Gen5 2.06 Exported Data

Installation
------------

You can install gen5helper from github with:

``` r
# install.packages("devtools")
devtools::install_github("yanxianucsb/gen5helper")
```

Example
-------

After exporting tab-delim ascii files (named 'data.txt' for instance), this is a basic example for further cleaning and annotating:

``` r
## clean
df <- g5h.clean('data.txt')
## add time intervals
df <- g5h.set_time(df)
## add mean, standard deviation, treatment and dose.
df <- g5h.annotate(df)
```
