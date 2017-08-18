registers
=========

This package is a very basic wrapper for the [registers](https://registers.cloudapps.digital/) [API](https://registers-docs.cloudapps.digital/#api-documentation-for-registers).

Registers are authoritative lists of things, built and maintained by the UK government, for example, the `country` register is a list of countries.

A single function `get_all_records()` is provided, which downloads all the records of a register into a tibble. This saves you from manually handling the paged download interface of large registers.

Installation
------------

``` r
devtools::install_github("openregisters/RegistersClientR")
```

Example
-------

This is an example of downloading the `country` register.

``` r
library(RegistersClientR)
rr_records("country")
#> # A tibble: 199 x 12
#>    register phase `index-entry-number` `entry-number`   `entry-timestamp`
#>       <chr> <chr>                <int>          <int>              <dttm>
#>  1  country  beta                  206            206 2017-03-29 14:22:30
#>  2  country  beta                  205            205 2016-11-11 16:25:07
#>  3  country  beta                  204            204 2016-04-05 13:23:05
#>  4  country  beta                  203            203 2016-04-05 13:23:05
#>  5  country  beta                  200            200 2016-04-05 13:23:05
#>  6  country  beta                  199            199 2016-04-05 13:23:05
#>  7  country  beta                  198            198 2016-04-05 13:23:05
#>  8  country  beta                  197            197 2016-04-05 13:23:05
#>  9  country  beta                  196            196 2016-04-05 13:23:05
#> 10  country  beta                  194            194 2016-04-05 13:23:05
#> # ... with 189 more rows, and 7 more variables: key <chr>, country <chr>,
#> #   name <chr>, `official-name` <chr>, `citizen-names` <chr>,
#> #   `start-date` <date>, `end-date` <date>
```

By default, the 'beta' version of the register is downloaded. If you need another version -- usually when the register is still in discovery or alpha, then specify the phase.

``` r
rr_records("country", "alpha")
```

The default page size is 5000, which is the current maximum supported by the API and is sufficient to download most registers in one page. But to show that multiple pages are handled, you can specify a smaller page size.

``` r
rr_records("country", page_size = 10)
#> # A tibble: 199 x 12
#>    register phase `index-entry-number` `entry-number`   `entry-timestamp`
#>       <chr> <chr>                <int>          <int>              <dttm>
#>  1  country  beta                  206            206 2017-03-29 14:22:30
#>  2  country  beta                  205            205 2016-11-11 16:25:07
#>  3  country  beta                  204            204 2016-04-05 13:23:05
#>  4  country  beta                  203            203 2016-04-05 13:23:05
#>  5  country  beta                  200            200 2016-04-05 13:23:05
#>  6  country  beta                  199            199 2016-04-05 13:23:05
#>  7  country  beta                  198            198 2016-04-05 13:23:05
#>  8  country  beta                  197            197 2016-04-05 13:23:05
#>  9  country  beta                  196            196 2016-04-05 13:23:05
#> 10  country  beta                  194            194 2016-04-05 13:23:05
#> # ... with 189 more rows, and 7 more variables: key <chr>, country <chr>,
#> #   name <chr>, `official-name` <chr>, `citizen-names` <chr>,
#> #   `start-date` <date>, `end-date` <date>
```

Data types
----------

Each page is imported with character data types first, then combined, and finally converted to (hopefully) appropriate data types by `readr::parse_guess()`. This avoids any problem with fields in the first page not being representative of later pages.
