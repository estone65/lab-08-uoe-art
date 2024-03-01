Lab 08 - University of Edinburgh Art Collection
================
Eric Stone
3.1.24

### Load packages and data

``` r
library(tidyverse) 
library(skimr)
```

``` r
# Remove eval = FALSE or set it to TRUE once data is ready to be loaded
uoe_art <- read_csv("data/uoe-art.csv")
```

### Exercise 9

``` r
uoe_art <- uoe_art %>%
  separate(title, into = c("title", "date"), sep = "\\(") %>%
  mutate(year = str_remove(date, "\\)") %>% as.numeric()) %>%
  select(title, artist, year, date)
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 45 rows [3, 91, 120, 197,
    ## 232, 287, 344, 351, 399, 443, 521, 564, 569, 653, 755, 775, 891, 1063, 1159,
    ## 1170, ...].

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 728 rows [9, 12, 14, 16,
    ## 18, 27, 28, 31, 44, 47, 58, 59, 60, 63, 66, 67, 70, 72, 73, 75, ...].

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `year = str_remove(date, "\\)") %>% as.numeric()`.
    ## Caused by warning in `str_remove(date, "\\)") %>% as.numeric()`:
    ## ! NAs introduced by coercion

### Exercise 10

> Fill in the blanks in to implement the data wrangling we described
> above. Note that this approach will result in some warnings when you
> run the code, and that’s OK! Read the warnings, and explain what they
> mean, and why we are ok with leaving them in given that our objective
> is to just capture year where it’s convenient to do so.

``` r
uoe_art_cleandate <- uoe_art %>%
  mutate(date = as.numeric(str_remove_all(date, "[^0-9]")))
```

I believe the warnings are saying that not all of the pieces have titles
and dates to be separated. That’s okay – we just want to get rid of the
dates from those that have them.

### Exercise 11

> Print out a summary of the data frame using the skim() function. How
> many pieces have artist info missing? How many have year info missing?

``` r
library(skimr)
skim(uoe_art_cleandate)
```

|                                                  |                   |
|:-------------------------------------------------|:------------------|
| Name                                             | uoe_art_cleandate |
| Number of rows                                   | 3275              |
| Number of columns                                | 4                 |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                   |
| Column type frequency:                           |                   |
| character                                        | 2                 |
| numeric                                          | 2                 |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                   |
| Group variables                                  | None              |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| title         |         0 |          1.00 |   0 |  95 |     7 |     1595 |          0 |
| artist        |       115 |          0.96 |   2 |  55 |     0 |     1205 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |  p25 |  p50 |  p75 |     p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|-----:|-----:|-----:|---------:|:------|
| year          |      1568 |          0.52 |    1964.37 |      53.57 |   2 | 1953 | 1962 | 1977 |     2020 | ▁▁▁▁▇ |
| date          |      1042 |          0.68 | 1699133.89 | 5430175.99 |   2 | 1954 | 1963 | 1987 | 20022007 | ▇▁▁▁▁ |

115 have artist missing. 1568 have year missing.

### Exercise 12

> Make a histogram of years. Use a reasonable binwidth. Do you see
> anything out of the ordinary?

``` r
ggplot(data = uoe_art_cleandate, aes(x = year)) +
  geom_histogram(binwidth = 0.2)
```

    ## Warning: Removed 1568 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](lab-08_files/figure-gfm/histogram%20for%20year-1.png)<!-- -->

I couldn’t see it in the histogram, so I produced a frequencies table

``` r
frequency_table <- uoe_art_cleandate %>%
  count(year) %>%
  arrange(desc(year))
print(frequency_table, n=6000)
```

    ## # A tibble: 101 × 2
    ##      year     n
    ##     <dbl> <int>
    ##   1  2020     6
    ##   2  2019     5
    ##   3  2018     5
    ##   4  2017     9
    ##   5  2016    11
    ##   6  2015     9
    ##   7  2014    12
    ##   8  2013    18
    ##   9  2012     2
    ##  10  2007    30
    ##  11  2006    32
    ##  12  2005     3
    ##  13  2004     4
    ##  14  2003    12
    ##  15  2002    23
    ##  16  2001     2
    ##  17  1999     5
    ##  18  1998     5
    ##  19  1997    10
    ##  20  1996     3
    ##  21  1995     3
    ##  22  1994    10
    ##  23  1993     7
    ##  24  1992     3
    ##  25  1991     7
    ##  26  1990     7
    ##  27  1989    25
    ##  28  1988     6
    ##  29  1987    28
    ##  30  1986    20
    ##  31  1985    10
    ##  32  1984    19
    ##  33  1983    38
    ##  34  1982     9
    ##  35  1981    10
    ##  36  1980     6
    ##  37  1979     2
    ##  38  1978     4
    ##  39  1977     8
    ##  40  1976     8
    ##  41  1975     7
    ##  42  1974    10
    ##  43  1973     5
    ##  44  1972     6
    ##  45  1971     8
    ##  46  1970     5
    ##  47  1969    35
    ##  48  1968    38
    ##  49  1967    45
    ##  50  1966    34
    ##  51  1965    35
    ##  52  1964    40
    ##  53  1963    85
    ##  54  1962    68
    ##  55  1961    33
    ##  56  1960    77
    ##  57  1959    82
    ##  58  1958    62
    ##  59  1957    58
    ##  60  1956    32
    ##  61  1955    30
    ##  62  1954    37
    ##  63  1953    66
    ##  64  1952    66
    ##  65  1951    33
    ##  66  1950    83
    ##  67  1949    47
    ##  68  1948    26
    ##  69  1947    26
    ##  70  1946    24
    ##  71  1944     1
    ##  72  1943     1
    ##  73  1942     1
    ##  74  1941     1
    ##  75  1939    10
    ##  76  1938     2
    ##  77  1937     1
    ##  78  1936     4
    ##  79  1934     2
    ##  80  1932     1
    ##  81  1929     1
    ##  82  1928     1
    ##  83  1924     5
    ##  84  1922     1
    ##  85  1918     1
    ##  86  1912     3
    ##  87  1901     2
    ##  88  1897     6
    ##  89  1896     1
    ##  90  1893     1
    ##  91  1854     1
    ##  92  1838     1
    ##  93  1837    12
    ##  94  1835     2
    ##  95  1834     1
    ##  96  1831     1
    ##  97  1822     1
    ##  98  1820     1
    ##  99  1819     1
    ## 100     2     1
    ## 101    NA  1568

There’s a “2” for year.

### Exercise 13

> Find which piece has the out of the ordinary year and go to its page
> on the art collection website to find the correct year for it. Can you
> tell why our code didn’t capture the correct year information? Correct
> the error in the data frame and visualize the data again.

It’s Death Mask, by H. Dempshall.

On the website, it reads: Death Mask (2) (1964)

Evidently, when reading in the information, it read the (2) as the date,
not 1964.

> Hint: You’ll want to use mutate() and if_else() or case_when() to
> implement the correction.

``` r
uoe_art_cleanyear <- uoe_art_cleandate %>%
  mutate (newyear = case_when(
    year == 2 ~ 1964,
    TRUE ~ year 
  )) 
```

### Exercise 14

> Who is the most commonly featured artist in the collection? Do you
> know them? Any guess as to why the university has so many pieces from
> them?

``` r
frequency_table_artist <- uoe_art_cleanyear %>%
  count(artist) %>%
  arrange(desc(n))
print(frequency_table_artist)
```

    ## # A tibble: 1,206 × 2
    ##    artist               n
    ##    <chr>            <int>
    ##  1 Unknown            374
    ##  2 Emma Gillies       175
    ##  3 <NA>               115
    ##  4 Ann F Ward          23
    ##  5 John Bellany        22
    ##  6 Zygmunt Bukowski    21
    ##  7 Boris Bućan         17
    ##  8 Marjorie Wallace    17
    ##  9 Gordon Bryce        16
    ## 10 William Gillon      16
    ## # ℹ 1,196 more rows

Emma Gillies. Nope, never heard of her, but that doesn’t mean much.
According to google, the University of Edinburgh has a special
collection of her work.

### Exercise 15

> Final question! How many art pieces have the word “child” in their
> title? See if you can figure it out, and ask for help if not.

``` r
uoe_art_cleanyear_child <- uoe_art_cleanyear %>%
   mutate (child = case_when(
    str_detect(title, regex("Child", ignore_case = TRUE)) ~ 1,
    TRUE ~ 0 
  )) 
```

``` r
frequency_table_child <- uoe_art_cleanyear_child %>%
  count(child) %>%
  arrange(desc(child))
print(frequency_table_child)
```

    ## # A tibble: 2 × 2
    ##   child     n
    ##   <dbl> <int>
    ## 1     1    11
    ## 2     0  3264

Child (or some version thereof) appears to show up in 11 titles. Note I
didn’t fully follow your hint, but this approach seemed to work fine.
