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

    ## Error in eval(expr, envir, enclos): object 'uoe_art' not found

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

    ## Error in eval(expr, envir, enclos): object 'uoe_art' not found

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

    ## Error in eval(expr, envir, enclos): object 'uoe_art_cleandate' not found

Add exercise headings as needed.
