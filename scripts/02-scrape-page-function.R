# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)


first_url <- "https://collections.ed.ac.uk/art/search/*:*/Collection:%22edinburgh+college+of+art%7C%7C%7CEdinburgh+College+of+Art%22?offset=0"

second_url <- "https://collections.ed.ac.uk/art/search/*:*/Collection:%22edinburgh+college+of+art%7C%7C%7CEdinburgh+College+of+Art%22?offset=10"


# function: scrape_page --------------------------------------------------------

scrape_page <- function(url){
  
  # read page --------------------------------------------------------------
  
  page <- read_html(url)
  
  # scrape titles ----------------------------------------------------------------
  
  titles <- page %>%
    html_nodes(".iteminfo") %>%
    html_node("h3 a") %>%
    html_text() %>%
    str_squish()
  
  # scrape links -----------------------------------------------------------------
  
  links <- page %>%
    html_nodes(".iteminfo") %>%
    html_node("h3 a") %>%
    html_attr("href") %>%
    str_replace("\\.", "https://collections.ed.ac.uk/art/")
  
  # scrape artists ---------------------------------------------------------------
  
  artists <- page %>%
    html_nodes(".iteminfo") %>%
    html_node(".artist") %>%
    html_text() %>%
    str_squish()
  
  # put together in a data frame -------------------------------------------------
  
  first_ten <- tibble(
    title = titles,
    artist = artists,
    link = links
  )
  return(first_ten)
  
}
