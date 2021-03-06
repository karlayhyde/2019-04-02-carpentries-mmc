#gapminder_data.csv

#  Download Gapminder data for analysis
#  
#
#  set a relative project root
library(here)

file_url <- "https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder_data.csv"

download.file(url = file_url, destfile = here("data/gapminder/raw/gapminder_data"))

view(gap_wide)
view(gapminder)
