#Title: Simple Web Scraper
#From Sharon Machlis
#From Ch. 4.8 in Machlis:
#‘Scrape’ data from Web pages with the rvest package 
#and SelectorGadget browser extension or JavaScript bookmarklet. 
#
#SelectorGadget helps you discover the CSS elements of data you want to copy 
#that are on an HTML page; then rvest uses R to find and save that data. 
#Instructions and a video: http://bit.ly/Rscraping. 
#RStudio webinar: https://www.rstudio.com/resources/webinars/extracting-data-from-the-web-part-2/ .


pacman::p_load(rvest, robotstxt, dplyr, purrr)

vignette(package = "robotstxt")
vignette("using_robotstxt")

library(robotstxt)
paths_allowed("https://www.rstudio.com/resources/cheatsheets/")

my_css <- ".button-default"

library(rvest)
my_html <- read_html("https://www.rstudio.com/resources/cheatsheets/")

my_nodes <- html_nodes(my_html, my_css)

my_nodes[[1]]

my_urls <- html_nodes(my_html, my_css) %>%
  html_attr('href')

my_urls[1]


my_nodes_text <- html_nodes(my_html, my_css) %>%
  html_text()
my_nodes_text[1]

basename("https://github.com/rstudio/cheatsheets/raw/master/rmarkdown-2.0.pdf")

my_filenames <- map_chr(my_urls, basename)

walk2(my_urls, my_filenames, download.file)
