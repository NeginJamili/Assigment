library(tidyverse)
library(xml2)
library(RCurl)
base_url <- "https://www.cia.gov/library/publications/the-world-factbook/"


#' Question 1: Get Population Ranking
#'
#' @return all_data: A dataframe including country, country link, their population and ranking
#' @export
#'
#' @examples
get_population_ranking <- function(){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  url = str_c(base_url, "fields/335rank.html")
  
  #download url and execute all XPath queries which will each return a column for a data_frame
  url_data <- read_html(download_html(url))
  all_countries <- xml_find_all(url_data, xpath_expressions['country'])
  country_links <- xml_find_all(url_data, xpath_expressions['country_link'])
  value <- xml_find_all(url_data, xpath_expressions['value'])
  rank <- xml_find_all(url_data, xpath_expressions['rank'])
  
  #make the necessary adjustments to the data frame as given by the assignment
  all_data <- data.frame(country = c(sapply(all_countries,xml_text)), 
                         country_link = c(gsub("\\../","", sapply(country_links, xml_text))),
                         population=c(sapply(value,xml_text)),
                         rank.population=c(sapply(rank,xml_text)))
  return(all_data)
}

get_population_ranking ()

#' Question 2: Retrieve Land Area
#'
#' @param country_link A character vector of one or more country_link urls
#'
#' @return A character vector with the same length as the country_link parameter.
#' @export
#'
#' @examples
get_land_area <- function(country_link){
  xpath <- str_c("//div[@id='","field-area","']/div[",2,"]/span[2]")
  #download the file from country_link and execute the xpath query
  area_url = str_c(base_url, country_link)
  area_data <- vector(length = length(country_link))
  
  for (i in 1:length(country_link)) {
    Temp1 <- read_html(download_html(area_url[i]))
    Temp2 <- xml_find_all(Temp1, xpath)
    area_data[i] <- xml_text(Temp2)
  }
  
  return(area_data)
}

# Testing the funtion:
country_link <- all_data$country_link
get_land_area(country_link)


#' Question 3: Get Population Density
#'
#' @return
#' @export
#'
#' @examples
get_population_density <- function(){
  
}


#' Question 4: Get All Provided Rankings
#'
#' @return
#' @export
#'
#' @examples
get_rankings <- function(){
  url <- "https://www.cia.gov/library/publications/the-world-factbook/docs/rankorderguide.html"
  xpath <- c("characteristic" = "//div[@class='field_label']/strong/a",
             "characteristic_link" = "//div[@class='field_label']/strong/a/@href")
  #...
}


#' Question 5 - Part 1: Get Ranking
#'
#' @param url The url of the ranking
#' @param characteristic What this ranking is about
#'
#' @return
#' @export
#'
#' @examples
get_ranking <- function(url = "fields/335rank.html", characteristic = "population"){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  #...
}

#' Question 5 - Part 2: Get Country Characteristic
#'
#' @param country_link 
#' @param xpath_field_id 
#' @param item 
#'
#' @return
#' @export
#'
#' @examples
get_country_characteristic <- function(country_link, xpath_field_id = "field-area", item = 2){
  #update the xpath and use similar code other than that
}


#' Question 6: Combine Rankings
#'
#' @param rankings Rankings from get_rankings (or a selection thereof)
#'
#' @return
#' @export
#'
#' @examples
combine_rankings <- function(rankings){
  
}



