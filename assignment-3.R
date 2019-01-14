library(tidyverse)
library(xml2)
library(RCurl)
base_url <- "https://www.cia.gov/library/publications/the-world-factbook/"


# Q1 ----------------------------------------------------------------------

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
  all_data <- data.frame(country_link = c(gsub("\\../","", sapply(country_links, xml_text))),
                         country = c(sapply(all_countries, xml_text)),
                         population = c(sapply(value, xml_text)),
                         rank.population = c(sapply(rank, xml_text)))
  return(all_data)
}

# Testing the funtion:
all_data <- get_population_ranking()
View(all_data)

# Q2 ----------------------------------------------------------------------

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
  #area_data <- c()
  
  #for (i in 1:10) {
  for (i in 1:length(country_link)) {
    Temp1 <- read_html(download_html(area_url[i]))
    Temp2 <- xml_find_all(Temp1, xpath)
    area_data[i] <- xml_text(Temp2)
  }
  
  return(area_data)
}

# Testing the funtion (one URL and a vecotr of URLs):
country_link <- "geos/us.html"
area_data <- get_land_area(country_link)
View(area_data)

country_link <- all_data$country_link
area_data <- get_land_area(country_link)
View(area_data)

# In order to have the output in a better structure, I've written the following function,
# where the output includes the country name and its area as well
get_land_area2 <- function(country_link){
  xpath <- str_c("//div[@id='","field-area","']/div[",2,"]/span[2]")
  #download the file from country_link and execute the xpath query
  area_url = str_c(base_url, country_link)
  area_data2 <- vector(length = length(country_link))
  country_name <- vector(length = length(country_link))
  
  #for (i in 1:10) {
  for (i in 1:length(country_link)) {
    
    Temp1 <- read_html(download_html(area_url[i]))
    Temp2 <- xml_find_all(Temp1, xpath)
    area_data2[i] <- xml_text(Temp2)
    name <- country_link[i]
    country_name[i] <- as.character(unique(all_data$country[all_data$country_link == name])) 
  }
  area_data2 <- cbind(area_data2, country_name)
  colnames(area_data2)[1] <- "area"
  colnames(area_data2)[2] <- "country"
  
  return(area_data2)
}

# Testing the funtion (one URL and a vecotr of URLs):
country_link <- "geos/us.html"
area_data2 <- get_land_area2(country_link)
View(area_data2)

country_link <- all_data$country_link
area_data2 <- get_land_area2(country_link)
View(area_data2)

# Q3 ----------------------------------------------------------------------

#' Question 3: Get Population Density
#'
#' @return
#' @export
#'
#' @examples
get_population_density <- function(){
  compeleted_data <- cbind(all_data,area_data)
  compeleted_data$area_data <- parse_number(compeleted_data$area_data)
  compeleted_data$population <- parse_number(compeleted_data$population)
  compeleted_data[12, "area_data"] <- 1000000
  compeleted_data <- mutate(compeleted_data, population_density = population/area_data) 
  
  # Replacing the NA error
  compeleted_data <- replace(compeleted_data, is.na(compeleted_data), "No Data")
  
  # In order to have the dataset in a more proper order, I've changed the order of columns
  compeleted_data <- subset(compeleted_data, select=c(country_link, country, rank.population,
                                                      population, area_data, population_density))
  
  return(compeleted_data)
}

# Testing the funtion:
compeleted_data <- get_population_density()
View(compeleted_data)

# Q4 ----------------------------------------------------------------------

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
  
  url_data2 <- read_html(download_html(url))
  
  characteristic_data <- xml_find_all(url_data2, xpath['characteristic'])
  links_data <- xml_find_all(url_data2, xpath['characteristic_link'])
  
  rankings <- data.frame(characteristic = c(tolower(gsub(".$","", sapply(characteristic_data, xml_text)))),
                           characteristic_link = c(gsub("\\../","", sapply(links_data, xml_text))))
  
  return(rankings)
  
}

# Testing the funtion:
rankings <- get_rankings()
View(rankings)

# Q5 ----------------------------------------------------------------------

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
  finalURL = str_c(base_url, url)
  url_data <- read_html(download_html(finalURL))
  
  all_countries <- xml_find_all(url_data, xpath_expressions['country'])
  country_links <- xml_find_all(url_data, xpath_expressions['country_link'])
  value <- xml_find_all(url_data, xpath_expressions['value'])
  rank <- xml_find_all(url_data, xpath_expressions['rank'])
  
  all_data <- data.frame(country_link = c(gsub("\\../","", sapply(country_links, xml_text))),
                         country = c(sapply(all_countries,xml_text)),
                         var = c(sapply(value,xml_text)),
                         rank = c(sapply(rank,xml_text)), stringsAsFactors=FALSE)
  all_data <- all_data %>% rename(!!characteristic := var) 
  colnames(all_data)[4] <- paste("rank", characteristic, sep = ".")
                          
  return(all_data)
}

# Testing the funtion:
example1 <- get_ranking("fields/355rank.html", "life expectancy at birth")
View(example1)

example2 <- get_ranking()
View(example2)

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
  xpath <- str_c("//div[@id='",xpath_field_id,"']/div[",item,"]/span[2]")
  characteristic_url = str_c(base_url, country_link)
  characteristic_data <- vector(length = length(country_link))
  
  #for (i in 1:5) {
  for (i in 1:length(country_link)) {
    Temp1 <- read_html(download_html(characteristic_url[i]))
    Temp2 <- xml_find_all(Temp1, xpath)
    characteristic_data[i] <- xml_text(Temp2)
  }
  
  return(characteristic_data)
  
}

# Testing the funtion:
country_link <- all_data$country_link
Output1 <- get_country_characteristic(country_link, "field-area", 3)
View(Output1)

Output2 <- get_country_characteristic(country_link)
View(Output2)

# Q6 ----------------------------------------------------------------------

#' Question 6: Combine Rankings
#'
#' @param rankings Rankings from get_rankings (or a selection thereof)
#'
#' @return
#' @export
#'
#' @examples
combine_rankings <- function(rankings){
  current_rankings <- c()
  # Considering data of the first characteristic as the default data 
  # in order to join the others with this one
  all_rankings <- get_ranking(as.character(rankings[1, "characteristic_link"]),
                              as.character(rankings[1, "characteristic"]))
  
  # Starting from the second data...
  #for (i in 2:10){
  for (i in 2:nrow(rankings)){
    url_var <- as.character(rankings[i, "characteristic_link"])
    characteristic_var <- as.character(rankings[i, "characteristic"])
    current_rankings <- get_ranking(url_var, characteristic_var)
    # Removing the link column in order to have it only once in the final dataset
    current_rankings <- current_rankings[,-(1)] 
    all_rankings <- full_join(all_rankings, current_rankings, by="country")
  }
  
  # Replacing the NA error
  all_rankings <- replace(all_rankings, is.na(all_rankings), "No Data")
  
  return(all_rankings)
}

# Testing the function
all_rankings <- combine_rankings(rankings)
View(all_rankings)


