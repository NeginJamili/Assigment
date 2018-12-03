library(tidyverse)


# Question 1 ------------------------------------------------------------------------------------------------------



#' Title
#' 
#' @param data 
#' @param column_prefix 
#'
#' @return A data frame with one column for the variables start with column_prefix
#' and another for their values
#' @export
#'
#' @examples
tidy_df <- function(data, column_prefix = "var"){
  data %>%
    gather(starts_with(column_prefix), key = "variable", value = "value") %>% 
    select(variable, value, everything())
}
# I reordered the table to have the columns that start with the given prefix as
# the first columns

# To check the function:
Example <- data.frame(
  var1 = c(1:3),
  var2 = c(6, 7, 4),
  others = c(4:6)
)
Example <- tidy_df(Example, "var")
Example

# Question 2 ------------------------------------------------------------------------------------------------------

#' Get the Jane Austen data
#'
#' It will attempt to install the right package for you. If it does not work,
#'   try to install it manually.
#'
#' @return A data frame with Jane Austen texts, one line per row
get_jane_austen_data <- function(){
  
  tryCatch({library(gutenbergr)}, error = function(e){install.packages("gutenbergr")})
  library(gutenbergr)
  
  austen_text <- gutenberg_works(author == "Austen, Jane") %>% 
    gutenberg_download(meta_fields = "title") %>% mutate(id = row_number(gutenberg_id))
  assign("austen_text", austen_text, envir=.GlobalEnv)
  invisible()
}

get_jane_austen_data ()

# extract_possible_names 
#' Title
#'
#' @param data 
#' @param nameCol: is the column in which the text is given
#' @param idCol: is the column in which id of each text is provided
#' @maxWords is created to find number of words in each row, 
#' to be able to separate into given number of columns
#' @newColNames is the new of new columns
#' the function then separates all words of a sentence into several columns
#' and uses the previous function to gather all the words in 1 column
#' 
#' The col names are then adjusted and new unique ID is assigned to the words
#' @return a table with 3 columns
#' @export
#'
#' @examples
extract_possible_names <- function(data, nameCol, idCol) {
  data <- mutate (data, NoWords = 
                    str_count(data[[nameCol]], '\\w+'))
  
  maxWords <- max(data$NoWords) + 1
  # first I didn't add 1, then I noticed this error:
  # Additional pieces discarded in 7 rows. I found out that the reason is that " is counted as one word 
  # in sentences that start with ". So I added 1 to resolve this error
  
  newColNames = c(paste("word",1:maxWords,sep=""))
  data <- data %>%
    separate(text, into = newColNames, remove = TRUE)
  data <- tidy_df(data, "word")
  data <- data %>% 
    filter(value > 0 & str_detect(value,"^[A-Z]."))
  #  . is added to remove one-letter words such as "A" or "I" from the list
  data <- data %>% 
    rename(text_id = id, name = value) %>% 
    mutate(id = rownames(data)) %>% 
    select(id, text_id, name)
}

# To check the function:
austen_text1 <- austen_text
austenWords <- extract_possible_names(austen_text1, "text", "id")

# Question 3 ------------------------------------------------------------------------------------------------------
austen_word_freqs <- readRDS("austen_word_freqs.Rds")

# filter_names
#' Title
#'
#' @param data: the data u have from the previous part
#' @param dataBase: the data in which the frequency of words is given and has two columns: word, count
#' @param name: the column name in data in which the words are 
#' Since in the data the capitalized word are given, and in the data base
#' the words are in lowercase, this function first create a column to change the words into lowercase ones
#' The number of times where the words are capitalized is then calculated (captilizedTimes)
#' The percentage is calculated based on the count value in data base and the captilizedTimes
#' and then this value is attached to the initial data
#'
#' @return: the initial data with 2 more columns: word (the names in lowercase), percentage
#' @export
#'
#' @examples
filter_names <- function(data, dataBase, name) {
  data$lowerCaseName <- tolower(data[[name]])
  summarizedData <- data %>% 
    group_by(lowerCaseName) %>% 
    summarise(captilizedTimes = n()) %>% 
    rename(word = lowerCaseName) %>% 
    inner_join(dataBase, by = "word")
  
  summarizedData <- summarizedData %>% 
    mutate(percentage = round(captilizedTimes/count, digits = 2)*100) %>% 
    select(-captilizedTimes, -count)
                            
  data <- data %>% 
    rename(word = lowerCaseName) %>% 
    inner_join(summarizedData, by = "word") %>% 
    filter(percentage >= 75)
  
}

# To check the function:
austenWords <- filter_names(austenWords, austen_word_freqs, "name")

# Question 4 ------------------------------------------------------------------------------------------------------

# count_names_per_book
