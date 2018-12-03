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



# Question 4 ------------------------------------------------------------------------------------------------------

# count_names_per_book
