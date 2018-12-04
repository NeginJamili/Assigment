library(tidyverse)


# Question 1 ------------------------------------------------------------------------------------------------------



#' tidy_df
#' 
#' @param data 
#' @param column_prefix 
#'
#' @return A data frame with one column for the variables start with column_prefix
#' and another for their values
#' @export
#'
#' @examples
tidy_df <- function(data, column_prefix = "var") {
  data %>%
    gather(starts_with(column_prefix), key = "variable", value = "value") %>% 
    select(variable, value, everything())
}
# I reordered the table to have the columns that start with the given prefix as
# the first columns

# To check the function:
example <- data.frame(
  var1 = c(1:3),
  var2 = c(6, 7, 4),
  others = c(4:6)
)
example <- tidy_df(example, "var")
example

# Question 2 ------------------------------------------------------------------------------------------------------

#' Get the Jane Austen data
#'
#' It will attempt to install the right package for you. If it does not work,
#'   try to install it manually.
#'
#' @return A data frame with Jane Austen texts, one line per row
get_jane_austen_data <- function() {
  
  tryCatch({library(gutenbergr)}, error = function(e){install.packages("gutenbergr")})
  library(gutenbergr)
  
  austen_text <- gutenberg_works(author == "Austen, Jane") %>% 
    gutenberg_download(meta_fields = "title") %>% mutate(id = row_number(gutenberg_id))
  assign("austen_text", austen_text, envir=.GlobalEnv)
  invisible()
}

get_jane_austen_data()

#' extract_possible_names
#'
#' @param data 
#' @param text_column: is the column in which the text is given
#' @param id_column: is the column in which id of each text is provided
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
extract_possible_names <- function(data, text_column, id_column) {
  data <- mutate(data, no_words = 
                    str_count(data[[text_column]], '\\w+'))
  
  max_words <- max(data$no_words) + 1
  # first I didn't add 1, then I noticed this error:
  # Additional pieces discarded in 7 rows. I found out that the reason is that " is counted as one word 
  # in sentences that start with ". So I added 1 to resolve this error
  
  new_names = c(paste("word",1:max_words,sep=""))
  data <- data %>%
    separate(text_column, into = new_names, remove = TRUE)
  data <- tidy_df(data, "word")
  data <- data %>% 
    filter(value > 0 & str_detect(value,"^[A-Z]."))
  #  . is added to remove one-letter words such as "A" or "I" from the list
  data <- data %>% 
    rename(text_id = id_column, name = value) %>% 
    mutate(id = rownames(data)) %>% 
    select(id, text_id, name)
}

# To check the function:
austen_text1 <- austen_text
austen_words <- extract_possible_names(austen_text1, "text", "id")
View(austen_words)

# Question 3 ------------------------------------------------------------------------------------------------------
austen_word_freqs <- readRDS("austen_word_freqs.Rds")

#' filter_names
#'
#' @param data: the data u have from the previous part
#' @param database: the data in which the frequency of words is given and has two columns: word, count
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
filter_names <- function(data, database, name) {
  data$lowercase_name <- tolower(data[[name]])
  summarized_data <- data %>% 
    group_by(lowercase_name) %>% 
    summarise(captilized_times = n()) %>% 
    rename(word = lowercase_name) %>% 
    inner_join(database, by = "word")
  
  summarized_data <- summarized_data %>% 
    mutate(percentage = round(captilized_times/count, digits = 2) * 100) %>% 
    select(-captilized_times, -count)
                            
  data <- data %>% 
    rename(word = lowercase_name) %>% 
    inner_join(summarized_data, by = "word") %>% 
    filter(percentage >= 75) %>% 
    select(-word)
}

# To check the function:
austen_words <- filter_names(austen_words, austen_word_freqs, "name")
View(austen_words)

# Question 4 ------------------------------------------------------------------------------------------------------

#' count_names_per_book
#'
#' @param database: is the original data containing title of books in column "title" and text_id in "id"
#' @param data: is a table driven from database with id, text_id, name 
#'
#' The required data is first extracted from database (text_id and title) 
#' in order to assign the words in the data to the books.
#' @return WordsPerBook
#' @export
#'
#' @examples
count_names_per_book <- function(database, data) {
    database <- database %>% 
    select(id, title) %>% 
    rename(text_id = id) 
   
  data <- inner_join(data, database, by = "text_id")
  
  words_per_book <- data %>% 
    group_by(title) %>% 
    summarise(unique_names = length(unique(name)), name_occurrences = n()) 
    
}

# To check the function:
austen_text2 <- austen_text
words_per_book <- count_names_per_book(austen_text2, austen_words)
View(words_per_book)

words_per_book <- words_per_book[order(-words_per_book$unique_names), ]
message(sprintf(paste0("The book containing the highest number of unique is: ", words_per_book$title[1])))

words_per_book <- words_per_book[order(-words_per_book$name_occurrences), ]
message(sprintf(paste0("The book containing the most occurrences of names is: ", words_per_book$title[1])))

