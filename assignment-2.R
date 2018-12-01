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

austen_text1 <- austen_text
# to find number of words in each row, to be able to separate into columns
austen_textCop <- mutate (austen_text1, NoWords = 
                            str_count(austen_text1$text, '\\w+'))
# find the maximum number of words to know the number of new columns to be created
maxWords <- max(austen_text1$NoWords) + 1
# first I didn't add 1, then I noticed this error:
# Additional pieces discarded in 7 rows. I found out that the reason is that " is counted as one word 
# in sentences that start with ". So I added 1 to resolve this error

# name of the new columns
mynames = c(paste("word",1:maxWords,sep=""))
# separating all words of a sentence into several columns 
Austen_Sep <- separate(austen_text1, "text", into = mynames, remove = TRUE)
# using the previous function to gather all the words in 1 column
tidy_df <- function(data, column_prefix = "var"){
  data %>%
    gather(starts_with(column_prefix), key = "variable", value = "value") %>% 
    select(variable, value, everything())
}
Austen_Sep <- tidy_df(Austen_Sep, "word")
# to filter those with capital first letter (. to removw "A" or "I" from the list)
Austen_Sep <- filter(Austen_Sep, value > 0 & str_detect(value,"^[A-Z]."))
#Adjusting the column names
Austen_Sep = rename(Austen_Sep, text_id = id, name = value)
# defining unique id
Austen_Sep <- mutate(Austen_Sep, id = rownames(Austen_Sep))

Austen_Sep <- select(Austen_Sep, id, text_id, name)


# Question 3 ------------------------------------------------------------------------------------------------------

# filter_names



# Question 4 ------------------------------------------------------------------------------------------------------

# count_names_per_book
