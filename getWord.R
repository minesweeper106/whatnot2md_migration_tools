# Load the officer package
library(officer)
library(dplyr)
library(stringr)
# Define the path to the Word file
path <- "Designing Products People Love_ - Scott Hurff.docx"
datelang <- "eng"



getGooglePlay <- function(path, datelang) {
# Read the Word file
doc <- read_docx(path)

# Extract the text from the Word file
text <- docx_summary(doc)


# Cut empty rows
text <- replace(text, is.na(text), "")
text <- text %>% filter(text!='')
text <- text[2:4]


text$text[text$style_name == "heading 2"] <- paste("###", text$text[text$style_name == "heading 2"]," \n", sep = " ")
row_num <- which(startsWith(text$text, "##"))[1]
text<-text[row_num:length(text$text), ]

remove_dates <- function(text) {
  # Define the regular expression pattern for dates
#pol date:  
 # pattern <- "\\d{1,2}\\s\\p{L}+\\s\\d{4}"
#ang date:
pattern <- "[A-Za-z]+ \\d{1,2}, \\d{4}"
  # Replace the matching pattern with an empty string
  text_mod <- str_replace(text, pattern, " Page ")
  
  # Trim any leading or trailing white space
  text_mod <- str_trim(text_mod)
  
  # Return the modified text
  return(text_mod)
}
text$text<-unlist(lapply(text$text, remove_dates))

quote_transform <- function(text) {
  # Check if the string starts with "##"
  if (substr(text, 1, 2) == "##") {
    return(text)
  }
  
  # Match the page number and text, and capture them in separate groups
  match <- gregexpr("Page\\s\\d+\\s*(.*?)($|\n(?=Page))", text, perl=TRUE)
  page_nums <- regmatches(text, match)[[1]]
  text <- regmatches(text, match, invert=TRUE)[[1]]
  
  # Format the quotes and page numbers
  quotes <- paste0("> [!quote] ", page_nums, "\n> ", text[1])
  return(paste(quotes, collapse="\n\n"))
}


text$text<-unlist(lapply(text$text, quote_transform))
my_text <-paste(text$text, collapse = "\n\n")
output_path <- paste0("~/rdev/OUTPUT/",path, ".md") 
writeLines(my_text, output_path)
return("DONE!")
} 
