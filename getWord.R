
library(officer)
library(dplyr)
library(stringr)

getGooglePlay <- function(path) {

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


find_date_lang_format <- function(sample=text$text[2]) {
  
  langs<-c('pl', 'eng')
  
  is_pl <- FALSE 
  score_pl <- unlist(gregexpr("\\d{1,2}\\s\\p{L}+\\s\\d{4}", sample, perl=TRUE))
  
  is_eng <- FALSE 
  score_eng <- unlist(gregexpr("[A-Za-z]+ \\d{1,2}, \\d{4}", sample, perl=TRUE))
  
  if (score_pl > 0) {
    is_pl <- TRUE
  } else if (score_eng > 0) {
    is_eng <- TRUE
  }
  
  datelang <- langs[c(is_pl,is_eng)]
  return(datelang)
}

format_d <- find_date_lang_format()

remove_dates <- function(text, datelang=format_d) {
# Define the regular expression pattern for dates
#pol date:  
# pattern <- "\\d{1,2}\\s\\p{L}+\\s\\d{4}"
#ang date:
#pattern <- "[A-Za-z]+ \\d{1,2}, \\d{4}"
  switch(datelang,
         'pl' = {
           pattern <- "\\d{1,2}\\s\\p{L}+\\s\\d{4}"
         },
         'eng' = {
           pattern <- "[A-Za-z]+ \\d{1,2}, \\d{4}"
         },
         stop("specify 'pl' or 'eng'")
  )
  
  # Replace the matching pattern with an empty string
  text_mod <- str_replace(text, pattern, " Page ")
  
  # Trim any leading or trailing white space
  text_mod <- str_trim(text_mod)
  
  # Return the modified text
  return(text_mod)
}
text$text<-unlist(lapply(text$text, remove_dates))

remove_links <- function(text) {
  match<-unlist(gregexpr('HYPERLINK[^"]+"', text, perl=TRUE))
  if (match > 0) {
    text_clean <- gsub('\\s(HYPERLINK)\\s\"\\w.*\"\\s','',text)
    return(text_clean)
  } else {
    return(text)
  }
  }

text$text<-unlist(lapply(text$text,remove_links))


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
