
library(officer)
library(dplyr)
library(stringr)

proj_path <- getwd()
input_path <- 'INPUT'
output_path <- 'OUTPUT'
getGooglePlay <- function(title_docx, admonitions = TRUE) {

#doc <- read_docx(paste0(proj_path,'/INPUT/',path))
doc <- read_docx(file.path(proj_path,input_path,title_docx))
# Extract the text from the Word file
text <- docx_summary(doc)


# Cut empty rows
text <- replace(text, is.na(text), "")
text <- text %>% filter(text!='')
text <- text[2:4]

# Chapters as Headers
text$text[text$style_name == "heading 2"] <- paste("###", text$text[text$style_name == "heading 2"]," \n", sep = " ")
row_num <- which(startsWith(text$text, "##"))[1]
text<-text[row_num:length(text$text), ]

# function to determine date format 
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

# Function to remove dates from annotations
remove_dates <- function(text, datelang=format_d) {
# Define the regular expression pattern for dates
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

# Function to remove link from annotations
remove_links <- function(text) {
  match<-unlist(gregexpr('HYPERLINK[^"]+"', text, perl=TRUE))
  if (match > 0) {
    text_clean <- gsub('\\s(HYPERLINK)\\s\"\\w.*\"\\s','',text)
    return(text_clean)
  } else {
    return(text)
  }
  }




quote_transform <- function(text,...) {
  # Ignore headings
  if (substr(text, 1, 2) == "##") {
    return(text)
  }
  
  # Match the page number and quote, and capture them in separate groups
  match <- gregexpr("Page\\s\\d+\\s*(.*?)($|\n(?=Page))", text, perl=TRUE)
  page_nums <- regmatches(text, match)[[1]]
  text <- regmatches(text, match, invert=TRUE)[[1]]
  
  if (admonitions==TRUE){
    quote_format<-"> [!quote] "
  } else {
    quote_format<-"> "
  }
  
  # Format the quotes and page numbers
  quotes <- paste0(quote_format, page_nums, "\n> ", text[1])
  return(paste(quotes, collapse="\n\n"))
}

text$text<-unlist(lapply(text$text,remove_links))
text$text<-unlist(lapply(text$text, quote_transform))
md_text <-paste(text$text, collapse = "\n\n")

title<- gsub('\\.docx','',title_docx)
title_md<- gsub('\\.docx','.md',title_docx)

writeLines(md_text, file.path(proj_path,output_path,title_md))
print("DONE!")
} 
