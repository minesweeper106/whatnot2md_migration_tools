library(tidyverse)
library(officer)
library(dplyr)
library(stringr)
library(dbplyr)
library(RSQLite)
library(jsonlite)


proj_path <- getwd()
input_path <- 'INPUT'
output_path <- 'OUTPUT'
data_path <- 'DATA'

find_date_lang_format <- function(sample) {
  langs <- c('pl', 'eng')
  score_pl <- unlist(gregexpr("\\d{1,2}\\s\\p{L}+\\s\\d{4}", sample, perl = TRUE))
  score_eng <- unlist(gregexpr("[A-Za-z]+ \\d{1,2}, \\d{4}", sample, perl = TRUE))
  datelang <- langs[c(score_pl > 0, score_eng > 0)]
  return(datelang)
}

remove_dates <- function(text, datelang) {
  pattern <- switch(datelang,
                    'pl' = "\\d{1,2}\\s\\p{L}+\\s\\d{4}",
                    'eng' = "[A-Za-z]+ \\d{1,2}, \\d{4}",
                    stop("specify 'pl' or 'eng'"))
  text_mod <- str_replace(text, pattern, " Page ")
  text_mod <- str_trim(text_mod)
  return(text_mod)
}

remove_links <- function(text) {
  match <- unlist(gregexpr('HYPERLINK[^"]+"', text, perl = TRUE))
  if (match > 0) {
    text_clean <- gsub('\\s(HYPERLINK)\\s\"\\w.*\"\\s', '', text)
    return(text_clean)
  } else {
    return(text)
  }
}

quote_transform <- function(text, admonitions) {
  if (substr(text, 1, 2) == "##") {
    return(text)
  }
  match <- gregexpr("Page\\s\\d+\\s*(.*?)($|\n(?=Page))", text, perl = TRUE)
  page_nums <- regmatches(text, match)[[1]]
  text <- regmatches(text, match, invert = TRUE)[[1]]
  quote_format <- if (admonitions) "> [!quote] " else "> "
  quotes <- paste0(quote_format, page_nums, "\n> ", text[1])
  return(paste(quotes, collapse = "\n\n"))
}

getGooglePlay <- function(title_docx, admonitions = TRUE) {
  doc <- read_docx(file.path(proj_path, input_path, title_docx))
  text <- docx_summary(doc) %>%
    replace(is.na(.), "") %>%
    filter(. != '') %>%
    .[2:4]
  
  text$text[text$style_name == "heading 2"] <- paste("###", text$text[text$style_name == "heading 2"], " \n", sep = " ")
  row_num <- which(startsWith(text$text, "##"))[1]
  text <- text[row_num:length(text$text),]
  
  format_d <- find_date_lang_format(text$text[2])
  
  text$text <- unlist(lapply(text$text, remove_dates, datelang = format_d))
  text$text <- unlist(lapply(text$text, remove_links))
  text$text <- unlist(lapply(text$text, quote_transform, admonitions = admonitions))
  md_text <- paste(text$text, collapse = "\n\n")
  
  title_md <- gsub('\\.docx', '.md', title_docx)
  writeLines(md_text, file.path(proj_path, output_path, title_md))
  print("DONE!")
}
