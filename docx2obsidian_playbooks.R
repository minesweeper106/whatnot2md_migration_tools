
#ENV
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
#---


getGooglePlay <- function(title_docx, admonitions = TRUE) {
  
  # Extract the text from the Word file
  doc <- read_docx(file.path(proj_path, input_path, title_docx))
  text <- docx_summary(doc)
  
  # Cut empty rows
  text <- replace(text, is.na(text), "")
  text <- text %>% filter(text != '')
  text <- text[2:4]
  
  # Chapters as Headers
  text$text[text$style_name == "heading 2"] <-
    paste("###", text$text[text$style_name == "heading 2"], " \n", sep = " ")
  row_num <- which(startsWith(text$text, "##"))[1]
  text <- text[row_num:length(text$text),]
  
  # function to determine date format
  find_date_lang_format <- function(sample = text$text[2]) {
    langs <- c('pl', 'eng')
    
    is_pl <- FALSE
    score_pl <-
      unlist(gregexpr("\\d{1,2}\\s\\p{L}+\\s\\d{4}", sample, perl = TRUE))
    
    is_eng <- FALSE
    score_eng <-
      unlist(gregexpr("[A-Za-z]+ \\d{1,2}, \\d{4}", sample, perl = TRUE))
    
    if (score_pl > 0) {
      is_pl <- TRUE
    } else if (score_eng > 0) {
      is_eng <- TRUE
    }
    
    datelang <- langs[c(is_pl, is_eng)]
    return(datelang)
  }
  format_d <- find_date_lang_format()
  
  # Function to remove dates from annotations
  remove_dates <- function(text, datelang = format_d) {
    # Define the regular expression pattern for dates
    switch(datelang,
           'pl' = {
             pattern <- "\\d{1,2}\\s\\p{L}+\\s\\d{4}"
           },
           'eng' = {
             pattern <- "[A-Za-z]+ \\d{1,2}, \\d{4}"
           },
           stop("specify 'pl' or 'eng'"))
    
    # Replace the matching pattern with an empty string
    text_mod <- str_replace(text, pattern, " Page ")
    
    # Trim any leading or trailing white space
    text_mod <- str_trim(text_mod)
    
    # Return the modified text
    return(text_mod)
  }
  text$text <- unlist(lapply(text$text, remove_dates))
  
  # Function to remove link from annotations
  remove_links <- function(text) {
    match <- unlist(gregexpr('HYPERLINK[^"]+"', text, perl = TRUE))
    if (match > 0) {
      text_clean <- gsub('\\s(HYPERLINK)\\s\"\\w.*\"\\s', '', text)
      return(text_clean)
    } else {
      return(text)
    }
  }

  quote_transform <- function(text, ...) {
    # Ignore headings
    if (substr(text, 1, 2) == "##") {
      return(text)
    }
    
    # Match the page number and quote, and capture them in separate groups
    match <-
      gregexpr("Page\\s\\d+\\s*(.*?)($|\n(?=Page))", text, perl = TRUE)
    page_nums <- regmatches(text, match)[[1]]
    text <- regmatches(text, match, invert = TRUE)[[1]]
    
    if (admonitions == TRUE) {
      quote_format <- "> [!quote] "
    } else {
      quote_format <- "> "
    }
    
    # Format the quotes and page numbers
    quotes <- paste0(quote_format, page_nums, "\n> ", text[1])
    return(paste(quotes, collapse = "\n\n"))
  }
  
  text$text <- unlist(lapply(text$text, remove_links))
  text$text <- unlist(lapply(text$text, quote_transform))
  md_text <- paste(text$text, collapse = "\n\n")
  
  title <- gsub('\\.docx', '', title_docx)
  title_md <- gsub('\\.docx', '.md', title_docx)
  
  writeLines(md_text, file.path(proj_path, output_path, title_md))
  print("DONE!")
}

fill_frontmatter <- function(search_title, filename) {
  
  #----Metadata manifest as a df------ 
  meta <- as.data.frame(matrix(NA, ncol = 15, nrow = 1))
  
  names(meta) <-
    c(
      "title",
      "original_title",
      "short_title",
      "publisher",
      "author",
      "coauthors",
      "translator",
      "isbn",
      "oclc",
      "language",
      "original_language",
      "pages",
      "pub_date",
      "pub_year",
      "pub_place"
    )
  
  
  #----Query Calibre database----
  con <- dbConnect(SQLite(), dbname = file.path(proj_path,data_path,"metadata.db"))
  query <-
    paste0(
      "SELECT books.title, authors.name AS author, books.pubdate, publishers.name AS publisher, languages.lang_code AS lang, custom_column_11.value AS pages
FROM books
LEFT JOIN books_authors_link ON books.id = books_authors_link.book
LEFT JOIN authors ON books_authors_link.author = authors.id
LEFT JOIN books_publishers_link ON books.id = books_publishers_link.book
LEFT JOIN publishers ON books_publishers_link.publisher = publishers.id
LEFT JOIN books_languages_link ON books.id = books_languages_link.book
LEFT JOIN languages ON books_languages_link.lang_code = languages.id
LEFT JOIN custom_column_11 ON books.id = custom_column_11.book
WHERE title LIKE '",
      search_title,
      "';"
    )
  
  query_isbn <- paste0(
    "SELECT identifiers.val
FROM books
LEFT JOIN identifiers ON books.id = identifiers.book
WHERE books.title LIKE '",
    search_title,
    "' AND identifiers.type = 'isbn';"
  )
  
  meta_calibre <- dbGetQuery(con, query)
  meta_calibre_isbn <- dbGetQuery(con, query_isbn)
  
  if (nrow(meta_calibre_isbn) == 1) {
    meta$isbn <- as.character(meta_calibre_isbn)
  } else {
    rm(meta_calibre_isbn)
  }
  
  # Close the connection to the database
  dbDisconnect(con)
  rm(con)
  
  #----Calibre metadata Extraction----
  meta$title <- as.character(meta_calibre$title[1])
  meta$short_title <- str_extract(meta$title, "[^.:]+")
  meta$publisher <- as.character(meta_calibre$publisher[1])
  meta$author <- as.character(meta_calibre$author[1])
  
  #coauthors
  if (length(meta_calibre$author) > 1) {
    meta$coauthors <-
      paste0('[', paste(meta_calibre$author[2:length(meta_calibre$author)], collapse =
                          ", "), ']')
  } else {
    meta$coauthors <- ''
  }
  
  isbn_exist <- !is.na(meta$isbn)
  
  meta$language <- as.character(meta_calibre$lang[1])
  meta$pages <- as.numeric(meta_calibre$pages[1])
  
  pub_date_raw <- as.POSIXct(
    as.character.Date(meta_calibre$pubdate[1]),
    tz = "",
    tryFormats = c(
      "%Y-%m-%d %H:%M:%OS",
      "%Y/%m/%d %H:%M:%OS",
      "%Y-%m-%d %H:%M",
      "%Y/%m/%d %H:%M",
      "%Y-%m-%d",
      "%Y/%m/%d"
    )
  )
  meta$pub_date <- format(pub_date_raw, "%Y-%m-%d")
  meta$pub_year <- format(pub_date_raw, "%Y")
  
  #----Biblioteka Narodowa-----
  
  #if ISBN missing then use different condition
  if (isbn_exist) {
    bn_url <-
      paste0("https://data.bn.org.pl/api/institutions/bibs.json?isbnIssn=",
             meta$isbn)
  } else {
    bn_url <-
      URLencode(
        paste0(
          "https://data.bn.org.pl/api/institutions/bibs.json?title=",
          meta$title,
          "&limit=1"
        )
      )
  }
  json_object_bn <- fromJSON(bn_url)
  
  pos <-
    which(!is.na(json_object_bn[["bibs"]][["marc"]][["fields"]][[1]][["041"]][["ind1"]]))
  if (length(pos) >= 1) {
    meta$original_language <-
      json_object_bn[["bibs"]][["marc"]][["fields"]][[1]][["041"]][["subfields"]][[pos]][["h"]][2]
  } else {
    rm(pos)
    
  }
  
  
  if (!isbn_exist) {
    meta$isbn <- json_object_bn[["bibs"]][["isbnIssn"]]
  }
  
  oc <- json_object_bn[["bibs"]][["marc"]][["fields"]][[1]][["035"]][3]
  uoc <- unlist(oc, recursive = TRUE, use.names = FALSE)
  ocl <- grep("^\\(OCoLC\\)", uoc, value = TRUE)
  meta$oclc <- substr(ocl, start = 8, stop = 100)
  
  plc <-
    json_object_bn[["bibs"]][["marc"]][["fields"]][[1]][["260"]][["subfields"]]
  plb <- unlist(plc, recursive = TRUE, use.names = FALSE)
  pub_place <- plb[1]
  meta$pub_place <- substring(pub_place, 1, nchar(pub_place) - 2)
  
  if (!is.na(meta$original_language)) {
    tl <-
      json_object_bn[["bibs"]][["marc"]][["fields"]][[1]][["700"]][["subfields"]]
    tl <- unlist(tl, recursive = TRUE, use.names = FALSE)
    translator <- tl[1]
    translator <- strsplit(translator, " ")[[1]]
    translator <- rev(translator)
    translator <- paste(translator, collapse = " ")
    meta$translator <- substring(translator, 1, nchar(translator) - 1)
  } else {
    meta$translator <- ''
    
  }
  
  
  if (!is.na(meta$original_language)) {
    pos <-
      which(!is.na(json_object_bn[["bibs"]][["marc"]][["fields"]][[1]][["246"]][["ind1"]]))
    or <-
      json_object_bn[["bibs"]][["marc"]][["fields"]][[1]][["246"]][["subfields"]][pos]
    or <- unlist(or, recursive = TRUE, use.names = FALSE)
    or <- or[!is.na(or)]
    #original_title<-paste0(or[2],or[3])
    char <- ","
    og <- or[2]
    
    if (substr(og, nchar(og), nchar(og)) == char) {
      meta$original_title <- og
    } else {
      meta$original_title <- paste0(or[2], or[3])
    }
  } else {
    meta$original_title <- ''
    meta$original_language <- ''
  }
  
  
  
  #---Functions----
  
  
  #Appending the front-matter at the beginning of file
  meta_appender <- #---ok
    function(filename, new_text) {
      # Read the contents of the file into a character vector
      lines <- readLines(filename)
      # Concatenate the new text and the existing lines into character vector
      lines <- c(new_text, lines)
      # Write the modified lines back to the file
      writeLines(lines, filename)
    }
  
  # Constructing the front-matter
  
  #Conditionality on translator
  fmatter_constructor <- function() {
    
    #----TODO citation as function----
    if (meta$original_language == '') {
      citation <-
        paste0(
          "'",
          meta$author,
          ", ",
          meta$title,
          ", ",
          meta$publisher,
          ", ",
          meta$pub_place,
          " ",
          meta$pub_year,
          "'"
        )
    } else {
      citation <-
        paste0(
          "'",
          meta$author,
          ", ",
          meta$title,
          ", tłum. ",
          meta$translator,
          ", ",
          meta$publisher,
          ", ",
          meta$pub_place,
          " ",
          meta$pub_year,
          "'"
        )
    }
    
    note_meta <- paste0(
      "---\n# NOTE-META\ncreated: ",as.character(today()),"\n",
      "type: 'reference'\n",
      "subtype: 'book'\n",
      "status: 'DRAFT'\n",
      "tags: [Wiedza/Streszczenia, ]\n",
      "alias: \n",
      "#\n"
    )
    object_meta <- paste0(
      "# OBJECT-META\n",
      "title: '",
      meta$title,
      "'\n",
      "shorttitle: ",
      meta$short_title,
      "\n",
      "original_title: ",
      meta$original_title,
      "\n",
      "author: '",
      meta$author,
      "'\n",
      "coauthors: ",
      meta$coauthors,
      "\n",
      "translator: ",
      meta$translator,
      "\n",
      "subject: \n",
      "language: ",
      meta$language,
      "\n",
      "original_language: ",
      meta$original_language,
      "\n",
      "pages: ",
      meta$pages,
      "\n",
      "pub_date: ",
      meta$pub_date,
      "\n",
      "pub_place: ",
      meta$pub_place,
      "\n",
      "publisher: ",
      meta$publisher,
      "\n",
      "ISBN: ",
      meta$isbn,
      "\n",
      "OCLC: ",
      meta$oclc,
      "\n",
      "#\n"
    )
    sub_meta <- paste0("# SUB-META\n",
                       "read: TRUE\n",
                       "read_year: \n",
                       "citation: ",
                       citation,
                       "\n---\n")
    fmatter <- paste0(note_meta, object_meta, sub_meta)
  }
  #Exec
  fmatter <- fmatter_constructor()
  meta_appender(filename, fmatter)
}

