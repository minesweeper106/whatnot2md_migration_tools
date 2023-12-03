library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)
library(httr)
library(xml2)
library(tidyverse)

ui <- grid_page(
  layout = c(
    "header  header",
    "sidebar tabs  ",
    "area4   plotly"
  ),
  row_sizes = c(
    "100px",
    "1fr",
    "1.02fr"
  ),
  col_sizes = c(
    "0.37fr",
    "1.63fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_header("Search"),
    card_body(
      textInput(
        inputId = "title_input",
        label = "Title",
        value = ""
      ),
      textInput(
        inputId = "isbn_Input",
        label = "ISBN",
        value = ""
      ),
      textInput(
        inputId = "myTextInput",
        label = "Publication year",
        value = ""
      ),
      actionButton(inputId = "searchButton", label = "Search")
    )
  ),
  grid_card_text(
    area = "header",
    content = "BibHoarder",
    alignment = "start",
    is_title = TRUE
  ),
  grid_card(
    area = "plotly",
    card_header("Data Table"),
    card_body(DTOutput(outputId = "myTable", width = "100%"))
  ),
  grid_card(
    area = "tabs",
    card_body(
      tabsetPanel(
        nav_panel(
          title = "BibTex",
          textOutput(outputId = "textOutputBibtex")
        ),
        nav_panel(
          title = "Yaml",
          textOutput(outputId = "textOutputYaml")
        ),
        nav_panel(
          title = "ISO",
          textOutput(outputId = "textOutputISO")
        )
      )
    )
  ),
  grid_card(area = "area4", card_body())
)


server <- function(input, output) {
    observeEvent(input$searchButton, {
      # Encode the book title for URL
      encoded_title <- URLencode(input$title_input)
      
      # Construct the URL with the encoded title
      url <- paste0("https://data.bn.org.pl/api/institutions/bibs.marcxml?limit=1&title=", encoded_title)
      
      # Fetch and process the data
      response <- httr::GET(url)
      content <- httr::content(response, "text")
      xml_data <- xml2::read_xml(content)
      
      ns <- xml2::xml_ns_rename(xml2::xml_ns(xml_data), d1 = "marc")
      
      records <- xml2::xml_find_all(xml_data, ".//marc:record", ns)
      parse_field <- function(field) {
        tag <- xml2::xml_attr(field, "tag")
        #ind1 <- xml2::xml_attr(field, "ind1")
        #ind2 <- xml2::xml_attr(field, "ind2")
        subfields <- xml2::xml_find_all(field, ".//marc:subfield", ns)
        
        subfields_data <- map_df(subfields, function(subfield) {
          code <- xml2::xml_attr(subfield, "code")
          value <- xml2::xml_text(subfield)
          tibble(code = code, value = value)
        })
        
        tibble(tag = tag, subfields = list(subfields_data))
      }
      
      parsed_records <- map_df(records, function(record) {
        controlfields <- xml2::xml_find_all(record, ".//marc:controlfield", ns)
        datafields <- xml2::xml_find_all(record, ".//marc:datafield", ns)
        
        all_fields <- c(controlfields, datafields)
        
        fields_data <- map_df(all_fields, parse_field)
        
        tibble(fields = list(fields_data))
      })
      #parsed_records<-parsed_records[[1]][[1]]
      # Add a record identifier
      parsed_records <- parsed_records %>%
        mutate(record_id = row_number())
      
      # Flatten the nested tibble
      flattened <- parsed_records %>%
        unnest(fields) %>%
        unnest(cols = subfields) %>%
        select(tag, code, value)
      UWD <- flattened %>%
        filter(tag == "015", code == "a") %>%
        reframe(value = if(n() == 0) NA else first(value)) %>%
        pull(value)
      
      
      book_id <- flattened %>%
        filter(tag == "035", code == "a") %>%
        pull(value) %>%
        first()
      
      original_title <- flattened %>%
        filter(tag == "246", code == "a") %>%
        reframe(value = if(n() == 0) NA else first(value)) %>%
        pull(value) %>%
        str_remove(",$")
      # Check for original subtitle
      if (!is.na(original_title) && endsWith(original_title, ":")) {
        additional_value <- flattened %>%
          filter(tag == "246", code == "b") %>%
          summarize(value = first(value)) %>%
          pull(value)
        
        original_title <- paste0(original_title, additional_value)
      }
      
      
      author <- flattened %>%
        filter(tag == "100", code == "a") %>%
        pull(value)  # Extracting the 'value' column
      
      title <- flattened %>%
        filter(tag == "245", code == "a") %>%
        pull(value) %>%
        str_remove("/$") %>%
        str_remove(":$") %>%# Removes the slash at the end of the string if present 
        str_trim() 
      
      subtitle <- flattened %>%
        filter(tag == "245", code == "b") %>%
        reframe(value = if(n() == 0) NA else first(value)) %>%
        pull(value)
      
      
      publisher <- flattened %>%
        # Attempt to find value for tag 710, code a
        filter(tag == "710", code == "a") %>%
        reframe(value_710 = first(value, default = NA))
      # Get values for tag 260, code b and paste them together if there are multiple
      values_260 <- flattened %>%
        filter(tag == "260", code == "b") %>%
        pull(value) %>%
        paste(collapse = " ")
      # Conditional check and selection of publisher value
      if (is.na(publisher$value_710[1])) {
        publisher <- values_260
      } else {
        publisher <- publisher$value_710[1]
      }
      
      
      publish_place <- flattened %>%
        filter(tag == "260", code == "a") %>%
        pull(value) %>%
        str_remove(":$") %>%  # Removes the colon at the end of the string
        str_trim()   # Trims leading and trailing whitespace
      
      
      publish_year <- flattened %>%
        filter(tag == "260", code == "c") %>%
        pull(value) %>%
        str_extract_all("\\d+") %>%  # Extract all sequences of digits
        sapply(function(x) paste(x, collapse = ""))  # Collapse them into single strings
      
      pages <- flattened %>%
        filter(tag == "300", code == "a") %>%
        pull(value) %>%
        str_extract("\\d{1,5}")  # Extract the first occurrence of 1 to 5 digits
      
      
      
      language <- flattened %>%
        filter(tag == "041", code == "a") %>%
        reframe(value = if(n() == 0) NA else first(value)) %>%
        pull(value)
      
      original_language <- flattened %>%
        filter(tag == "041", code == "h") %>%
        reframe(value = if(n() == 0) language else first(value)) %>%
        pull(value)
      
      # Defaulting to Polish if the values are empty
      if (is.na(language) && is.na(original_language)){
        language <-"pol"
      }
      
      translator <- flattened %>%
        filter(tag == "700", code == "a") %>%
        reframe(value = if(n() == 0 || is.na(original_language)) NA else first(value)) %>%
        pull(value)
      
      ISBN <- flattened %>%
        filter(tag == "020", code == "a") %>%
        reframe(value = if(n() == 0) NA else first(value)) %>%
        pull(value)
      
      OCLC <- flattened %>%
        filter(tag == "035", code == "a") %>%
        pull(value) %>%
        .[4] %>%
        str_extract("\\d{9}")  # Extract all digits
      
      series <- flattened %>%
        filter(tag == "490", code == "a") %>%
        reframe(value = if(n() == 0) NA else first(value)) %>%
        pull(value)
      
      series_volume <- flattened %>%
        filter(tag == "500", code == "a") %>%
        reframe(value = if(n() == 0) NA else first(value)) %>%
        pull(value)
      
      full_entry <- flattened %>%
        filter(tag == "245") %>%
        group_by(tag) %>%
        reframe(
          title,
          # title = first(value[code == "a"], default = NA_character_),
          other_values = paste(value[code != "a"], collapse = " ")
        ) %>%
        transmute(
          combined_title = if_else(
            is.na(title), 
            other_values, 
            if_else(
              other_values == "", 
              title, 
              paste(title,": ", other_values, 
                    sep = "")
            )
          )
        ) %>%
        pull(combined_title) %>%
        str_replace_all("\\[.*?\\]\\s?.", "") %>%  # Remove content within square brackets and the first character after
        str_trim()  # Trim whitespace
      
      genre <- flattened %>%
        filter(tag == "655", code == "a") %>%
        reframe(value = if(n() == 0) NA else first(value)) %>%
        pull(value)
    # generate bins based on input$bins from ui.R
  
  
  output$myTable <- renderDT({
    
      book_entry<-tibble(book_id,title,subtitle, full_entry, author, genre ,ISBN,OCLC,publisher, publish_place,publish_year, pages, language, original_language, original_title, translator)
    })
  output$textOutputBibtex <- renderText({
    book_entry<-tibble(book_id,title,subtitle, full_entry, author, genre ,ISBN,OCLC,publisher, publish_place,publish_year, pages, language, original_language, original_title, translator)
    generate_bibtex <- function(book_entry) {
      # Function to create a single BibTeX entry
      create_entry <- function(book) {
        entry_type <- "book"  # Assuming the type is 'book', this can be adjusted
        bibtex_key <- paste0(book$title, book$publish_year)  # Construct a unique key for the BibTeX entry
        
        # Start the BibTeX entry
        entry <- paste0("@", entry_type, "{", bibtex_key, ",\n")
        
        # Function to add fields to the entry
        add_field <- function(field_name, field_value) {
          if (!is.na(field_value) && field_value != "") {
            return(paste0("  ", field_name, " = {", field_value, "},\n"))
          }
          return("")
        }
        
        # Adding fields to the entry
        fields <- c(
          add_field("author", book$author),
          add_field("title", book$title),
          add_field("year", book$publish_year),
          add_field("publisher", book$publisher),
          add_field("publish_place", book$publish_place),
          add_field("isbn", book$ISBN),
          add_field("oclc", book$OCLC),
          #add_field("series", book$series),
          add_field("pages", book$pages)
          # Add more fields as required
        )
        
        # Concatenate all fields and end the entry
        entry <- paste0(entry, paste0(fields, collapse = ""), "}")
        entry <- gsub(",\n}", "\n}", entry)  # Clean up the last comma
        return(entry)
      }
      
      # Apply the function to each row of the tibble
      bibtex_entries <- book_entry %>% rowwise() %>% mutate(bibtex = create_entry(cur_data())) %>% pull(bibtex)
      paste0(bibtex_entries, collapse = "\n\n")
    }
    
     bibtex_output <- generate_bibtex(book_entry)
     print(bibtex_output)
  })
  
  })
}

shinyApp(ui, server)
  

