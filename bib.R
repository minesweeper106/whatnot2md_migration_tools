library(dplyr)

generate_bibtex <- function(book_entry) {
  # Function to create a single BibTeX entry
  create_entry <- function(book) {
    entry_type <- "book"  # Assuming the type is 'book', this can be adjusted
    bibtex_key <- paste0(book$title, book$year)  # Construct a unique key for the BibTeX entry
    
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

# Example usage
# Assuming book_entry is a tibble with the necessary columns
# bibtex_output <- generate_bibtex(book_entry)
# cat(bibtex_output)  # Print the BibTeX entries
