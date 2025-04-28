#### About this script ####
## Title: Retrieving Electronic ISSNs from the ISSN Portal.
##
## Author: Javier Mancilla Galindo
## ORCiD: https://orcid.org/0000-0002-0718-467X
##
## Purpose: This script aims to enrich a dataframe (`journals_crossref`)
## by retrieving the electronic ISSN (eISSN) for each journal. It
## defines a function `get_eissn` that takes an ISSN as input and
## queries the ISSN Portal API. The function extracts all ISSNs listed
## on the portal page for the given ISSN and returns the first one
## that is different from the input ISSN, assuming it to be the eISSN.
## The script first initializes an empty character column named
## `electronic_issn` in the `journals_crossref` dataframe. It then
## attempts to retrieve the eISSN using the standard ISSN column. For
## any journals where the eISSN is still missing after the first attempt,
## the script makes a second attempt using the print ISSN column to
## find the corresponding electronic ISSN. The results are stored back
## into the `electronic_issn` column of the `journals_with_eissn`
## dataframe.

## Note: This script is sourced into the main quarto markdown file (.qmd) and 
## shall not be ran independently as it assumes that the objects have already 
## been defined in the global environment. 

get_eissn <- function(issn) {
  # Skip NA values
  if (is.na(issn)) return(NA_character_)
  
  # Add delay to respect server limits
  Sys.sleep(0.5)
  
  tryCatch({
    # Build the URL
    url <- paste0("https://portal.issn.org/resource/ISSN/", issn)
    
    # Make request
    response <- GET(url)
    
    if (status_code(response) == 200) {
      # Get the page content as text
      content_text <- content(response, "text", encoding = "UTF-8")
      
      # Find all ISSNs in the text
      all_issns <- unique(str_extract_all(content_text, "\\d{4}-\\d{3}[\\dX]")[[1]])
      
      # Filter out the input ISSN
      other_issns <- all_issns[all_issns != issn]
      
      # If there's another ISSN, it's likely the electronic one
      if (length(other_issns) > 0) {
        return(other_issns[1])
      }
    }
    
    return(NA_character_)
  }, error = function(e) {
    # Silently handle errors
    return(NA_character_)
  })
}

# First convert the electronic_issn column from logical to character
journals_crossref <- journals_crossref %>%
  mutate(electronic_issn = as.character(NA))

# Try with issn first
journals_with_eissn <- journals_crossref %>%
  mutate(electronic_issn = sapply(issn, get_eissn))

# For rows where electronic_issn is still NA, try with the print_issn column
journals_with_eissn <- journals_with_eissn %>%
  mutate(electronic_issn = ifelse(
    is.na(electronic_issn) & !is.na(print_issn), 
    sapply(print_issn[is.na(electronic_issn) & !is.na(print_issn)], get_eissn),
    electronic_issn
  ))

# Save as excel file to resolve some remaining collapsed rows and errors
write_xlsx(journals_with_eissn, path = file.path(tempfolder, "OSHLINE_journals_eissn.xlsx")) 
