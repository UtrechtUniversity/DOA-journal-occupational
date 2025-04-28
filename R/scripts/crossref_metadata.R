#### About this script ####
## Title: Journal Metadata Retrieval using Crossref API.
##
## Author: Javier Mancilla Galindo
## ORCiD: https://orcid.org/0000-0002-0718-467X

## Purpose: This script retrieves and enriches journal metadata by
## querying the Crossref API. It begins by extracting a list of ISSNs 
## from a dataframe named `journal_df`. For each ISSN, data retrieved
## includes the title, publisher, and both print and electronic ISSNs. 
## In cases where metadata retrieval via ISSN is unsuccessful, the 
## script employs a fallback mechanism, using the journal's name. 
## The retrieved metadata is then merged back into the original `journal_df`, 
## creating an enriched dataframe (`journal_df_enriched`). The resulting  
## is saved as an Excel file within a temporary folder for manual review
## and further investigation of missing information.

## Note: This script is sourced into the main quarto markdown file (.qmd) and 
## shall not be ran independently as it assumes that the objects have already 
## been defined in the global environment. 

issn_list <- journal_df %>%
  # distinct(issn) %>% (in case there would be duplicates)
  pull(issn)

# Function to get metadata via ISSN
get_crossref_metadata <- function(issn) {
  Sys.sleep(0.5)  # Respect Crossref rate limits
  tryCatch({
    res <- cr_journals(issn)
    data <- res$data
    if (nrow(data) > 0) {
      tibble(
        issn = issn,
        title = data$title,
        publisher = data$publisher,
        print_issn = ifelse(length(data$issn[[1]]) > 0, data$issn[[1]][1], NA_character_),
        electronic_issn = ifelse(length(data$issn[[1]]) > 1, data$issn[[1]][2], NA_character_)
      )
    } else {
      tibble(issn = issn, title = NA_character_, publisher = NA_character_,
             print_issn = NA_character_, electronic_issn = NA_character_)
    }
  }, error = function(e) {
    tibble(issn = issn, title = NA_character_, publisher = NA_character_,
           print_issn = NA_character_, electronic_issn = NA_character_)
  })
}


# Query by ISSN
crossref_metadata <- map_dfr(issn_list, get_crossref_metadata)

# Merge with journal_df
journal_df_enriched <- journal_df %>%
  left_join(crossref_metadata, by = "issn")


# Names of journals with no ISSN metadata match
missing_metadata <- journal_df_enriched %>%
  filter(is.na(title)) %>%
  distinct(journal_name) %>%
  pull(journal_name)

# Function to get metadata via journal name if ISSN search failed 
get_crossref_by_name <- function(journal_name) {
  Sys.sleep(1)
  tryCatch({
    res <- cr_journals(query = journal_name)
    data <- res$data
    if (nrow(data) > 0) {
      issns <- data$issn[[1]]
      tibble(
        journal_name = journal_name,
        title = data$title[1],
        publisher = data$publisher[1],
        print_issn = ifelse(length(issns) > 0, issns[1], NA_character_),
        electronic_issn = ifelse(length(issns) > 1, issns[2], NA_character_)
      )
    } else {
      tibble(journal_name = journal_name, title = NA_character_, publisher = NA_character_,
             print_issn = NA_character_, electronic_issn = NA_character_)
    }
  }, error = function(e) {
    tibble(journal_name = journal_name, title = NA_character_, publisher = NA_character_,
           print_issn = NA_character_, electronic_issn = NA_character_)
  })
}

# Query missing journals by name
name_fallback_data <- map_dfr(missing_metadata, get_crossref_by_name)

# Merge fallback results, update only where original metadata is missing
journal_df_enriched <- journal_df_enriched %>%
  rows_update(name_fallback_data, by = "journal_name", unmatched = "ignore")

# Check final structure
glimpse(journal_df_enriched)

# Save as excel file to examine manually and search for missing metadata
write_xlsx(journal_df_enriched, path = file.path(tempfolder, "OSHLINE_crossref.xlsx")) 
