#### About this script ####
## Title: Checking for journal inclusion in the DOAJ using ISSNs.
##
## Author: Javier Mancilla Galindo
## ORCiD: https://orcid.org/0000-0002-0718-467X

## Purpose: This script determines whether journals listed in the
## `ulrichsweb` dataframe are indexed in the Directory
## of Open Access Journals (DOAJ). It defines a function `get_doaj_match`
## that takes an ISSN as input and queries the DOAJ API to check for
## a matching journal. The function returns `TRUE` if a journal with
## the given ISSN is found in the DOAJ and `FALSE` otherwise. The
## script then iterates through each row of the `ulrichsweb`
## and applies the `get_doaj_match` function to the `issn` column to create
## a new logical column named `doaj_match`. Finally, the resulting
## dataframe `journals_doaj` is saved as an Excel file ("journals_DOAJ.xlsx")
## within a temporary folder.

## Note: This script is sourced into the main quarto markdown file (.qmd) and 
## shall not be ran independently as it assumes that the objects have already 
## been defined in the global environment. 

get_doaj_match <- function(issn) {
  Sys.sleep(0.5)
  # Skip if NA or empty
  if (is.na(issn) || issn == "") {
    return(FALSE)
  }
  
  base_url <- paste0("https://doaj.org/api/v2/search/journals/issn:", issn)
  res <- GET(base_url)
  if (status_code(res) == 200) {
    json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
    return(length(json$results) > 0)
  }
  return(FALSE)
}

journals_doaj <- ulrichsweb %>% 
  rowwise() %>%
  mutate(doaj_match = get_doaj_match(issn)) %>%
  ungroup()

write_xlsx(journals_doaj, path = file.path(psfolder, "journals_DOAJ.xlsx")) 
