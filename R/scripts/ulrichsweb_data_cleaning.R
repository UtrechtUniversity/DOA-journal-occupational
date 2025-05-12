#### About this script ####
## Title: Preparing and deduplicating Ulrichsweb journal data.
##
## Author: Javier Mancilla Galindo
## ORCiD: https://orcid.org/0000-0002-0718-467X

## Purpose: This script reads and processes a dataset of journals exported from
## Ulrichsweb. It performs several data cleaning steps including renaming the
## ISSN column, removing extraneous rows, and standardizing journal titles. A
## deduplicated list of journal titles is created and assigned unique IDs.
## Country and publisher information is then merged from the full dataset into
## the deduplicated journal list. Country names are standardized for
## compatibility with the `maps` package. The script also extracts and separates
## ISSNs by print and online formats, attaching them to the deduplicated journal
## records. The resulting dataframe `journals_dedup` is structured for
## subsequent geographic or bibliometric analysis.

## Note: This script is sourced into the main quarto markdown file (.qmd) and 
## shall not be ran independently as it assumes that the objects have already 
## been defined in the global environment. 

# Load dataset 
ulrichsweb <- read.csv(
  file.path(inputfolder, "2025_05_06_Ulrichsweb_journals.csv"), 
  na.strings = "null"
) %>% 
  slice(1:(n() - 1)) %>% # remove the last row containing the copyright notice
  rename(issn = "ISSN") %>% 
  rowid_to_column("id") 

# Remove "(Online)" from Journal of Occupational and Environmental Hygiene (Online)
# and replace with "Journal of Occupational and Environmental Hygiene"

ulrichsweb <- ulrichsweb %>% 
  mutate(
    Title = case_when(
      Title == "Journal of Occupational and Environmental Hygiene (Online)" ~ 
        "Journal of Occupational and Environmental Hygiene",
      T ~ Title
    )
  )

# Create a dataframe with the unduplicated journals
journals_dedup <- ulrichsweb %>% 
  distinct(Title) %>% 
  rowid_to_column("id")

# Add rowid to ulrichsweb by matching Title 
ulrichsweb <- ulrichsweb %>% 
  mutate(
    id = case_when(
      Title %in% journals_dedup$Title ~ journals_dedup$id[match(Title, journals_dedup$Title)],
      T ~ NA_real_
    )
  )

# Check for different values in Publisher column
ulrichsweb %>%
  distinct(id, Country) %>%
  summarise(n_country = n_distinct(id)) 

# Check for different values in Publisher column
ulrichsweb %>%
  distinct(id, Publisher) %>%
  summarise(n_publishers = n_distinct(id)) 

# There are no IDs with more than 1 country or publisher

# Add these columns to journals_dedup
journals_dedup <- journals_dedup %>% 
  mutate(
    country = ulrichsweb$Country[match(id, ulrichsweb$id)],
    publisher = ulrichsweb$Publisher[match(id, ulrichsweb$id)]
  )

# Change names of countries for later compatibility with maps package
journals_dedup <- journals_dedup %>% 
  mutate(
    country = case_when(
      country == "United Kingdom" ~ "UK",
      country == "Russian Federation" ~ "Russia",
      country == "United States" ~ "USA",
      country == "Egypt (Arab Republic of Egypt)" ~ "Egypt",
      country == "Iran, Islamic Republic of" ~ "Iran",
      country == "Korea, Republic of" ~ "Korea South",
      country == "Hong Kong" ~ "China",  # Note: Maps package typically includes Hong Kong as part of China
      T ~ country
    )
  )

# Generate separate subsets for print and online ISSNs
ulrichsweb_print <- ulrichsweb %>% 
  filter(Format == "Print") %>% 
  select(id, issn)

ulrichsweb_online <- ulrichsweb %>%
  filter(Format == "Online") %>% 
  select(id, issn)

# Add issn columns
journals_dedup <- journals_dedup %>% 
  mutate(
    issn_print = ulrichsweb_print$issn[match(id, ulrichsweb_print$id)],
    issn_online = ulrichsweb_online$issn[match(id, ulrichsweb_online$id)]
  ) %>% 
  relocate(c(issn_print, issn_online), .after = Title)