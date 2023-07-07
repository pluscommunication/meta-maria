#Rulare pachete####
library(revtools); library(PRISMA2020); library(dplyr)
library(bib2df); library(writexl)

# Loading initial articles database ####
# Scopus - 11; Google Scholar 997; CrossRef;  
articles <- data.frame(ID = NA, Type = NA, Authors = NA, Year = NA, Title = NA, Journal = NA,
                       Abstract = NA, Keywords = NA, DOI = NA, Source = NA)
sursa <- bib2df("maria1.bib"); source <- "WOS"; rec <- 1
sursa <- bib2df("scopus.bib"); source <- "Scopus"; rec <- 1
sursa <- bib2df("GScholar.bib"); source <- "Google Scholar"; rec <- 1
while (rec <= nrow(sursa)) {
  rand <- sursa %>% dplyr::filter(BIBTEXKEY == sursa$BIBTEXKEY[rec])
  id <- rand$BIBTEXKEY
  type <- rand$CATEGORY
  authors <- paste(unlist(rand$AUTHOR), collapse = "; ")
  year <- rand$YEAR
  title <- gsub("[{|}]", "", rand$TITLE)
  journal <- rand$JOURNAL
  abstract <- rand$ABSTRACT
  keywords <- rand$AUTHOR_KEYWORDS
  if(source == 'PubMed') keywords <- ""
  if(source == 'Google Scholar') keywords <- ""
  if(source == 'Science Direct') keywords <- rand$KEYWORDS
  doi <- rand$DOI
  # Updating articles' database
  articles <- rbind(articles,
                    c(id, type, authors, year, title, journal, abstract, keywords, doi, source))
  rec <- rec + 1 # Jump to the next article
}
