# Installing and loading packages
if(!require(revtools)) install.packages("revtools")
if(!require(PRISMA2020)) install.packages("PRISMA2020")
if(!require(dplyr)) install.packages("dplyr")
if(!require(bib2df)) install.packages("bib2df")
if(!require(writexl)) install.packages("writexl")

library(revtools); library(PRISMA2020); library(dplyr)
library(bib2df); library(writexl)

# Creating PRISMA structure ####
PRISM.tpl <- read.csv(system.file("extdata", "PRISMA.csv", package = "PRISMA2020"))
PRISM.tpl <- PRISM.tpl %>% 
  dplyr::mutate(boxtext = case_when(data == "identification"~"Identification", T~boxtext))

# Loading initial articles database ####
# Scopus - 99; PubMed = 54
articles <- data.frame(ID = NA, Type = NA, Authors = NA, Year = NA, Title = NA, Journal = NA,
                       Abstract = NA, Keywords = NA, DOI = NA, Source = NA)
sursa <- bib2df("scopus.bib"); source <- "Scopus"; rec <- 1
sursa <- bib2df("pubmed.bib"); source <- "PubMed"; rec <- 1
sursa <- bib2df("sciencedirect.bib"); source <- "Science Direct"; rec <- 1
sursa <- bib2df("gscholar.bib"); source <- "Google Scholar"; rec <- 1
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

# Deleting first record and all used objects
articles <- articles[-1,]
rm(rand, sursa, id, type, authors, year, title, journal, abstract, keywords, doi,
   rec, source)
# Saving dataset - FULL ARTICLES
save(articles, file = "Articles 1. RData"); total.db <- nrow(articles)

# PRISMA - INITIAL number of articles ####
PRISM.tpl$n[which(PRISM.tpl$data == 'database_results')] <- total.db
PRISM.tpl$n[which(PRISM.tpl$data == 'register_results')] <- 0
save(PRISM.tpl, file = 'PRISMA.RData')

# I. Searching and removing duplicates ####
no.duplicates <- find_duplicates(data = articles, match_variable = "Title",
                                 match_function = "exact")
no.duplicates <- extract_unique_references(x = articles, matches = no.duplicates)
articles <- no.duplicates
no.duplicates <- find_duplicates(data = articles, match_variable = "DOI",
                                 match_function = "exact")
no.duplicates <- extract_unique_references(x = articles, matches = no.duplicates)
## Search and removing duplicates - MANUALLY
tmp <- screen_duplicates(x = no.duplicates)
save(tmp, file = "Temp.Rdata")
tmp <- screen_duplicates(x = tmp)

total.duplicates <- total.db - nrow(tmp)
articles <- tmp; save(articles, file = "Article 2.RData")
# PRISMA - DUPLICATES ####
PRISM.tpl$n[which(PRISM.tpl$data == 'duplicates')] <- total.duplicates
save(PRISM.tpl, file = 'PRISMA.RData')

# II. Searching by automatic tools ####
tmp <- screen_topics(x = articles)
save(tmp, file = "Temp.Rdata")
tmp <- screen_topics(x = tmp)
tmp <- tmp$raw %>% dplyr::filter(screened_topics == 'selected')

total.automatic <- nrow(articles) - nrow(tmp)
articles <- tmp; save(articles, file = "Article 3.RData")
# PRISMA - DUPLICATES ####
PRISM.tpl$n[which(PRISM.tpl$data == 'excluded_automatic')] <- total.automatic
save(PRISM.tpl, file = 'PRISMA.RData')

# III. Searching by TITLE ####
tmp <- screen_titles(x = articles)
save(tmp, file = "Temp.Rdata")
tmp <- screen_titles(x = tmp)

# IV. Searching by ABSTRACT ####


# Writing final Excel file ####


# Draw and save PRISMA flowdiagram ####
PRISMA <- PRISMA_flowdiagram(data = PRISMA_data(PRISM.tpl), previous = F, other = F,
                             fontsize = 10); PRISMA
PRISMA_save(PRISMA, overwrite = T, filename = "PRISMA.png", filetype = "PNG")
