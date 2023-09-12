
rm(list = ls())
.libPaths(new=c("/Users/tabea/Dropbox/progs/R/library"))
system('R_LIBS=/Users/tabea/Dropbox/progs/R/library')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
HOME=getwd()

# Load and install libraries
.libPaths("~/Dropbox/progs/R/library")
load.lib=c('compute.es', 'dplyr', 'data.table', 'RISmed', 'readxl','ggplot2', 'metafor', 'MAd', 'metap', 'ggpubr',
           'pwr', 'magrittr', 'gmodels', 'revtools', 'stringr', 'latex2exp', 'svglite', ' ggridges', 'Rcpp', 'PRISMAstatement',
           'DiagrammeRsvg', 'tidyverse', 'grViz', 'magick', 'latex2exp', 'metaviz', 'plyr' , 'revtools', 'gridExtra', 'grid',
           'MBESS', "pbapply", 'esc')
install.lib<-load.lib[!load.lib %in% installed.packages()]
# Install missing packages
for(lib in install.lib) install.packages(lib, dependencies=TRUE, lib = "/Users/tabea/Dropbox/progs/R/library")
# Load all packages
sapply(load.lib,require,character=TRUE)
sessionInfo()
#update.packages(oldPkgs = load.lib)


# Define cannabis terms
cannabis_vector=c("hashis*",
                  "hash",
                  "skunk",
                  "bhang",
                  "ganja",
                  "ganjah",
                  "hemp",
                  "charas",
                  "cannabis",
                  "marijuana",
                  "marihuana",
                  "dronabinol",
                  "marinol",
                  "levonantradol",
                  "tetrahydrocannabinol",
                  "cesamet",
                  "THC",
                  "nabiximols",
                  "sativex",
                  "cannabidiol",
                  "JWH-018",
                  "bedrobinol",
                  "WIN,55",
                  "CP55940",
                  "bedrocan",
                  "cannabinoid",
                  "cannabinoids",
                  "nabilone")


# Define outcome terms
outcome_vector=c("psychosis",
                 "psychotic",
                 "psychoses",
                 "psychotomimetic",
                 "'community assessment of psychic experience scale'",
                 "'positive and negative syndrome scale'",
                 "'brief psychiatric rating scale'",
                 "'scale for the assessment of positive symptoms'",
                 "'psychotomimetic states inventory'",
                 "'cannabis experiences questionnaire'",
                 "paranoia",
                 "hallucinations",
                 "hallucination",
                 "delusion",
                 "delusions",
                 "intoxication",
                 "intoxicating",
                 "'acute effect'",
                 "'acute effects'",
                 "'adverse effect'",
                 "'adverse effects'",
                 "'adverse event'",
                 "'adverse events'",
                 "'adverse drug reaction'",
                 "'adverse drug reactions'",
                 "'visual analogue scal'*")

searchTermsText=paste0("using ",NROW(cannabis_vector)," search terms indexing cannabis/THC and ",NROW(outcome_vector)," terms indexing psychosis-like outcomes or cannabis-intoxication experiences")
max.len = max(length(cannabis_vector), length(outcome_vector))
canDF = c(cannabis_vector, rep(" ", max.len - length(cannabis_vector)))
outcomeDF = c(outcome_vector, rep(" ", max.len - length(outcome_vector)))
search_terms_table=data.frame(cannabis=canDF, outcome=outcomeDF)

# ====== PUBMED SEARCH =========
# Is the search case-sensitive? No, the search engine is not case-sensitive.
#  PubMed truncation symbol is the asterisk (*)

# Function to generate search terms
searchPubmedTerms=function(vector){
  brakets_vec=paste(rep("(", length(vector)-1), collapse="")
  vector_start=vector[1]
  vector_end=vector[length(vector)]
  vector_middle=vector[which(vector!=vector_start & vector!=vector_end)]
  start=paste0(brakets_vec ,vector_start, "[Title/Abstract])")
  middle=paste(paste0("(",vector_middle, "[Title/Abstract]))"), collapse = " OR ")
  end=paste0("(",vector_end, "[Title/Abstract])")
  vector_pasted=paste(start,middle,end , sep = " OR ")
  print(vector_pasted)
  return(vector_pasted)
}

cannabis_vector_title_comb=searchPubmedTerms(cannabis_vector)
outcome_vector_title_comb=searchPubmedTerms(outcome_vector)
# Combine both search terms
cannabis_outcome_pubmed_comb=paste0("(",cannabis_vector_title_comb,")" ," AND ", "(", outcome_vector_title_comb,")")
# Search pubmed
dateSincePrev="2022/10/21" # date since prevous search
dateUpdate="2023/06/12"
search_query_counts=EUtilsSummary(cannabis_outcome_pubmed_comb)@count
search_query_pubmed <- EUtilsSummary(cannabis_outcome_pubmed_comb, 
                                     retmax=search_query_counts, 
                                     mindate=dateSincePrev, # update existing search
                                     maxdate=dateUpdate,
                                     db="pubmed" )
fetch_pubmed <- EUtilsGet(search_query_pubmed, type = "efetch", db = "pubmed") # get actual data from PubMed

# see the ids of our returned query (all published until to date)
search_query_pubmedALL <- EUtilsSummary(cannabis_outcome_pubmed_comb, retmax=search_query_counts, db="pubmed" )
searchIDsALL=QueryId(search_query_pubmedALL)
pubmed_findings_n=NROW(searchIDsALL) 

# Extract publication type
dfPubType=data.frame(ArticleType1=unlist(lapply(fetch_pubmed@PublicationType, `[`, 1)),
                     ArticleType2=unlist(lapply(fetch_pubmed@PublicationType, `[`, 2) ),
                     ArticleType3=unlist(lapply(fetch_pubmed@PublicationType, `[`, 3) ),
                     ArticleType3=unlist(lapply(fetch_pubmed@PublicationType, `[`, 4) ))


levels(as.factor(dfPubType$ArticleType1))
table(dfPubType$ArticleType1)
table(dfPubType$ArticleType2)
table(dfPubType$ArticleType3)
dfPubType$type=NA
vecExclude=c("Review", "Systematic Review", "Case Reports", "Comment", "Guideline", "Editorial", "Letter", "News", "Newspaper Article", "Letter", "Clinical Trial Protocol", "Case Reports", "Meta-Analysis", "Systematic Review", "Conference Abstract", "Conference Paper", "Conference Review") # Article types to exclude
# Exclude if a study is labelled as any of the above study types
for ( i in 1:length(dfPubType$ArticleType1)) {
  dichExclude = vecExclude %in% levels(as.factor(dfPubType[i,]))
  any(dichExclude) # Are Some Value True?
  dfPubType$type[i]=ifelse(  (any(dichExclude) ==TRUE), "Exclude", "Article")
}
table(dfPubType$type)

# Format the results for export
pubmed_data_list=list()
for ( i in 1:length(Author(fetch_pubmed))) {
  x=Author(fetch_pubmed)[[i]]
  if(NCOL(x)<=1) next 
  author=paste(paste0(x$LastName, ", ", x$ForeName ), collapse = "; ")
  pubmed_data_list[[i]] <- data.frame('DOI'= fetch_pubmed@DOI[i], 'PMID'=fetch_pubmed@PMID[i],'Author'=author, 'Year'=YearPubmed(fetch_pubmed)[[i]], 'ISSN' = ISSN(fetch_pubmed)[[i]],'Title'=ArticleTitle(fetch_pubmed)[[i]],'Abstract'=AbstractText(fetch_pubmed)[[i]], 'Stage1_screening'=NA, 'Stage2_screening'=NA, 'Stage2_comment'=NA, 'SearchDate'=dateUpdate, 'PubType'=fetch_pubmed@PublicationType[[i]])
}


pubmed_data=do.call(rbind, pubmed_data_list)
pubmed_data$Abstract <- as.character(pubmed_data$Abstract)
pubmed_data$Abstract <- gsub(",", " ", pubmed_data$Abstract, fixed = TRUE)
pubmed_data=subset(pubmed_data, !PubType %in% vecExclude)

# ====== OVID SEARCH =========
# The truncation symbols (*) or ($) can be used as a substitute for any string of zero or more characters at the end of a word.
# not case-sensitive; whether your search terms are entered in upper- or lowercase, the same records will be retrieved.
# https://medlinetranspose.github.io/index.html#results
outcome_ovid_vector=paste0("(", paste(paste0("(", outcome_vector, ")" ),  collapse = " or "), ").ab,ti.")
cannabis_ovid_vector=paste0("(",paste( paste0("(", cannabis_vector, ")" ),  collapse = " or "), ").ab,ti.")
# ab,ti # Abstract and title
# Search phrase to be included in Ovid ('basic search')
cannabis_outcome_ovid_comb=paste0(cannabis_ovid_vector, " and " , outcome_ovid_vector)



# ======= IMPORT EMBASE SEARCH OUTPUT
# Go though the following steps
#     1) Go to embase database
#       => http://library-guides.ucl.ac.uk/az.php?q=embase (just the normal embase, not classic)
#     2) Copy the number of yielded search results (eg. '9300') in the 'jump to page xx' box and click 'go'
#     3) Filter by: specific year range
#     3) Download as excel file
#        => remove first row and save
#     4) Select 'Complete reference' in fields to select

# Remove the first row, otherwise error
embase_outAll <- read_excel(paste0(HOME,"/data/embase.xls"))
labels(embase_outAll);head(embase_outAll)
embase_findingd_n=10060 #total number identified in EMBASE, regardless of year NROW(embase_outAll)
embase_out=subset(embase_outAll, YR>=2022) # year of publication 
embase_data <- data.frame("DOI" = embase_out$DO, 'PMID'=embase_out$PM, 'ISSN' =embase_out$IS,'Author'=embase_out$AU, 'Year'=embase_out$YR ,'Title'=embase_out$TI,'Abstract'=embase_out$AB,  'Stage1_screening'=NA, 'Stage2_screening'=NA, 'Stage2_comment'=NA, 'SearchDate'=dateUpdate,  'PubType'=embase_out$PT)
# Shorten DOI
embase_data$DOI=sapply(str_split(embase_data$DOI, "/"), tail, 1)
embase_data=subset(embase_data, !PubType %in% vecExclude)


# ======= IMPORT PSYCHINFO SEARCH OUTPUT
# Go though the following steps
#     1) Go to PsychInfo database
#     2) Copy the number of yielded search results (eg. '9300') in the 'jump to page xx' box and click 'go'
#     3) Download as excel file
#        => remove first row and save
#     4) Select 'Complete reference' in fields to select

psychinfo_outALL <- read_excel(paste0(HOME,"/data/psychinfo.xls"))
psychinfo_findingd_n=NROW(psychinfo_outALL)
psychinfo_out=subset(psychinfo_outALL, YR>=2022) # year of publication 
psychinfo_data <- data.frame("DOI" = psychinfo_out$DO, 'PMID'=psychinfo_out$PM, 'ISSN' =psychinfo_out$IS,'Author'=psychinfo_out$AU, 'Year'=psychinfo_out$YR ,'Title'=psychinfo_out$TI,'Abstract'=psychinfo_out$AB, 'Stage1_screening'=NA, 'Stage2_screening'=NA, 'Stage2_comment'=NA, 'SearchDate'=dateUpdate, 'PubType'=psychinfo_out$DT)
# Shorten DOI
psychinfo_data$DOI=sapply(str_split(psychinfo_data$DOI, "/"), tail, 1)



# combine all searchers
search_comb=rbind(pubmed_data, embase_data, psychinfo_data)


# ====== STEP 3: EXLCUDE IRRELEVANT STUDY TYPES =========
# Exclude reviews
search_comb$PubType=revalue(search_comb$PubType,
                            c("Letter" = "Exclude",
                              "Review-Book" = "Exclude",
                              "Note" = "Exclude",
                              "Comment/Reply" = "Exclude",
                              "Review" = "Exclude",
                              "Chapter" = "Exclude",
                              "Dissertation" = "Exclude",
                              "Conference Abstract" = "Exclude",
                              "Conference Paper" = "Exclude",
                              "Editorial" = "Exclude",
                              "Clinical Trial"="Journal Article",
                              "Clinical Trial Protocol"="Exclude",
                              "Case Reports"= "Exclude",
                              "Comparative Study"="Journal Article",
                              "Controlled Clinical Trial"="Journal Article",
                              "Conference Review" = "Exclude",
                              "Meta-Analysis"="Exclude",
                              "Erratum" = "Exclude",
                              "News"= "Exclude",
                              "Systematic Review"= "Exclude",
                              "Erratum/Correction"= "Exclude",
                              "Column/Opinion "= "Exclude"))

search_comb_clean=subset(search_comb, PubType!="Exclude")
table(search_comb_clean$PubType)

# Select columns for dataframe
screened_search_select=subset(search_comb_clean, select = c(PMID, Author, Year, ISSN, Title , Abstract, Stage1_screening, Stage2_screening,Stage2_comment, SearchDate ))
screened_search_select_ordered= screened_search_select[order(screened_search_select$Year, decreasing = TRUE) , ]

# ====== STEP 2: REMOVE DUPLICATES =========
matches <- find_duplicates(screened_search_select_ordered, match_variable = "Title" )
data_litsearch_unique <- extract_unique_references(screened_search_select_ordered, matches)

# Remove duplicates based on PMID
data_litsearch_unique_rem=data_litsearch_unique[duplicated(data_litsearch_unique$PMID), ]
data_litsearch_unique_keep=data_litsearch_unique[!duplicated(data_litsearch_unique$PMID), ]
NROW(data_litsearch_unique_keep)
NROW(data_litsearch_unique_rem)

# Check if publicates have the same title
CHECKdup=subset(merge(data_litsearch_unique_keep, data_litsearch_unique_rem, by="PMID"),
                select=c(PMID, Title.x, Title.y))

# Number of duplicates excluded
removedDuplictes=length(screened_search_select_ordered$PMID)-length(data_litsearch_unique_keep$PMID)


# Export search results
#write.csv(data_litsearch_unique_keep,paste0(HOME,"/data/2March2021.csv"), na = "")

# ======== Export output ============
# save output
#write.csv(data_litsearch_unique_keep, paste0(HOME,"/data/search21Oct2022.csv"))

# save number of identified studies
searchIdentified=data.frame(database=c("pubmed", "embase", "psychinfo"), nIdentified=c(pubmed_findings_n, embase_findingd_n, psychinfo_findingd_n))
saveRDS(searchIdentified, paste0(HOME,"/output/searchIdentified.rds"))

# Combine with previous search results
initialSearch=read.csv(paste0(HOME,"/data/search22Oct2022.csv")) # Read in inital search
initialSearch$PubType="Journal Article"
initialSearchSel=subset(initialSearch, select=colnames(data_litsearch_unique_keep))
initialSearchSub=subset(initialSearchSel, Year<2022)
initialSearchUpdated=rbind(data_litsearch_unique_keep, initialSearchSub)

initialRem=subset(initialSearchSel, Year>=2022, select=c("PMID", "Title", "Stage1_screening", "Stage2_screening", "Stage2_comment") )
initialSearchUpdatedRem=left_join(initialSearchUpdated, initialRem, by = "PMID", suffix=c("", "_rem"))  
#write.csv(initialSearchUpdatedRem, paste0(HOME,"/data/search12June2023.csv"))


172*0.1

667*0.10


