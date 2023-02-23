
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
dateSincePrev="2021/03/01" # date since prevous search
search_query_counts=EUtilsSummary(cannabis_outcome_pubmed_comb)@count
search_query_pubmed <- EUtilsSummary(cannabis_outcome_pubmed_comb, 
                                     retmax=search_query_counts, 
                                     mindate=dateSincePrev, # update existing search
                                     maxdate="2022/10/21",
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

table(dfPubType$ArticleType1)
table(dfPubType$ArticleType2)
table(dfPubType$ArticleType3)
dfPubType$type=NA
vecExclude=c("Review", "Systematic Review", "Case Reports", "Comment", "Guideline", "Editorial", "Letter", "News", "Newspaper Article", "Letter", "Clinical Trial Protocol", "Case Reports") # Article types to exclude
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
  pubmed_data_list[[i]] <- data.frame('DOI'= fetch_pubmed@DOI[i], 'PMID'=fetch_pubmed@PMID[i],'Author'=author, 'Year'=YearPubmed(fetch_pubmed)[[i]], 'ISSN' = ISSN(fetch_pubmed)[[i]],'Title'=ArticleTitle(fetch_pubmed)[[i]],'Abstract'=AbstractText(fetch_pubmed)[[i]], 'Stage1_screening'=NA, 'Stage2_screening'=NA, 'Stage2_comment'=NA, 'SearchDate'="21.10.2022", 'PubType'=fetch_pubmed@PublicationType[[i]])
}


pubmed_data=do.call(rbind, pubmed_data_list)
pubmed_data$Abstract <- as.character(pubmed_data$Abstract)
pubmed_data$Abstract <- gsub(",", " ", pubmed_data$Abstract, fixed = TRUE)


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
embase_findingd_n=NROW(embase_outAll)
embase_out=subset(embase_outAll, YR>=2021) # year of publication 
embase_data <- data.frame("DOI" = embase_out$DO, 'PMID'=embase_out$PM, 'ISSN' =embase_out$IS,'Author'=embase_out$AU, 'Year'=embase_out$YR ,'Title'=embase_out$TI,'Abstract'=embase_out$AB,  'Stage1_screening'=NA, 'Stage2_screening'=NA, 'Stage2_comment'=NA, 'SearchDate'="21.10.2022",  'PubType'=embase_out$PT)
# Shorten DOI
embase_data$DOI=sapply(str_split(embase_data$DOI, "/"), tail, 1)



# ======= IMPORT PSYCHINFO SEARCH OUTPUT
# Go though the following steps
#     1) Go to PsychInfo database
#     2) Copy the number of yielded search results (eg. '9300') in the 'jump to page xx' box and click 'go'
#     3) Download as excel file
#        => remove first row and save
#     4) Select 'Complete reference' in fields to select

psychinfo_outALL <- read_excel(paste0(HOME,"/data/psychinfo.xls"))
psychinfo_findingd_n=NROW(psychinfo_outALL)
psychinfo_out=subset(psychinfo_outALL, YR>=2021) # year of publication 
psychinfo_data <- data.frame("DOI" = psychinfo_out$DO, 'PMID'=psychinfo_out$PM, 'ISSN' =psychinfo_out$IS,'Author'=psychinfo_out$AU, 'Year'=psychinfo_out$YR ,'Title'=psychinfo_out$TI,'Abstract'=psychinfo_out$AB, 'Stage1_screening'=NA, 'Stage2_screening'=NA, 'Stage2_comment'=NA, 'SearchDate'="21.10.2022", 'PubType'=psychinfo_out$DT)
# Shorten DOI
psychinfo_data$DOI=sapply(str_split(psychinfo_data$DOI, "/"), tail, 1)

# Combine with previous search results
initialSearch=litSearch=read.csv(paste0(HOME,"/data/search2March2021.csv")) # Read in inital search
initialSearch$X=NULL
initialSearch$PubType="Journal Article"
search_comb=rbind(initialSearch, pubmed_data, embase_data, psychinfo_data)
head(search_comb)


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


# ======== Generate PRISMA FLOW DIAGRAM ===================
#library(PRISMAstatement)
#library(svglite)
noStud=readRDS(paste0(HOME,"/output/searchIdentified.rds"))
totNumberIdentified=sum(noStud$nIdentified)
litSearch=read.csv(paste0(HOME,"/data/search22Oct2022.csv"))
totNumberStudies=NROW(litSearch)
s2Included=NROW(subset(litSearch, Stage1_screening=="yes"))
s3Included=NROW(subset(litSearch, Stage2_screening=="yes"))

datExtracted=read.csv(paste0(HOME, "/data/Data_extraction_1Dec_2022.csv"),
                      header=T,
                      na.strings=c("","NA"))

datExtracted=subset(datExtracted, IncludeStudy=="Yes")
finalSinclusion=length(levels(as.factor(datExtracted$Reference)))

reasonEx=subset(litSearch, Stage2_screening=="no")
reasonEx$reasonCoded=revalue(reasonEx$Stage2_comment,
                             c( "CAPS not assessed" = "CAPS_notReported",
                                "no CaPS assessed" = "CAPS_notReported",
                                "no CAPS assessed" = "CAPS_notReported",
                                "no CaPS reported" = "CAPS_notReported",
                                "no CAPS reported" = "CAPS_notReported",
                                "qualitative analysis" = "qualitative"))

countReason=as.data.frame(table(reasonEx$reasonCoded))
propReason=as.data.frame(prop.table(table(reasonEx$reasonCoded)))
reasonExdf=data.frame(reason=countReason$Var1, count=countReason$Freq, prop=propReason$Freq)
reasonExdf <- reasonExdf[order(-reasonExdf$count),] 
reasonText=paste0(paste0(reasonExdf$reason, ": (k=", reasonExdf$count, "; ", round(reasonExdf$prop*100,2), "%)"), collapse=" || ")

# not identified by search:
# 1) Bonn-Miller, Marcel O., Sue Sisley, Paula Riggs, Berra Yazar-Klosinski, Julie B. Wang, Mallory JE Loflin, Benjamin Shechet et al. "The short-term impact of 3 smoked cannabis preparations versus placebo on PTSD symptoms: A randomized cross-over clinical trial." Plos one 16, no. 3 (2021): e0246990.
# 2) Contreras, Alexandra Elyse, Katelyn E. Hall, Daniel I. Vigil, Allison Rosenthal, Alejandro Azofeifa, and Michael Van Dyke. "Results from the Colorado Cannabis Users Survey on Health (CUSH), 2016." International Journal of Mental Health and Addiction 18, no. 1 (2020): 1-13.
# 3) Costiniuk, Cecilia T., Zahra Saneei, Syim Salahuddin, Joseph Cox, Jean-Pierre Routy, Sergio Rueda, Sara J. Abdallah et al. "Cannabis consumption in people living with HIV: Reasons for use, secondary effects, and opportunities for health education." Cannabis and cannabinoid research 4, no. 3 (2019): 204-213.
# 4) Cuttler, Carrie, Laurie K. Mischley, and Michelle Sexton. "Sex differences in cannabis use and effects: a cross-sectional survey of cannabis users." Cannabis and cannabinoid research 1, no. 1 (2016): 166-175.
# 5) de la Fuente, Alethia, Federico Zamberlan, Andr√©s S√°nchez Ferr√°n, Facundo Carrillo, Enzo Tagliazucchi, and Carla Pallavicini. "Over eight hundred cannabis strains characterized by the relationship between their psychoactive effects, perceptual profiles, and chemical compositions." bioRxiv (2019): 759696.
# 6) Krebs, M. O., Y. Morvan, T. Jay, R. Gaillard, and O. Kebir. "Psychotomimetic effects at initiation of cannabis use are associated with cannabinoid receptor 1 (CNR1) variants in healthy students." Molecular psychiatry 19, no. 4 (2014): 402-403.
# 7) Levy, Sharon, and Elissa R. Weitzman. "Acute Mental Health Symptoms in Adolescent Marijuana Users." JAMA pediatrics 173, no. 2 (2019): 185-186.
# 8) Lichtman, Aron H., Eberhard Albert Lux, Robert McQuade, Sandro Rossetti, Raymond Sanchez, Wei Sun, Stephen Wright, Elena Kornyeyeva, and Marie T. Fallon. "Results of a double-blind, randomized, placebo-controlled study of nabiximols oromucosal spray as an adjunctive therapy in advanced cancer patients with chronic uncontrolled pain." Journal of pain and symptom management 55, no. 2 (2018): 179-188.
# 9) Martin-Santos, R., J. a Crippa, A. Batalla, S. Bhattacharyya, Z. Atakan, S. Borgwardt, P. Allen et al. "Acute effects of a single, oral dose of d9-tetrahydrocannabinol (THC) and cannabidiol (CBD) administration in healthy volunteers." Current pharmaceutical design 18, no. 32 (2012): 4966-4979.
# 10) Milosev, Leonie M., Nikolas Psathakis, Natalia Szejko, Ewgeni Jakubovski, and Kirsten R. M√ºller-Vahl. "Treatment of Gilles de la Tourette Syndrome with Cannabis-Based Medicine: Results from a Retrospective Analysis and Online Survey." Cannabis and Cannabinoid Research 4, no. 4 (2019): 265-274.
# 11) Naef, Myrtha, Michele Curatolo, Steen Petersen-Felix, Lars Arendt-Nielsen, Alex Zbinden, and Rudolf Brenneisen. "The analgesic effect of oral delta-9-tetrahydrocannabinol (THC), morphine, and a THC-morphine combination in healthy subjects under experimental pain conditions." Pain 105, no. 1-2 (2003): 79-88.
# 12) Spindle, Tory R., Edward J. Cone, Elia Goffi, Elise M. Weerts, John M. Mitchell, Ruth E. Winecker, George E. Bigelow, Ronald R. Flegel, and Ryan Vandrey. "Pharmacodynamic effects of vaporized and oral cannabidiol (CBD) and vaporized CBD-dominant cannabis in infrequent cannabis users." Drug and Alcohol Dependence (2020): 107937.
# 13) Stone, J.M., Morrison, P.D., Brugger, S., Nottage, J., Bhattacharyya, S., Sumich, A., Wilson, D., Tunstall, N., Feilding, A., Brenneisen, R. and McGuire, P., 2012. Communication breakdown: delta-9 tetrahydrocannabinol effects on pre-speech neural coherence. Molecular psychiatry, 17(6), pp.568-569.


textB1=paste0(sum(noStud$nIdentified), " total records identified from: ", paste(paste0(noStud$database, " (n=", noStud$nIdentified, ")"), collapse = " "))
textB2=paste0("Records removed before screening: Records marked as ineligible by automation tools (n=", totNumberIdentified-totNumberStudies, ")")
textB3=paste0("Non-relevant records excluded by abstract (n=", totNumberStudies-s2Included, ")")
textB4=paste0("Full text studies assessed for eligibility (n=", s2Included, ")")
textB5=paste0("Additional studies that were identified (n=", finalSinclusion-s3Included, ")")
textB6=paste0(s2Included-s3Included, " did not meet inclusion criteria. " )
textB7=paste0("Number of additional studies identified: ", finalSinclusion-s3Included)
textB8=paste0("Final number of included studies: ", finalSinclusion)
textB9=paste0("Reason for exclusion at stage 2 include: ", reasonText)
textFlowChart=c(textB1,textB2, textB3, textB4, textB5, textB6, textB7, textB8, textB9)
