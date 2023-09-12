


library("openxlsx")

# +++++++++++++++++++++++++ RUN FUNCTIOND AND DEFINE STYLES ++++++++++++++++++++++++++++++++++++++++++++++++++
startRow_figures=8
ColStart=2
RowHeader=2
RowSubheaderStart=3
RowSubheaderEnds=6
RowTable=8

# Create info text
createInfo=function(dataInfoPath){
  datOut=read.csv(dataInfoPath,header=T)
  datOut$X=NULL
  datOut_merged=paste0(datOut[,1],": " ,datOut[,2])
  return(datOut_merged)
}

# Headerstyle
hs1 <- createStyle(halign = "CENTER", textDecoration = "Bold",
                   border = "Bottom", fontColour = "black", fgFill = "white")

addTable=function(sheet, table){
  writeDataTable(meta_out, sheet, table, headerStyle=hs1, tableStyle = "TableStyleLight1",
                 startRow = RowTable, startCol = ColStart)
}

# HEADER
headerFunc=function(TITLE, sheet){
  writeData(meta_out, sheet = sheet, TITLE,
            startCol = ColStart, startRow = RowHeader)
}


# INFO ROW
InfoFunc=function(TITLE, sheet){
  writeData(meta_out, sheet = sheet, TITLE,
            colNames = FALSE, rowNames = FALSE,
            startCol = ColStart, startRow = RowSubheaderStart)
}
library("openxlsx")

meta_out <- openxlsx::createWorkbook()

# Create new workbookd
createSheet=function(meta_out, sheet, title_name, table, Info_text){
  # Run functions
  addWorksheet(meta_out, sheet)
  addTable(sheet, table)
  headerFunc(title_name, sheet)
  InfoFunc(Info_text, sheet)
}



# ================= TABLE =========================
colnames(summaryTableIncStudiesMED)=c("Condition treated with medicinal cannabis product", "Number of studies", "% studies per condition", "number of non-overlapping included individuals")

# Add parameters
title_name="Table 1. Conditions treated with medicinal cannabis products containing THC"
sheet="Table 1"
table=summaryTableIncStudiesMED
Info_text=""
# Run functions
# Run functions
createSheet(meta_out, sheet, title_name, table, Info_text)


# ================= TABLE =========================
# create supplementary table (individual study estimates)
MetaRatesDF$symptom=do.call(rbind, strsplit(MetaRatesDF$outcome, split = "[.]"))[,2]

ratesIndividualSupplement=data.frame(ref=paste0(MetaRatesDF$author, " et al. (", year=MetaRatesDF$year,")"), design= MetaRatesDF$StudyTypeCoded, symptom=MetaRatesDF$symptom,age=MetaRatesDF$AgeMean, n=round(MetaRatesDF$n,0), severity=MetaRatesDF$Severity, rate=paste0(round(MetaRatesDF$est*100, 2) , " (", round(MetaRatesDF$est_lowerCI*100,2), "; ", round(MetaRatesDF$est_upperCI*100,2) , ")"), refFull= MetaRatesDF$Reference)


ratesIndividualSupplement$severity=revalue(ratesIndividualSupplement$severity,
               c( "full_episode" = "Full episode",
                  "AE" = "Adverse event",
                  "psych_like" = 'Psychosis-like'))

ratesIndividualSupplement$design=revalue(ratesIndividualSupplement$design,
                                           c( "experiment" = "Experiment",
                                              "medical" = "Medicinal cannabis study",
                                              "observational" = 'Observational study'))


ratesIndividualSupplement$symptom=revalue(ratesIndividualSupplement$symptom,
                                         c( "DEL" = "Cannabis-associated delusions",
                                            "HAL" = "Cannabis-associated hallucinations",
                                            "PAR" = 'Cannabis-associated paranoia',
                                            "PLE" = 'Cannabis-associated psychosis-like symptoms'))


colnames(ratesIndividualSupplement)=c("Study authors", "Study design" , "Symptom profile of CAPS", "Mean age of study participants", "Sample size","Severity of CAPS", "% rate CAPS (95% Confidennce Interval)", "Reference (full)")

# Add parameters
title_name="Table 2. Overview of individual studies assessing rates of cannabis-associated psychotic symptoms"
sheet="Table 2"
table=ratesIndividualSupplement
Info_text=""
# Run functions
createSheet(meta_out, sheet, title_name, table, Info_text)


# ================= TABLE =========================
# create supplementary table (pooled estimates)
MetaRatesEventClean$pvalMetaRate=2*pnorm( (MetaRatesEventClean$est/MetaRatesEventClean$se), lower.tail = F )

MetaRatesEventClean$I2coded=ifelse(is.na(MetaRatesEventClean$I2)==T, "0%", paste0(round(MetaRatesEventClean$I2, 2),"%"))
MetaRatesEvenSupplement=data.frame(design=MetaRatesEventClean$drug, symp=MetaRatesEventClean$symp_clean, rate=paste0(round(MetaRatesEventClean$est, 2) , " (", round(MetaRatesEventClean$est_lowerCI,2), "; ", round(MetaRatesEventClean$est_upperCI,2) , ")"), I2=MetaRatesEventClean$I2coded, Qtest_p=MetaRatesEventClean$Qtest_p, n=paste0(MetaRatesEventClean$n, " (k=",MetaRatesEventClean$independentStudies_n, ")"))
MetaRatesEvenSupplement$design=revalue(MetaRatesEvenSupplement$design,
                                       c("Can" = "Observational studies",
                                         "THC" = "Experiments",
                                         "Med" = "Medicinal cannabis studies"))

colnames(MetaRatesEvenSupplement)=c("Study design", "Symptom profile of CAPS", "% rate CAPS (95% Confidennce Interval)", "I2", "p-value (Q-test for heterogeneity)" ,"Pooled sample size (k number of non-overlapping samples)")

# Add parameters
title_name="Table 3. Summary of pooled rates of cannabis-associated psychotic symptoms"
sheet="Table 3"
table=MetaRatesEvenSupplement
Info_text=""
# Run functions
# Run functions
createSheet(meta_out, sheet, title_name, table, Info_text)


# ================= TABLE =========================
datMeta$pubRef=paste0(datMeta$Cohort, " et al. (", datMeta$YearPublished, ")")
datMeta$Reference
datMeta$dSumCoded=paste0(round(datMeta$d, 2), " (", round(datMeta$d_lowerCI, 2), "; ", round(datMeta$d_upperCI, 2), ")")

datMeta$OutcomeDesign=ifelse(grepl("Med", datMeta$OutcomeDefinitionLabel)==TRUE, "Medicinal cannabis study", NA)
datMeta$OutcomeDesign=ifelse(grepl("THC", datMeta$OutcomeDefinitionLabel)==TRUE, "Experiment", datMeta$OutcomeDesign)
datMeta$OutcomeDesign=ifelse(grepl("Can", datMeta$OutcomeDefinitionLabel)==TRUE, "Observational study", datMeta$OutcomeDesign)

datMeta$OutcomeSymp=ifelse(grepl("DEL", datMeta$OutcomeDefinitionLabel)==TRUE, "Cannabis-associated delusions", NA)
datMeta$OutcomeSymp=ifelse(grepl("HAL", datMeta$OutcomeDefinitionLabel)==TRUE, "Cannabis-associated hallucinations", datMeta$OutcomeSymp)
datMeta$OutcomeSymp=ifelse(grepl("PAR", datMeta$OutcomeDefinitionLabel)==TRUE, "Cannabis-associated paranoia", datMeta$OutcomeSymp)
datMeta$OutcomeSymp=ifelse(grepl("PLE", datMeta$OutcomeDefinitionLabel)==TRUE, "Cannabis-associated psychosis-like symptoms", datMeta$OutcomeSymp)

datMetaIndividual=subset(datMeta, select=c(pubRef, Reference, OutcomeDesign, Country, AgeMean, OutcomeSymp, PredictorCoded, dSumCoded, N_total, Predictor_clean))
datMetaIndividual$PredictorCoded=redocePre(datMetaIndividual$PredictorCoded)
datMetaIndividual$PredictorCoded=ifelse(datMetaIndividual$PredictorCoded=="Other", paste0(datMetaIndividual$PredictorCoded, " (", datMetaIndividual$Predictor_clean, ")"), datMetaIndividual$PredictorCoded)
datMetaIndividual$Predictor_clean=NULL

colnames(datMetaIndividual)=c("Author", "Reference", "Study design", "Country", "Age (mean)", "Definition of CAPS", "Assessed predictor", "Cohen's d (95% Confidence Interval)", "Number of included individuals")
# Add parameters
title_name="Table 4. Oveview of studies assessing predictors of cannabis-associated psychotic symptoms"
sheet="Table 4"
table=datMetaIndividual
Info_text=""
# Run functions
# Run functions
createSheet(meta_out, sheet, title_name, table, Info_text)




# ================= TABLE =========================
# Meta-models: predictors of CAPS
# Cohen column
PredictorMetaAll_combined$Cohen=paste0(round(PredictorMetaAll_combined$d,2), " (", round(PredictorMetaAll_combined$d_lowerCI,2), "; ", round(PredictorMetaAll_combined$d_upperCI,2), ")")
# Heterogeneity test column
PredictorMetaAll_combined$Qtest=paste0(round(PredictorMetaAll_combined$I2,2), "% (", formatC(PredictorMetaAll_combined$Qtest_p, 2), ")")
# Add NA's for estimates derived from aggregation
PredictorMetaAll_combined$Qtest=ifelse(PredictorMetaAll_combined$model=="Aggregated", "", PredictorMetaAll_combined$Qtest)
# Dervie new dataframe
metaTable=subset(PredictorMetaAll_combined, select = c(PredictorBroadRecoded, Predictor,Cohen, model, Qtest, StudyCount, n))
metaTable$PredictorBroadRecoded=metaTable$PredictorBroadRecoded %>% str_replace("\n", "")
colnames(metaTable)=c("Category","Factor", "Pooled Cohen's d", "Model", "Heterogeneity test (I2, p-value)", "Number of included estimates", "Number of included (non-overlapping) individuals")
# adjustedEffect, moderatorEffect
# paste0("Pooled Cohen's d (95% Confidennce Interval) estimated for mean year of publication (",yearAllmean,")"), "Moderator effect (year of publication)" 

# Add parameters
title_name="Table 5. Summary of meta-analytical models assessing predictors of cannabis-associated psychotic symptoms"
sheet="Table 5"
table=metaTable
Info_text=""
# Run functions
# Run functions
createSheet(meta_out, sheet, title_name, table, Info_text)






# ======================== Export output
# ++++++++++++++++++ WRITE TABLE ++++++++++++++++++++++++++++++++++++

# Adjust specific tables
mergeCells(meta_out,sheet = names(meta_out)[1], cols = 2:9, rows = RowSubheaderStart:RowSubheaderEnds)
mergeCells(meta_out,sheet = names(meta_out)[2], cols = 2:9, rows = RowSubheaderStart:RowSubheaderEnds)
mergeCells(meta_out,sheet = names(meta_out)[3], cols = 2:7, rows = RowSubheaderStart:RowSubheaderEnds)
mergeCells(meta_out,sheet = names(meta_out)[4], cols = 2:8, rows = RowSubheaderStart:RowSubheaderEnds)
mergeCells(meta_out,sheet = names(meta_out)[5], cols = 2:6, rows = RowSubheaderStart:RowSubheaderEnds)
#mergeCells(meta_out,sheet = names(meta_out)[6], cols = 2:6, rows = RowSubheaderStart:RowSubheaderEnds)
#mergeCells(meta_out,sheet = names(meta_out)[7], cols = 2:6, rows = RowSubheaderStart:RowSubheaderEnds)
#mergeCells(meta_out,sheet = names(meta_out)[8], cols = 2:6, rows = RowSubheaderStart:RowSubheaderEnds)
#mergeCells(meta_out,sheet = names(meta_out)[9], cols = 2:6, rows = RowSubheaderStart:RowSubheaderEnds)
#mergeCells(meta_out,sheet = names(meta_out)[10], cols = 2:6, rows = RowSubheaderStart:RowSubheaderEnds)

# Create new styles
s <- createStyle(fgFill = "#FFFFFF")
h_info <- createStyle(halign = "left",
                      border = "BOTTOM", fontColour = "black", fgFill = "white", fontSize=16, textDecoration = "Bold", numFmt="TEXT", borderColour = "black")
info_info <- createStyle(halign = "left",
                         border = NULL, fontColour = "black", fgFill = "white", fontSize=14, textDecoration = NULL, numFmt="TEXT", wrapText=TRUE)

# Run loop
for(curr_sheet in names(meta_out)){
  addStyle(meta_out,sheet = curr_sheet, s, cols=1:40, rows=1:200, gridExpand = TRUE)
  setColWidths(meta_out, sheet = curr_sheet, cols=1:40, widths = 20)
  
  addStyle(meta_out,sheet = curr_sheet, h_info, cols=ColStart:20, rows=RowHeader, gridExpand = TRUE)
  addStyle(meta_out,sheet = curr_sheet, info_info, cols=ColStart:5, rows=RowSubheaderStart, gridExpand = TRUE)
  
}


setwd(paste0(HOME,"/results/tables"))
openxlsx::saveWorkbook(meta_out, "metaCAPS_Sep2023.xlsx", overwrite = TRUE)
# Open File
openXL(meta_out)



