

#=====================================================================================================================
#======= Read in the data ============================================================================================
#=====================================================================================================================
# Read in R script that contains all extracted data
source(paste0(HOME, "/analysis/ExtractedDataMeta.R"))


# Read in corresponding excel sheet
datMeta_all=read.csv(paste0(HOME, "/data/Data_extraction_June_2023.csv"),
                     header=T,
                     na.strings=c("","NA"))

datExtracted=subset(datMeta_all, IncludeStudy=="Yes")

yearAll=as.data.frame(datExtracted %>%
                        group_by(ReferenceCoded) %>%
                        summarise_at(vars(YearPublished), list(year = mean)))
yearAllmean=round(mean(yearAll$year),0)



######################################################################
## ==================== Literature search ========================= ###
######################################################################

# ======== Generate PRISMA FLOW DIAGRAM ===================
#library(PRISMAstatement)
#library(svglite)
noStud=readRDS(paste0(HOME,"/output/searchIdentified.rds"))
totNumberIdentified=sum(noStud$nIdentified)
litSearch=read.csv(paste0(HOME,"/data/search12June2023.csv"))
totNumberStudies=NROW(litSearch)
s2Included=NROW(subset(litSearch, Stage1_screening=="yes"))
s3Included=NROW(subset(litSearch, Stage2_screening=="yes"))

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


######################################################################
## ==================== Publication plot ========================= ###
######################################################################
# ===== Number if included studies and (independent participants)
library(doBy)
dataListall=list()


for ( i in 1:NROW(datExtracted) ) {
  print(datExtracted$Rcode[i])
  readDF=eval(parse(text=as.character(datExtracted$metaP_code[i])))
  
  if(NCOL(readDF)<2){
    readDF=eval(parse(text=as.character(datExtracted$Rcode[i])))
    readDF$Ntot=readDF$n
  }
  dfOut=data.frame(ID=datExtracted$ID[i], nestingVar=datExtracted$ReferenceCoded[i], n=readDF$Ntot, design=datExtracted$StudyType[i])
  dataListall[[i]]=dfOut
}

dataAll=do.call(rbind, dataListall)
levels(as.factor(datExtracted$StudyType))

nAllTHC=summaryBy(n~nestingVar, data = subset(dataAll, design=="Experiment"), FUN = max)
nAllMED=summaryBy(n~nestingVar, data = subset(dataAll, design=="MedicalSample" | design=="MedicalTrial"), FUN = max)
nAllOBS=summaryBy(n~nestingVar, data = subset(dataAll, design=="Observational"), FUN = max)
nAllQE=summaryBy(n~nestingVar, data = subset(dataAll, design=="QuasiExperimental"), FUN = max)


textPubSum=paste0("Included participants \nObservational:", sum(nAllOBS$n.max), " \nExperimental:", sum(nAllTHC$n.max), " \nQuasi-Experimental:", sum(nAllQE$n.max), " \nMedical:", sum(nAllMED$n.max))
totalNinluded=sum(nAllOBS$n.max)+sum(nAllTHC$n.max)+sum(nAllQE$n.max)+sum(nAllMED$n.max)
totalNinludedText=paste0("Out all particopants included (n=", totalNinluded, ") most came from observational research (n=",sum(nAllOBS$n.max),"; ",round(sum(nAllOBS$n.max)/totalNinluded*100,2),"%), followed by studies assessing medical cannabis products (n=",sum(nAllMED$n.max),"; ",round(sum(nAllMED$n.max)/totalNinluded*100,2),"%), experimental studies administering THC (n=",sum(nAllTHC$n.max),"; ",round(sum(nAllTHC$n.max)/totalNinluded*100,2),"%) and Quasi-experimental studies (n=",sum(nAllQE$n.max),"; ",round(sum(nAllQE$n.max)/totalNinluded*100,2),"%)" )

# ================ Year of publication
datExtracted$StudyTypeCoded=revalue(datExtracted$StudyType,
                                    c("MedicalTrial" = paste0("Medicinal cannabis (n=",  sum(nAllMED$n.max), ")"),
                                      "MedicalSample" =   paste0("Medicinal cannabis (n=",  sum(nAllMED$n.max), ")"),
                                      "Experiment" =  paste0("Experimental (n=",  sum(nAllTHC$n.max), ")"),
                                      "Observational"=  paste0("Observational (n=",  sum(nAllOBS$n.max), ")"),
                                      "QuasiExperimental" =  paste0("Quasi-Experimental (n=",  sum(nAllQE$n.max), ")")))

datExtractedUnique=datExtracted[!duplicated(datExtracted$Reference), ]
YearSum <- as.data.frame(datExtractedUnique %>% dplyr::group_by(YearPublished, StudyTypeCoded) %>% 
                           dplyr::summarise(total_count=dplyr::n(),
                                            .groups = 'drop') )


yearSearch=ggplot(data=YearSum, aes(x=YearPublished, y=total_count, fill=StudyTypeCoded)) +
  geom_bar(stat="identity") +    
  theme_classic() + 
  theme(strip.background = element_rect( color="white", fill="white", size=3, linetype="solid" ),
        strip.text.x = element_text(size = 15),
        legend.position="top") + scale_fill_manual("",values=c("#994455","#EE99AA", "#6699CC", "#009988")) +
  labs(title="",
       x ="Year of publication", y = "Number of included studies") + guides(fill=guide_legend(nrow=2,byrow=TRUE))

print(yearSearch)
ggsave(file=paste0(HOME,"/results/figures/yearSearch.pdf"), plot = yearSearch, width = 15, height = 12, units = "cm")


table(YearSum$StudyTypeCoded)


#=====================================================================================================================
#======= Estimate pooled rates =======================================================================================
#=====================================================================================================================
library("DescTools")
datMetaRates=subset(datMeta_all, IncludeStudy=="Yes" & effectType == "rates")

# ================== Compare Treatment verus Placebo rates ====================
# run for placebo group
datMetaRatesControl=subset(datMeta_all, is.na(ratesControl)==FALSE)
datMetaRatesControl$Rcode=datMetaRatesControl$ratesControl
datMetaRatesControlSum=extractRates(df=datMetaRatesControl, return="sum")
datMetaRatesControlSum=subset(datMetaRatesControlSum, symp=="all")
datMetaRatesControlSum$arm="control"

# run for treatment group
datMetaRatesTreatment=subset(datMeta_all, is.na(ratesControl)==FALSE)
datMetaRatesTreatmentSum=extractRates(df=datMetaRatesTreatment, return="sum")
datMetaRatesTreatmentSum=subset(datMetaRatesTreatmentSum, symp=="all")
datMetaRatesTreatmentSum$arm="cases"

# combine
compareTreatControl=rbind(datMetaRatesTreatmentSum, datMetaRatesControlSum)
compareTreatControl$se=(compareTreatControl$est_upperCI-compareTreatControl$est_lowerCI)/3.92
compareTreatControl$est=compareTreatControl$est*100
compareTreatControl$est_lowerCI=compareTreatControl$est_lowerCI*100
compareTreatControl$est_upperCI=compareTreatControl$est_upperCI*100
compareTreatControl$text=paste0(round(compareTreatControl$est,1) , "% (", round(compareTreatControl$est_lowerCI,1) , "%-", round(compareTreatControl$est_upperCI,1), "%); n=", compareTreatControl$n, " (k=", compareTreatControl$nStudiesIncluded, ")")
compareTreatControl$symp_clean=paste0(compareTreatControl$symp, "_", compareTreatControl$arm)

compareTreatControl$symp_clean=revalue(compareTreatControl$symp_clean,
                                   c("all_cases" = "Combined symptoms",
                                     "all_control" = "Combined symptoms (controls)"))
compareTreatControl$symp_clean <- factor(compareTreatControl$symp_clean, levels = c("Combined symptoms", "Combined symptoms (controls)"))


compareTreatControl$classGroupClean=revalue(compareTreatControl$drug,
                                        c( "THC" = "Experimental studies",
                                          "Med" = "Medicinal cannabis studies"))
compareTreatControl$classGroupClean <- factor(compareTreatControl$classGroupClean, levels = c("Experimental studies", "Medicinal cannabis studies"))
compareTreatControl$colLabel=paste0(compareTreatControl$severity, "_", compareTreatControl$symp)


MetaRatesComparePlacebo= 
  ggplot( data=compareTreatControl, aes(x=fct_rev(factor(symp_clean)),
                                         y=est,
                                         ymin=est_lowerCI,
                                         ymax=est_upperCI ,
                                         color =  factor(symp_clean))) +
   geom_pointrange() +
  
  coord_flip() +
  geom_hline(yintercept=0, lty=2,  color="grey") +
  labs(title = "", x = "", y = "", color = c(""))  +   scale_shape_discrete(name  ="") +
  scale_y_continuous(breaks=seq(0, 100, by = 20)) +
 # ylim(0, 55) +
  facet_grid(. ~ classGroupClean, switch = "y") +
  themeCanSens + scale_color_manual(values=c("darkblue", "darkgrey") ) +
  geom_text(aes(label=text), position=position_nudge(x = -0.25, y=-40), y=12, vjust=0, size=4) +
  theme_classic() + 
  theme(strip.background = element_rect( color="white", fill="white", size=3, linetype="solid" ),
        strip.text.x = element_text(size = 15),
        legend.position="none") +
  labs(title="",
       x ="", y = "%")+
  geom_point( shape=19) +
  scale_size_continuous(range = c(2, 8))
MetaRatesComparePlacebo




# ================== Cross check with indendent data extraction ====================
library("DescTools")
datMetaRatesSelCheck=subset(datMeta_all, checkWikus=="yes" & effectType == "rates")
# run for cannabis-exposured (all) group
MetaRateCheckD=extractRates(df=datMetaRatesSelCheck, return="data")
saveRDS(MetaRateCheckD, paste0(HOME, "/output/MetaRateCheckD.rds"))
# get sum
MetaRateCheckS=extractRates(df=datMetaRatesSelCheck, return="sum")
saveRDS(MetaRateCheckS, paste0(HOME, "/output/MetaRateCheckS.rds"))

# ================== Get overall rates ====================
# run for cannabis-exposured (all) group
MetaRatesBindDFCases=extractRates(df=datMetaRates, return="sum")
MetaRatesBindDFCases$arm="cases"

# publication bias
plotPubBias=extractRates(df=datMetaRates, return="pubBias")
funnelPlotrates=cowplot::plot_grid(plotlist = plotPubBias, ncol = 1)
ggsave(paste0(HOME,"/results/figures/funnelPlotrates_comb.pdf"), funnelPlotrates,  width = 14, height = 27, units = "cm", limitsize = FALSE)

# Peters Test
MetaRatesBindDFall=MetaRatesBindDFCases

MetaRatesBindDFall$nStudiesIncluded=ifelse(is.na(MetaRatesBindDFall$nStudiesIncluded)==T, MetaRatesBindDFall$independentStudies_n, MetaRatesBindDFall$nStudiesIncluded)
MetaRatesBindDF=subset(MetaRatesBindDFall, independentStudies_n!=0)
MetaRatesBindDF$ID_sorted=seq(from=1, to=NROW(MetaRatesBindDF),1)
MetaRatesBindDF$se=(MetaRatesBindDF$est_upperCI-MetaRatesBindDF$est_lowerCI)/3.92
MetaRatesBindDF$est=MetaRatesBindDF$est*100
MetaRatesBindDF$est_lowerCI=MetaRatesBindDF$est_lowerCI*100
MetaRatesBindDF$est_upperCI=MetaRatesBindDF$est_upperCI*100
MetaRatesBindDF$text=paste0(round(MetaRatesBindDF$est,1) , "% (", round(MetaRatesBindDF$est_lowerCI,1) , "%-", round(MetaRatesBindDF$est_upperCI,1), "%); n=", MetaRatesBindDF$n, " (k=", MetaRatesBindDF$nStudiesIncluded, ")")


MetaRatesBindDF$symp_clean=paste0(MetaRatesBindDF$symp, "_", MetaRatesBindDF$arm)
levels(as.factor(MetaRatesBindDF$symp_clean))

MetaRatesBindDF$symp_clean=revalue(MetaRatesBindDF$symp_clean,
                                   c("HAL_cases" = "Hallucinations",
                                     "PAR_cases" = "Paranoia",
                                     "PLE_cases" = "Psychosis-like",
                                     "DEL_cases" = "Delusions",
                                     "all_cases" = "Combined symptoms",
                                     "all_control" = "Combined symptoms (controls)"))
MetaRatesBindDF$symp_clean <- factor(MetaRatesBindDF$symp_clean, levels = c("Psychosis-like", "Paranoia", "Hallucinations", "Delusions", "Combined symptoms", "Combined symptoms (controls)"))


MetaRatesBindDF$classGroupClean=revalue(MetaRatesBindDF$drug,
                                        c("Can" = "Observational studies",
                                          "THC" = "Experimental studies",
                                          "Med" = "Medicinal cannabis studies"))
MetaRatesBindDF$classGroupClean <- factor(MetaRatesBindDF$classGroupClean, levels = c("Observational studies", "Experimental studies", "Medicinal cannabis studies"))
MetaRatesBindDF$colLabel=paste0(MetaRatesBindDF$severity, "_", MetaRatesBindDF$symp)


# check reates for studies using BPRS
MetaRatesDF=extractRates(datMetaRates, return="data")
MetaRatesRating=subset(MetaRatesDF, OutcomeMeasureCoded=="PANSS" | OutcomeMeasureCoded=="BPRS")
MetaRatesRating=subset(MetaRatesRating, outcome=="THC.PLE")
MetaRatesRatingModel=getI2( df=MetaRatesRating, symp="all", model="all", severity="event", drug="THC")


# =========================== Plot results
library("ggrepel")
library("labeling")
MetaRatesEventClean=subset(MetaRatesBindDF, severity!="episode")
MetaRatesBindOrdered=MetaRatesEventClean %>%
  mutate(symp = fct_reorder(symp, as.numeric(-1*ID_sorted)))



colRates=c( "darkorchid4", "darkorchid4", "darkorchid4", "darkorchid4", "darkblue", "darkgrey") 



MetaRatesPlotGroup= 
  ggplot( data=MetaRatesBindOrdered, aes(x=fct_rev(factor(symp_clean)),
                                         y=est,
                                         ymin=est_lowerCI,
                                         ymax=est_upperCI ,
                                         color =  factor(symp_clean))) +
  geom_pointrange(position=position_dodge(width=c(1,0.4, 0.5, 1)), size=1, fatten = 2, lineend="round", shape=23) +
 # geom_pointrange() +
  
  coord_flip() +
  geom_hline(yintercept=0, lty=2,  color="grey") +
  #geom_errorbar(width=.2,position = position_dodge(width = 1/2) )   +
  labs(title = "", x = "", y = "", color = c(""))  +   scale_shape_discrete(name  ="") +
  scale_y_continuous(breaks=seq(0, 100, by = 20)) +
  ylim(0, 55) +
  facet_grid(. ~ classGroupClean, switch = "y") +
  themeCanSens + scale_color_manual(values=colRates ) +
  #geom_text(aes(label=text), position=position_nudge(x = -.1), vjust=0, y=60, size=3) +
  geom_text(aes(label=text), position=position_nudge(x = -0.25), y=30, vjust=0, size=4) +
  theme_classic() + 
  theme(strip.background = element_rect( color="white", fill="white", size=3, linetype="solid" ),
        strip.text.x = element_text(size = 15),
        legend.position="none") +
  labs(title="",
       x ="", y = "%")+
  #geom_point(aes(size = 1/se^2), shape=18) +
  geom_point( shape=19) +
  scale_size_continuous(range = c(2, 8))
MetaRatesPlotGroup


ggsave(paste0(HOME,"/results/figures/MetaRatesPlotGroup.pdf"), MetaRatesPlotGroup,  width = 29, height = 17, units = "cm", limitsize = FALSE)


MetaRatesDF$StudyTypeCoded=revalue(MetaRatesDF$StudyType,
                                   c("MedicalTrial" = "medical",
                                     "MedicalSample" = "medical",
                                     "Experiment" = "experiment",
                                     "Observational"= "observational"))
print("Individual plots per study design")
ratedPlotIndividual(dfindi=MetaRatesDF, dfpool=MetaRatesBindDF, model1="medical", model2="Med")
ratedPlotIndividual(dfindi=MetaRatesDF, dfpool=MetaRatesBindDF, model1="observational", model2="Can")
ratedPlotIndividual(dfindi=MetaRatesDF, dfpool=MetaRatesBindDF, model1="experiment", model2="THC")


print("=============== FULL Pychotic Episode =================")
print("plot individual studies for those reporting on full episodes")
MetaRatesEpisode=subset(MetaRatesDF, Severity=="full_episode")
MetaRatesEpisode$labelEpisode=paste0(MetaRatesEpisode$author, " (",MetaRatesEpisode$Year,") \n(", MetaRatesEpisode$StudyTypeCoded, ", n=", MetaRatesEpisode$n, ")")
MetaRatesEpisode$model="individualStudy"
MetaRatesEpisode=MetaRatesEpisode %>%
  mutate(labelEpisode = fct_reorder(labelEpisode, as.numeric(-1*est)))
MetaRatesEpisode$ID=seq(1,NROW(MetaRatesEpisode), 1)


print("Add pooled estimate (full episode")
metaModelEpisode=as.data.frame(matrix(ncol=NCOL(MetaRatesEpisode), nrow=1))
colnames(metaModelEpisode)=colnames(MetaRatesEpisode)
resSubpsychEpisodeHet=getI2( df=MetaRatesEpisode, symp="combined", model="combined", severity="episode", drug="all")
metaModelEpisode$est=resSubpsychEpisodeHet$est
metaModelEpisode$est_lowerCI=resSubpsychEpisodeHet$est_lowerCI
metaModelEpisode$est_upperCI=resSubpsychEpisodeHet$est_upperCI
metaModelEpisode$labelEpisode=paste0("Pooled estimate (k=" , resSubpsychEpisodeHet$independentStudies_n, "; n=", resSubpsychEpisodeHet$n, ")" )
metaModelEpisode$model="pooledEstimate"
metaModelEpisode$ID=NROW(MetaRatesEpisode)+1


print("check when exlcuding outlier (1970) - favrat")
exlcusionFavrat=getI2( df=subset(MetaRatesDF, Severity == "full_episode" & author!="Favrat"), symp="combined", model="combined", severity="episode", drug="all")


# Combine
MetaRatesEpisodeComb=rbind(MetaRatesEpisode, metaModelEpisode)
MetaRatesEpisodeComb$est=MetaRatesEpisodeComb$est*100
MetaRatesEpisodeComb$est_lowerCI=MetaRatesEpisodeComb$est_lowerCI*100
MetaRatesEpisodeComb$est_upperCI=MetaRatesEpisodeComb$est_upperCI*100
MetaRatesEpisodeComb$text=paste0(round(MetaRatesEpisodeComb$est,2) , "% (", round(MetaRatesEpisodeComb$est_lowerCI,2) , "%-", round(MetaRatesEpisodeComb$est_upperCI,2), "%)")
MetaRatesEpisodeComb$text=ifelse(MetaRatesEpisodeComb$model=="pooledEstimate", paste0(MetaRatesEpisodeComb$text, "\n(I2=", round(resSubpsychEpisodeHet$I2, 2), "%; p(I2)=", resSubpsychEpisodeHet$Qtest_p, ")"), MetaRatesEpisodeComb$text)

MetaRatesEpisodeComb$est_upperCITrunc=ifelse(MetaRatesEpisodeComb$est_upperCI>26, 26, MetaRatesEpisodeComb$est_upperCI)
maxUpper=max(MetaRatesEpisodeComb$est_upperCITrunc)+1

# correct reordering
MetaRatesEpisodeComb$method.source <- fct_reorder(MetaRatesEpisodeComb$labelEpisode,  MetaRatesEpisodeComb$ID)
MetaRatesEpisodeComb$se= (MetaRatesEpisodeComb$est_upperCI- MetaRatesEpisodeComb$est_lowerCI ) / 3.92

MetaRatesEpisodeOrdered=MetaRatesEpisodeComb %>%
  mutate(labelEpisode = fct_reorder(MetaRatesEpisodeComb$labelEpisode, -ID))

MetaRatesEpisodePlot=   ggplot(data= MetaRatesEpisodeOrdered, 
                               aes(x=labelEpisode,
                                   y=est,
                                   ymin=est_lowerCI,
                                   ymax=est_upperCITrunc ,
                                   color =  factor(model) )) +
  geom_pointrange(position = position_dodge(width=1/2), size=1, fatten = 2, lineend="round" ) +
  coord_flip() +
  geom_hline(yintercept=0, lty=2,  color="grey") +
  labs(title = "", x = "", y = "", color = c(""))  +   
  scale_shape_discrete(name  ="") +
  scale_y_continuous(breaks=seq(0, 100, by = 20)) +
  geom_text(aes(label=text), position=position_nudge(x = 0.05), y=15, vjust=0, size=4) +
  ylim(0, maxUpper) +
  themeCanSens + scale_color_manual(values=c("darkslateblue", "dodgerblue1")) +
  theme_classic() + 
  theme(strip.background = element_rect( color="white", fill="white", size=3, linetype="solid" ),
        strip.text.x = element_text(size = 15),
        legend.position="none") +
  labs(title="",
       x ="", y = "%")

print(MetaRatesEpisodePlot)
#ggsave(paste0(HOME,"/results/figures/MetaRatesEpisodePlot.pdf"), MetaRatesEpisodePlot,  width = 15, height = 15, units = "cm", limitsize = FALSE)


# combine both plots
MetaRatesPLBEpisode=ggarrange(MetaRatesComparePlacebo, 
                              MetaRatesEpisodePlot, 
                              nrow=2, 
                              align="h",
                              heights = c(0.5,1), 
                              font.label=list(color="black",size=14),
                              labels=c("A.", 
                                       "B."))
ggsave(paste0(HOME,"/results/figures/MetaRatesPLBEpisode.pdf"), MetaRatesPLBEpisode,  width = 22, height = 23, units = "cm", limitsize = FALSE)




# ================================================ #
# ===== extract Text for rates of CAPD =========== #
# ================================================ #

# Conditions treated with medical cannabis products
medStudies=subset(MetaRatesDF, StudyTypeCoded=="medical")
freqTableMed=table(medStudies$Sample)
propTableMed=prop.table(freqTableMed)

# Get independent N
nMedPerConditionCohort=summaryBy(n~nestingVar+Sample, data = medStudies, FUN = max)
nMedPerCondition=summaryBy(n.max~Sample, data = nMedPerConditionCohort, FUN = sum)
# Summary table
summaryTableIncStudiesMED=data.frame(Sample=as.data.frame(freqTableMed)$Var1,
                                     NStudies=as.data.frame(freqTableMed)$Freq,
                                     PercStudies=round(as.data.frame(propTableMed)$Freq,4)*100,
                                     n_tot= nMedPerCondition$n.max.sum)

summaryTableIncStudiesMEDordered <- summaryTableIncStudiesMED[order(-summaryTableIncStudiesMED$N, summaryTableIncStudiesMED$n_tot),]
summaryTableIncStudiesMED_formatted=summaryTableIncStudiesMEDordered
colnames(summaryTableIncStudiesMED_formatted)=c("Medical condition treated with medical cannabis", "Number of included studies per medical condition", "Percentage of studies per medical condition", "Total number of patients assessed")


# ========== Total number of individuals with data on rates
nAllConditionCohort=summaryBy(n~nestingVar, data = MetaRatesDF, FUN = max)
# THC studies
thcStudies=subset(MetaRatesDF, StudyTypeCoded=="experiment")
# Get independent N
nTHCPerConditionCohort=summaryBy(n~nestingVar, data = thcStudies, FUN = max)
meanAgeTHC=summaryBy(AgeMean~nestingVar, data = thcStudies, FUN = mean, rm.na=NA)$AgeMean.mean

# Observational studies
CanStudies=subset(MetaRatesDF, StudyTypeCoded=="observational")
# Get independent N
nCanPerConditionCohort=summaryBy(n~nestingVar, data = CanStudies, FUN = max)
meanAgeObs=summaryBy(AgeMean~nestingVar, data = CanStudies, FUN = mean, rm.na=NA)$AgeMean.mean

# Medical trials
MedStudies=subset(MetaRatesDF, StudyTypeCoded=="medical")
nMedPerConditionCohort=summaryBy(n~nestingVar, data = MedStudies, FUN = max)
meanAgeMed=summaryBy(AgeMean~nestingVar, data = MedStudies, FUN = mean, rm.na=NA)$AgeMean.mean


# text for manuscript
textMedCanRates=paste0("A total of ", length(unique(MetaRatesDF$Reference)), " studies published between ", min(MetaRatesDF$Year), " and ", max(MetaRatesDF$Year), " reported on rates of CAPS and were included in the analysis. In total, ", sum(nAllConditionCohort$n.max), " independent individuals were assessed in the included studies. We extracted data from ", length(unique(CanStudies$uniqueID)), " observational studies assessing  ",sum(nCanPerConditionCohort$n.max), " cannabis users. Of experimental studies administering THC, included were ", length(thcStudies$uniqueID), " studies, comprising ", length(unique(thcStudies$nestingVar)), " independent samples and a total sample size of ", sum(nTHCPerConditionCohort$n.max), " individuals. Finally, ", length(unique(MedStudies$uniqueID))," (n=", sum(nMedPerConditionCohort$n.max), ") studies assessed efficacy and tolerability of medical cannabis products containing THC. Of those, the most common condition treated with THC included ", tolower(summaryTableIncStudiesMEDordered$Sample[1]), " [k=" , summaryTableIncStudiesMEDordered$NStudies[1], " (", summaryTableIncStudiesMEDordered$PercStudies[1], "%)], followed by ", tolower(summaryTableIncStudiesMEDordered$Sample[2]), " [k=" , summaryTableIncStudiesMEDordered$NStudies[2], " (", summaryTableIncStudiesMEDordered$PercStudies[2], "%)]. A table with further study details, as well as the individual rates of CAPS per study is reported in sTable x-x")
textAgeDistribution=paste0("The age distributions of the included participants were similar in observational studies (mean age=", round(mean(meanAgeObs, na.rm=T),2), ", ranging from ", round(min(meanAgeObs, na.rm=T),2), " to ",  round(max(meanAgeObs, na.rm=T), 2), ") and experimental studies (mean age=", round(mean(meanAgeTHC, na.rm=T),2), ", ranging from ", round(min(meanAgeTHC, na.rm=T),2), " to ",  round(max(meanAgeTHC, na.rm=T), 2), "). Substantially older were individuals taking part in medical trails (mean age=", round(mean(meanAgeMed, na.rm=T),2), ", ranging from ", round(min(meanAgeMed, na.rm=T),2), " to ",  round(max(meanAgeMed, na.rm=T), 2), ")" )




#=====================================================================================================================
#======= Compute Cohen D  ============================================================================================
#=====================================================================================================================
datMeta=subset(datMeta_all, IncludeStudy=="Yes" & effectType == "cohen" & grepl("High", datMeta_all$OutcomeDefinitionLabel)==FALSE)
# Create empty vetcors
datMeta$p=NA
datMeta$p_N=NA
datMeta$d_raw=NA
datMeta$d_var=NA
datMeta$d_pval=NA
datMeta$N_total=NA
datMeta$d_lower_rawCI=NA
datMeta$d_upper_rawCI=NA


#Run loop to derive Cohen for all included studiesz
for ( i in 1:length(datMeta$Rcode) ) {
  print(paste0("Iteration D: ", i))
  datMeta$ID[i]
  print(datMeta$ReferenceCoded[i])
  datMeta$d_raw[i]=eval(parse(text=as.character(datMeta$Rcode[i])))$d
  datMeta$d_var[i]=eval(parse(text=as.character(datMeta$Rcode[i])))$var.d
  datMeta$d_pval[i]=eval(parse(text=as.character(datMeta$Rcode[i])))$pval.d
  datMeta$N_total[i]=eval(parse(text=as.character(datMeta$Rcode[i])))$N.total
  datMeta$d_lower_rawCI[i]=eval(parse(text=as.character(datMeta$Rcode[i])))$l.d
  datMeta$d_upper_rawCI[i]=eval(parse(text=as.character(datMeta$Rcode[i])))$u.d
}
datMeta$d_var=ifelse(  datMeta$d_var==0, 0.000000000000001,   datMeta$d_var)


# Recode p-values
for ( i in 1:length(datMeta$Rcode) ) {
  if ( ((datMeta$metaP_code[i])=="notReported")==FALSE) {
    print(datMeta$ReferenceCoded[i])
    print("P value reported in study")
    datMeta$p[i]=eval(parse(text=as.character(datMeta$metaP_code[i])))$pEstimate
    datMeta$p_N[i]=eval(parse(text=as.character(datMeta$metaP_code[i])))$Ntot
  } else {
    datMeta$p[i]=NA
    datMeta$p_N[i]=datMeta$N_total[i]
  }
}


# Extract first name from reference
datMeta$RefCleaned=unlist(lapply(strsplit(datMeta$Reference, ","), '[[', 1))




#==========================================================
#======= P-value check            =========================
#==========================================================

# Get correlation between original study p-values and p-values estimated for Cohen D
corPestimates_Pstudy=cor.test(datMeta$p, datMeta$d_pva)

# Dichotomise p-value variables
# => Original p-value
datMeta$p_dich=NA
datMeta$p_dich[datMeta$p<0.05]="sig"
datMeta$p_dich[datMeta$p>=0.05]="ns"
# => Cohen d p-value
datMeta$d_p_dich=NA
datMeta$d_p_dich[datMeta$d_pva<0.05]="sig"
datMeta$d_p_dich[datMeta$d_pva>=0.05]="ns"
# Check consistency
compareP=as.data.frame(CrossTable(datMeta$p_dich, datMeta$d_p_dich)$prop.row)
compareP$Perc=round(compareP$Freq*100, 2)

# Reverse d whereever coding is negative
datMeta$d=ifelse(datMeta$ReverseD=="yes", -1*datMeta$d_raw , datMeta$d_raw)
datMeta$d_upperCI=datMeta$d + (1.96 * sqrt(datMeta$d_var))
datMeta$d_lowerCI=datMeta$d - (1.96 * sqrt(datMeta$d_var))

# Upload data to be cross checked 
CohenCheckT=subset(datMeta, checkWikus=="yes" & effectType == "cohen" )
saveRDS(CohenCheckT, paste0(HOME, "/output/CohenCheckT.rds"))



# Check which studies have only p-values available
datMeta$p_dichLabel=NA
datMeta$p_dichLabel=ifelse(is.na(datMeta$p)==TRUE, "P_na" , "P_extracted")
# Extract p-value estimates from cohen D estimates
datMeta$p_combined=NULL
datMeta$p_combined=ifelse(datMeta$p_dichLabel=="P_extracted", as.numeric(datMeta$p) , as.numeric(datMeta$d_pval))



# ========================================================
# ++++++++++++++++++++  Meta analysis ++++++++++++++++++++
#=========================================================
# Include only studies for which d estimates exist
vectorPredAll=levels(droplevels(as.factor(datMeta$PredictorCoded)))

# ========== RUN LOOP for meta models ==========
# Create subgroup DFs
listAllPred=list()
for ( i in 1:length(vectorPredAll) ) {
  listAllPred[[i]]=subset(datMeta, PredictorCoded==as.character(vectorPredAll[i]))
}
names(listAllPred)=vectorPredAll
attributes(listAllPred)
str(listAllPred)


#==========================================================
# +++++++ meta-models  ++++++++++++++++++++++++++++++++++++
#==========================================================
#df=listAllPred[[53]]


# ============== Publication bias
listPubPlot=lapply(listAllPred, function(x)
  runMetaModel(df=x,
               cor=r_withinSubj,
               return="plot")
)
names(listPubPlot)=vectorPredAll
listPubPlotClean=listPubPlot[lapply(listPubPlot,length)>0] ## you can use sapply,rapply
funnelPlot=cowplot::plot_grid(plotlist = listPubPlotClean, ncol = 4)
ggsave(paste0(HOME,"/results/figures/funnelPlot.pdf"), funnelPlot,  width = 25, height = 20, units = "cm", limitsize = FALSE)


# ================= Quality assessment
listHetOut=lapply(listAllPred, function(x)
  runMetaModel(df=x,
               cor=r_withinSubj,
               return="het",
               specify="hetplot")
)
names(listHetOut)=vectorPredAll
listHetOutClean=listHetOut[lapply(listHetOut,length)>0] ## you can use sapply,rapply
listHet=chunkMake(names(listHetOutClean), round(length(names(listHetOutClean))/5,0) )

for ( i in 1:length(listHet) ) {
  hetPlot=cowplot::plot_grid(plotlist = lapply(listHet[[i]], function(x) listHetOutClean[[x]]), ncol = 2)
  ggsave(paste0(HOME,"/results/figures/hetPlot_",i,".pdf"), hetPlot,  width = 30, height = 50, units = "cm", limitsize = FALSE)
}


# get data for models excludin outlier
listHetDatOut=lapply(listAllPred, function(x)
  runMetaModel(df=x,
               cor=r_withinSubj,
               return="het",
               specify="hetdata")
)
listHetDatdf=do.call(rbind, listHetDatOut)

listOutliers=subset(listHetDatdf, nRemoved>0)
outlierText=paste0("Outliers were identified for ", NROW(listOutliers), " meta-analytical models. However, removing outliers from the models did not substantially alter the conclusions drawn from the models, as the corrected pooled estimates were similar to those including all effect size for  ", paste0(listOutliers$model, " (d=",round(listOutliers$d_all, 2), ", dcorr=", round(listOutliers$d_corr, 2), ")", collapse="; "))


palette <- rainbow(NROW(listHetDatdf))  
scatterHET=ggplot(aes(y=d_all, x=d_corr, color=as.factor(model)), data=listHetDatdf) +
  geom_abline(intercept = 0, slope = 1, color="black", linetype="solid", size=1) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 0.5) +
  geom_vline(xintercept=0, linetype="dashed", color = "grey", size = 0.5) +
  geom_point() +
  geom_errorbar(aes(ymin=d_lCI, ymax=d_uCI),height=0,  position=position_dodge(.9)) +
  geom_errorbarh(aes(xmin=d_corrLCI, xmax=d_corrUCI), width=0,  position=position_dodge(.9)) +
  #scale_colour_brewer(palette = "YlOrRd", direction = - 1) +
  #scale_color_brewer(palette = "Paired") +
  scale_color_manual(values=palette)  +
  scale_fill_discrete(name="") +
  theme_classic() +
  theme(legend.position = "top" , legend.title=element_blank())  +
  labs(x = expression(paste( italic(d[corrected]) )), 
       y = expression(paste( italic(d) ))   ) 


ggsave(paste0(HOME,"/results/figures/scatterHET.pdf"), scatterHET,  width = 20, height = 20, units = "cm", limitsize = FALSE)


# ============ Run meta-models over all lists
listMetaOut=lapply(listAllPred, function(x)
  runMetaModel(df=x,
               cor=r_withinSubj,
               return="data")
)
names(listMetaOut)=vectorPredAll

# Merge all lists
PredictorMetaAll=do.call(rbind,listMetaOut)
# Generate ticks that apply to all plots
minVal=round(min(PredictorMetaAll$d_lowerCI),0)
xMax=ceiling(max(PredictorMetaAll$d_upperCI))
limitsDefined=c(round(min(PredictorMetaAll$d_lowerCI),0), xMax)

# =================== PLOT RESUTS -  Cohen D ===========================
# ======= Plot only meta models
# Select data for plot
PredictorMetaAll_combined=sumPooled(PredictorMetaAll)
PredictorMetaAll_combined=subset(PredictorMetaAll_combined, StudyCount>1) # remove estimates that come from only a single study

# Add Q-test results
PredictorMetaAll_combined$Qtest=paste0(round(PredictorMetaAll_combined$I2,2), "% (p=", round(PredictorMetaAll_combined$Qtest_p, 2), ")")




# Generate plot
limitsDefinedAll=c(-1.5,4)
xMaxAll=ceiling(max(PredictorMetaAll_combined$d_upperCI))

PredictorMetaAll_combined$PredictorBroad
PredictorMetaAll_combined$PredictorBroad <- factor(PredictorMetaAll_combined$PredictorBroad, levels = c("CannabisDynamics", "CannabisHistory", "CannabisStrain", "Demographic", "MentalHealthPersonality", "Neurotransmission", "CandidateGene", "OtherDrugs"))

PredictorMetaAll_combined$PredictorBroadRecoded=revalue(PredictorMetaAll_combined$PredictorBroad,
                                                        c("CannabisDynamics" = "Pharmaco-\ndynamic",
                                                          "CannabisHistory" = "Cannabis \nhistory",
                                                          "CannabisStrain" = "Cannabis \nhistory",
                                                          "Demographic" = "Demographic",
                                                          "MentalHealthPersonality" = "Mental Health \n Personality",
                                                          "Neurotransmission" = "Neuro- \ntransmiss.",
                                                          "OtherDrugs" = "Other drug \nuse",
                                                          "CandidateGene" = "Candidate \ngene"))

levels(as.factor(PredictorMetaAll_combined$Predictor))
PredictorMetaAll_combined$Predictor=redocePre(PredictorMetaAll_combined$Predictor)



levels(as.factor(PredictorMetaAll_combined$StudyType))
PredictorMetaAll_combined$StudyType=revalue(PredictorMetaAll_combined$StudyType,
                                            c( "Experiment" = "Experiment (THC)",
                                               "MedicalTrial" = "Trial (medicinal cannabis)",
                                               "Observational" = "Observational study",
                                               "QuasiExperimental" = "Quasi-experiment",
                                               "mixed" = "Mixed study designs"))
PredictorMetaAll_combined$StudyType <- factor(PredictorMetaAll_combined$StudyType, levels = c("Experiment (THC)","Trial (medicinal cannabis)" ,"Quasi-experiment", "Mixed study designs", "Observational study"))
levels(PredictorMetaAll_combined$StudyType)



MetaPlot_out=generateSumPlot(df=PredictorMetaAll_combined, minVal=-1.3, limitsDefined=limitsDefinedAll, xMax=2) +
  facet_grid(rows = vars(PredictorBroadRecoded), scales = "free", space = "free") + theme_classic() + 
  scale_color_manual(values=c("#56B4E9","seagreen1" ,"cyan4", "forestgreen", "darkslategrey"))  +
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal',
        axis.title.y = element_text(face = "italic"),
        axis.title.x = element_text(face = "italic")) +
  labs(title="",
       x ="", y = "Cohen's d")



print(MetaPlot_out)

# Save output
ggsave(paste0(HOME,"/results/figures/MetaPlot.pdf"),
       MetaPlot_out,
       width = 23,
       height = 27,
       units = "cm",
       limitsize = T)

print(MetaPlot_out)

# Add legend
legendMeta=read.csv(paste0(HOME, "/data/legendVariables.csv"),
                    header=T,
                    na.strings=c("","NA"))


# ============ Run meta-models per study design
metaDat=do.call(rbind, listAllPred)
metaDat$PredictorDesign=paste0(metaDat$StudyType, "_", metaDat$PredictorCoded)

MetaDesignL=lapply(levels(as.factor(metaDat$PredictorDesign)), function(x)
  runMetaModel(df=metaDat,
               cor=r_withinSubj,
               return="data",
               designEffect=x)
)
MetaDesign=do.call(rbind, MetaDesignL)
MetaDesign$CategoryDesign=paste0(MetaDesign$StudyType, "_", MetaDesign$Category)
MetaDesign$PredictorCat=MetaDesign$Predictor
MetaDesign$Predictor=MetaDesign$CategoryDesign
MetaDesign_combined=sumPooled(df=MetaDesign, count="CategoryDesign")
countDesign=subset(as.data.frame(MetaDesign_combined %>% dplyr::count(PredictorCat)), n>1) # include only estimates if >1 different study design
MetaDesignSel=subset(MetaDesign_combined, PredictorCat %in% countDesign$PredictorCat)


MetaDesignSel$Predictor_info=paste0("k=", MetaDesignSel$StudyCount, "; n=",round(MetaDesignSel$n,0) )
MetaDesignSel$Qtest=paste0(round(MetaDesignSel$I2,2), "% (p=", formatC(MetaDesignSel$Qtest_p, 2), ")")
MetaDesignSel$Qtest=ifelse(is.na(MetaDesignSel$I2)==T, "", MetaDesignSel$Qtest)
MetaDesignSel$LabelSum=paste0(round(MetaDesignSel$d,2), " (", round(MetaDesignSel$d_lowerCI,2), "; ", round(MetaDesignSel$d_upperCI,2), ")" )

MetaDesignSel$StudyTypeCoded=revalue(MetaDesignSel$StudyType,
                                   c("MedicalTrial" = "Medicinal",
                                     "MedicalSample" = "Medicinal",
                                     "Experiment" = "Experiment",
                                     "Observational"= "Observational",
                                     "QuasiExperimental" = "Quasi-experimental"))

var="Age"

MetaDesignSel$col=revalue(MetaDesignSel$StudyType, c( "Experiment" = "#00AEDB",
                                    "Observational" = "darkred",
                                    "QuasiExperimental"="orange",
                                    "MedicalSample" = "darkgreen"))

legendDesign=as_ggplot(get_legend( ggplot(MetaDesignSel, aes(d, d, color = col, group=factor(StudyTypeCoded) )) +
  geom_point() +
  theme(legend.position = "top", legend.box = "horizontal") +
  scale_color_identity("",
                       guide = "legend", 
                       breaks = MetaDesignSel$col,
                       labels=MetaDesignSel$StudyTypeCoded)))


plotDesign=function(df, var, label, legend="none"){
  dfIn=subset(df, df[[label]]==var)
  
  
  plot <- ggplot(data = dfIn, aes(x = reorder(PredictorCat, d), y = d, ymin = d_lowerCI, ymax = d_upperCI, color = col, group = factor(StudyType) )) +
    geom_pointrange( lwd = 1/2, position = position_dodge(width = 1/2)) +
    geom_hline(yintercept = 0, lty = 1) +
    geom_errorbar( width = 0.2, position = position_dodge(width = 1/2)) +
    coord_flip() +
    labs(title = "", x = "", y = "Cohen's d") +
    themeCanSens +
    #scale_color_manual(values=c("#00AEDB","darkred" ,"orange"))  +
    #scale_color_manual(values=  dfIn$col) +
    scale_color_identity()+
    
    # facet_grid(rows = vars(PredictorBroad), scales = "free", space = "free") +
    theme_classic() +
    theme(legend.position = "none",
          legend.justification = 'left',
          legend.direction = 'horizontal',
          #axis.title.y = element_text(face = "italic"),
          axis.title.y = element_blank(),  # Remove x-axis title
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = margin(t=1, r=1, b=0, l=1, "cm"),
          axis.text.y = element_blank()) +
    #geom_text(aes(label = LabelSumQ), position = position_dodge(width = 0.5), vjust = -0.7, size = 3.5) +   
    labs(title = redocePre(var) )
  
  table=data.frame(StudyType=dfIn$StudyTypeCoded, n=dfIn$Predictor_info, label=dfIn$LabelSum, het= dfIn$Qtest)
  colnames(table)=c("Study design", "k estimates/sample size","Model (d, 95%CI)", "Heterogeneity")
  tablePLot <- tableGrob(table, theme=ttheme_minimal(base_size = 7,
                                                     core = list(padding=unit(c(1, 1), "mm"))),
                         rows = NULL)
  
  plotT=grid.arrange(plot, tablePLot, ncol=1, heights=c(3,1))
  
  print(plotT)
  return(plotT)
}


plotDesignL=lapply(levels(as.factor(MetaDesignSel$PredictorCat)), function(x) plotDesign(df=MetaDesignSel, label="PredictorCat", var=x))
names(plotDesignL)=levels(as.factor(MetaDesignSel$PredictorCat))
MetaPlotDesign=ggarrange(plotlist=plotDesignL, nrow = 3)


# Convert the arranged_plots to a grob object
MetaPlotDesignS <- arrangeGrob(grobs = MetaPlotDesign, nrow = 3)
MetaPlotDesignL=ggarrange(legendDesign, MetaPlotDesignS, nrow = 2, heights=c(1,80))

# Save output
ggsave(paste0(HOME,"/results/figures/MetaPlotDesign.pdf"),
       MetaPlotDesignL,
       width = 30,
       height = 50,
       units = "cm",
       limitsize = T)


names(plotDesignL)

MetaPlotDesignMain=
# Convert the arranged_plots to a grob object
nrowMain=1
MetaPlotDesignMain=ggarrange(plotlist=list(plotDesignL[["Age"]], plotDesignL[["Gender"]], plotDesignL[["Age onset"]],plotDesignL[["THCcontent"]]))
MetaPlotDesignMainL=ggarrange(legendDesign, MetaPlotDesignMain, nrow = 2, heights=c(1,40))

# Save output
ggsave(paste0(HOME,"/results/figures/MetaPlotDesignMain.pdf"),
       MetaPlotDesignMainL,
       width = 25,
       height = 20,
       units = "cm",
       limitsize = T)


# =================== PLOT RESUTS -  Cohen D (individual study estimates) ===========================
PredictorMetaAll_combined$ID=seq(1:length(PredictorMetaAll_combined$d))

PredictorMetaAll_combined$ModelInfo=paste0(redocePre(PredictorMetaAll_combined$Category),
                                           " \n d=",round(PredictorMetaAll_combined$d,2), " (", round(PredictorMetaAll_combined$d_lowerCI,2), "; ", round(PredictorMetaAll_combined$d_upperCI,2), ")",
                                           " \n N=", PredictorMetaAll_combined$n)
addInfo=data.frame(Category=PredictorMetaAll_combined$Category, ModelInfo=PredictorMetaAll_combined$ModelInfo )

PredictorMetaInfo=merge(PredictorMetaAll, addInfo, by="Category", all.x=T)
PredictorMetaInfo$ID=seq(1:length(PredictorMetaInfo$d))
# Create unique predictor
limitsDefined=c(round(min(PredictorMetaAll$d_lowerCI),0), xMax)
PredictorMetaInfoInc=subset(PredictorMetaInfo, is.na(ModelInfo)==F)

PredictorMetaInfoInc$model=revalue(PredictorMetaInfoInc$model,
                 c( "OriginalStudy" = "Individual study estimate (d)"))
                    




# ===== plot individual study estimates
# plit data into seven
listDim=chunkMake(levels(as.factor(PredictorMetaInfoInc$Category)), 7)
lapply(seq(1,length(listDim),1), function(x) plotSubForest(x))



# Estimates in text
# ===== Predictors of CAPS
length(unique(datMeta$ReferenceCoded))
length(levels(droplevels(as.factor(datMeta$PredictorCoded))))
library(doBy)
maxNperCohort=summaryBy(p_N~ReferenceCoded, data = datMeta, FUN = max)
IncludedPredictors=paste0("Included were ",length(unique(datMeta$Reference)), " studies published between ", min(datMeta$YearPublished), " and ", max(datMeta$YearPublished), ", corresponding to ",length(unique(datMeta$ReferenceCoded))," independent samples (n=", sum(maxNperCohort$p_N.max), "). In total, we extracted ", length(unique(datMeta$ID)), " Cohen's d that were grouped together and analzyed in ",length(droplevels(as.factor(PredictorMetaAll_combined$Predictor))), " meta-analytical models. A brief summary of the included samples is provided in sTable X.")

# comparison of p-values
pvalueCompare=paste0("Comparing the p-values corresponding to Cohen's d to the original p-values as reported by the studies revealed a high level of concordance (r=", round(corPestimates_Pstudy$estimate, 2), " p=", formatC(corPestimates_Pstudy$p.value, 2), ")")


####################################################################
# =============== Create Supplement Tables =========================
####################################################################

source(paste0(HOME, "/analysis/createTable.R"))






