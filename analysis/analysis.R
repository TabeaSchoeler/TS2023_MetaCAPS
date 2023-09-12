
rm(list = ls())
.libPaths(new=c("/Users/tabea/Dropbox/progs/R/library"))
system('R_LIBS=/Users/tabea/Dropbox/progs/R/library')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
HOME=getwd()

# ========= Load functions =================
source(paste0(HOME, "/analysis/functions.R"))

# ========= Run Meta-analysis ==============
source(paste0(HOME, "/analysis/runMeta.R"))
# text for publication
pvalueCompare
textFlowChart
totalNinludedText
textMedCanRates
textAgeDistribution
IncludedPredictors
pvalueCompare


# ========= GWAS on schizophrenia ==============
schiz=fread(paste0(HOME, "/data/schz.vcf.tsv.gz"))
head(schiz)
snps=c("rs4680", "rs2494732", "rs1130233", "rs806379", "rs1535255", "rs2023239", "rs1049353", "rs12720071", "rs6265", "rs324420")
schizsel=subset(schiz, ID %in% snps, select=c(ID, A1 ,A2, BETA, SE, PVAL))
schizsel$PVAL=formatC(schizsel$PVAL, 2)
