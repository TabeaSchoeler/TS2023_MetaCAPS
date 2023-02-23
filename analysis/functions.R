
# ====================================================================================================
#======= Conversions effect sizes ====================================================================
#=====================================================================================================

# Resources providing info on inclusion of dependent effect sizes

# Viechtbauer:
# https://wviechtb.github.io/meta_analysis_books/cooper2019.html?s=09#13)_Stochastically_Dependent_Effect_Sizes

# Dose response
# Following the tutorial:
# https://www.jepusto.com/imputing-covariance-matrices-for-multi-variate-meta-analysis/


# ============= Dervive functions to transform effect sizes to Cohen D =============
### FUNCTION for transforming z values into cohen d effect size
ZScoreToD_transform = function(Est, SE, N1, N2) {
  z_adj = Est/SE
  p_z=2*pnorm(-abs(z_adj))
  p_z[which(p_z <=0.000001)] = 0.000001
  ES=pes(p_z,  N1, N2, dig = 20)
  return(ES)
}
# p_z[which(p_z <=0.000001)] = 0.000001 ==> if too large p-values, estimates are getting imprecise
# Average Treatment Effect (ATE) transformed to z-score by dividing by SE
# => reference where I found this: http://logisticregressionanalysis.com/1577-what-are-z-values-in-logistic-regression/
# => The z-value is the regression coefficient divided by its standard error


### FUNCTION for transforming means and SD into cohen d effect size
MeanToD_transform = function(m1, m2, sd1, sd2, n1, n2)
{
  ES=mes(m1, m2, sd1, sd2, n1, n2, dig=20)
  return(ES)
}

### FUNCTION for transforming f-statistic into cohen d effect size
FstatisticToD_transform = function(f, n1, n2){
  ES= fes(f, n1, n2, dig=20)
  return(ES)
}

### FUNCTION for transforming t-test value into cohen d effect size
TtestStatisticToD_transform = function(Ttest_estimate, group1, group2)
{
  ES=tes(Ttest_estimate , group1, group2, dig=20)
  return(ES)
}

### FUNCTION for transforming Odds ratio (using log odds) into cohen d effect size
logOddsintoD_transform = function(or, l.or, u.or, n1, n2)
{
  se = (log(u.or)-log(l.or))/(2*1.96) # calculate the standard error
  est=log(or)
  ES=lores(est, se^2, n1, n2, dig=20)
  return(ES)
}
## log odds are transformed into effect sizes using the lores() command
# the Variance of the log odds ratio is computed by var.lor = SE^2  (SE = standard error)


### FUNCTION for correlation coefficient into cohen d effect size
correlationToD_transform = function(correlation, N_subjects)
{
  ES=res(correlation, n=N_subjects, dig=20)
  return(ES)
}


d_meanDiff_dependent=function(m1=NULL, m2=NULL, sd1=NULL, sd2=NULL,var1=NULL, var2=NULL, m_diff=NULL, sd_diff=NULL, n, r){
  if (is.numeric(m1)) {
    if (is.numeric(sd1)) {
      sd_diff = sqrt(sd1^2 + sd2^2 - 2*r*sd1*sd2) # Estimate average standard deviation. REF: A review of effect sizes and their con1dence intervals, Part I: The Cohen’s d family. ALso reported in: Introduction toMeta-AnalysisMichael Borenstein
    }
    if (is.numeric(var1)) {
      sd_diff= sqrt(var1 + var2 - 2*r*sqrt(var1)*sqrt(var2))  # The standard deviation is the square root of the variance
    }
    d = (m1 - m2 ) / sd_diff
  }
  if (is.numeric(m_diff)) {
    d = m_diff / sd_diff # If estimates are reported as mean differences between time points
  }
  #var.d=(2*(1-r)/n)+(d/(2*n-2)) # REF: http://methods.sagepub.com/Reference//encyc-of-research-design/n58.xml?PageNum=185
  var.d <- ((1/n) + ((d^2)/(2*n)))*(2-2*r) # where r = cor of pre and post test correlation,  taken from Borenstein (p.29, Formula 4.28)
  ### E.g. dependent sample t-test with r=0.5 =>   vi <- (1/ni)+di^2/(2*ni)
  l.d=d-1.96 * sqrt(var.d)
  # compute a confidence interval (CI) for d: The z in the formula corresponds to the z-score value on the normal distribution corresponding to the desired probability level (e.g., 1.96 for a 95% CI)
  #REFERENCE: http://methods.sagepub.com/Reference//encyc-of-research-design/n58.xml?PageNum=185
  u.d=d+1.96 * sqrt(var.d)
  se.d = (u.d-l.d)/(2*1.96) # calculate the standard error
  z = d/se.d
  pval.d=2*pnorm(-abs(z))
  N.total=n
  out=as.data.frame(cbind(d, var.d,  l.d, u.d, N.total, pval.d))
  return(out)
}



# Get difference estimate from a comparison of two independent Cohen's D estimates
compare_2cohenD = function(d1, d1.var, d2, d2.var,n, r=NULL,method=NULL){
  # Compare dependent d
  # Start with two REM with effect sizes (e.g d=0.324 and d2= 0.611), and their variance.
  # Then, we apply the usual meta-analysis methods to compute Q or z-test to quantify the mean difference
  if (method=="independent") {
    se_diff=sqrt(d1.var+d2.var)
  }
  if (method=="dependent") {
    var_diff=d1.var + d2.var - 2*r*sqrt(d1.var)*sqrt(d2.var) # Taken from Borenstein, p 228
    se_diff=sqrt(var_diff)
  }
  # Formula taken from Borenstein (2009), p.159: Comparing A versus B : a Z-test
  d_diff=d1-d2 # Further details provided here: https://stats.stackexchange.com/questions/77269/statistical-comparison-of-2-independent-cohens-ds
  z_diff=d_diff/se_diff
  # get estimates for output
  pval.d=2*pnorm(-abs(z_diff)) # for a two-tailed test
  var.d=se_diff^2
  l.d= d_diff - 1.96 * se_diff
  u.d= d_diff + 1.96 * se_diff
  N.total=n
  out=as.data.frame(cbind(d_diff, var.d,  l.d, u.d, N.total, pval.d))
  return(out)
}




beta_to_d_independent <- function(beta, se, n=NULL, N1=NULL, N2=NULL){
  # Jessie: I also found another formula in one of Wolfgang's papers that doesn't require the SD of variables (the problem I was having before) in case useful to compare:
  # To convert unstandardised betas to Cohen's D, use equation outlined in in Wolfgang's Res. Syn. Meth. 2017 paper,
  # using variance from the formula here: https://stats.stackexchange.com/questions/410725/meta-analysis-metafor-using-cohens-d-what-is-the-variance
  # and derived in the function "esc.vd" from the "esc" package: https://rdrr.io/cran/esc/src/R/esc_helper.R
  print("Check if study reported N1 and N2 separately")
  if(class(n) == "numeric"){
    N_samp1=n/2
    N_samp2=n/2
  }
  if(class(N1) == "numeric"){
    N_samp1=N1
    N_samp2=N2
  }
  df <- (N_samp1 + N_samp2) - 2
  t <- beta/se
  d <- t * sqrt(1/N_samp1 + 1/N_samp2)
  l.d <- as.numeric(unlist(ci.smd(smd=d, n.1=N_samp1, n.2=N_samp2)[1]))
  u.d <- as.numeric(unlist(ci.smd(smd=d, n.1=N_samp1, n.2=N_samp2)[3]))
  se.d <- (u.d -l.d)/3.92
  #se.d =((N_samp1+N_samp2)/(N_samp1*N_samp2) ) + d^2/(2*(N_samp1+N_samp2))
  var.d <- se.d^2
  #l.d  <- d - stats::qnorm(.975) * sqrt( var.d)
  #u.d <-  d + stats::qnorm(.975) * sqrt( var.d)
  N.total=N_samp1+N_samp2
  zval.d <- d/se.d
  pval.d <- 2 * pt(abs(zval.d), df, lower.tail = FALSE) # p-value calculated from a t-distribution [vgl. p-value from a normal distribution: 2*pnorm(-abs(z))]
  out=as.data.frame(cbind(d, var.d,  l.d, u.d, N.total, pval.d))
  print(out)
  return(out)
}


p_to_d_independent=function(p, n){
  TINV<-qt((1-p/2),df)
  d<-TINV*sqrt((n)/(n^2))
  var.d = n/(n^2)+ (d^2)/(2*(n))
  l.d= d-1.96 * sqrt(var.d)
  u.d= d+1.96 * sqrt(var.d)
  se.d = sqrt(var.d) # can also use (u.d-l.d)/(2*1.96)
  z.d = d/ se.d
  pval.d = 2*pnorm(-abs(z.d))
  N.total=n
  out=as.data.frame(cbind(d, var.d,  l.d, u.d, N.total, pval.d))
  return(out)
}

# Estimate effective sample size
# The sample size needed in within-designs (NW) relative to the sample needed in between-designs (NB), assuming normal distributions, is (from Maxwell & Delaney, 2004, p. 561, formula 45):
n_effective_dependent=function(n_within, r){
  # Original formula: n_within=2*n_between/(-r+1), taken from https://www.mathpapa.com/calc.html?q=4x+7%3D2x+1
  # As follows when solved for n_within
  n_between=2*n_within*(1+r) # Note: When r=0 (ie complete independence, then the within subject number is double the between subject)
  
  return(  round(n_between,0) )
}


# Get_ncp (used for repated t-test)
get_ncp_t <- function(t, df_error, conf.level = 0.95) {
  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)
  
  if (isTRUE(all.equal(t, 0))) {
    t_ncp <- qt(probs, df_error)
    return(t_ncp)
  }
  
  ncp <- suppressWarnings(optim(
    par = 1.1 * rep(t, 2),
    fn = function(x) {
      p <- pt(q = t, df = df_error, ncp = x)
      
      abs(max(p) - probs[2]) +
        abs(min(p) - probs[1])
    },
    control = list(abstol = 1e-09)
  ))
  t_ncp <- unname(sort(ncp$par))
  if (isTRUE(all.equal(t_ncp[1], 0))) {
    t_ncp[1] <- qt(probs[1], df_error)
  }
  
  if (isTRUE(all.equal(t_ncp[2], 0))) {
    t_ncp[2] <- qt(probs[2], df_error)
  }
  return(t_ncp)
}


# convert t-test value from repeated measures to Cohen d
convertT_dependent = function(t, df, n) {
  d = t / sqrt(df) # Amended from package 'effectsize', https://github.com/easystats/effectsize/blob/master/R/convert_tFz_to_d.R
  t_ncp <- get_ncp_t(t, df) # NCP (noncentrality parameter) method: positions the observed ncp at the 0.025 quantile at the 0.975 quantile. Then, the interval between these two distributions’ ncp is taken to form the upper and lower bound of the 95% CI respectively.
  l.d <- t_ncp[1] / sqrt(df) # REF: A review of effect sizes and their con1dence intervals, Part I: The Cohen’s d family
  u.d <- t_ncp[2] / sqrt(df)
  se.d = (u.d-l.d)/(2*1.96) # calculate the standard error
  var.d= se.d^2
  z = d/se.d
  pval.d=2*pnorm(-abs(z))
  N.total=n
  out=as.data.frame(cbind(d, var.d,  l.d, u.d, N.total, pval.d))
  return(out)
}
convertT_dependent(t=5, df=1, n=52)

# convert t-test value from repeated measures to Cohen d (provides similar results to convertT_dependent)
convertT_repeated=function(t, df, r, n) {
  d= t / sqrt(df) # REF: Calculating and reporting effect sizes to facilitate cumulative science : a practical primer for t-tests and ANOVAs, DOI: 10.3389/fpsyg.2013.00863
  var.d=(2*(1-r)/n)+(d/(2*n-2)) # REF: http://methods.sagepub.com/Reference//encyc-of-research-design/n58.xml?PageNum=185
  #var.d<- ((1/n) + ((d^2)/(2*n)))*2*(1-r) # where r = cor of pre and post test correlation
  # var.d=(2*(1-r)/n)+ d^2/ (2*n)
  l.d=d-1.96 * sqrt(var.d) # sqrt(var.d) = SE
  u.d=d+1.96 * sqrt(var.d)
  se.d = (u.d-l.d)/(2*1.96) # calculate the standard error
  z = d/se.d
  pval.d=2*pnorm(-abs(z))
  N.total=n
  out=as.data.frame(cbind(d, var.d,  l.d, u.d, N.total, pval.d))
  return(out)
}


# convert t-test value from repeated measures to Cohen d
convertF_dependent = function(f, df, alpha=0.05, n) {
  if (df > 1) {
    stop("Cannot convert F with more than 1 df to (partial) r.")
  }
  t=sqrt(f)
  d = t / sqrt(df)
  alpha <- alpha
  probs <- c(alpha / 2, 1 - alpha / 2) # Amended from package 'effectsize', https://github.com/easystats/effectsize/blob/master/R/convert_tFz_to_d.R
  t_ncp <- qt(probs, df) # NCP (noncentrality parameter) method: positions the observed ncp at the 0.025 quantile at the 0.975 quantile. Then, the interval between these two distributions’ ncp is taken to form the upper and lower bound of the 95% CI respectively.
  l.d <- t_ncp[1] / sqrt(df) # REF: A review of effect sizes and their con1dence intervals, Part I: The Cohen’s d family
  u.d <- t_ncp[2] / sqrt(df)
  se.d = (u.d-l.d)/(2*1.96) # calculate the standard error
  var.d= se.d^2
  z = d/se.d
  pval.d=2*pnorm(-abs(z))
  N.total=n
  out=as.data.frame(cbind(d, var.d,  l.d, u.d, N.total, pval.d))
  return(out)
}


chiSquareCalc2x2=function(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB){
  dout=esc_2x2(grp1yes = NriskA_case, grp1no = NriskA_control, grp2yes =  NriskB_case, grp2no = NriskB_control,  es.type="d")
  if( dout$es=="Inf" |  dout$es=="-Inf"){
    datTable <- matrix(c(NriskA_case,NriskB_case,NriskA_control,NriskB_control), ncol=2)
    chiSquEstimate=as.numeric(chisq.test(datTable, correct=FALSE)$statistic) # correct=FALSE, turn off Yates’ continuity correction.
    chiSquEstimate_p=as.numeric(chisq.test(datTable, correct=FALSE)$p.value)
    Ntot=sum(datTable)
    dout=chies(chiSquEstimate, Ntot, dig=50)
    dout$totaln=Ntot
  } else {
    dout$d=abs(dout$es)
    dout$var.d= dout$se^2
    dout$l.d=abs(dout$ci.lo)
    dout$u.d=abs(dout$ci.hi)
    dout$N.total=dout$totaln
    dout$pval.d=2*pnorm(-abs(dout$es/dout$se))
  }

  out=data.frame( d=dout$d,  var.d=dout$var.d,   l.d=dout$l.d,  u.d=dout$u.d,  N.total=dout$N.total, pval.d= dout$pval.d)
  print(out)
  return(out)
}




# Get proportion table
chiSquareCalc=function(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB){
  datTable <- matrix(c(NriskA_case,NriskB_case,NriskA_control,NriskB_control), ncol=2)
  colnames(datTable) <- c('case', 'control')
  rownames(datTable) <- c(labelA, labelB)
  print(as.table(datTable))
  proportions= CrossTable(datTable)$prop.row[,1]
  
  print("Proportion of cases in each group")
  print(proportions)
  chiSquEstimate=as.numeric(chisq.test(datTable, correct=FALSE)$statistic) # correct=FALSE, turn off Yates’ continuity correction.
  chiSquEstimate_p=as.numeric(chisq.test(datTable, correct=FALSE)$p.value)
  Ntot=sum(datTable)
  NCases=sum(as.data.frame(datTable)[,1])
  out=cbind(chiSquEstimate, Ntot, NCases, chiSquEstimate_p)
  print(out)
  return(out)
}


chiSquareCalc_repeated = function(percCan, percPLB, nTot, r){
  # Derive effective sample size
  n_effective=n_effective_dependent(nTot, r=r)
  # Estimate number of cases with adverse responses to cannabis vs. no adverse responses to cannabis
  n_can=n_effective/2
  n_PLB=n_effective/2
  casesCan=round((n_can/100)*percCan,0)
  healthyCan=round(n_can-casesCan,0)
  casesPLB=round((n_PLB/100)*percPLB,0)
  healthyPLB=round(n_PLB-casesPLB,0)
  # Get chi square estimates
  datTable <- matrix(c(casesCan,casesPLB,healthyCan,healthyPLB), ncol=2)
  colnames(datTable) <- c('case', 'healthy')
  rownames(datTable) <- c("Cannabis", "Placebo")
  print(as.table(datTable))
  proportions= CrossTable(datTable)$prop.row[,1]
  print("Proportion of cases in each group")
  print(proportions)
  chiSquEstimate=as.numeric(CrossTable(datTable, chisq = T)$chisq$statistic)
  chiSquEstimate_p=as.numeric(CrossTable(datTable, chisq = T)$chisq$p.value)
  #chiSquEstimate2=as.numeric(chisq.test(datTable, correct=FALSE)$statistic)
  #chiSquEstimate_p=as.numeric(chisq.test(datTable, correct=FALSE)$p.value)
  Ntot=sum(datTable)
  NCases=sum(as.data.frame(datTable)[,1])
  out=cbind(chiSquEstimate, Ntot, NCases, chiSquEstimate_p)
  print(out)
}


# mcnemar.test when estimating chi-square from dependent samples
mcnemarCalc=function(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB){
  datTable <- matrix(c(NriskA_case,NriskB_case,NriskA_control,NriskB_control), ncol=2)
  colnames(datTable) <- c('case', 'control')
  rownames(datTable) <- c(labelA, labelB)
  print(as.table(datTable))
  proportions= CrossTable(datTable)$prop.row[,1]
  print("Proportion of cases in each group")
  print(proportions)
  mcnemar.test(proportions)
  chiSquEstimate=as.numeric(mcnemar.test(prop.table(datTable))$statistic)
  chiSquEstimate_p=as.numeric(mcnemar.test(prop.table(datTable))$p.value)
  Ntot=sum(datTable)
  NCases=sum(as.data.frame(datTable)[,1])
  out=cbind(chiSquEstimate, Ntot, NCases, chiSquEstimate_p)
  print(out)
  return(out)
}



# Generate data when prevalences were reported
genDat=function(n_cases, n_tot){
  dat1=c(rep(0,n_tot-n_cases), rep(1,n_cases))
  dat2=rep(0,n_tot)
  m1=mean(dat1)
  m2=mean(dat2)
  sd1=sd(dat1)
  sd2=sd(dat2)
  out=data.frame(m1, m2, sd1, sd2, n_tot)
  print(out)
  return(out)
}



# Repeated measures using escalc
#       - Here, one needs to specify m1i and m2i, the observed means at the two measurement occasions, sd1i and sd2i for the corresponding observed standard deviations,
#       - The options for the measure argument are then:
#               => "MC" for the raw mean change.
#               => ""SMCC" for the standardized mean change using change score standardization.
#               => ""SMCR" for the standardized mean change using raw score standardization.
#               => ""SMCRH" for the standardized mean change using raw score standardization with heteroscedastic population variances at the two measurement occasions (Bonett, 2008).
#               => ""ROMC" for the log transformed ratio of means (Lajeunesse, 2011).




# ====================================================================================================
#======= Function for analyses ======================================================================
#=====================================================================================================

emptyDF=function(label, df, dfTemp, drug, severity){
  metaRates=data.frame(matrix(nrow=1, ncol=length(colnames(dfTemp))))
  colnames(metaRates)=colnames(dfTemp)
  metaRates$independentStudies_n=NROW(df)
  
  if(NROW(df)>0){
    metaRates$n=df$n
    metaRates$est=df$est
    metaRates$est_lowerCI=df$est_lowerCI
    metaRates$est_upperCI=df$est_upperCI
    metaRates$drug=drug
    metaRates$severity=severity
    metaRates$labelPlot=paste0("individual_" ,label, "_" , drug, "_" , severity)
  }
  metaRates$symp=label
  return(metaRates)
}



getI2=function( df, model, symp, drug, severity){
  
  sympAll= unlist(strsplit(outcomesRates[1], "[.]"))[1]
  
  nStudies=length(levels(as.factor(df$Reference)))
  resGroup=rma.mv(yi, vi, random = ~ 1 | nestingVar/uniqueID, data=df,
                  method="REML", control=list(optimizer="optim", optmethod="Nelder-Mead"))

  if(sum(resGroup$sigma2)==0){
    print("Absence of heterogeneity")
    resGroup$I2=0
  } else {
    print("Estimate heterogeneity for multilevel models")
    # Functions taken from: http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
    W <- diag(1/df$vi)
    X <- model.matrix(resGroup)
    P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
    resGroup$I2 = 100 * sum(resGroup$sigma2) / (sum(resGroup$sigma2) + (resGroup$k-resGroup$p)/sum(diag(P))) # sums up the two variance components in the numerator and denominator
    #  this statistic can be thought of as the overall I2 value that indicates how much of the total variance can be attributed to the total amount of heterogeneity (which is the sum of between- and within-cluster heterogeneity)
    # => how much % of the total variance due to heterogeneity
  }
  
  n_aggGroup=aggregate(df$n~df$nestingVar, FUN=max)
  
  resGroupOut=data.frame(est=resGroup$beta, se=resGroup$se, est_lowerCI=resGroup$ci.lb, est_upperCI=resGroup$ci.ub, Qtest_p=resGroup$QEp, I2=resGroup$I2, n=round(sum(n_aggGroup[2]),0), model=model, symp=symp, drug=drug, severity=severity, nStudiesIncluded=nStudies, independentStudies_n=length(levels(as.factor(  df$nestingVar))))
  resGroupOut$labelPlot=paste0(model,"_" ,symp, "_" , drug, "_" , severity)
  
  return(resGroupOut)
}


# ======= Functons to pool together Cohen's d
runMetaModel=function(df, cor, return){
  # Format original dataframe
  groupVar=levels(as.factor(df$PredictorCoded))
  uniqueID=df$ID
  nestingVar=as.factor(df$ReferenceCoded)
  individualLabel=paste0(df$Predictor_clean, " [", df$OutcomeDefinitionLabel, "] - ", df$RefCleaned)
  d=df$d
  d_var=df$d_var
  d_lowerCI=df$d_lowerCI
  d_upperCI=df$d_upperCI
  se=sqrt(df$d_var)
  n=df$p_N
  PredictorBroad=df$PredictorBroad
  StudyType=df$StudyType
  StudyLabel=paste0(df$Cohort, " (", df$YearPublished, ")")


  df_in=data.frame(d, 
                   d_var,
                   d_lowerCI,
                   d_upperCI,
                   se,
                   I2=NA,
                   Qtest_p=NA,
                   model="OriginalStudy",
                   n,
                   PredictorBroad,
                   StudyType,
                   StudyLabel)
  
  df_in$d_pval=2*pnorm(-abs(( df_in$d/df_in$se)))
  df_in$Predictor=individualLabel
  df_in$nestingVar=nestingVar
  df_in$uniqueID=uniqueID
  typeInput=ifelse(length(levels(as.factor(df_in$StudyType)))>1, "mixed", df_in$StudyType[1])

  # time-and-fill not available in MREM, do in REM and remove all duplicates
  dfUnique=df_in %>% 
    group_by(nestingVar) %>% 
    slice(which.max(n))
  
  print(paste0("Test of predictor: ",  levels(as.factor(groupVar))))
  # Run alternative loop if available number of studies for a predictor group is n=1
  if ((length(uniqueID)==1)==TRUE & ( groupVar=="Other" )!=TRUE){
    print("Cannot derive pooled effect size - single study only")
    df_out=data.frame(d, 
                      d_var,
                      d_lowerCI,
                      d_upperCI,
                      se,
                      I2=NA,
                      Qtest_p=NA,
                      model="individual",
                      n=n,
                      PredictorBroad=PredictorBroad[1],
                      StudyType=StudyType[1],
                      StudyLabel=StudyLabel[1])
  }
  
  if ( (length(levels(droplevels(as.factor(nestingVar) )))==1)==TRUE & ( groupVar=="Other" )!=TRUE) {
    print("Aggregate effect size - effect sizes from the same study")
    agg=agg(id=nestingVar, es=d, var=d_var,  cor=cor, method="BHHR", data=df_in)
    # Estimate n per aggregated group
    agg$n=aggregate(n~nestingVar, FUN=mean)$n
    df_out=data.frame(d=agg$es,
                      d_var=agg$var,
                      d_lowerCI=agg$es-1.96 * sqrt(agg$var),
                      d_upperCI=agg$es+1.96 * sqrt(agg$var),
                      se=sqrt(agg$var),
                      I2=NA,
                      Qtest_p=NA,
                      model="Aggregated",
                      n= round( agg$n,0),
                      PredictorBroad=PredictorBroad[1],
                      StudyType=typeInput,
                      StudyLabel="Pooled")
    REMTFtext=""
  }
  
  if (NROW(df)>1 & (length(nestingVar)==length(levels(droplevels(as.factor(nestingVar)))))==TRUE) {
    print("No clustering of effect sizes (all effect sizes independent): Use Random Effects Model")
    
    REM=rma(d, d_var, data=df_in,  method="REML", control=list(optimizer="optim", optmethod="Nelder-Mead"))
    
    if(NROW(df_in)>=3){
    REMTF <- trimfill(REM)
    REMTFtext=paste0("Trim-and-fill: d=", round(REMTF$beta, 2), " (95% CI ", round(REMTF$ci.lb, 2), "; ", round(REMTF$ci.ub, 2), ")")
    } else{
      REMTFtext=""
    }
    # Sum up participants
    n_sum=sum(n)
    df_out=data.frame(d=REM$beta,
                      d_var=REM$se^2 ,
                      d_lowerCI=REM$ci.lb,
                      d_upperCI=REM$ci.ub,
                      se=REM$se,
                      I2=REM$I2,
                      Qtest_p=REM$QEp,
                      model="REM",
                      n=n_sum,
                      PredictorBroad=PredictorBroad[1],
                      StudyType=typeInput,
                      StudyLabel="Pooled")
  }
  if (NROW(df)>1 &  (length(levels(droplevels(as.factor(nestingVar) )))>1)==TRUE & (length(nestingVar)>length(levels(droplevels(as.factor(nestingVar)))))==TRUE & ( groupVar=="Other" )!=TRUE  ) {
    print("Clustering of effect sizes (effect sizes are dependent): Use Multilevel Random Effects Model")
    MREM=rma.mv(d, d_var, random = ~ 1 | nestingVar/uniqueID, data=df_in,
                method="REML", control=list(optimizer="optim", optmethod="Nelder-Mead"))
  
    
    if(NROW(dfUnique)>=3){
      REM=rma(d, d_var, data=dfUnique,  method="REML", control=list(optimizer="optim", optmethod="Nelder-Mead"))
      REMTF <- trimfill(REM)
      REMTFtext=paste0("Trim-and-fill: d=", round(REMTF$beta, 2), " (95% CI ", round(REMTF$ci.lb, 2), "; ", round(REMTF$ci.ub, 2), ")")
    } else{
      REMTFtext=""
    }
 
    if(sum(MREM$sigma2)==0){
      print("Absence of heterogeneity")
      MREM$I2=0
    } else {
      print("Estimate heterogeneity for multilevel models")
      # Functions taken from: http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
      W <- diag(1/df_in$d_var)
      X <- model.matrix(MREM)
      P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
      MREM$I2 = 100 * sum(MREM$sigma2) / (sum(MREM$sigma2) + (MREM$k-MREM$p)/sum(diag(P))) # sums up the two variance components in the numerator and denominator
      #  this statistic can be thought of as the overall I2 value that indicates how much of the total variance can be attributed to the total amount of heterogeneity (which is the sum of between- and within-cluster heterogeneity)
      # => how much % of the total variance due to heterogeneity
    }
    
    # Estimate n per aggregated group and take the sum
    n_agg=aggregate(n~nestingVar, FUN=mean)
    n_agg_sum=sum(n_agg$n)
    df_out=data.frame(d=MREM$beta,
                      d_var=MREM$se^2 ,
                      d_lowerCI=MREM$ci.lb,
                      d_upperCI=MREM$ci.ub,
                      se=MREM$se,
                      I2=MREM$I2,
                      Qtest_p=MREM$QEp,
                      model="MREM",
                      n=round(n_agg_sum,0),
                      PredictorBroad=PredictorBroad[1],
                      StudyType=typeInput,
                      StudyLabel="Pooled")
  }
  
  if ( ( groupVar=="Other" )==TRUE) {
    df_out=data.frame(d=NA,
                      d_var=NA ,
                      d_lowerCI=NA,
                      d_upperCI=NA,
                      se=NA,
                      I2=NA,
                      Qtest_p=NA,
                      model="Other",
                      n=NA,
                      PredictorBroad = "Other",
                      StudyType=typeInput,
                      StudyLabel="Other")
  }
  

  
  # Estimate p-value
  df_out$d_pval=2*pnorm(-abs(( df_out$d/df_out$se)))
  df_out$Predictor=as.character(levels(as.factor(groupVar)))
  # Combine input and output
  df_in$nestingVar=NULL
  df_in$uniqueID=NULL
  combinedDF=rbind(df_in, df_out)
  # Test for publication bias
  print(paste0("Test for publication bias: ",  levels(as.factor(groupVar))))
  #pubBias=as.data.frame(summary(lm(df_in$d~df_in$d_var))$coefficients) # d as outcome predicted by d-Variance:
  pubBias=as.data.frame(summary(lm(df_in$d~df_in$d_var, weights = 1/df_in$d_var))$coefficients)
  combinedDF$PubBias_estimate=NA
  combinedDF$PubBias_estimate[combinedDF$model=="REM" | combinedDF$model=="MREM" | combinedDF$model=="Aggregated"] = pubBias$Estimate[2]
  combinedDF$PubBias_p=NA
  combinedDF$PubBias_p[combinedDF$model=="REM" | combinedDF$model=="MREM" | combinedDF$model=="Aggregated"] = pubBias$`Pr(>|t|)`[2]
  # Order according to effect size but have REM last
  combinedDF=combinedDF[order(combinedDF$d),]
  combinedDF$Order=seq(1:length(combinedDF$Predictor))
  combinedDF$Order[combinedDF$model=="REM" | combinedDF$model=="MREM" | combinedDF$model=="Aggregated"] <- (length(combinedDF$Predictor)+1)
  combinedDF=combinedDF[order(combinedDF$Order),]
  # Remove rows with missing data
  combinedDF_clean=combinedDF[complete.cases(combinedDF$d),]
  combinedDF_clean$Category=as.character(levels(as.factor(groupVar)))

  if(return=="forest"){
    plot=plotForestIndividual(data=combinedDF_clean)
    return(plot)
  }
  
  print(paste0("Meta-analysis on - ",  levels(as.factor(groupVar)), " - completed"))
  
  if(return=="plot" & groupVar!="Other" & df_out$model!="Aggregated" & NROW(dfUnique)>=3 ){
    
    estimate = df_out$d
    lCI= df_out$d_lowerCI
    uCI= df_out$d_upperCI
    se = df_out$se
    se.seq=seq(0, max(df_in$se), 0.001)
    ll95 = estimate-(1.96*se.seq)
    ul95 = estimate+(1.96*se.seq)
    ll99 = estimate-(3.29*se.seq)
    ul99 = estimate+(3.29*se.seq)
    meanll95 = estimate-(1.96*se)
    meanul95 = estimate+(1.96*se)
    dfCI = data.frame(ll95, ul95, ll99, ul99, se.seq, estimate, meanll95, meanul95)

    df_plot=df_in
    df_plot$label=do.call(rbind, strsplit(df_in$Predictor, split = " - ") )[,2]
    df_plot$se=round(df_plot$se, 2)
    
  
    groupVarRec=redocePreP(groupVar)

    REMTFtext=ifelse(pubBias$`Pr(>|t|)`[2]<0.05, REMTFtext, "")
    # https://stats.stackexchange.com/questions/5195/how-to-draw-funnel-plot-using-ggplot2-in-r
    fp = ggplot(aes(x = se, y = round(d,2), colour=se), data = df_plot) +
      geom_point() +
      xlab('Standard Error') + ylab('Effect')+
      geom_line(aes(x = se.seq, y = ll95), linetype = 'dotted', data = dfCI, colour="grey") +
      geom_line(aes(x = se.seq, y = ul95), linetype = 'dotted', data = dfCI, colour="grey") +
      geom_line(aes(x = se.seq, y = ll99), linetype = 'dashed', data = dfCI, colour="grey") +
      geom_line(aes(x = se.seq, y = ul99), linetype = 'dashed', data = dfCI, colour="grey") +
      #geom_segment(aes(x = min(se.seq), y = meanll95, xend = max(se.seq), yend = meanll95), colour="darkgrey", data=dfCI) +
      #geom_segment(aes(x = min(se.seq), y = meanul95, xend = max(se.seq), yend = meanul95), colour="darkgrey", data=dfCI) +
      scale_x_reverse()+
     # scale_y_continuous(breaks=seq(round(min( ll95)-0.5, 1),round(max( ul99)+0.5, 1),0.5))+
      scale_y_continuous(n.breaks=5)+
      geom_hline(yintercept=estimate, lty=1) +
      geom_hline(yintercept=lCI, lty=1,  colour="darkgrey", ) +
      geom_hline(yintercept=uCI, lty=1,  colour="darkgrey", ) +
      #scale_y_continuous(breaks=seq(-1.25,2,0.25))+
      coord_flip() +
      scale_color_gradient(low="blue", high="red") +
      theme_classic() + 
      theme(legend.position="none",
            plot.margin = margin(t=0, r=0, b=1, l=0, "cm") ) +
      labs(title = groupVarRec,
           caption = paste0("Test for publication bias: p=", formatC(pubBias$`Pr(>|t|)`[2], 2), ".\n", REMTFtext ) )

    print(fp)
    
    return(fp)
  } 
  
 if(return=="data"){ 
    # Return output
    return(combinedDF_clean)
  }

}

# ====== Function to add table to a graph
ggTable=function(data){
  print(paste0("Read in: ",   data$Category[1]))
  data$estCI=paste0(round(data$d,2), " (", round(data$d_lowerCI,2), "-", round(data$d_upperCI,2), ")")
  data$Qtest_p=round(  data$Qtest_p,2)
  stable =subset(data, model!="OriginalStudy", select = c(estCI, model, Qtest_p) )
  colnames(stable)=c("Cohen's d (95%CI)", "Model", "Q-statistic (p)")
  mytheme <- gridExtra::ttheme_default(
    core = list(padding=unit(c(1, 1), "mm"))
  )
  
  #stable.p = tableGrob(dfNEW,
  #                     theme = mytheme )
  #stable.p <- ggtexttable(dfNEW, rows = rownames(dfNEW),
  #                        theme = ttheme("mOrange"))
  stable.p=tableGrob(stable, theme = mytheme)
  print(stable.p)
  return(stable.p)
}


# Generate forest plot
generateForest=function(data, namePredictor, order, minVal, limitsDefined, xMax){
  print(paste0("Read in: ",   data$Category[1]))
  plotOut=ggplot(data=data, aes(x=reorder(namePredictor, order), y=d, ymin=d_lowerCI, ymax=d_upperCI, color =  factor(StudyType)))
  plotOut = plotOut +
    geom_pointrange( lwd = 1/2, position = position_dodge(width = 1/2)) +
    geom_hline(yintercept=0, lty=1) +
    geom_errorbar(width=.2,position = position_dodge(width = 1/2) )  + coord_flip() +
    labs(title = "", x = "", y = "", color = c(""))  +   scale_shape_discrete(name  ="") +
    scale_y_continuous(breaks=seq(minVal, xMax, by = 1), limits = limitsDefined) +
    themeCanSens +
    guides(color=guide_legend(nrow=2, byrow=TRUE))
  
  
  print(plotOut)
  return(plotOut)
}





# Function to merge forest plot and summary table
mergeTableForest=function(plot, table){
  merged=grid.arrange(ggplotGrob(plot), table, nrow=1)
  #merged=grid.arrange(plot, table, nrow = 1, ncol=2)
  print(merged)
  return(merged)
}



# Function to select only pooled estimates
sumPooled=function(df){
  DF_selected=subset(df, model!="OriginalStudy" & Category!="Other")
  # Count number of estimtes included in each model
  DF_original=subset(df, model=="OriginalStudy")
  countDf=as.data.frame(table(DF_original$Category))
  colnames(countDf)=c("Predictor", "StudyCount")
  df_combined=merge(DF_selected, countDf, by = "Predictor", all.x = T)
  print(df_combined)
  return(df_combined)
}



# Forest plot

# Function to generate summary plot
generateSumPlot=function(df, minVal, limitsDefined, xMax){
  df$Predictor_info=paste0(df$Predictor, " (k=", df$StudyCount, "; n=",round(df$n,0) , ")")
  df$model=ifelse(df$StudyCount==1, "Single", df$model)
  
  # Heterogeneity test column
  df$Qtest=paste0("; I2=",round(df$I2,2), "% (", formatC(df$Qtest_p, 2), ")")
  # Add NA's for estimates derived from aggregation
  df$Qtest=ifelse(df$model=="Aggregated", "", df$Qtest)
  df$LabelSum=paste0(round(df$d,2), " (", round(df$d_lowerCI,2), "; ", round(df$d_upperCI,2), ")" )
  
  plot=NULL
  plot=generateForest(data=df, namePredictor = df$Predictor_info, order=df$d, minVal=minVal,limitsDefined=limitsDefined, xMax=xMax)
  plotOut= plot +geom_text(aes(label = LabelSum, y=1.8), colour="black", size=3.8, hjust = 0) 
  print(plotOut)
  return(plotOut)
}



redocePre=function(var){
  varOut=revalue(var,
                 c( "Age onset" = "Age onset (cannabis)",
                    "CannabisSeverity" = "Severity of cannabis use",
                    "THCcontent" = "Cannabis strain (high THC)",
                    "CBDcontent" = "Cannabis strain (high CBD)",
                    "PsychosisLiability"  = "Psychosis liability",
                    "SingleDose" = "Single dose THC",
                    "DoseResponseTHC" = "Dose-response effects (THC)",
                    "SubjectiveHigh" = "Subjective high",
                    "THCplasmaLevel" = "THC placma level"))
  return(varOut)
}

redocePreP=function(var){
  varOut=revalue(var,
                 c( "Age onset" = "Age onset \n(cannabis)",
                    "CannabisSeverity" = "Severity of \ncannabis use",
                    "Reason (medicinal vs. recreational)" = "Reason (medicinal \nvs. recreational)",
                    "Medical cannabis (placebo controlled)" = "Medical cannabis \n(placebo controlled)",
                    "THCcontent" = "Cannabis strain \n(high THC)",
                    "CBD (administration)" = "CBD \n(administration)", 
                    "Dopaminergic function" = "Dopaminergic \nfunction", 
                    "Opioidergic function" = "Opioidergic \nfunction",
                    "CBDcontent" = "Cannabis strain \n(high CBD)",
                    "PsychosisLiability"  = "Psychosis \nliability",
                    "SingleDose" = "Single dose THC",
                    "DoseResponseTHC" = "Dose-response \neffects (THC)",
                    "SubjectiveHigh" = "Subjective high",
                    "THCplasmaLevel" = "THC placma level"))
  return(varOut)
}



plotForestIndividual=function(data){
  
  print(paste0("Read in: ",   data$Category[1]))
  
  pooledD=subset(data, model!="OriginalStudy")$d
  
  data$size=ifelse(data$model=="OriginalStudy", 0.7, 1.5)
  data$shape=ifelse(data$model=="OriginalStudy", 16, 18)
  
  pooledDest=subset(data, model!="OriginalStudy")
  LabelSum=paste0(round(pooledDest$d,2), " (", round(pooledDest$d_lowerCI,2), "; ", round(pooledDest$d_upperCI,2), ")" )
  
  plotOut=ggplot(data=data, aes(x=reorder(Predictor, -Order), y=d, ymin=d_lowerCI, ymax=d_upperCI, group =  factor(Order), colour=model, shape=model ) ) +
    geom_pointrange( lwd = 1/2, position = position_dodge(width = 1/2), size = data$size, shape=  data$shape) +
    geom_hline(yintercept=0, lty=1) +
    geom_hline(yintercept=pooledD, lty=2, size=1.4, colour="blue4") +
    geom_errorbar(width=.2,position = position_dodge(width = 1/2) )  + 
    coord_flip() +
    labs(title = "", x = "", y = "", color = c(""))  +   
    scale_color_manual(values=c("blue4", "cornflowerblue") ) +
    scale_shape_discrete(name  ="") +
    scale_y_continuous(n.breaks=5) +
    #scale_y_continuous(breaks=seq(min(data$d_lowerCI), max(data$d_upperCI), by = 1)) +
    theme_classic() + 
    theme(legend.position="none",
          plot.margin = margin(t=0, r=0, b=1, l=0, "cm") ) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
    labs(title = pooledDest$Predictor,
         caption = paste0("Pooled estimate d (95% CI):", LabelSum) )
   diff=min(data$d_lowerCI)- max(data$d_upperCI)
      
 # ggsave(plot = plotOut, height = NROW(data)/2 , width =6, filename = paste0(HOME,"/results/figures/mergeForest",pooledDest$Predictor,".pdf"), limitsize = FALSE )
  
  return(plotOut)
}



#vec=1
plotSubForest=function(vec){
  selDim=listDim[[vec]]
  dfIn=subset( PredictorMetaInfoInc, Category %in% selDim)
  dfIn$Order=ifelse(dfIn$StudyLabel=="Pooled", max(  dfIn$Order)+1,    dfIn$Order)
  
  # dfIn %>%
  #   dplyr::group_by(Category) %>%
  #  dplyr::summarise(count=nrows())
  # ddply(dfIn, .(Category), nrow)

  

dfIn$modelRec <- factor(dfIn$model, levels = c("Individual study estimate (d)", "Aggregated", "MREM", "REM"))

if(NROW(levels(factor(dfIn$model)))==4){
  colVals=c("darkslateblue", "darkmagenta", "hotpink3", "darkgrey")
}
if(NROW(levels(factor(dfIn$model)))==3){
  colVals=c("darkslateblue", "darkmagenta", "darkgrey")
}

  subgroupMeta=ggplot(data=dfIn, 
                      aes(x=reorder(StudyLabel, -Order), 
                          y=d, 
                          ymin=d_lowerCI, 
                          ymax=d_upperCI, 
                          color =  reorder(factor(modelRec), -Order),
                          group=ID)) +
    
    geom_pointrange( lwd = 1/2, position = position_dodge(width = 1/2))  +
    geom_hline(yintercept=0, lty=1) +
    geom_errorbar(width=.2,position = position_dodge(width = 1/2) )  + 
    coord_flip() +
    theme(legend.position="top") +
    labs(title = "", x = "", y = "", color = c(""))  +   scale_shape_discrete(name  ="") +
    #facet_grid(vars(ModelInfo), scales = "free", space = "free",  cols=2) +
    #ggforce::facet_col(vars(ModelInfo), scales = "free_y", space = "free")  +
    facet_wrap(vars(ModelInfo), scales = "fixed", ncol = 2, drop=TRUE,strip.position = "top")  +
    #scale_y_continuous(breaks=seq(round(min(PredictorMetaInfoInc$d_lowerCI),0), xMax, by = 1), limits = c(round(min(PredictorMetaInfoInc$d_lowerCI),0), xMax)) +
    themeCanSens + theme(strip.background = element_blank(), strip.placement = "inside") + 
    scale_color_manual(values=colVals) +
    theme(strip.text.y = element_text(angle = 0, size=10))
  
  ggsave( paste0(HOME,"/results/figures/subgroupMeta",vec,".pdf"), subgroupMeta,  width = 22, height = 30, units = "cm", limitsize = FALSE)
}



