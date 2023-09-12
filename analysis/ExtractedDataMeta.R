# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ===================== EXTRACT DATA FROM STUDIES ========================================================================================================
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Schoeler (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Schoeler gender (reference is femalse)
NriskA_case=177
n_Acontrol=107002
NriskB_case=95
n_Bcontrol=39559
labelA="Male"
labelB="Female"
# Get cohen d
extract_Schoeler_gender=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_gender=data.frame(pEstimate=0.003, Ntot=extract_Schoeler_gender$N.total)

# Schoeler age
NriskA_case=163
n_Acontrol=51654
NriskB_case=114
n_Bcontrol=95971
labelA="younger"
labelB="older"
# Get cohen d
extract_Schoeler_age=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_age=data.frame(pEstimate=NA, Ntot=extract_Schoeler_gender$N.total)

# Schoeler exercise
NriskA_case=107
n_Acontrol=55773
NriskB_case=114
n_Bcontrol=69959
labelA="little"
labelB="regular"
# Get cohen d
extract_Schoeler_exercise=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_exercise=data.frame(pEstimate=0.225, Ntot=extract_Schoeler_exercise$N.total)

# Schoeler education
NriskA_case=28
n_Acontrol=22197
NriskB_case=84
n_Bcontrol=49460
labelA="noEdu"
labelB="higherEdu"
# Get cohen d
extract_Schoeler_edu=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_edu=data.frame(pEstimate=0.173, Ntot=extract_Schoeler_edu$N.total)

# Schoeler BMI (underweight)
NriskA_case=31
n_Acontrol=14962
NriskB_case=92
n_Bcontrol=47773
labelA="underweight"
labelB="normal"
# Get cohen d
extract_Schoeler_underW=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_underW=data.frame(pEstimate=0.724, Ntot=extract_Schoeler_underW$N.total)


# Schoeler BMI (overweight)
NriskA_case=65
n_Acontrol=36668
NriskB_case=92
n_Bcontrol=47773
labelA="overweight"
labelB="normal"
# Get cohen d
extract_Schoeler_overW=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_overW=data.frame(pEstimate=0.609, Ntot=extract_Schoeler_overW$N.total)


# Schoeler reason
NriskA_case=252
n_Acontrol=136658
NriskB_case=21
n_Bcontrol=10137
labelA="recreational"
labelB="medical"
# Get cohen d
extract_Schoeler_reason=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_reason=data.frame(pEstimate=0.608, Ntot=extract_Schoeler_reason$N.total)


# Schoeler cannabis type
NriskA_case=102
n_Acontrol=60271
NriskB_case=106
n_Bcontrol=60289
labelA="highPotency"
labelB="herbal"
# Get cohen d
extract_Schoeler_potency=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_potency=data.frame(pEstimate=0.783, Ntot=extract_Schoeler_potency$N.total)


# Schoeler cannabis frequency
NriskA_case=111
n_Acontrol=64902
NriskB_case=169
n_Bcontrol=83207
labelA="highFreq"
labelB="lowFreq"
# Get cohen d
extract_Schoeler_frequency=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_frequency=data.frame(pEstimate=0.159, Ntot=extract_Schoeler_frequency$N.total)

# Schoeler tobacco mix
NriskA_case=163
n_Acontrol=58016
NriskB_case=106
n_Bcontrol=81066
labelA="tobacco"
labelB="noTobacco"
# Get cohen d
extract_Schoeler_tobacco=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_tobacco=data.frame(pEstimate=NA, Ntot=extract_Schoeler_tobacco$N.total)

# ROA (eaten vs joint)
NriskA_case=3
n_Acontrol=1458
NriskB_case=162
n_Bcontrol=65045
labelA="eaten"
labelB="joint"
# Get cohen d
extract_Schoeler_eaten=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_eaten=data.frame(pEstimate=0.743, Ntot=extract_Schoeler_eaten$N.total)


# ROA (knife vs joint)
NriskA_case=1
n_Acontrol=171
NriskB_case=162
n_Bcontrol=65045
labelA="knife"
labelB="joint"
# Get cohen d
extract_Schoeler_knife=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_knife=data.frame(pEstimate=0.393, Ntot=extract_Schoeler_knife$N.total)

# ROA (pipe vs joint)
NriskA_case=38
n_Acontrol=17721
NriskB_case=162
n_Bcontrol=65045
labelA="pipe"
labelB="joint"
# Get cohen d
extract_Schoeler_pipe=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_pipe=data.frame(pEstimate=0.406, Ntot=extract_Schoeler_pipe$N.total)


# ROA (vape vs joint)
NriskA_case=3
n_Acontrol=4556
NriskB_case=162
n_Bcontrol=65045
labelA="vape"
labelB="joint"
# Get cohen d
extract_Schoeler_vape=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_vape=data.frame(pEstimate=0.022, Ntot=extract_Schoeler_vape$N.total)

# alcohol use
NriskA_case=22
n_Acontrol=19868
NriskB_case=252
n_Bcontrol=126595
labelA="alc"
labelB="nosub"
# Get cohen d
extract_Schoeler_alc=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_alc=data.frame(pEstimate=0.008, Ntot=extract_Schoeler_alc$N.total)

# cocaine use
NriskA_case=96
n_Acontrol=47537
NriskB_case=97
n_Bcontrol=42510
labelA="cocaine"
labelB="nosub"
# Get cohen d
extract_Schoeler_cocaine=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_cocaine=data.frame(pEstimate=0.396, Ntot=extract_Schoeler_cocaine$N.total)

# mdma use
NriskA_case=145
n_Acontrol=59634
NriskB_case=60
n_Bcontrol=35962
labelA="mdma"
labelB="nosub"
# Get cohen d
extract_Schoeler_mdma=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_mdma=data.frame(pEstimate=0.014, Ntot=extract_Schoeler_mdma$N.total)


# amphetamine use
NriskA_case=91
n_Acontrol=33905
NriskB_case=93
n_Bcontrol=44847
labelA="amphetamine"
labelB="nosub"
# Get cohen d
extract_Schoeler_amphetamine=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_amphetamine=data.frame(pEstimate=0.080, Ntot=extract_Schoeler_amphetamine$N.total)


# methamphetamine use
NriskA_case=15
n_Acontrol=4861
NriskB_case=99
n_Bcontrol=38623
labelA="methamphetamine"
labelB="nosub"
# Get cohen d
extract_Schoeler_methamphetamine=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_methamphetamine=data.frame(pEstimate=0.502, Ntot=extract_Schoeler_methamphetamine$N.total)

# ketamine use
NriskA_case=31
n_Acontrol=19974
NriskB_case=106
n_Bcontrol=39681
labelA="ketamine"
labelB="nosub"
# Get cohen d
extract_Schoeler_ketamine=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_ketamine=data.frame(pEstimate=0.008, Ntot=extract_Schoeler_ketamine$N.total)


# lsd use
NriskA_case=61
n_Acontrol=32176
NriskB_case=115
n_Bcontrol=47951
labelA="lsd"
labelB="nosub"
# Get cohen d
extract_Schoeler_lsd=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_lsd=data.frame(pEstimate=0.137, Ntot=extract_Schoeler_lsd$N.total)

# MHx depression
NriskA_case=71
n_Acontrol=21913
NriskB_case=115
n_Bcontrol=95253
labelA="depression"
labelB="noMH"
# Get cohen d
extract_Schoeler_depression=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_depression=data.frame(pEstimate=NA, Ntot=extract_Schoeler_depression$N.total)


# MHx anxiety
NriskA_case=45
n_Acontrol=15592
NriskB_case=115
n_Bcontrol=95253
labelA="anxiety"
labelB="noMH"
# Get cohen d
extract_Schoeler_anxiety=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_anxiety=data.frame(pEstimate=NA, Ntot=extract_Schoeler_anxiety$N.total)

# MHx bipolar
NriskA_case=16
n_Acontrol=3085
NriskB_case=115
n_Bcontrol=95253
labelA="bipolar"
labelB="noMH"
# Get cohen d
extract_Schoeler_bipolar=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_bipolar=data.frame(pEstimate=NA, Ntot=extract_Schoeler_bipolar$N.total)


# MHx psychosis
NriskA_case=24
n_Acontrol=1419
NriskB_case=115
n_Bcontrol=95253
labelA="psychosis"
labelB="noMH"
# Get cohen d
extract_Schoeler_psychosis=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_psychosis=data.frame(pEstimate=NA, Ntot=extract_Schoeler_psychosis$N.total)

# MHx ADHD
NriskA_case=14
n_Acontrol=5321
NriskB_case=115
n_Bcontrol=95253
labelA="adhd"
labelB="noMH"
# Get cohen d
extract_Schoeler_adhd=chiSquareCalc2x2(NriskA_case, NriskB_case, n_Acontrol-NriskA_case, n_Bcontrol-NriskB_case, labelA, labelB)
# Get p-value
pEstimate_Schoeler_adhd=data.frame(pEstimate=0.006, Ntot=extract_Schoeler_adhd$N.total)




# Rates (lifetime occurence)
schoelerRatesEver_n=72892
schoelerRatesEver_psychosisDF=data.frame(event=356, n=schoelerRatesEver_n)  
schoelerRatesEver_paranoiaDF=data.frame(event=191, n=schoelerRatesEver_n) 
schoelerRatesEver_hallucinationsDF=data.frame(event=54, n=schoelerRatesEver_n)  





# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Thomas ====================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Thomas, Huw. "A community survey of adverse effects of cannabis use." Drug and alcohol dependence 42, no. 3 (1996): 201-207.

# Thomas gender and cannabis-psychosis (Table 1) ("After taking cannabis have you ever had strange, unpleasant experiences such as hearing voices or be- coming convinced that someone is trying to harm you, or that you are being persecuted?")
thomasGenderMale_Psychosis=13 # number of male individuals reporting psychosis
thomasGenderMale_n=100 # total number of male individuals assessed
thomasGenderFemale_Psychosis=16 # number of female individuals reporting psychosis
thomasGenderFemale_n=94 # total number of female individuals assessed
labelA="Male"
labelB="Female"
extract_thomas_gender=chiSquareCalc2x2(thomasGenderMale_Psychosis, thomasGenderFemale_Psychosis, thomasGenderMale_n-thomasGenderMale_Psychosis, thomasGenderFemale_n-thomasGenderFemale_Psychosis, labelA, labelB)
# Get p-value
pEstimate_thomas_gender=data.frame(pEstimate=NA, Ntot=extract_thomas_gender$N.total)

# Thomas cannabis frequency (table 2)
thomasFrequencyHeavy_Psychosis=18 # number of heavy cannabis users reporting psychosis
thomasFrequencyHeavy_n = 106 # total number of heavy cannabis users 
thomasFrequencyLight_Psychosis=12 # number of light cannabis users reporting psychosis
thomasFrequencyLight_n=89 # total number of light cannabis users 
labelA="HighFrequency"
labelB="LowFrequency"
extract_thomas_frequency=chiSquareCalc2x2(thomasFrequencyHeavy_Psychosis, thomasFrequencyLight_Psychosis, thomasFrequencyHeavy_n-thomasFrequencyHeavy_Psychosis, thomasFrequencyLight_n-thomasFrequencyLight_Psychosis, labelA, labelB)
pEstimate_thomas_frequency=data.frame(pEstimate=NA, Ntot=extract_thomas_frequency$N.total)


# Thomas cannabis recency (current vs. former user) (table 3)
# Current user: n=54, cannabis use within the past week
# Former: n=140, had used cannabis but not within the past week
thomasCurrentUser_Psychosis=4 # number of current cannabis users reporting psychosis
thomasCurrentUser_n=52  # total number of current cannabis users 
thomasFormerUser_Psychosis=25 # number of former (ex) cannabis users reporting psychosis
thomasFormerUser_n=139 # total number of former cannabis users 
labelA="CurrentUser"
labelB="FormertUser"
extract_thomas_currentCan=chiSquareCalc2x2(thomasCurrentUser_Psychosis, thomasFormerUser_Psychosis, thomasCurrentUser_n-thomasCurrentUser_Psychosis, thomasFormerUser_n-thomasFormerUser_Psychosis, labelA, labelB)
pEstimate_thomas_currentCan=data.frame(pEstimate=NA, Ntot=extract_thomas_currentCan$N.total)

# Thomas cannabis dependency (table 4)
thomasDependent_Psychosis=13 # number of dependent cannabis users reporting psychosis
thomasDependent_n=57 # total number of dependent cannabis users 
thomasNondependent_Psychosis=16  # number of non-dependent cannabis users reporting psychosis
thomasNondependent_n=131 # total number of non-dependent cannabis users 
labelA="Dependent"
labelB="Non-dependent"
extract_thomas_dependeny=chiSquareCalc2x2(thomasDependent_Psychosis, thomasNondependent_Psychosis, thomasDependent_n-thomasDependent_Psychosis, thomasNondependent_n-thomasNondependent_Psychosis, labelA, labelB)
pEstimate_thomas_dependeny=data.frame(pEstimate=NA, Ntot=extract_thomas_dependeny$N.total)

# Get estimates for rates
thomas_Psychosis_n=30 # number of cannabis users reporting to experience psychotic symptoms following cannabis use
thomas_Psychosis_perc=15  # percentage of cannabis users reporting to experience psychotic symptoms following cannabis use
thomas1996_canPLEDF=data.frame(event=thomas_Psychosis_n, n=(thomas_Psychosis_n/thomas_Psychosis_perc)*100)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Sexton ====================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Sexton, Michelle, Carrie Cuttler, and Laurie K. Mischley. "A survey of cannabis acute effects and withdrawal symptoms: differential responses across user types and age." The Journal of Alternative and Complementary Medicine 25, no. 3 (2019): 326-335.

# Sexton paranoia - medical vs recreational
# Medical
allA=891
percA=8.3
NriskA_case=round((allA/100)*percA,0)
NriskA_control=allA-NriskA_case
# Recreational
allB=1110
percB=20
NriskB_case=round((allB/100)*percB,0)
NriskB_control=allB-NriskB_case
# Get chi square estimates
labelA="Medical"
labelB="Recreational"
extract_SextonParanoia_reasonUse=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_SextonParanoia_reasonUse=data.frame(pEstimate=NA, Ntot=extract_SextonParanoia_reasonUse$N.total)


# Sexton hallucinatoons - medical vs recreational
# Medical
allA=891
percA=3.3
NriskA_case=round((allA/100)*percA,0)
NriskA_control=allA-NriskA_case
# Recreational
allB=1110
percB=4.8
NriskB_case=round((allB/100)*percB,0)
NriskB_control=allB-NriskB_case
# Get chi square estimates
labelA="Medical"
labelB="Recreational"
extract_SextonHallucinations_reasonUse=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_SextonHallucinations_reasonUse=data.frame(pEstimate=NA, Ntot=extract_SextonHallucinations_reasonUse$N.total)

# Sexton paranoia - younger vs middle age vs. older age
chiSquEstimateSextonParanoia_age=32
NtotSexton_age=1300+1048+507
# Get cohen d
extract_SextonParanoiaAge=chies(chiSquEstimateSextonParanoia_age, NtotSexton_age, dig=50)
# Get p-value
pEstimate_SextonParanoiaAge=data.frame(pEstimate=NA, Ntot=NtotSexton_age) # p < 0.001

# Sexton hallucinations - younger vs middle age vs. older age
chiSquEstimateSextonHallucination_age=3.19
# Get cohen d
extract_SextonHallucinationAge=chies(chiSquEstimateSextonHallucination_age, NtotSexton_age, dig=50)
# Get p-value
pEstimate_SextonHallucinationAge=data.frame(pEstimate=0.2, Ntot=NtotSexton_age)


# RATES
sexton2019_canParDF=data.frame(event=(2878/100)*14.5, n=2878)
sexton2019_canHalDF=data.frame(event=(2878/100)*3.8, n=2878)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Arendt ====================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# REF: Arendt, Mikkel, Raben Rosenberg, Lone Fjordback, Jack Brandholdt, Leslie Foldager, Leo Sher, and Povl Munk-Jørgensen. "Testing the self-medication hypothesis of depression and aggression in cannabis-dependent subjects." Psychological medicine 37, no. 7 (2007): 935.

# %%%%%%%%% Predictor: Depression %%%%%%%%%%%
# Extract n
arendt_depressed_n=55
arendt_nonDepressed_n=64
arendt_depTot_n=arendt_depressed_n+arendt_nonDepressed_n

# Depression => Paranoia
# subjects with depression were more likely to experience paranoia (OR 3.1 95% CI 1.5–6.2, p <0.01)
arendt_depression_paranoia_or=3.1
arendt_depression_paranoia_l.or=1.5
arendt_depression_paranoia_u.or=6.2
arendt_depression_paranoiaEstimate=logOddsintoD_transform(arendt_depression_paranoia_or, arendt_depression_paranoia_l.or, arendt_depression_paranoia_u.or, arendt_depressed_n, arendt_nonDepressed_n)
pEstimate_arendt_depression_paranoia=data.frame(pEstimate=NA, Ntot=arendt_depTot_n)
# Depression => Delusions
pes(p=pValueNS_allStudies, n.1=arendt_depressed_n, n.2=arendt_nonDepressed_n)
pEstimate_arendt_depression_delusions=data.frame(pEstimate=NA, Ntot=arendt_depTot_n)
# Depression => Depersionalisation
pes(p=pValueNS_allStudies, n.1=arendt_depressed_n, n.2=arendt_nonDepressed_n)
pEstimate_arendt_depression_depersionalisation=data.frame(pEstimate=NA, Ntot=arendt_depTot_n)
# Depression => Hallucinations
pes(p=pValueNS_allStudies, n.1=arendt_depressed_n, n.2=arendt_nonDepressed_n)
pEstimate_arendt_depression_hallucinations=data.frame(pEstimate=NA, Ntot=arendt_depTot_n)

# %%%%%%%%% Predictor: Violence %%%%%%%%%%%
# Extract n
arendt_violent_n=58
arendt_nonViolent_n=54
arendt_violTot_n=arendt_violent_n+arendt_nonViolent_n

# Violence => Paranoia
pes(p=pValueNS_allStudies, n.1=arendt_violent_n, n.2=arendt_nonViolent_n)
pEstimate_arendt_violence_paranoia=data.frame(pEstimate=NA, Ntot=arendt_violTot_n) # None of the estimates are significant for violence

# Violence => Delusions
pes(p=pValueNS_allStudies, n.1=arendt_violent_n, n.2=arendt_nonViolent_n)
pEstimate_arendt_violence_delusions=data.frame(pEstimate=NA, Ntot=arendt_violTot_n) # None of the estimates are significant for violence

# Violence => Depersionalisation
pes(p=pValueNS_allStudies, n.1=arendt_violent_n, n.2=arendt_nonViolent_n)
pEstimate_arendt_violence_depersionalisation=data.frame(pEstimate=NA, Ntot=arendt_violTot_n) # None of the estimates are significant for violence

# Violence => Hallucinations
pes(p=pValueNS_allStudies, n.1=arendt_violent_n, n.2=arendt_nonViolent_n)
pEstimate_arendt_violence_hallucinations=data.frame(pEstimate=NA, Ntot=arendt_violTot_n) # None of the estimates are significant for violence


# RATES
arendt2007_canDelDF=data.frame(event=(119/100)*44.5, n=119)
arendt2007_canParDF=data.frame(event=(119/100)*24.4, n=119)
arendt2007_canHalDF=data.frame(event=(119/100)*5, n=119)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Levy ======================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Levy, Sharon, and Elissa R. Weitzman. "Acute mental health symptoms in adolescent marijuana users." JAMA pediatrics 173, no. 2 (2019): 185-186.

# %%%%%%%%%%%%%%%% HALLUCINATIONS %%%%%%%%%%%%%%%%

# Levy hallucinations - CUD presence vs absence
NriskA_case=19
NriskA_control=21
NriskB_case=21
NriskB_control=85
# Get chi square estimates
labelA="PresenceCUD"
labelB="AbsenceCUD"
extract_levyHallucinations_CUD=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_levyHallucinations_CUD=data.frame(pEstimate=NA, Ntot=extract_levyHallucinations_CUD$N.total)

# Levy hallucinations - high vs.low frequency
NriskA_case=28
NriskA_control=12
NriskB_case=42
NriskB_control=64
# Get chi square estimates
labelA="HighFrequency"
labelB="LowFrequency"
extract_levyHallucinations_frequency=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_levyHallucinations_frequency=data.frame(pEstimate=NA, Ntot=extract_levyHallucinations_frequency$N.total)


# Levy anxiety - presence vs.absence
NriskA_case=10
NriskA_control=30
NriskB_case=16
NriskB_control=90
# Get chi square estimates
labelA="AnxietyPresent"
labelB="AnxietyAbsent"
extract_levyHallucinations_anxiety=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_levyHallucinations_anxiety=data.frame(pEstimate=NA, Ntot=extract_levyHallucinations_anxiety$N.total)


# Levy depression - presence vs.absence
NriskA_case=12
NriskA_control=28
NriskB_case=23
NriskB_control=83
# Get chi square estimates
labelA="DepressionPresent"
labelB="DepressionAbsent"
extract_levyHallucinations_depression=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_levyHallucinations_depression=data.frame(pEstimate=NA, Ntot=extract_levyHallucinations_depression$N.total)




# %%%%%%%%%%%%%%%% Paranoia %%%%%%%%%%%%%%%%

# Levy paranoia - CUD presence vs absence
NriskA_case=21
NriskA_control=28
NriskB_case=19
NriskB_control=78
# Get chi square estimates
labelA="PresenceCUD"
labelB="AbsenceCUD"
extract_levyParanoia_CUD=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_levyParanoia_CUD=data.frame(pEstimate=NA, Ntot=extract_levyParanoia_CUD$N.total)


# Levy Paranoia - high vs.low frequency
NriskA_case=33
NriskA_control=16
NriskB_case=37
NriskB_control=60
# Get chi square estimates
labelA="HighFrequency"
labelB="LowFrequency"
extract_levyParanoia_frequency=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_levyParanoia_frequency=data.frame(pEstimate=NA, Ntot=extract_levyParanoia_frequency$N.total)

# Levy anxiety - presence vs.absence
NriskA_case=13
NriskA_control=36
NriskB_case=13
NriskB_control=84
# Get chi square estimates
labelA="AnxietyPresent"
labelB="AnxietyAbsent"
extract_levyParanoia_anxiety=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_levyParanoia_anxiety=data.frame(pEstimate=NA, Ntot=extract_levyParanoia_anxiety$N.total)

# Levy depression - presence vs.absence
NriskA_case=20
NriskA_control=29
NriskB_case=15
NriskB_control=82
# Get chi square estimates
labelA="DepressionPresent"
labelB="DepressionAbsent"
extract_levyParanoia_depression=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_levyParanoia_depression=data.frame(pEstimate=NA, Ntot=extract_levyParanoia_depression$N.total)



# %%%%%%%%%%%%%%%% NS results %%%%%%%%%%%%%%%%

#  Neither halluci- nations nor paranoia or anxiety was associated with age, sex, race/ethnicity, general health status, or socioeconomic sta- tus
levy_n=146

pEstimate_levy_paranoia_age=data.frame(pEstimate=NA, Ntot=levy_n) # NS, ie below study-defined p-value threshold
pEstimate_levy_hallucination_age=data.frame(pEstimate=NA, Ntot=levy_n) # NS, ie below study-defined p-value threshold

pEstimate_levy_paranoia_gender=data.frame(pEstimate=NA, Ntot=levy_n) # NS, ie below study-defined p-value threshold
pEstimate_levy_hallucination_gender=data.frame(pEstimate=NA, Ntot=levy_n) # NS, ie below study-defined p-value threshold

pEstimate_levy_paranoia_ethnicity=data.frame(pEstimate=NA, Ntot=levy_n) # NS, ie below study-defined p-value threshold
pEstimate_levy_hallucination_ethnicity=data.frame(pEstimate=NA, Ntot=levy_n) # NS, ie below study-defined p-value threshold

pEstimate_levy_paranoia_health=data.frame(pEstimate=NA, Ntot=levy_n) # NS, ie below study-defined p-value threshold
pEstimate_levy_hallucination_health=data.frame(pEstimate=NA, Ntot=levy_n) # NS, ie below study-defined p-value threshold

pEstimate_levy_paranoia_ses=data.frame(pEstimate=NA, Ntot=levy_n) # NS, ie below study-defined p-value threshold
pEstimate_levy_hallucination_ses=data.frame(pEstimate=NA, Ntot=levy_n) # NS, ie below study-defined p-value threshold



# RATES
levy2019_canParDF=data.frame(event=49, n=146)
levy2019_canHalDF=data.frame(event=40, n=146)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Barkus ====================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Barkus, Emma, and Shon Lewis. "Schizotypy and psychosis-like experiences from recreational cannabis in a non-clinical sample." Psychological Medicine 38, no. 9 (2008): 1267-1276.
barkus_n=86+95+351
#Participants were divided into three groups according to their total SPQ score: high ST (n=86) low ST (n=95), middle (n=351)
# Note: Three groups are compared using t-est
# - All the three groups scored significantly different from one another on the Psychosis- Like Experiences subscale
# - F(2, 529)=32.27, p<0.001
barkus_schizotype_psych_f=32.27
fes(barkus_schizotype_psych_f, barkus_n/2, barkus_n/2,  dig=20)
pEstimate_barkus_schizotype_psych=data.frame(pEstimate=0.0009, Ntot=barkus_n)
# Psy-chosis-Like Experiences [High 61.40 (16.88), middle 50.72 (16.26), Low 42.42 (13.44)];
# Note by compute.es developer: If only the total sample size is reported simply splitthe number in half for entry into the function.

# NO RATES REPORTED

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bianconi ==================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Bianconi, F., M. Bonomo, A. Marconi, A. Kolliakou, S. A. Stilo, C. Iyegbe, P. Gurillo Muñoz et al. "Differences in cannabis-related experiences between patients with a first episode of psychosis and controls." Psychological medicine 46, no. 5 (2016): 995-1003.
# %%%% Paranoia %%%%%%%%%
# Patients vs healthy: paranoia - rarely vs sometimes vs often
#  => How often have you felt suspicious while smoking cannabis?
bianconi_patients_paranoia_n=100+14+74
bianconi_patients_paranoia_chisquare=9.556
# Get cohen d
extracted_bianconi_PsychosisLiability_paranoia=chies(bianconi_patients_paranoia_chisquare, bianconi_patients_paranoia_n, dig=50)
# Get p-value
pEstimate_bianconi_patients_paranoia=data.frame(pEstimate=0.008, Ntot=bianconi_patients_paranoia_n)

# %%%% Hallucinations %%%%%%%%%
# Patients vs healthy: voices - rarely vs sometimes vs often
# How often have your heard voices while smoking cannabis?
bianconi_patients_hallucination_n=143+10+36
bianconi_patients_hallucination_chisquare=10.644
# Get cohen d
extracted_bianconi_PsychosisLiability_hallucinations=chies(bianconi_patients_hallucination_chisquare, bianconi_patients_hallucination_n, dig=50)
# Get p-value
pEstimate_bianconi_patients_hallucination=data.frame(pEstimate=0.005, Ntot=bianconi_patients_paranoia_n)


# %%%% Extract beta estimates from regression models %%%%%%%%%

# Caseness
bianconi_regression_n=252+217
bianconi_caseness_beta=0.23
bianconi_caseness_se=0.16
# Get cohen d
extracted_bianconi_Caseness_PsychosisLike_regression=beta_to_d_independent(beta=bianconi_caseness_beta, se=bianconi_caseness_se, N1=252, N2=217) # p-value matches study p-value
# Get p-value
pEstimate_bianconi_caseness=data.frame(pEstimate=0.143, Ntot=bianconi_regression_n)

# Type of cannabis
bianconi_typeCan_beta=1.03
bianconi_typeCan_se=0.34
# Get cohen d
extracted_bianconi_TypeCan_PsychosisLike_regression=beta_to_d_independent(beta=bianconi_typeCan_beta, se=bianconi_typeCan_se, n=bianconi_regression_n) # p-value matches study p-value
# Get p-value
pEstimate_bianconi_typeCan=data.frame(pEstimate=0.009, Ntot=bianconi_regression_n) # <0.01

# Belief about cannabis
bianconi_belief_beta=0.96
bianconi_belief_se=0.27
# Get cohen d
extracted_bianconi_belief_PsychosisLike_regression=beta_to_d_independent(beta=bianconi_belief_beta, se=bianconi_belief_se, n=bianconi_regression_n) # p-value matches study p-value
# Get p-value
pEstimate_bianconi_belief=data.frame(pEstimate=0.0009, Ntot=bianconi_regression_n) # <0.001


# %%%%%%% Predictors which did not show significant results %%%%%%%

# Alcohol use
# Other drugs use
# Tobacco use
# Amount of money spent on cannabis
# Duration of cannabis use
# Frequency of cannabis use
pes(p=NA, n.1=bianconi_regression_n/2, n.2=bianconi_regression_n/2)
pEstimate_bianconi_ns=data.frame(pEstimate=pValueNS_allStudies, Ntot=bianconi_regression_n)


# RATES (only healthy controls)
bianconi2016_canParDF=data.frame(event=60, n=130)
bianconi2016_canHalDF=data.frame(event=13, n=129)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Morgan (2016) =============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Morgan, C. J. A., T. P. Freeman, J. C. H. V. Powell, and H. V. Curran. "AKT1 genotype moderates the acute psychotomimetic effects of naturalistically smoked cannabis in young cannabis smokers." Translational psychiatry 6, no. 2 (2016): e738.
n_morgan=422

# AKT1 (T/T,C/T, C/C)
# Genotype AKT1 = T/T,C/T, C/C
# AKT1 genotype was a significant predictor of acute psychotomimetic symptoms (P=0.015), with increasing ‘dosage’ of C allele being associated with increased acute psychotomimetic symptoms induced by cannabis (see Figure 1).
compute.es::tes(2.443 , n_morgan/2, n_morgan/2, dig=20)
pEstimate_morgan_akt=data.frame(pEstimate=0.015, Ntot=n_morgan)

# Gender (male vs. female)
compute.es::tes(1.042 , n_morgan/2, n_morgan/2, dig=20) # Gender estimates needs to reflect that females have more psych sympotoms when compared to men
# i.e. ES in meta reflects that males have higher estimates => therefore reverse the d obtained from Morgan
pEstimate_morgan_gender=data.frame(pEstimate=NA, Ntot=n_morgan) # NS

# Schizotype
compute.es::tes(8.410 , n_morgan/2, n_morgan/2, dig=20)  # Schizotype
pEstimate_morgan_schizotype=data.frame(pEstimate=NA, Ntot=n_morgan) # P<0.01

# Cannabis dependency (present vs. absent)
compute.es::tes(0.688 , n_morgan/2, n_morgan/2, dig=20) # Cannabis Dependence (present vs absent)
pEstimate_morgan_canDep=data.frame(pEstimate=NA, Ntot=n_morgan) # NS

# Ethnicity # Ethnicity (white caucasion vs. others)
compute.es::tes(1.614 , n_morgan/2, n_morgan/2, dig=20)
pEstimate_morgan_ethnicity=data.frame(pEstimate=NA, Ntot=n_morgan) # NS

# YearsCannabisUse
compute.es::tes(2.019 , n_morgan/2, n_morgan/2, dig=20) # higher number of years of cannabis use linked to less actue psychosis-like symptoms => reverse d
pEstimate_morgan_yearsCan=data.frame(pEstimate=NA, Ntot=n_morgan) # P<0.05

# COMT (rs4680; A/A, A/G, G/G)
# rs4680 = A = Met
# rs4680 = G = Val
# COMT (Met/Met; Val/Met; Val/Va)
compute.es::tes(0.691 , n_morgan/2, n_morgan/2, dig=20)
pEstimate_morgan_comt=data.frame(pEstimate=NA, Ntot=n_morgan) # NA

# THC/CBD ratio
n_morgan_sub=303
compute.es::tes(0.799 , n_morgan_sub/2, n_morgan_sub/2, dig=20)
pEstimate_morgan_strain=data.frame(pEstimate=NA, Ntot=n_morgan_sub) # NA

# NO RATES REPORTED

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Curran (2019) ====================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Curran, H. Valerie, Chandni Hindocha, Celia JA Morgan, Natacha Shaban, Ravi K. Das, and Tom P. Freeman. "Which biological and self-report measures of cannabis use predict cannabis dependency and acute psychotic-like effects?." Psychological medicine 49, no. 9 (2019): 1574-1580.

curran2019_n=410
#
# Cannabis on psychosis-like outcomes
# CAPS = change in psychosis-like symptoms following cannabis consumption

# PSI
# Cannabis increased psychotic-like symptoms on the PSI (t = 10.067, p < 0.001, d = 0.516)
curran2019_dose_PSI_t=10.067
curran2019_dose_PSI_df=383
extract_curran2019_dose_PSI=convertT_dependent(curran2019_dose_PSI_t, curran2019_dose_PSI_df, curran2019_dose_PSI_df-1)
pEstimate_curran2019_dose_PSI=data.frame(pEstimate=NA, Ntot=curran2019_dose_PSI_df-1) # p < 0.001

# BPRS
# Cannabis increased psychotic-like symptoms on the BPRS (t = 7.857, p < 0.001, d = 0.413).
curran2019_dose_BPRS_t=7.857
curran2019_dose_BPRS_df=401 # For a t-test the degrees of freedom of the single mean is n-1
extract_curran2019_dose_BPRS=convertT_dependent(curran2019_dose_BPRS_t, curran2019_dose_BPRS_df, curran2019_dose_BPRS_df-1)
pEstimate_curran2019_dose_BPRS=data.frame(pEstimate=NA, Ntot=curran2019_dose_BPRS_df-1) # p < 0.001

# Predictor: Cannabis dependence YES/NO
curran2019_cannabis_dependence_n=412
curran2019_PSI_cannabis_dependence_r=-0.036
res(curran2019_PSI_cannabis_dependence_r, n=curran2019_cannabis_dependence_n, dig=20)
pEstimate_curran2019_PSI_cannabis_dependence=data.frame(pEstimate=pValueNS_allStudies, Ntot=curran2019_cannabis_dependence_n) # NS
# BPRS
curran2019_BPRS_cannabis_dependence_r=-0.023
res(curran2019_BPRS_cannabis_dependence_r, n=curran2019_cannabis_dependence_n, dig=20)
pEstimate_curran2019_BPRS_cannabis_dependence=data.frame(pEstimate=NA, Ntot=curran2019_cannabis_dependence_n) # NS

# Predictor: Cannabis dependence (DSM diagnosis)
curran2019_dsm_CUD_n=411
curran2019_PSI_dsm_CUD_r=-0.060
res(curran2019_PSI_dsm_CUD_r, n=curran2019_dsm_CUD_n, dig=20)
pEstimate_curran2019_PSI_dsm_CUD=data.frame(pEstimate=NA, Ntot=curran2019_dsm_CUD_n) # NS
# BPRS
curran2019_BPRS_dsm_CUD_r=-0.068
res(curran2019_BPRS_dsm_CUD_r, n=curran2019_dsm_CUD_n, dig=20)
pEstimate_curran2019_BPRS_dsm_CUD=data.frame(pEstimate=NA, Ntot=curran2019_dsm_CUD_n) # NS

# Predictor: Age
curran2019_age_n=416
curran2019_PSI_age_r=-0.032
res(curran2019_PSI_age_r, n=curran2019_age_n, dig=20)
pEstimate_curran2019_PSI_age=data.frame(pEstimate=NA, Ntot=curran2019_age_n) # NS
# BPRS
curran2019_BPRS_age_r=-0.127
res(curran2019_BPRS_age_r, n=curran2019_age_n, dig=20)
pEstimate_curran2019_BPRS_age=data.frame(pEstimate=NA, Ntot=curran2019_age_n) # p≤ .05

# Predictor: gender (increasing risk with being female => alinged directions with findings from Morgan 2016)
curran2019_gender_n=418
curran2019_PSI_gender_r=-0.019 # direction has do reflect greater impairment in females => leave negative as male is reference group in meta
res(curran2019_PSI_gender_r, n=curran2019_gender_n, dig=20)
pEstimate_curran2019_PSI_gender=data.frame(pEstimate=NA, Ntot=curran2019_gender_n) # NS
# BPRS
curran2019_BPRS_gender_r=-0.224
res(curran2019_BPRS_gender_r, n=curran2019_gender_n, dig=20)
pEstimate_curran2019_BPRS_gender=data.frame(pEstimate=NA, Ntot=curran2019_gender_n) # NS

# Predictor: THC percentage in smoked cannabis
curran2019_perTHCcontent_n=366
curran2019_PSI_perTHCcontent_r=-0.083
res(curran2019_PSI_perTHCcontent_r, n=curran2019_perTHCcontent_n, dig=20)
pEstimate_curran2019_PSI_perTHCcontent=data.frame(pEstimate=NA, Ntot=curran2019_perTHCcontent_n) # NS
# BPRS
curran2019_BPRS_perTHCcontent_r=-0.028 # BPRS correlated negatively THC in hair
res(curran2019_BPRS_perTHCcontent_r, n=curran2019_perTHCcontent_n, dig=20)
pEstimate_curran2019_BPRS_perTHCcontent=data.frame(pEstimate=NA, Ntot=curran2019_perTHCcontent_n) # NS

# Predictor: CBD percentage in smoked cannabis
curran2019_perCBDcontent_n=366
curran2019_PSI_perCBDcontent_r=0.037
res(curran2019_PSI_perCBDcontent_r, n=curran2019_perCBDcontent_n, dig=20)
pEstimate_curran2019_PSI_perCBDcontent=data.frame(pEstimate=NA, Ntot=curran2019_perCBDcontent_n) # NS
# BPRS
curran2019_BPRS_perCBDcontent_r=-0.067
res(curran2019_BPRS_perCBDcontent_r, n=curran2019_perCBDcontent_n, dig=20)
pEstimate_curran2019_BPRS_perCBDcontent=data.frame(pEstimate=NA, Ntot=curran2019_perCBDcontent_n) # NS

# Predictor: urine THC-COOH concentrations
curran2019_THCconcentration_urine_n=348
curran2019_PSI_THCconcentration_urine_r=-0.196
res(curran2019_PSI_THCconcentration_urine_r, n=curran2019_THCconcentration_urine_n, dig=20)
pEstimate_curran2019_PSI_THCconcentration_urine=data.frame(pEstimate=NA, Ntot=curran2019_THCconcentration_urine_n) # *** p≤ .001
# BPRS
curran2019_BPRS_THCconcentration_urine_r=-0.078
res(curran2019_BPRS_THCconcentration_urine_r, n=curran2019_THCconcentration_urine_n, dig=20)
pEstimate_curran2019_BPRS_THCconcentration_urine=data.frame(pEstimate=NA, Ntot=curran2019_THCconcentration_urine_n) # NS

# Predictor: hair THC
curran2019_THCconcentration_hair_n=412
curran2019_PSI_THCconcentration_hair_r=-0.094
res(curran2019_PSI_THCconcentration_hair_r, n=curran2019_THCconcentration_hair_n, dig=20)
pEstimate_curran2019_PSI_THCconcentration_hair=data.frame(pEstimate=NA, Ntot=curran2019_THCconcentration_hair_n) # NS
# BPRS
curran2019_BPRS_THCconcentration_hair_r=-0.135
res(curran2019_BPRS_THCconcentration_hair_r, n=curran2019_THCconcentration_hair_n, dig=20)
pEstimate_curran2019_BPRS_THCconcentration_hair=data.frame(pEstimate=NA, Ntot=curran2019_THCconcentration_hair_n) # <0.01

# Predictor: hair CBD
curran2019_CBDconcentration_hair_n=412
curran2019_PSI_CBDconcentration_hair_r=0.071
res(curran2019_PSI_CBDconcentration_hair_r, n=curran2019_CBDconcentration_hair_n, dig=20)
pEstimate_curran2019_PSI_CBDconcentration_hair=data.frame(pEstimate=NA, Ntot=curran2019_CBDconcentration_hair_n) # NS
# BPRS
curran2019_BPRS_CBDconcentration_hair_r=0.123 # reversed estimates so that presence of CBD is the reference group
res(curran2019_BPRS_CBDconcentration_hair_r, n=curran2019_CBDconcentration_hair_n, dig=20)
pEstimate_curran2019_BPRS_CBDconcentration_hair=data.frame(pEstimate=NA, Ntot=curran2019_CBDconcentration_hair_n) # <0.05

# Predictor: hair CBN
curran2019_CBNconcentration_hair_n=344
curran2019_PSI_CBNconcentration_hair_r=-0.102
res(curran2019_PSI_CBNconcentration_hair_r, n=curran2019_CBNconcentration_hair_n, dig=20)
pEstimate_curran2019_PSI_CBNconcentration_hair=data.frame(pEstimate=NA, Ntot=curran2019_CBNconcentration_hair_n) # <0.05
# BPRS
curran2019_BPRS_CBNconcentration_hair_r=-0.116
res(curran2019_BPRS_CBNconcentration_hair_r, n=curran2019_CBNconcentration_hair_n, dig=20)
pEstimate_curran2019_BPRS_CBNconcentration_hair=data.frame(pEstimate=NA, Ntot=curran2019_CBNconcentration_hair_n) # <0.05

# Predictor: hair THC-COOH
curran2019_THC_COOHhair_n=344
curran2019_PSI_THC_COOHhair_r=-0.068
res(curran2019_PSI_THC_COOHhair_r, n=curran2019_THC_COOHhair_n, dig=20)
pEstimate_curran2019_PSI_THC_COOHhair=data.frame(pEstimate=NA, Ntot=curran2019_THC_COOHhair_n) # NS
# BPRS
curran2019_BPRS_THC_COOHhair_r=-0.087
res(curran2019_BPRS_THC_COOHhair_r, n=curran2019_THC_COOHhair_n, dig=20)
pEstimate_curran2019_BPRS_THC_COOHhair=data.frame(pEstimate=NA, Ntot=curran2019_THC_COOHhair_n) # NS

# Predictor: hair THC-OH
curran2019_THC_OHhair_n=344
curran2019_PSI_THC_OHhair_r=-0.021
res(curran2019_PSI_THC_OHhair_r, n=curran2019_THC_OHhair_n, dig=20)
pEstimate_curran2019_PSI_THC_OHhair=data.frame(pEstimate=NA, Ntot=curran2019_THC_OHhair_n) # NS
# BPRS
curran2019_BPRS_THC_OHhair_r=-0.072
res(curran2019_BPRS_THC_OHhair_r, n=curran2019_THC_OHhair_n, dig=20)
pEstimate_curran2019_BPRS_THC_OHhair=data.frame(pEstimate=NA, Ntot=curran2019_THC_OHhair_n) # NS

# Predictor: Age of onset of cannabis use
# Note: age of first use was positively associated with a greater CAPS
# => Being older at first use of cannabis predicted higher levels of acute psychotic-like symptoms,
# => suggesting that those individuals who started using at a younger age had developed greater tolerance to these acute effects of the drug.
curran2019_age_firstUse_n=413
curran2019_PSI_age_firstUse_r=0.206
res(curran2019_PSI_age_firstUse_r, n=curran2019_age_firstUse_n, dig=20)
pEstimate_curran2019_PSI_age_firstUse=data.frame(pEstimate=NA, Ntot=curran2019_age_firstUse_n) # *** p≤ .001
# BPRS
curran2019_BPRS_age_firstUse_r=0.032
res(curran2019_BPRS_age_firstUse_r, n=curran2019_age_firstUse_n, dig=20)
pEstimate_curran2019_BPRS_age_firstUse=data.frame(pEstimate=NA, Ntot=curran2019_age_firstUse_n) # NS

# Predictor: Recency of use (days last used)
curran2019_recency_days_n=401
curran2019_PSI_recency_days_r=0.162
res(curran2019_PSI_recency_days_r, n=curran2019_recency_days_n, dig=20)
pEstimate_curran2019_PSI_recency_days=data.frame(pEstimate=NA, Ntot=curran2019_recency_days_n) # ** p≤ .01
# BPRS
curran2019_BPRS_recency_days_r=0.069
res(curran2019_BPRS_recency_days_r, n=curran2019_recency_days_n, dig=20)
pEstimate_curran2019_BPRS_recency_days=data.frame(pEstimate=NA, Ntot=curran2019_recency_days_n) # NS

# Predictor: Days per month cannabis used
curran2019_frequecny_month_n=398
curran2019_PSI_frequecny_month_r=-0.141
res(curran2019_PSI_frequecny_month_r, n=curran2019_frequecny_month_n, dig=20)
pEstimate_curran2019_PSI_frequecny_month=data.frame(pEstimate=NA, Ntot=curran2019_frequecny_month_n) # ** p≤ .01
# BPRS
curran2019_BPRS_frequecny_month_r=-0.049
res(curran2019_BPRS_frequecny_month_r, n=curran2019_frequecny_month_n, dig=20)
pEstimate_curran2019_BPRS_frequecny_month=data.frame(pEstimate=NA, Ntot=curran2019_frequecny_month_n) # NS

# Predictor: Time to smoke 3.5g (days)
# coded as follows: shorter time = > higher symptoms
# higher values in predictor indicating shorter time needed to smoked 3.5g
curran2019_time_smoked3.5g_n=392
curran2019_PSI_time_smoked3.5g_r=-0.143
res(curran2019_PSI_time_smoked3.5g_r, n=curran2019_time_smoked3.5g_n, dig=20)
pEstimate_curran2019_PSI_time_smoked3.5g=data.frame(pEstimate=NA, Ntot=curran2019_time_smoked3.5g_n) # ** p≤ .01
# BPRS
curran2019_BPRS_time_smoked3.5g_r=0.023
res(curran2019_BPRS_time_smoked3.5g_r, n=curran2019_time_smoked3.5g_n, dig=20)
pEstimate_curran2019_BPRS_time_smoked3.5g=data.frame(pEstimate=NA, Ntot=curran2019_time_smoked3.5g_n) # NS

# Predictor: Preference for high potency cannabis
# This included questions about participants’ preference (yes/no) for high potency cannabis (skunk, sensimilla)
curran2019_highPotency_preference_n=413
curran2019_PSI_highPotency_preference_r=-0.076
res(curran2019_PSI_highPotency_preference_r, n=curran2019_highPotency_preference_n, dig=20)
pEstimate_curran2019_PSI_highPotency_preference=data.frame(pEstimate=NA, Ntot=curran2019_highPotency_preference_n)
# BPRS
curran2019_BPRS_highPotency_preference_r=-0.073
res(curran2019_BPRS_highPotency_preference_r, n=curran2019_highPotency_preference_n, dig=20)
pEstimate_curran2019_BPRS_highPotency_preference=data.frame(pEstimate=NA, Ntot=curran2019_highPotency_preference_n) # NS

# Predictor: Money spent per week on cannabis(£)
curran2019_money_spent_week_n=411
curran2019_PSI_money_spent_week_r=-0.144
res(curran2019_PSI_money_spent_week_r, n=curran2019_money_spent_week_n, dig=20)
pEstimate_curran2019_PSI_money_spent_week=data.frame(pEstimate=NA, Ntot=curran2019_money_spent_week_n)
# BPRS
curran2019_BPRS_money_spent_week_r=-0.020
res(curran2019_BPRS_money_spent_week_r, n=curran2019_money_spent_week_n, dig=20)
pEstimate_curran2019_BPRS_money_spent_week=data.frame(pEstimate=NA, Ntot=curran2019_money_spent_week_n) # NS

# NO RATES REPORTED


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Henquet ===================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Henquet, Cécile, Araceli Rosa, Lydia Krabbendam, Sergi Papiol, Lourdes Fa_anás, Marjan Drukker, Johannes G. Ramaekers, and Jim van Os. "An experimental study of catechol-o-methyltransferase Val 158 Met moderation of _-9-tetrahydrocannabinol-induced effects on psychosis and cognition." Neuropsychopharmacology 31, no. 12 (2006): 2748.

n_henquet_tot=19+35+20
# no significant condition 􏰅 genotype interaction was observed on the psychotic symptom outcome (w2 1⁄4 1.19, df 1⁄4 1, p 1⁄4 0.27)

# Effect of dose of THC
# THC was not associated with a significant increase in positive symptoms
beta_henquet_THC=0.04
beta_lCI_henquet_THC=0.02
beta_uCI_henquet_THC=0.09
n_effective_henquet=n_effective_dependent(n_henquet_tot, r=r_withinSubj_between_timepoints)
se_henquet_THC = (beta_uCI_henquet_THC-beta_lCI_henquet_THC)/(2*1.96) # calculate the standard error
# Test eqution for beta estimates
beta_to_d_independent(beta=beta_henquet_THC, se=se_henquet_THC, n=n_effective_henquet) # Problem: gives lower p-values
pEstimate_henquet_THCdose=data.frame(pEstimate=0.19, Ntot=n_henquet_tot)

# Dose of THC
# 300mg THC/kg body weight in tobacco cigarettes

# Effect of dose of THC x gene interaction
# Comt, with degree of Val loading (0 1⁄4 0 Val alleles, 1 1⁄4 1 Val allele, 21⁄42 Val alleles).
# COMT (A/A, A/G, G/G)
# A = Met
# G = Val
# COMT (Met/Met; Val/Met; Val/Va)
chies(1.19, n_effective_henquet, dig=50)
pEstimate_henquet_gen_can_interaction=data.frame(pEstimate=0.27, Ntot=n_henquet_tot)
# NO RATES REPORTED



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Cuttler ===================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Cuttler, Carrie, Laurie K. Mischley, and Michelle Sexton. "Sex differences in cannabis use and effects: a cross-sectional survey of cannabis users." Cannabis and cannabinoid research 1, no. 1 (2016): 166-175.

# Hallucinations
# Male
allA=1370
percA=4
NriskA_case=round((allA/100)*percA,0)
NriskA_control=allA-NriskA_case
# Female
allB=1004
percB=3.4
NriskB_case=round((allB/100)*percB,0)
NriskB_control=allB-NriskB_case
# Get chi square estimates
labelA="Male"
labelB="Female"
extract_cuttler_hallucinations=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_cuttler_hallucinations=data.frame(pEstimate=0.38, Ntot=extract_cuttler_hallucinations$N.total)


# Paranoia
# Male
allA=1370
percA=14.5
NriskA_case=round((allA/100)*percA,0)
NriskA_control=allA-NriskA_case
# Female
allB=1004
percB=15.2
NriskB_case=round((allB/100)*percB,0)
NriskB_control=allB-NriskB_case
# Get chi square estimates
labelA="Male"
labelB="Female"
extract_cuttler_paranoia=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_cuttler_paranoia=data.frame(pEstimate=0.59, Ntot=extract_cuttler_paranoia$N.total)


# Rates
n_male=1370
n_female=1004
cases_paranoia_male=(n_male/100)*14.5
cases_paranoia_female=(n_female/100)*15.2
cases_hallucination_male=(n_male/100)*4
cases_hallucination_female=(n_female/100)*3.4

# RATES
cuttler2016_canParDF=data.frame(event=cases_paranoia_male+cases_paranoia_female, n=n_male+n_female)
cuttler2016_canHalDF=data.frame(event=cases_hallucination_male+cases_hallucination_female, n=n_male+n_female)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Vadhan ====================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
n_Vadhan=12

# ======== Extract Cohen D ============
# "I feel paranoid"
# placebo marijuana cigarettes containing one of two Δ9 -THC concentrations:
# 0.0% or 5.5% (all cigarettes contained <0.5% cannabidiol)

# ++++ Effect of dose: In high risk individuals +++
vadhan_cases_n=6
vadhan_cases_5percTHC_t1_m=27.3
vadhan_cases_placebo_t1_m=0
vadhan_cases_5percTHC_t1_se=9.8
vadhan_cases_placebo_t1_se=0
vadhan_cases_5percTHC_t1_sd=vadhan_cases_5percTHC_t1_se*sqrt(vadhan_cases_n)
vadhan_cases_placebo_t1_sd=vadhan_cases_placebo_t1_se*sqrt(vadhan_cases_n)
# Derive cohen d
extract_vadhan_cases_paranoia_smoked_0perc_5.5percTHC=d_meanDiff_dependent(m1=vadhan_cases_5percTHC_t1_m, m2=vadhan_cases_placebo_t1_m, sd1=vadhan_cases_5percTHC_t1_sd , sd2=vadhan_cases_placebo_t1_sd, n=vadhan_cases_n, r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_vadhan_cases_paranoia_smoked_0perc_5.5percTHC=data.frame(pEstimate=0.03, Ntot=vadhan_cases_n)



# ++++ Effect of dose: In healthyindividuals +++
vadhan_healthy_n=6
vadhan_healthy_5percTHC_t1_m=2.7
vadhan_healthy_placebo_t1_m=0
vadhan_healthy_5percTHC_t1_se=6.1
vadhan_healthy_placebo_t1_se=0
vadhan_healthy_5percTHC_t1_sd=vadhan_healthy_5percTHC_t1_se*sqrt(vadhan_healthy_n)
vadhan_healthy_placebo_t1_sd=vadhan_healthy_placebo_t1_se*sqrt(vadhan_healthy_n)
# Derive cohen d
extract_vadhan_healthy_paranoia_smoked_0perc_5.5percTHC=d_meanDiff_dependent(m1=vadhan_healthy_5percTHC_t1_m, m2=vadhan_healthy_placebo_t1_m, sd1=vadhan_healthy_5percTHC_t1_sd , sd2=vadhan_healthy_placebo_t1_sd, n=vadhan_healthy_n, r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_vadhan_healthy_paranoia_smoked_0perc_5.5percTHC=data.frame(pEstimate=0.35, Ntot=vadhan_healthy_n)

# ++++ Effect of condition: UHR vs healthy (at time point 1 5% THC) +++
mes(vadhan_cases_5percTHC_t1_m, vadhan_healthy_5percTHC_t1_m, vadhan_cases_5percTHC_t1_sd, vadhan_healthy_5percTHC_t1_sd, vadhan_cases_n, vadhan_healthy_n, dig=20)
pEstimate_vadhan_caseness_paranoia_smoked_0perc_5.5percTHC=data.frame(pEstimate=NA, Ntot=vadhan_cases_n+vadhan_healthy_n)

# NO RATES REPORTED

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Sami ======================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Sami, Musa, Caitlin Notley, Christos Kouimtsidis, Michael Lynskey, and Sagnik Bhattacharyya. "Psychotic-like experiences with cannabis use predict cannabis cessation and desire to quit: a cannabis discontinuation hypothesis." Psychological medicine 49, no. 1 (2019): 103-112.
# ======== Extract Cohen D ============

# Intention to quit: Within continued cannabis users, future intention to quit was significantly associated with greater psychotic-like experiences
# t = 3.95, p < 0.001 (Table 4)

# Ceased vs Continues cannabis user and psychosis-like experiences
Sami_ceased_N=260
Sami_continued_N=839
# Mean estimates (all) in Table 2
Sami_ceased_m=10.02
Sami_continued_m=7.98
# Standard error in Table 2
Sami_ceased_se=0.28
Sami_continued_se=0.08
# Convert SE to SD
Sami_ceased_sd=Sami_ceased_se*sqrt(Sami_ceased_N)
Sami_continued_sd=Sami_continued_se*sqrt(Sami_continued_N)
# Get cohen d
extracted_Sami_PsychLike_ceased_continued=mes(Sami_continued_m, Sami_ceased_m, Sami_continued_sd, Sami_ceased_sd, Sami_continued_N, Sami_ceased_N, dig=20)
pEstimate_Sami_psychlike_ceased_vs_continued=data.frame(pEstimate=NA, Ntot=Sami_ceased_N+Sami_continued_N)  # p < 0.001


# Predictor 2:  Male vs. female in ceased cannabis user and psychosis-like experiences
Sami_ceased_male_N=102 # n male users in ceased group
Sami_ceased_female_N=157 # n female users in ceased group
# Mean estimates (ceased group) in Table 2
Sami_ceased_male_m=9.93 # mean in male users in ceased group
Sami_ceased_female_m=10.04 # mean in female users in ceased group
# Standard error in Table 2
Sami_ceased_male_se=0.44 # standard error in male users in ceased group
Sami_ceased_female_se=0.36 # standard error in female users in ceased group
# Convert SE to SD
Sami_ceased_male_sd=Sami_ceased_male_se*sqrt(Sami_ceased_male_N)
Sami_ceased_female_sd=Sami_ceased_female_se*sqrt(Sami_ceased_female_N)
# Get cohen d
extracted_Sami_PsychLike_ceased_sex=mes(Sami_ceased_male_m, Sami_ceased_female_m, Sami_ceased_male_sd, Sami_ceased_female_sd, Sami_ceased_male_N, Sami_ceased_female_N, dig=20)
pEstimate_Sami_psychlike_gender_ceased=data.frame(pEstimate=NA, Ntot=Sami_ceased_male_N+Sami_ceased_female_N)  # not reported


# Predictor 3: Male vs. female in continued cannabis user and psychosis-like experiences
Sami_continued_male_N=657 # n male users in continued group
Sami_continued_female_N=172 # n female users in continued group
# Mean estimates (continued group) in Table 2
Sami_continued_male_m=7.90 # mean in male users in continued group
Sami_continued_female_m=8.24 # mean in female users in continued group
# Standard error in Table 2
Sami_continued_male_se=0.09 # standard error in male users in continued group
Sami_continued_female_se=0.23 # standard error in female users in continued group
# Convert SE to SD
Sami_continued_male_sd=Sami_continued_male_se*sqrt(Sami_continued_male_N)
Sami_continued_female_sd=Sami_continued_female_se*sqrt(Sami_continued_female_N)
# Get cohen d
extracted_Sami_PsychLike_continued_sex=mes(Sami_continued_male_m, Sami_continued_female_m, Sami_continued_male_sd, Sami_continued_female_sd, Sami_continued_male_N, Sami_continued_female_N, dig=20)
pEstimate_Sami_psychlike_gender_continued=data.frame(pEstimate=NA, Ntot=Sami_continued_male_N+Sami_continued_female_N)  # not reported


# Future intention to quit (yes vs no) in continued cannabis user and psychosis-like experiences
Sami_continued_quitYes_N=148
Sami_continued_quitNo_N=691
# Mean estimates (all) in Table 2
Sami_continued_quitYes_m=8.88
Sami_continued_quitNo_m=7.78
# Standard error in Table 2
Sami_continued_quitYes_se=0.27
Sami_continued_quitNo_se=0.08
# Convert SE to SD
Sami_continued_quitYes_sd=Sami_continued_quitYes_se*sqrt(Sami_continued_quitYes_N)
Sami_continued_quitNo_sd=Sami_continued_quitNo_se*sqrt(Sami_continued_quitNo_N)
# Get cohen d
extracted_Sami_PsychLike_quit=mes(Sami_continued_quitYes_m, Sami_continued_quitNo_m, Sami_continued_quitYes_sd, Sami_continued_quitNo_se, Sami_continued_quitYes_N, Sami_continued_quitNo_N, dig=20)
pEstimate_Sami_continued_quit_yes_vs_no=data.frame(pEstimate=0.0009, Ntot=Sami_continued_quitNo_N+Sami_continued_quitYes_N) # p < 0.001


# RATES
sami2019survey_canParDF=data.frame(event=524, n=1117)
sami2019survey_canHalDF=data.frame(event=100, n=1117)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Spindle ===================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Spindle, Tory R., Edward J. Cone, Nicolas J. Schlienz, John M. Mitchell, George E. Bigelow, Ronald Flegel, Eugene Hayes, and Ryan Vandrey. "Acute effects of smoked and vaporized cannabis in healthy adults who infrequently use cannabis: a crossover trial." JAMA network open 1, no. 7 (2018): e184841-e184841.

#  All participants completed six 8.5-hour outpatient sessions that differed only by inhalation method (smoked vs vaporized) and THC dose (0 mg, 10 mg, or 25 mg).
# Seventeen healthy adult participants (9 men and 8 women) completed the study
n_Spindle=17

# ======== Extract Cohen D ============
# ++++ Effect of dose: Smoke condition +++
corChangeTHC_smoke=(0.38+0.51)/2 # From supplement table: average correlations (Pearson r) between individual change from baseline values for 2 doses of THC Levels and subjective drug effects
# SMCC = the standardized mean change using change score standardization.
extract_spindle_paranoia_smoked_0mg_25mgTHC=d_meanDiff_dependent(m2=0.1, m1=10, sd2=.5 , sd1=22, n=17, r=r_withinSubj_between_measures)
extract_spindle_paranoia_smoked_0mg_10mgTHC=d_meanDiff_dependent(m2=0.1, m1=5.5, sd2=.5 , sd1=17.7, n=17, r=r_withinSubj_between_measures)
# Double check using escale
# effectSmoke_Spindle=unlist(escalc(measure = "SMCC",m1i=0.1, m2i=10, sd1i=.5 , sd2i=22, ni=17, ri=corChangeTHC_smoke))

# ++++ Effect of dose: Vape condition +++
# For both smoked and vaporized cannabis, at both active doses, whole-blood THC concentrations were positively correlated with subjective ratings of drug effect (r > 0.37)
corChangeTHC_vape=(0.54+0.52)/2 # average correlations (Pearson r) between individual change from baseline values for 2 doses of THC Levels and subjective drug effects
extract_spindle_paranoia_vaped_0mg_10mgTHC=d_meanDiff_dependent(m2=0, m1=7.9, sd2=0 , sd1=16.9, n=17, r=r_withinSubj_between_measures)
extract_spindle_paranoia_vaped_0mg_25mgTHC=d_meanDiff_dependent(m2=0, m1=17.4, sd2=0 , sd1=30, n=17, r=r_withinSubj_between_measures)

# ++++ Assess dose response relationshops
# Dose response effects - vapes
extract_spindle_paranoia_vaped_10mg_25mgTHC= d_meanDiff_dependent(m1=17.4,
                                                                  m2=7.9,
                                                                  sd1=30,
                                                                  sd2=16.9,
                                                                  n=17,
                                                                  r=r_DoseResponse)
pEstimate_spindle_paranoia_vaped_10mg_25mgTHC=data.frame(pEstimate=NA, Ntot=n_Spindle)

# Dose response effects - smoked
extract_spindle_paranoia_smoked_10mg_25mgTHC= d_meanDiff_dependent(m1=10,
                                                                   m2=5.5,
                                                                   sd1=22,
                                                                   sd2=17.7,
                                                                   n=17,
                                                                   r=r_DoseResponse)
pEstimate_spindle_paranoia_smoked_10mg_25mgTHC=data.frame(pEstimate=NA, Ntot=n_Spindle)

# ++++ Effect of condition: Vape vs smoke (at 25mg THC) +++
# Compare vape and smoked condition
extract_spindle_paranoia_25mgTHC_smoked_vs_vaped=d_meanDiff_dependent( m1=17.4, m2=10, sd1=22, sd2=30 ,  n=17, r=r_withinSubj_between_measures)
#extract_spindle_delusional_10mgTHC_smoked_vs_vaped=d_meanDiff_dependent(m2=7.9, m1=5.5, sd2=16.9 , sd1=17.7, n=17, r=r_withinSubj_between_measures)


# ======== Extract original p-value estimate ============
# Study-deinfed p-value threhold = significant difference from 0-mg THC within that route of administration (allPvalues<.025).
# Results: All comparisons NS, execpt the effects of the drug (25mg) in the vape condiction

# ++++ Effect of dose: Smoke condition +++
pEstimate_spindle_paranoia_smoked_0mg_10mgTHC=data.frame(pEstimate=NA, Ntot=n_Spindle) # NS, ie below study-defined p-value threshold
pEstimate_spindle_paranoia_smoked_0mg_25mgTHC=data.frame(pEstimate=NA, Ntot=n_Spindle) # NS, ie below study-defined p-value threshold
# ++++ Effect of dose: Vape condition +++
pEstimate_spindle_paranoia_vaped_0mg_10mgTHC=data.frame(pEstimate=NA, Ntot=n_Spindle) # NS, ie below study-defined p-value threshold
pEstimate_spindle_paranoia_vaped_0mg_25mgTHC=data.frame(pEstimate=0.024, Ntot=n_Spindle) # significant difference from 0-mg THC at Pvalues<.025).
# Vape vs. smoke (25mg condiction = highest dose of THC)
# Significant difference between vape and smoke contidion (p<0.05)
pEstimate_Spindle_paranoia_vaporizer_smoke_25mgTHC=data.frame(pEstimate=NA, Ntot=n_Spindle)


# ==== Dose-response effects:
# => Not formally assessed

# NO RATES REPORTED

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Sholler (2020) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sholler, D.J., Strickland, J.C., Spindle, T.R., Weerts, E.M. and Vandrey, R., 2020. Sex differences in the acute effects of oral and vaporized cannabis among healthy adults. Addiction biology, p.e12968.

# Includes 4 studies

#         1) - the effects of smoked and vaporized cannabis (0, 10, or 25 mg THC)
#            - within-subjects design (only data from vaporized dose sessions were included here).3,20
#            - REF: Spindle TR, Bonn-Miller MO, Vandrey R. Changing landscape of cannabis: novel products, formulations, and methods of administration. Curr Opin Psychol. 2019;30:98-102.
#            - REF: Spindle TR, Cone EJ, Schlienz NJ, et al. Acute pharmacokinetic profile of smoked and vaporized cannabis in human blood and oral fluid. J Anal Toxicol. 2019;43(4):233-258.

#         2) - effects of oral (0, 10, or 25 mg THC) or vaporized (0, 5, or 20 mg THC) cannabis
#            - within-subjects design
#            - REF: Spindle TR, Grabnauer M, Martin E, Vandrey R. Assessment of impairment following oral and vaporized cannabis administration in infre- quent cannabis users. 81st Annual Meeting of the College on Problems of Drug Dependence; San Antonio, TX; 2020.

#         3) - effects of oral cannabis (0, 10, 25, or 50 mg THC)
#            - within-subjects design
#            - REF: Schlienz NJ, Spindle TR, Cone EJ, et al. Pharmacodynamic dose effects of oral cannabis ingestion in healthy adults who infrequently use cannabis. Drug Alcohol Depend. 2020;211:107969. Online ahead of print.

#         4) - effects of oral cannabis (10, 25, or 50 mg THC)
#            - between-subjects design
#            - REF: Vandrey R, Herrmann ES, Mitchell JM, et al. Pharmacokinetic profile of oral cannabis in humans: blood and oral fluid disposition and relation to pharmacodynamic outcomes. J Anal Toxicol. 2017;41(2):83-99.

shollerMEGA_THC_n=50
n_effective_shollerMEGA_THC=n_effective_dependent(shollerMEGA_THC_n, r=r_withinSubj_between_timepoints)

# Dose-response
# note: dose-response effects not previouslt reported in the individual studies
# For the purpose of comparison in the present analyses, doses were collapsed into placebo, low (5 or 10 mg) and high (20 or 25 mg) dose categories. This categorical parameterization allowed for loosened assumptions of linear dose effects and collapsed similar doses for parsimony.
shollerMEGA_THC_doseresponse_p=0.0009 # <.001
extract_shollerMEGA_THC_doseresponse=pes(shollerMEGA_THC_doseresponse_p, n_effective_shollerMEGA_THC/2, n_effective_shollerMEGA_THC/2, dig=50)
pEstimate_shollerMEGA_THC_doseresponse=data.frame(pEstimate=NA, Ntot=shollerMEGA_THC_n)

# Gender
shollerMEGA_THC_sex_p=.130 # <.001 (0: male; 1: female) => females had higher subj experiences
extract_shollerMEGA_THC_sex=pes(shollerMEGA_THC_sex_p, n_effective_shollerMEGA_THC/2, n_effective_shollerMEGA_THC/2, dig=50)
pEstimate_shollerMEGA_THC_sex=data.frame(pEstimate=shollerMEGA_THC_sex_p, Ntot=shollerMEGA_THC_n)

# route
shollerMEGA_THC_route_p=.125 #  0:oral; 1:vaporized  => higher subj experiences for vaporizer
extract_shollerMEGA_THC_route=pes(shollerMEGA_THC_route_p, n_effective_shollerMEGA_THC/2, n_effective_shollerMEGA_THC/2, dig=50)
pEstimate_shollerMEGA_THC_route=data.frame(pEstimate=shollerMEGA_THC_route_p, Ntot=shollerMEGA_THC_n)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Spindle (2019) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Spindle, Tory R., Edward J. Cone, Nicolas J. Schlienz, John M. Mitchell, George E. Bigelow, Ronald Flegel, Eugene Hayes, and Ryan Vandrey. "Acute pharmacokinetic profile of smoked and vaporized cannabis in human blood and oral fluid." Journal of analytical toxicology 43, no. 4 (2019): 233-258.
# Only data for adverse drug reaction reported (rates)
spindle2019vapeTHC_halDF=data.frame(event=1, n=17)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Spindle (2020) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Spindle, Tory R., Edward J. Cone, Elia Goffi, Elise M. Weerts, John M. Mitchell, Ruth E. Winecker, George E. Bigelow, Ronald R. Flegel, and Ryan Vandrey. "Pharmacodynamic effects of vaporized and oral cannabidiol (CBD) and vaporized CBD-dominant cannabis in infrequent cannabis users." Drug and Alcohol Dependence (2020): 107937.
spindle2020_male_n=9
spindle2020_female_n=9
spindle2020_n=spindle2020_male_n+spindle2020_female_n


# ======= Dose of vaporized CBD-dominant cannabis (100 mg CBD; 3.7 mg THC) vs  placebo
# The 4 conditions were:
#       (1) 100 mg oral CBD and va- porized placebo cannabis
#       (2) oral placebo and 100 mg vaporized CBD
#       (3) oral placebo and vaporized CBD-dominant cannabis containing 100 mg CBD and 3.7 mg THC
#       (4) oral placebo and vaporized placebo cannabis (placebo condition)
# Derive cohen d

spindle2020_paranoia_canCBD_mean=0.8
spindle2020_paranoia_placebo_mean=0.2
spindle2020_paranoia_canCBD_sd=2.4
spindle2020_paranoia_placebo_sd=0.4
# Extract Cohen D
extract_spindle2020_paranoia_canCBD=d_meanDiff_dependent(m1=spindle2020_paranoia_canCBD_mean,
                                                         m2=spindle2020_paranoia_placebo_mean,
                                                         sd1=spindle2020_paranoia_canCBD_sd ,
                                                         sd2=spindle2020_paranoia_placebo_sd,
                                                         n=spindle2020_n, r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_spindle2020_paranoia_canCBD=data.frame(pEstimate=NA, Ntot=spindle2020_n) # Not reported



# ======= Effect of sex (results reported in supplement table)
# Women reported stronger subjective drug effects on several items compared to men following inhalation of vaporized CBD and CBD- dominant cannabis.
spindle2020_paranoia_sex_f=0.6
fes(spindle2020_paranoia_sex_f, spindle2020_male_n, spindle2020_female_n,  dig=20)
pEstimate_spindle2020_paranoia_sex=data.frame(pEstimate=NA, Ntot=spindle2020_n) # ns


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Schlienz (2020) ====================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Schlienz, Nicolas J., Tory R. Spindle, Edward J. Cone, Evan S. Herrmann, George E. Bigelow, John M. Mitchell, Ronald Flegel, Charles LoDico, and Ryan Vandrey. "Pharmacodynamic dose effects of oral cannabis ingestion in healthy adults who infrequently use cannabis." Drug and alcohol dependence (2020): 107969.

# consumed a cannabis-infused brownie that contained 0, 10, 25, or 50 mg THC
# Participants were told that they would receive cannabis brownies containing either 10, 25, or 50 mg THC (or placebo)
# testing confirmed the con- version of THC-A to THC and that the desired doses were reliably achieved in each product using our preparation procedures.

schlienz2020_edibleTHC_n=17

# Placebo vs 10mg
extract_schlienz2020_edibleTHC_0mg_10mgTHC=d_meanDiff_dependent(m1=1.5,
                                                                m2=0.3,
                                                                sd1=0.5 ,
                                                                sd2=0.1,
                                                                n=schlienz2020_edibleTHC_n,
                                                                r=r_withinSubj_between_measures)
pEstimate_schlienz2020_edibleTHC_0mg_10mgTHC=data.frame(pEstimate=NA, Ntot=schlienz2020_edibleTHC_n)

# Placebo vs 25mg
extract_schlienz2020_edibleTHC_0mg_25mgTHC=d_meanDiff_dependent(m1=4.3,
                                                                m2=0.3,
                                                                sd1=1.0,
                                                                sd2=0.1,
                                                                n=schlienz2020_edibleTHC_n,
                                                                r=r_withinSubj_between_measures)
pEstimate_schlienz2020_edibleTHC_0mg_25mgTHC=data.frame(pEstimate=NA, Ntot=schlienz2020_edibleTHC_n)

# Placebo vs 50mg
extract_schlienz2020_edibleTHC_0mg_50mgTHC=d_meanDiff_dependent(m1=11.0,
                                                                m2=0.3,
                                                                sd1=1.5,
                                                                sd2=0.1,
                                                                n=schlienz2020_edibleTHC_n,
                                                                r=r_withinSubj_between_measures)
pEstimate_schlienz2020_edibleTHC_0mg_50mgTHC=data.frame(pEstimate=NA, Ntot=schlienz2020_edibleTHC_n)


# Dose response effects
# Signi"cant e!ects of Dose were observed for ratings of drug effect on paranoia (F3,48 = 8.8, p < .0001),
n_effective_schlienz2020_edibleTHC=n_effective_dependent(schlienz2020_edibleTHC_n, r=r_withinSubj_between_timepoints)
pes(0.00009, n_effective_schlienz2020_edibleTHC/2, n_effective_schlienz2020_edibleTHC/2, dig=50)
pEstimate_schlienz2020_edibleTHC_doseTHC=data.frame(pEstimate=NA, Ntot=schlienz2020_edibleTHC_n) # p<0.0001



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Fuente ====================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Cannabis strains obtained at dispensaries within the U.S. are typically classified as one of 3 species designations: indica, sativa, and “hybrid” (cross-bred cannabis plants containing characteristics of both indica and sativa varieties).
# However, little is known regarding differences between indica and sativa in terms of psychoactive effects or variations in cannabinoid concentration (i.e., amount of each cannabinoid).

# ======== Extract Cohen D ============
M_Indica=0.043 # Extracted from supplementary material
M_Sativa=0.055
M_Hybrid=0.051
SD_Indica=0.045
SD_Sativa=0.053
SD_Hybrid=0.053
N_Indica=265
N_Sativa=171
N_Hybrid=451
# Mean difference between groups (indica vs, sativa)
mes(M_Indica, M_Sativa, SD_Indica, SD_Sativa, N_Indica, N_Sativa, dig=20)
# Mean difference between groups (indica vs, hybrid)
mes(M_Indica, M_Hybrid, SD_Indica, SD_Hybrid, N_Indica, N_Hybrid, dig=20)
# Mean difference between groups (hybrid vs. sativa)
mes(M_Hybrid, M_Sativa, SD_Hybrid, SD_Sativa, N_Hybrid, N_Sativa, dig=20)

# ======== Extract original p-value estimate ============

pEstimate_fuente2019_paranoia_indica_sativa=data.frame(pEstimate=NA, Ntot=N_Indica+N_Sativa)
pEstimate_fuente2019_paranoia_indica_hybrid=data.frame(pEstimate=NA, Ntot=N_Indica+N_Hybrid)
pEstimate_fuente2019_paranoia_sativa_hybrid=data.frame(pEstimate=NA, Ntot=N_Sativa+N_Hybrid)

# NO RATES REPORTED

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Mason (2009) ==============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Mason, O., C. J. A. Morgan, S. K. Dhiman, A. Patel, N. Parti, and H. V. Curran. "Acute cannabis use causes increased psychotomimetic experiences in individuals prone to psychosis." Psychological medicine 39, no. 6 (2009): 951-956.

# ++++ Effect of frequency of cannabis use +++
# Frequency of cannabis use on psychosis like experiences while under the influence of cannabis
# A greater psychotomimetic state effect was associated with less frequent usage of cannabis (r=0.33, p< 0.005)  in the cannabis-using group
# Extract effect size
mason_can_frequency_n=140 # Current cannabis users (n=140) were recreational smokers who used cannabis at least once a month.
mason_can_r=-0.33   # negatively coded as higher frequency was linked to fewer psychotomimetic symptoms
res(mason_can_r, n=mason_can_frequency_n, dig=20)
# Extract p-value
pEstimate_mason_canFrequency_psychlike=data.frame(pEstimate=0.0049, Ntot=mason_can_frequency_n)

# ++++ Effect of group (current cannabis users vs control) +++
mason_can_n= 140 # Current cannabis users (n=140) were recreational smokers who used cannabis at least once a month
mason_control_n=144 # The control subjects = non-users of any psychotropic drug (other than alcohol and tobacco) including cannabis for at least 6 months.
N_mason=mason_can_n+mason_control_n


# Overall PSI scale (from text)
mason_can_vs_control_PSI_f=74.49 # significantly greater psychotomimetic effects on day 0 [F(1,282)=74.49, p<0.001] in the cannabis users compared to controls
fes(mason_can_vs_control_PSI_f, mason_can_n, mason_control_n,  dig=20)
pEstimate_mason_can_vs_control_PSI=data.frame(pEstimate=0.0009, Ntot=N_mason)
# Paranoia/suspiciousness (from table 2, day 0)
mason_can_vs_control_paranoia_f=9.50
fes(mason_can_vs_control_paranoia_f, mason_can_n, mason_control_n,  dig=20)
pEstimate_mason_can_vs_control_paranoia=data.frame(pEstimate=0.002, Ntot=N_mason)

# ++++ Effect of group (high vs low schizotype) +++
# Cohen D
# High and low schizotypy groups: main effect of group [F(1, 155)=17.13, p<0.001].
mason_highSchizotype_vs_lowSchizotype_PSI_f=17.13
fes(mason_highSchizotype_vs_lowSchizotype_PSI_f, mason_can_n, mason_control_n,  dig=20)
pEstimate_mason_highSchizotype_vs_lowSchizotype_PSI=data.frame(pEstimate=0.0009, Ntot=N_mason)


# NO RATES REPORTED

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== D'Souza (2004) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Publication 1: Dose-response relationship THC PANSS
# REF: D'Souza, Deepak Cyril, Edward Perry, Lisa MacDougall, Yola Ammerman, Thomas Cooper, Gabriel Braley, Ralitza Gueorguieva, and John Harrison Krystal. "The psychotomimetic effects of intravenous delta-9-tetrahydrocannabinol in healthy individuals: implications for psychosis." Neuropsychopharmacology 29, no. 8 (2004): 1558-1572.

# Positive symptoms (PANSS). In all, 22 subjects initiated at least one test day
# D-9-THC transiently increased scores of the PANSS positive symptoms subscale (dose (w2 chisquare=20.2, p<0.0001);
# Subjects completed three test days during which they received 5 or 2.5mg
souza_n_healthy=22
souza_n_cases=13
# Extract estimates from plot
# Dose 1
mean_2.5mg_THC=9.589989682558725
se_2.5mg_THC=mean_2.5mg_THC-8.72122513616623
sd_2.5mg_THC=sqrt(souza_n_healthy)*se_2.5mg_THC
# Dose 2
mean_5mg_THC=9.964836720493317
se_5mg_THC=10.68885716342347-mean_5mg_THC
sd_5mg_THC=sqrt(souza_n_healthy)*se_5mg_THC
# Placebo condition
mean_placebo_THC=6.787664659164527
se_placebo_THC=mean_placebo_THC-6.574717470067424
sd_placebo_THC=sqrt(souza_n_healthy)*se_placebo_THC

# Extract Cohen D and p-values: 2.5mg
extract_souza_2.5mg_THC_panss=d_meanDiff_dependent(mean_2.5mg_THC, mean_placebo_THC, sd_2.5mg_THC, sd_placebo_THC, n=souza_n_healthy, r=r_withinSubj_between_measures)
pEstimate_Dsouza_PANSS_pos_dose=data.frame(pEstimate=NA, Ntot=souza_n_healthy)

# Extract Cohen D and p-values: 5mg
extract_souza_5mg_THC_panss=d_meanDiff_dependent(mean_5mg_THC, mean_placebo_THC, sd_5mg_THC, sd_placebo_THC, n=souza_n_healthy, r=r_withinSubj_between_measures)
pEstimate_Dsouza_PANSS_pos_dose=data.frame(pEstimate=NA, Ntot=souza_n_healthy)

# Assess dose response relationshops
# Dose-resposne effects not included as reported in Ganesh (2020)

# NO RATES REPORTED



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Solowij ===================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Solowij, Nadia, Samantha Broyd, Lisa-marie Greenwood, Hendrika van Hell, Dave Martelozzo, Kuna Rueb, Juanita Todd et al. "A randomised controlled trial of vaporised _ 9-tetrahydrocannabinol and cannabidiol alone and in combination in frequent and infrequent cannabis users: acute intoxication effects." European archives of psychiatry and clinical neuroscience 269, no. 1 (2019): 17-35.
#  The 48-item Psychotomi- metic States Inventory (PSI) [76] was administered at time 0, ~ 15 min after the first top-up dose (time 3), and at recovery. The items, rated from 0 (not at all) to 3 (strongly), form six sub-scales: delusional thinking, perceptual distortion, cogni- tive disorganisation, anhedonia, mania and paranoia.

# There were five drug administration sessions in which the following compounds were administered by vaporisation, with a 1 week washout:
#  1) placebo (ethanol vehicle 400 μl)
#  2) THC alone (8 mg),
#  3) CBDhigh alone (400 mg),
#  4) THC + CBDlow (THC: 8 mg, CBD: 4 mg)
#  5) THC + CBDhigh (THC: 12 mg; CBD: 400 mg)
# Participants were required to attend 6 sessions in total at the University: a baseline assessment session and five drug administration sessions

# 5 Conditions:
#       (1) Placebo
#       (2) CBD alone (400 mg)
#       (3) THC alone (8 mg)
#       (4) THC combined with low (4 mg) doses of CBD
#       (5) THC combined with high (400 mg) doses of CBD

# Predictor 1: subjective (self-rated) measures of intoxication
# “On a scale from 1 to 10, where 10 is the most stoned you’ve ever been, how stoned do you feel now?”.

# Predictor 2: objective (observer rated) measure of intoxication
#  The objective measures were obtained by independent observers blinded to drug condition and group, rating participants from 0 (not at all) to 4 (extremely) on the 8 observer items of the Clinician Administered Disso- ciative States Scale (CADSS) [73].
#  Scores on the 8 items were summed to produce a composite score out of a total possible 32, reflecting the extent to which they observed the participant to be intoxicated. Example items include: “Did the subject appear to be separated or detached from what is going on, as if not a part of the experience or not respond- ing in a way that you would expect?” and “Did the subject say something bizarre or out of context, or not speak when you would have expected it?”.

# CEQ
# CEQ showed significant associations between psychotic-like effects and subjective intoxication at time 2 for THC + CBD- low (ρ = − 0.37, p = .028), supported by a trend level associa- tion also with objective intoxication (ρ = − 0.33, p = .051), and between psychotic-like effects and subjective intoxica- tion at time 1 for THC + CBDhigh (ρ = 0.34, p = .045).
# Cannabis Experiences Questionnaire (CEQ) was used to retrospectively assess symptoms experienced whilst intoxicated.

nSolowij=36
# Correlation between subjective high and psychosis-like experiences while under the influence of THC
# Condition THC alone (8 mg) (no placebo comparison)

# Paranoia
res(rValueNS_allStudies, n=nSolowij, dig=20)
pEstimate_solowij_paranoia=data.frame(pEstimate=NA, Ntot=nSolowij) # NS, ie below study-defined p-value threshold
# Delusional thinking
res(rValueNS_allStudies, n=nSolowij, dig=20)
pEstimate_solowij_delusions=data.frame(pEstimate=NA, Ntot=nSolowij) # NS, ie below study-defined p-value threshold
# Depersionalisation
rValue_solowj_depersonalisation=.304
res(rValue_solowj_depersonalisation, n=nSolowij, dig=20)
pValue_solowj_depersonalisation=0.1 # Significant at trend level associations
pEstimate_solowij_depersionalisation=data.frame(pEstimate=pValue_solowj_depersonalisation, Ntot=nSolowij)

# NO RATES REPORTED



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Krebs   ===================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Subjective experiences during first use were assessed using a 17-item self-report questionnaire (5-level rating), which has a confirmed factorial structure including one grouping psychotic-like experiences: hallucinations (visual and/or auditory) and ideas of reference (see Supplementary Material for more details).
# CNR1, AKT1, BDNF and COMT genes with PEFU and found a significant association with a functional haplotype block in CNR1.

# Outcome: High psychosis-like group vs. low psychosis-like group
# PEFU was defined as the experience of at least one of these psychotic symptoms ‘strongly‘ or ‘very strongly’ when first consuming cannabis.

# Predictors
# 3 Genes (10 variants)
# 1) CNR1 (rs806379, rs1535255, rs2023239, rs1049353, rs12720071, AAT repeat)
# 2) COMT (rs4680, also known as Val158Met)
# 3) AKT1 (rs2494732, rs1130233; BDNF: rs6265, also known as Val66Met).
# We found no significant association between any of these markers and high PEFU after Bonferroni correction for multiple testing.

# Gender
NriskA_case=35
NriskB_case=59-NriskA_case
NriskA_control=219
NriskB_control=485-NriskA_control
labelA="Male"
labelB="Female"
extract_krebs_gender=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_krebs_gender=data.frame(pEstimate=NA, Ntot=extract_krebs_gender$N.total)


# Family history mood disorder
NriskA_case=19
NriskB_case=38
NriskA_control=146
NriskB_control=319
labelA="Family hx of mood disorder"
labelB="No family hx"
extract_krebs_mood=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_krebs_mood=data.frame(pEstimate=NA, Ntot=extract_krebs_mood$N.total)


# Family history Psychosis
NriskA_case=2
NriskB_case=38
NriskA_control=20
NriskB_control=319
labelA="Family hx of psychosis"
labelB="No family hx"
extract_krebs_psychosishx=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_krebs_psychosishx=data.frame(pEstimate=NA, Ntot=extract_krebs_psychosishx$N.total)

# Age when cannabis was smoked for the first time
# i.e. => Early cannabis initiation (before age of 15 years)
NriskA_case=13
NriskB_case=NriskB_case=59-NriskA_case
NriskA_control=90
NriskB_control=485-NriskA_control
labelA="First use (before 15)"
labelB="First use (after 15)"
extract_krebs_age=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_krebs_age=data.frame(pEstimate=NA, Ntot=extract_krebs_age$N.total)




datKrebsGenes <- as.data.frame(matrix(nrow=27, ncol=6))
colnames(datKrebsGenes) <- c('ID','Gene', 'AlleleFreq', 'rsID', 'Case', 'Control')
datKrebsGenes$ID=rep(1:9, times=1, each=3)
datKrebsGenes$AlleleFreq=rep(c("aa", "aA", "AA"), 9)

# CNR1
datKrebsGenes$Gene[1:15]="CNR1"

nowN=1
datKrebsGenes$rsID[nowN:(nowN+2)]="rs806379"
datKrebsGenes$Case[nowN]=54.2
datKrebsGenes$Case[nowN+1]=45.8
datKrebsGenes$Case[nowN+2]=0
datKrebsGenes$Control[nowN]=69.78
datKrebsGenes$Control[nowN+1]=29.60
datKrebsGenes$Control[nowN+2]=0.62

nowN=4
datKrebsGenes$rsID[nowN:(nowN+2)]="rs1535255"
datKrebsGenes$Case[nowN]=67.2
datKrebsGenes$Case[nowN+1]=25.8
datKrebsGenes$Case[nowN+2]=7
datKrebsGenes$Control[nowN]=75.78
datKrebsGenes$Control[nowN+1]=22.36
datKrebsGenes$Control[nowN+2]=1.86

nowN=7
datKrebsGenes$rsID[nowN:(nowN+2)]="rs2023239"
datKrebsGenes$Case[nowN]=25.6
datKrebsGenes$Case[nowN+1]=42.4
datKrebsGenes$Case[nowN+2]=22
datKrebsGenes$Control[nowN]=22.32
datKrebsGenes$Control[nowN+1]=44.90
datKrebsGenes$Control[nowN+2]=25.78

nowN=10
datKrebsGenes$rsID[nowN:(nowN+2)]="rs1049353"
datKrebsGenes$Case[nowN]=44
datKrebsGenes$Case[nowN+1]=42.4
datKrebsGenes$Case[nowN+2]=13.6
datKrebsGenes$Control[nowN]=52.10
datKrebsGenes$Control[nowN+1]=37.60
datKrebsGenes$Control[nowN+2]=10.30

nowN=13
datKrebsGenes$rsID[nowN:(nowN+2)]="rs12720071"
datKrebsGenes$Case[nowN]=78
datKrebsGenes$Case[nowN+1]=22
datKrebsGenes$Case[nowN+2]=0
datKrebsGenes$Control[nowN]=81.86
datKrebsGenes$Control[nowN+1]=17.32
datKrebsGenes$Control[nowN+2]=0.42

# COMT
nowN=16
datKrebsGenes$Gene[nowN:18]="COMT"

datKrebsGenes$rsID[nowN:(nowN+2)]="rs4680"
datKrebsGenes$Case[nowN]=15.2
datKrebsGenes$Case[nowN+1]=45.8
datKrebsGenes$Case[nowN+2]=39
datKrebsGenes$Control[nowN]=22.13
datKrebsGenes$Control[nowN+1]=50.73
datKrebsGenes$Control[nowN+2]=27.14


# BDNF
nowN=19
datKrebsGenes$Gene[nowN:21]="BDNF"

datKrebsGenes$rsID[nowN:(nowN+2)]="rs6265"
datKrebsGenes$Case[nowN]=8.6
datKrebsGenes$Case[nowN+1]=27.6
datKrebsGenes$Case[nowN+2]=63.8
datKrebsGenes$Control[nowN]=8.8
datKrebsGenes$Control[nowN+1]=35.5
datKrebsGenes$Control[nowN+2]=55.7

# AKT
nowN=22
datKrebsGenes$Gene[nowN:27]="AKT"

datKrebsGenes$rsID[nowN:(nowN+2)]="rs1130233"
datKrebsGenes$Case[nowN]=52.5
datKrebsGenes$Case[nowN+1]=35.6
datKrebsGenes$Case[nowN+2]=11.9
datKrebsGenes$Control[nowN]=55.3
datKrebsGenes$Control[nowN+1]=37.8
datKrebsGenes$Control[nowN+2]=6.9

nowN=25
datKrebsGenes$rsID[nowN:(nowN+2)]="rs2494732"
datKrebsGenes$Case[nowN]=37.9
datKrebsGenes$Case[nowN+1]=43.1
datKrebsGenes$Case[nowN+2]=19
datKrebsGenes$Control[nowN]=30.8
datKrebsGenes$Control[nowN+1]=49.6
datKrebsGenes$Control[nowN+2]=19.6

datKrebsGenes$n_cases=59
datKrebsGenes$n_controls=485

# get chi square statistics
listGenesKrebs=list()
for ( i in 1:9 ) {
  datKrebsGenes_sub=subset(datKrebsGenes, ID==i)
  proportions=data.matrix(datKrebsGenes_sub[,5:6], rownames.force = NA)
  chiSquEstimate=as.numeric(chisq.test(proportions, correct=FALSE)$statistic)
  datKrebsGenes_sub$ChiSquare=chiSquEstimate
  listGenesKrebs[[i]]=datKrebsGenes_sub
}
# Merge all lists from the loop
listGenesKrebsAll=do.call(rbind,listGenesKrebs)
# Remove dublicate rsIDs
GenesKrebs_clean = listGenesKrebsAll[-which(duplicated(listGenesKrebsAll$rsID)), ]

# Copy estimate es commands into excel sheet
# rs806379
KrebsCNR1_rs806379=subset(GenesKrebs_clean, rsID=="rs806379")$ChiSquare[1]
chies(KrebsCNR1_rs806379,59+485, dig=50)
pEstimate_krebs_rs806379=data.frame(pEstimate=NA, Ntot=59+485)
# rs1535255
KrebsCNR1_rs1535255=subset(GenesKrebs_clean, rsID=="rs1535255")$ChiSquare[1]
chies(KrebsCNR1_rs1535255,58+485, dig=50)
pEstimate_krebs_rs1535255=data.frame(pEstimate=NA, Ntot=58+485)
# rs2023239
KrebsCNR1_rs2023239=subset(GenesKrebs_clean, rsID=="rs2023239")$ChiSquare[1]
chies(KrebsCNR1_rs2023239, 59+481, dig=50)
pEstimate_krebs_rs2023239=data.frame(pEstimate=NA, Ntot=59+481)
# rs1049353
KrebsCNR1_rs1049353=subset(GenesKrebs_clean, rsID=="rs1049353")$ChiSquare[1]
chies(KrebsCNR1_rs1049353, 59+476, dig=50)
pEstimate_krebs_rs1049353=data.frame(pEstimate=NA, Ntot=59+476)
# rs1049353
KrebsCNR1_rs12720071=subset(GenesKrebs_clean, rsID=="rs12720071")$ChiSquare[1]
chies(KrebsCNR1_rs12720071, 59+485, dig=50)
pEstimate_krebs_rs12720071=data.frame(pEstimate=NA, Ntot=59+485)
# rs4680
KrebsCOMT_rs4680=subset(GenesKrebs_clean, rsID=="rs4680")$ChiSquare[1]
chies(KrebsCOMT_rs4680, 59+479, dig=50)
pEstimate_krebs_rs4680=data.frame(pEstimate=NA, Ntot=59+479)
# rs6265
KrebsBDNF_rs6265=subset(GenesKrebs_clean, rsID=="rs6265")$ChiSquare[1]
chies(KrebsBDNF_rs6265, 59+479, dig=50)
pEstimate_krebs_rs6265=data.frame(pEstimate=NA, Ntot=59+479)
# rs1130233
KrebsAKT_rs1130233=subset(GenesKrebs_clean, rsID=="rs1130233")$ChiSquare[1]
chies(KrebsAKT_rs1130233, 59+479, dig=50)
pEstimate_krebs_rs1130233=data.frame(pEstimate=NA, Ntot=59+479)
# rs2494732
KrebsAKT_rs2494732=subset(GenesKrebs_clean, rsID=="rs2494732")$ChiSquare[1]
chies(KrebsAKT_rs2494732, 59+479, dig=50)
pEstimate_krebs_rs2494732=data.frame(pEstimate=NA, Ntot=59+479)

# Rates in the sample
krebs2014_PLE_DF=data.frame(event=59, n=59+485)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== D'Souza (2005) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Publication 2: psychosis patients vs health controls
# REF: D’Souza, Deepak Cyril, Walid Michel Abi-Saab, Steven Madonick, Kimberlee Forselius-Bielen, Anne Doersch, Gabriel Braley, Ralitza Gueorguieva, Thomas B. Cooper, and John Harrison Krystal. "Delta-9-tetrahydrocannabinol effects in schizophrenia: implications for cognition, psychosis, and addiction." Biological psychiatry 57, no. 6 (2005): 594-608.
# the interactions between group, dose, and time were not significant [􏰌2(2.87) 􏰅 .42, p 􏰅 .73].
n_effective_healthy_souza=n_effective_dependent(souza_n_healthy, r=r_withinSubj_between_timepoints)
n_effective_cases_souza=n_effective_dependent(souza_n_cases, r=r_withinSubj_between_timepoints)
p_souza_dose_case_interaction=0.73
chiSquEstimate_souza_dose_case_interaction=0.42
chies(chiSquEstimate_souza_dose_case_interaction, n_effective_healthy_souza+n_effective_cases_souza, dig=50)
pEstimate_Dsouza_PANSS_pos_dose_case_interaction=data.frame(pEstimate=p_souza_dose_case_interaction, Ntot=souza_n_healthy+souza_n_cases)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Barkus   ==================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Barkus, Emma J., John Stirling, Richard S. Hopkins, and Shon Lewis. "Cannabis-induced psychosis-like experiences are associated with high schizotypy." Psychopathology 39, no. 4 (2006): 175-178.
# In the sample who reported ever using cannabis (n =99)

# The SPQB provides a total score and scores on three sub-scales: ‘disorganized’ (SPQB-D), ‘cognitive-perceptual’ (SPQB-CP) and ‘interpersonal’ (SPQB-I).
N_Barkus_2006=99
barkus_cor_p=0.009 # all correlation estimates were reported at p < 0.01
#Psychosis-Like Experiences subscale and the SPQB-D
spqb_d_cor=0.40 # SPQB-D subscale (r = 0.40, p < 0.01)
res(spqb_d_cor, n=N_Barkus_2006, dig=20)
pEstimate_barkus_spqb_d_cor=data.frame(pEstimate=NA, Ntot=N_Barkus_2006)

#Psychosis-Like Experiences subscale and SPQB-CP subscale
spqb_cp_cor=0.33 # SPQB-CP subscale (r = 0.33,p < 0.01)
res(spqb_cp_cor, n=N_Barkus_2006, dig=20)
pEstimate_barkus_spqb_cp_cor=data.frame(pEstimate=NA, Ntot=N_Barkus_2006)

#Psychosis-Like Experiences subscale and SPQB total score
spqb_tot_cor=0.44 # SPQB-CP subscale (r = 0.44,,p < 0.01)
res(spqb_tot_cor, n=N_Barkus_2006, dig=20)
pEstimate_barkus_spqb_tot_cor=data.frame(pEstimate=NA, Ntot=N_Barkus_2006)

# NO RATES REPORTED

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Winstock ==================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Winstock, Adam R., and Monica J. Barratt. "Synthetic cannabis: a comparison of patterns of use and effect profile with natural cannabis in a large global sample." Drug and alcohol dependence 131, no. 1-2 (2013): 106-111.
nWinstock=837 # sample indicating to have used both synthetic and natural cannabis
# they were asked regarding the effects they have when someing synthetics and when smoking natural cannabis
m1_winstock=4.75
m2_winstock=3.89
sd1_winstock=3.11
sd2_winstock=2.43
n=nWinstock
extract_winstock_delusional=d_meanDiff_dependent(m1=m1_winstock, m2=m2_winstock, sd1=sd1_winstock, sd2=sd2_winstock, n=nWinstock, r=r_withinSubj_between_measures)
# Check if results are similar to results when using metafor => extactly the same
#extract_winstock_delusional_check=unlist(escalc(measure = "SMCC",m1i=m1_winstock, m2i=m2_winstock, sd1i=sd1_winstock , sd2i=sd2_winstock, ni=nWinstock, ri=r_withinSubj_between_measures))
# Check for t-stats => OK too
#extract_winstock_delusional_testT=convertT_repeated(t=9.91, df=889, r=r_withinSubj_between_measures, n=nWinstock)
p_winstock=0.0009
pEstimate_winstock_delusional=data.frame(pEstimate=NA, Ntot=nWinstock) # p < .001

# NO RATES REPORTED


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Englund ===================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Englund, Amir, Zerrin Atakan, Aleksandra Kralj, Nigel Tunstall, Robin Murray, and Paul Morrison. "The effect of five day dosing with THCV on THC-induced cognitive, psychological and physiological effects in healthy male human volunteers: A placebo-controlled, double-blind, crossover pilot trial." Journal of Psychopharmacology 30, no. 2 (2016): 140-151.

# Ten male cannabis users (<25 use occasions)
# participants were dosed for five days with THCV or identical placebo capsules, before administration of IV THC
# minimum lifetime cannabis use of at least once and no more than 25 times
# Dose THC: Synthetic THC, IV injection
# => 1 ml THC: THC was administered over 10 minutes with 1ml/min pulses (total dose 1mg)

# Assessment psychosis-like syptoms:
#      - Community Assessment of Psychic Experiences-state (CAPE- state).
#      - The CAPE-state is a 42-item validated scale which measures positive, negative and depressive dimensions of psychotic-like experiences (Stefanis et al., 2002)
#      -  each item has a yes/no response option.
#      - When a yes response has been given, the participant is asked to rate on a four-point scale how distressing the experience was to them.
# Results: There was no statistically significant change in positive symptom frequency or symptom distress scores across sessions under either condition

# THC condition vs placebo condition

# ++++ Effect of dose: THC condition vs placebo condition +++
englund_n=10
englund_cape_1mgTHC_m=0.4
englund_cape_placebo_m=0.3
englund_cape_1mgTHC_sd=0.5
englund_cape_placebo_sd=0.7
# Derive cohen d
extract_englund_cape_IV_1mg_THC=d_meanDiff_dependent(m1=englund_cape_1mgTHC_m, m2=englund_cape_placebo_m, sd1=englund_cape_1mgTHC_sd , sd2=englund_cape_placebo_sd, n=englund_n, r=r_withinSubj_between_timepoints)
# Extract p
pEstimate_englund_cape_IV_1mg_THC=data.frame(pEstimate=0.135, Ntot=englund_n)

# NO RATES REPORTED

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Stone ===================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Stone, J.M., Morrison, P.D., Brugger, S., Nottage, J., Bhattacharyya, S., Sumich, A., Wilson, D., Tunstall, N., Feilding, A., Brenneisen, R. and McGuire, P., 2012. Communication breakdown: delta-9 tetrahydrocannabinol effects on pre-speech neural coherence. Molecular psychiatry, 17(6), pp.568-569.

# THC led to a significant increase in self- rated measures of

#    - anxiety (t = 2.50, df = 15, P = 0.02)
#    - paranoid persecutory ideation (t = 2.97, df = 15, P = 0.01)
#    - perceptual abnormalities (t = 4.16, df = 15, P = 0.0008).

# Sixteen healthy volunteers (26 +/- 5 yrs; 9 females) with previous exposure to cannabis
# => received either intravenous THC (1.25mg) or placebo (normal saline) on two separate occasions in a randomised order.
# Symptoms were self-rated using a 5-domain, 24-item subjective effects of cannabis questionnaire (table 1), derived from the most commonly self-reported phenomena in 2 previous intravenous THC studies involving 32 individuals.3, 4 Ratings were taken at baseline and at 30 minutes post-injection (pi).

# paranoid persecutory ideation (t = 2.97, df = 15, P = 0.01)
stone_n=16
stone_t = 2.97
stone_df = 15
stone_p=0.01
# Convert t to d
extract_stone_paranoia_0perc_1.25mgTHC=convertT_dependent(stone_t, stone_df, n=stone_n)
# Note: can also use convertT_repeated(stone_t, stone_df, r=0.5, n=stone_n), gives same results
convertT_repeated(stone_t, stone_df, r_withinSubj_between_timepoints, stone_n)
pEstimate_stone_paranoia_0perc_1.25mgTHC=data.frame(pEstimate=stone_p, Ntot=stone_n)

# NO RATES REPORTED


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Morgan (2018) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Morgan CJA, Freeman TP, Hindocha C, Schafer G, Gardner C, Curran HV (2018) Individual and combined effects of acute delta-9-tetrahydrocannabinol and cannabidiol on psychotomimetic symptoms and memory function. Transl Psychiatry 8:181

# Design
#     - Repeated measures ANOVAs with two between-subjects variables (frequency of use, schizotypy).
#     - Drug was entered as a within subjects factor and was coded as a simple contrast (Placebo versus THC)

# Sample
n_morgan_experiment=48
n_morgan_light_experiment=24 # light (n = 24) and heavy users (n = 24).
n_morgan_heavy_experiment=24

# Estimate effective sample size
n_effective_morgan_experiment=n_effective_dependent(n_morgan_experiment, r=r_withinSubj_between_timepoints)
n_effective_morgan_light_experiment=n_effective_dependent(n_morgan_light_experiment, r=r_withinSubj_between_timepoints)
n_effective_morgan_heavy_experiment=n_effective_dependent(n_morgan_heavy_experiment, r=r_withinSubj_between_timepoints)


# %%%%%%%%%%% Effect of THC %%%%%%%%%%%
# The main findings of this study were an increase in psychotomimetic symptoms following administration of both THC alone

# ======== PSI (total score)
# There was a main effect of Drug, driven by increased scores relative to placebo for THC (p = 0.014)
p_PSI_8mgTHC=0.014
pes(p_PSI_8mgTHC, n_effective_morgan_experiment/2, n_effective_morgan_experiment/2, dig=20)
pEstimate_morgan_PSI_8mgTHC=data.frame(pEstimate=p_PSI_8mgTHC, Ntot=n_morgan_experiment)

# ======== PSI subscales - delusory thinking
# no drug effects were found for the remaining subscales of ‘Delusory Thinking’ (Fig. 1).
# Note: Estimates extracted from plot
# Placebo condition
morgan_psi_delusions_mean_placebo=0.9813084112149546
morgan_psi_delusions_se_placebo=1.1775700934579458-morgan_psi_delusions_mean_placebo
morgan_psi_delusions_sd_placebo=morgan_psi_delusions_se_placebo*sqrt(n_morgan_experiment)
# THC condition
morgan_psi_delusions_mean_8mgTHC=1.0560747663551429
morgan_psi_delusions_se_8mgTHC=1.3271028037383201-morgan_psi_delusions_mean_8mgTHC
morgan_psi_delusions_sd_8mgTHC=morgan_psi_delusions_se_8mgTHC*sqrt(n_morgan_experiment)
# Derive cohen d
extract_morgan_psi_delusions_8mgTHC=d_meanDiff_dependent(m1=morgan_psi_delusions_mean_8mgTHC,
                                                         m2=morgan_psi_delusions_mean_placebo,
                                                         sd1=morgan_psi_delusions_sd_8mgTHC ,
                                                         sd2=morgan_psi_delusions_sd_placebo,
                                                         n=n_morgan_experiment,
                                                         r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_morgan_psi_delusions_8mgTHC=data.frame(pEstimate=NA, Ntot=n_morgan_experiment)

# ======== PSI subscales - paranoia
# - no drug effects were found for the remaining subscales of 'Paranoia’ (Fig. 1)
# Note: Estimates extracted from plot
# Placebo condition
morgan_psi_paranoia_mean_placebo=0.8888888888888894
morgan_psi_paranoia_se_placebo=1.1851851851851856-morgan_psi_paranoia_mean_placebo
morgan_psi_paranoia_sd_placebo=morgan_psi_paranoia_se_placebo*sqrt(n_morgan_experiment)
# THC condition
morgan_psi_paranoia_mean_8mgTHC=1.324618736383443
morgan_psi_paranoia_se_8mgTHC=1.7080610021786493-morgan_psi_paranoia_mean_8mgTHC
morgan_psi_paranoia_sd_8mgTHC=morgan_psi_paranoia_se_8mgTHC*sqrt(n_morgan_experiment)
# Derive cohen d
extract_morgan_psi_paranoia_8mgTHC=d_meanDiff_dependent(m1=morgan_psi_paranoia_mean_8mgTHC,
                                                        m2=morgan_psi_paranoia_mean_placebo,
                                                        sd1=morgan_psi_paranoia_sd_8mgTHC ,
                                                        sd2=morgan_psi_paranoia_sd_placebo,
                                                        n=n_morgan_experiment,
                                                        r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_morgan_psi_paranoia_8mgTHC=data.frame(pEstimate=NA, Ntot=n_morgan_experiment)

# ======== BPRS subscales - positive
# - Exploration of the Drug by Subscale interaction revealed that for Positive items, cannabinoid administra- tion had no effects
# Note: Estimates extracted from plot

# Placebo condition
morgan_bprs_mean_placebo=6.422993492407809
morgan_bprs_se_placebo=6.531453362255965-morgan_bprs_mean_placebo
morgan_bprs_sd_placebo=morgan_bprs_se_placebo*sqrt(n_morgan_experiment)
# THC condition
morgan_bprs_mean_8mgTHC=6.683297180043383
morgan_bprs_se_8mgTHC=6.830802603036876-morgan_bprs_mean_8mgTHC
morgan_bprs_sd_8mgTHC=morgan_bprs_se_8mgTHC*sqrt(n_morgan_experiment)
# Derive cohen d
extract_morgan_bprs_8mgTHC=d_meanDiff_dependent(m1=morgan_bprs_mean_8mgTHC, m2=morgan_bprs_mean_placebo, sd1=morgan_bprs_sd_8mgTHC , sd2=morgan_bprs_sd_placebo, n=n_morgan_experiment, r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_morgan_bprs_8mgTHC=data.frame(pEstimate=NA, Ntot=n_morgan_experiment)


# %%%%%%%%%%% Effect of CBD %%%%%%%%%%%

# ==== BPRS
# CBD condition
morgan_bprs_mean_16mgCBD=6.5021645021645025
morgan_bprs_se_16mgCBD=6.623376623376624-morgan_bprs_mean_16mgCBD
morgan_bprs_sd_16mgCBD=morgan_bprs_se_16mgCBD*sqrt(n_morgan_experiment)
# Derive cohen d
extract_morgan_bprs_8mgTHC_16mgCBD=d_meanDiff_dependent(m1=morgan_bprs_mean_16mgCBD,
                                                        m2=morgan_bprs_mean_8mgTHC,
                                                        sd1=morgan_bprs_sd_16mgCBD,
                                                        sd2=morgan_bprs_sd_8mgTHC ,
                                                        n=n_morgan_experiment,
                                                        r=r_withinSubj_between_timepoints)
# Extract p value
# not reported for comparison between THC and THC-CBD condition
pEstimate_morgan_bprs_8mgTHC_16mgCBD=data.frame(pEstimate=NA, Ntot=n_morgan_experiment)

# ======== PSI subscales - delusory thinking
# Note: Estimates extracted from plot
# CBD condition
morgan_psi_delusions_mean_16mgCBD=1.0035169988276682
morgan_psi_delusions_se_16mgCBD=1.2473622508792497-morgan_psi_delusions_mean_16mgCBD
morgan_psi_delusions_sd_16mgCBD=morgan_psi_delusions_se_16mgCBD*sqrt(n_morgan_experiment)
# Derive cohen d
extract_morgan_psi_delusions_16mgCBD=d_meanDiff_dependent(m1=morgan_psi_delusions_mean_16mgCBD,
                                                          m2=morgan_psi_delusions_mean_8mgTHC,
                                                          sd1=morgan_psi_delusions_sd_16mgCBD ,
                                                          sd2=morgan_psi_delusions_sd_8mgTHC,
                                                          n=n_morgan_experiment,
                                                          r=r_withinSubj_between_timepoints)
# Extract p value
# not reported for comparison between THC and THC-CBD condition
pEstimate_morgan_delusions_8mgTHC_16mgCBD=data.frame(pEstimate=NA, Ntot=n_morgan_experiment)

# ======== PSI subscales - paranoia
# CBD condition
morgan_psi_paranoia_mean_16mgCBD=1.1428571428571423
morgan_psi_paranoia_se_16mgCBD=1.491821155943293-morgan_psi_paranoia_mean_16mgCBD
morgan_psi_paranoia_sd_16mgCBD=morgan_psi_paranoia_se_16mgCBD*sqrt(n_morgan_experiment)
# Derive cohen d
extract_morgan_psi_paranoia_16mgCBD=d_meanDiff_dependent(m1=morgan_psi_paranoia_mean_16mgCBD,
                                                         m2=morgan_psi_paranoia_mean_8mgTHC,
                                                         sd1=morgan_psi_paranoia_sd_16mgCBD ,
                                                         sd2=morgan_psi_paranoia_sd_8mgTHC,
                                                         n=n_morgan_experiment,
                                                         r=r_withinSubj_between_timepoints)
# Extract p value
# not reported for comparison between THC and THC-CBD condition
pEstimate_morgan_paranoia_8mgTHC_16mgCBD=data.frame(pEstimate=NA, Ntot=n_morgan_experiment)


# %%%%%%%%%%% Effect of frequency of use on response to THC %%%%%%%%%%%
# - Exploration of the interaction showed that light and heavy users had similar responses to THC (p = 0.504) relative to placebo
#p_PSI_8mgTHC_light_vs_heavy=0.504
#pes(p_PSI_8mgTHC_light_vs_heavy, n_effective_morgan_light_experiment, n_effective_morgan_heavy_experiment, dig=20)
#pEstimate_morgan_PSI_8mgTHC_light_vs_heavy=data.frame(pEstimate=p_PSI_8mgTHC_light_vs_heavy, Ntot=n_morgan_experiment)
# Not included in meta-analysis as results are already extracted from Freeman (2021)

# NO RATES REPORTED


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Colizzi (2020) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
colizzi2020_n=16
colizzi2020_sensitive_n=11
colizzi2020_NOTsensitive_n=5

#  intravenous administration of Δ9-THC (1.19 mg/2 ml) or pla- cebo.

# == THC-induced change in myo-inositol levels
# effect of dose THC not included as already extracted from Colizzi (2019)
# However, the Δ9-THC-induced change in myo-inositol levels differed significantly between those sensitive to (Δ9-THC minus placebo; M = −0.251, S.D. = 1.242) and those not sensitive (M = 1.615, S.D. = 1.753) to the psychotomimetic effects of the drug (t(14) = 2.459, p = 0.028
extract_colizzi2020_THCmyoinositol=mes(-0.251, 1.615, 1.242, 1.753, colizzi2020_sensitive_n, colizzi2020_NOTsensitive_n, dig=20)
# p-value
pEstimate_colizzi2020_THCmyoinositol=data.frame(pEstimate= 0.028, Ntot=colizzi2020_n)


# == THC-induced change in cortisol levels
# Further, compared to placebo, the Δ9-THC-induced change in cortisol levels over the study period (baseline minus 2.5 h post- drug injection) differed significantly between those sensitive to (Δ9-THC change minus placebo change
extract_colizzi2020_THCcortisol=mes(-275.4, 74.2, 207.519, 209.281, colizzi2020_sensitive_n, colizzi2020_NOTsensitive_n, dig=20)
# p-value
pEstimate_colizzi2020_THCcortisol=data.frame(pEstimate= 0.009, Ntot=colizzi2020_n)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Colizzi (2019) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Colizzi, Marco, Nathalie Weltens, Philip McGuire, David Lythgoe, Steve Williams, Lukas Van Oudenhove, and Sagnik Bhattacharyya. "Delta-9-tetrahydrocannabinol increases striatal glutamate levels in healthy individuals: implications for psychosis." Molecular psychiatry (2019): 1-10.

# Intravenous administration of Δ9-THC (1.19 mg/2 ml) vs saline.
# Sample size
colizzi_n=16
# Estimate effective sample size
n_effective_colizzi_experiment=n_effective_dependent(colizzi_n, r=r_withinSubj_between_timepoints)


# ========== Effect of THC on psychosis-like symptoms
# Δ9-THC was associated with acute induction of transient psychotic symptoms (PANSS positive symptoms subscale, t = 6.62, P < 0.001;
colizzi_t = 6.62
colizzi_df = #not reported
  colizzi_panss_pos_THC_p= 0.0009 # P < 0.001
# Derive d
pes(colizzi_panss_pos_THC_p, n_effective_colizzi_experiment/2, n_effective_colizzi_experiment/2, dig=20)
# Extract p value
pEstimate_colizzi_panss_pos_THC=data.frame(pEstimate=NA, Ntot=colizzi_n) # no precise estimate provided


# ========== Effetc of striatal glutamate on psychotomimetic effects induced by cannabis
# striatal glutamate => measured using proton magnetic resonance spectroscopy

# Glutamate levels (baseline)
# compared to individuals who were not sensitive to the psychotomimetic effects of Δ9-THC, individuals who developed transient psychotic-like symptoms (~70% of the sample) had significantly lower baseline Glx (placebo; P 7= 0.023)
# Glx values under the placebo condition were sig- nificantly lower in subjects who were sensitive to Δ9-THC- induced psychotomimetic effects (9.20 ± 1.93) compared to subjects who were not (11.85 ± 1.93; t = 2.54, P = 0.023).

# Sensitive subjects
colizzi_sensitive_n=11 #  Eleven subjects (69%) were identified as sensitive to the psychotomimetic effects
colizzi_can_sensitive_glutamate_baseline_mean=9.20  # Glx values under the placebo condition
colizzi_can_sensitive_glutamate_baseline_sd=1.93
# Non-sensitive subjects
colizzi_non_sensitive_n=colizzi_n-colizzi_sensitive_n
colizzi_can_non_sensitive_glutamate_baseline_mean=11.85  # Glx values under the placebo condition
colizzi_can_non_sensitive_glutamate_baseline_sd=1.93
# Derive d
colizzi_can_sensitive_glutamate_baseline=mes(colizzi_can_sensitive_glutamate_baseline_mean,
                                             colizzi_can_non_sensitive_glutamate_baseline_mean,
                                             colizzi_can_sensitive_glutamate_baseline_sd,
                                             colizzi_can_non_sensitive_glutamate_baseline_sd,
                                             colizzi_sensitive_n, colizzi_non_sensitive_n, dig=20)
# Extract p value
colizzi_can_sensitive_glutamate_baseline_p=0.023
pEstimate_colizzi_can_sensitive_glutamate_baseline=data.frame(pEstimate=colizzi_can_sensitive_glutamate_baseline_p, Ntot=colizzi_n)

# Change in glutamate levels following THC administration
# compared to non-sensitive individuals, sensitive individuals had a 2.27-times higher increase following Δ9-THC administration.

# subjects sensitive to the mimetic effects of Δ9-THC (11.85 ± 3.76).
colizzi_can_sensitive_glutamate_change_mean=11.85
colizzi_can_sensitive_glutamate_change_sd=3.76
# subject not sensitive to the psychotomimetic effects (13.01 ± 3.02)
colizzi_can_non_sensitive_glutamate_change_mean=13.01
colizzi_can_non_sensitive_glutamate_change_sd=3.02
# Derive d
mes(colizzi_can_sensitive_glutamate_change_mean, colizzi_can_non_sensitive_glutamate_change_mean, colizzi_can_sensitive_glutamate_change_sd, colizzi_can_non_sensitive_glutamate_change_sd, colizzi_sensitive_n, colizzi_non_sensitive_n, dig=20)
# Extract p value
pEstimate_colizzi_can_sensitive_glutamate_change=data.frame(pEstimate=NA, Ntot=colizzi_n)

# No rates reported (transient-psychotic symptoms not included as this indexes only a >2 point increase)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Theunissen ================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Theunissen, Eef L., Johannes T. Reckweg, Nadia RPW Hutten, Kim PC Kuypers, Stefan W. Toennes, Merja A. Neukamm, Sebastian Halter, and Johannes G. Ramaekers. "Psychotomimetic symptoms after a moderate dose of a synthetic cannabinoid (JWH-018): implications for psychosis." Psychopharmacology (2021): 1-11.
# Predictor: Subjective high
# Outcome depersonalization
# Dose THC: 75 lg/kg bodyweight
# Data from 17 subjects (7 males and 10 females). eight participants reached a subjective high score larger than 2, that is, indicating subjective in- toxication (Fig. 1). Therefore, we used a mixed analysis of variance, with ‘‘Responder’’ (i.e., subjective high score >2 [n = 8] vs. subjective high <2 [nonresponder; n = 9]). JWH-018 was either given in a fixed dose of 2 mg (N = 5) or 75 lg/kg bodyweight (N = 12; average dose was 3.95 mg). Measure: Clinician- Administered Dissociative States Scale
n_theunissen=17
n_theunissen_responders=8
n_theunissen_nonresponders=9
# Estimate effective sample size
n_effective_theunissen=n_effective_dependent(n_theunissen, r=r_withinSubj_between_timepoints)
n_effective_theunissen_responders=n_effective_dependent(n_theunissen_responders, r=r_withinSubj_between_timepoints)
n_effective_theunissen_nonresponders=n_effective_dependent(n_theunissen_nonresponders, r=r_withinSubj_between_timepoints)


# %%%% Effect of single dose THC
# Depersonalization
f_depersonalization_dose=10.8
df_depersonalization_dose=1.14
p_depersonalization_dose=0.003
fes(f_depersonalization_dose, n_effective_theunissen/2, n_effective_theunissen/2, dig=20)
# cannot used f conversion function
# convertF_dependent(f=3.286335, df=df_depersonalization_dose, n=n_theunissen)
pEstimate_theunissen_depersonalization_dose=data.frame(pEstimate=p_depersonalization_dose, Ntot=n_theunissen)

# Derealization
f_derealization_dose=4.85
p_derealization_dose=0.023
fes(f_derealization_dose, n_effective_theunissen/2, n_effective_theunissen/2, dig=20)
pEstimate_theunissen_derealization_dose=data.frame(pEstimate=p_derealization_dose, Ntot=n_theunissen)

# %%%% Differences between responders and non-responders
# Depersonalization
f_depersonalization_group=3.63
p_depersonalization_group=0.039
fes(f_depersonalization_group, n_effective_theunissen_responders, n_effective_theunissen_nonresponders, dig=20)
pEstimate_theunissen_depersonalization_group=data.frame(pEstimate=p_depersonalization_group, Ntot=n_theunissen)
# Derealization
pes(pValueNS_allStudies, n_effective_theunissen_responders, n_effective_theunissen_nonresponders, dig=20) #NS but no F-statistic reported
pEstimate_theunissen_derealization_group=data.frame(pEstimate=pValueNS_allStudies, Ntot=n_theunissen)




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Sami (2020) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Association of extent of cannabis use and acute intoxication experiences in a multi-national sample of first episode psychosis patients and controls
# Outcome: Sum of six cannabis-associated psychosis-like experiences  (feeling fearful; feeling crazy or mad; feeling nervy; feeling suspicious; hearing voices; seeing visions);

# PLEs were available for 598/655 (91.3%) cases and and 615/654 (94.0%) controls
eugei_sami_PLEs_n_cases=598 # reported ever use of cannabis and data analysis was restricted to them
eugei_sami_PLEs_n_controls=615
eugei_sami_PLEs_n_tot=eugei_sami_PLEs_n_cases+eugei_sami_PLEs_n_controls
# Caseness
# caseness predicted cPLEs independent of cEEs (b=0.826, t=7.86, p<0.001)

# ======= Caseness
# As hypothesised caseness predicted cPLEs independent of cEEs (b = 0.826, t = 7.86, p < 0.001)
# Get cohen d
eugei_sami_PLEs_caseness_t=7.86
eugei_sami_PLEs_casenessExtracted=compute.es::tes(eugei_sami_PLEs_caseness_t ,
                                                  eugei_sami_PLEs_n_cases,
                                                  eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
pEstimate_eugei_sami_PLEs_caseness=data.frame(pEstimate=NA, Ntot=eugei_sami_PLEs_n_tot )


# ======= Frequency of cannabis use
# As hypothesised frequency of use predicted cPLEs independent of cEEs(b = 0.502, t = 6.18, p < 0.001)
# frequency of use (b=0.502, t=6.18, p<0.001) on PLEs
# Get cohen d
eugei_sami_PLEs_frequency_t=6.18
eugei_sami_PLEs_frequencyExtracted=compute.es::tes(eugei_sami_PLEs_frequency_t ,
                                                   eugei_sami_PLEs_n_cases,
                                                   eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
pEstimate_eugei_sami_PLEs_frequency=data.frame(pEstimate=NA, Ntot=eugei_sami_PLEs_n_tot )

# ======= Frequency x caseness interaction
# a significant interaction between group and frequency such that increasing frequency was associated with increased difference in cPLEs between cases and controls (b = 0.229, t = 3.49, p = 0.001).
# Get cohen d
eugei_sami_PLEs_frequencyxcaseness_t=3.49
eugei_sami_PLEs_frequencyxcasenessExtracted=compute.es::tes(eugei_sami_PLEs_frequencyxcaseness_t ,
                                                            eugei_sami_PLEs_n_cases,
                                                            eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
pEstimate_eugei_sami_PLEs_frequencyxcaseness=data.frame(pEstimate=NA, Ntot=eugei_sami_PLEs_n_tot )

# ======= Potency of cannabis
# potency (b=0.543, t=2.36, p=0.019) n PLEs
# strain data was dichotomised into ‘high potency’ preparations (THC>10%) and ‘low potency’. using published data on the expected concentration of Delta-9-tetra- hydrocannabinol (THC) in the different types of cannabis available across the sites
# Get cohen d
eugei_sami_PLEs_potency_t=2.36
eugei_sami_PLEs_potencyExtracted=compute.es::tes(eugei_sami_PLEs_potency_t ,
                                                 eugei_sami_PLEs_n_cases,
                                                 eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_potency_p=0.019
pEstimate_eugei_sami_PLEs_potency=data.frame(pEstimate=eugei_sami_PLEs_potency_p, Ntot=eugei_sami_PLEs_n_tot)


# ======= Potency of cannabis x caseness interaction
#  signifi- cant interaction for caseness by potency (b = 0.438, t = 2.04, p = 0.042).
# Get cohen d
eugei_sami_PLEs_potencyxcaseness_t=2.04
eugei_sami_PLEs_potencyxcasenessExtracted=compute.es::tes(eugei_sami_PLEs_potencyxcaseness_t ,
                                                          eugei_sami_PLEs_n_cases,
                                                          eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_potencyxcaseness_p=0.042
pEstimate_eugei_sami_PLEs_potencyxcaseness=data.frame(pEstimate=eugei_sami_PLEs_potencyxcaseness_p, Ntot=eugei_sami_PLEs_n_tot )


# ======= Money spent on cannabis
#  money spent on cannabis per week (b=0.397, t=6.17, p<0.001)
# Get cohen d
eugei_sami_PLEs_money_t=6.17
eugei_sami_PLEs_moneyExtracted=compute.es::tes(eugei_sami_PLEs_money_t ,
                                               eugei_sami_PLEs_n_cases,
                                               eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
pEstimate_eugei_sami_PLEs_money=data.frame(pEstimate=NA, Ntot=eugei_sami_PLEs_n_tot )


# ======= Money spent on cannabis x caseness
# sig- nificant interaction between caseness and money spent such that more money spent was associated with increased difference in cPLEs between cases and controls (b = 0.177, t = 3.29, p = 0.001).
# Get cohen d
eugei_sami_PLEs_moneyxcaseness_t=3.29
eugei_sami_PLEs_moneyxcasenessExtracted=compute.es::tes(eugei_sami_PLEs_moneyxcaseness_t ,
                                                        eugei_sami_PLEs_n_cases,
                                                        eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
pEstimate_eugei_sami_PLEs_moneyxcaseness=data.frame(pEstimate=NA, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: Inhalants (lifetime)
eugei_sami_PLEs_inhalantsLifetime_t=1.57
eugei_sami_PLEs_inhalantsLifetimeExtracted=compute.es::tes(eugei_sami_PLEs_inhalantsLifetime_t ,
                                                           eugei_sami_PLEs_n_cases,
                                                           eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_inhalantsLifetime_p=0.116
pEstimate_eugei_sami_PLEs_inhalantsLifetime=data.frame(pEstimate=eugei_sami_PLEs_inhalantsLifetime_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: Inhalants (lifetime misuse)
eugei_sami_PLEs_inhalantsmisuse_t=2.74
eugei_sami_PLEs_inhalantsmisuseExtracted=compute.es::tes(eugei_sami_PLEs_inhalantsmisuse_t ,
                                                         eugei_sami_PLEs_n_cases,
                                                         eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_inhalantsmisuse_p=0.006
pEstimate_eugei_sami_PLEs_inhalantsmisuse=data.frame(pEstimate=eugei_sami_PLEs_inhalantsmisuse_p, Ntot=eugei_sami_PLEs_n_tot )


# ======= Other drug use: crack (lifetime)
eugei_sami_PLEs_crackLifetime_t=1.64
eugei_sami_PLEs_crackLifetimeExtracted=compute.es::tes(eugei_sami_PLEs_crackLifetime_t ,
                                                       eugei_sami_PLEs_n_cases,
                                                       eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_crackLifetime_p=0.101
pEstimate_eugei_sami_PLEs_crackLifetime=data.frame(pEstimate=eugei_sami_PLEs_crackLifetime_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: crack (lifetime misuse)
eugei_sami_PLEs_crackmisuse_t=2.9
eugei_sami_PLEs_crackmisuseExtracted=compute.es::tes(eugei_sami_PLEs_crackmisuse_t ,
                                                     eugei_sami_PLEs_n_cases,
                                                     eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_crackmisuse_p=0.004
pEstimate_eugei_sami_PLEs_crackmisuse=data.frame(pEstimate=eugei_sami_PLEs_crackmisuse_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: cocaine (lifetime)
eugei_sami_PLEs_cocaineLifetime_t=0.45
eugei_sami_PLEs_cocaineLifetimeExtracted=compute.es::tes(eugei_sami_PLEs_cocaineLifetime_t ,
                                                         eugei_sami_PLEs_n_cases,
                                                         eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_cocaineLifetime_p=0.654
pEstimate_eugei_sami_PLEs_cocaineLifetime=data.frame(pEstimate=eugei_sami_PLEs_cocaineLifetime_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: cocaine (lifetime misuse)
eugei_sami_PLEs_cocainemisuse_t=0.35
eugei_sami_PLEs_cocainemisuseExtracted=compute.es::tes(eugei_sami_PLEs_cocainemisuse_t ,
                                                       eugei_sami_PLEs_n_cases,
                                                       eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_cocainemisuse_p=0.729
pEstimate_eugei_sami_PLEs_cocainemisuse=data.frame(pEstimate=eugei_sami_PLEs_cocainemisuse_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: stimulants (lifetime)
eugei_sami_PLEs_stimulantsLifetime_t=1.09
eugei_sami_PLEs_stimulantsLifetimeExtracted=compute.es::tes(eugei_sami_PLEs_stimulantsLifetime_t ,
                                                            eugei_sami_PLEs_n_cases,
                                                            eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_stimulantsLifetime_p=0.277
pEstimate_eugei_sami_PLEs_stimulantsLifetime=data.frame(pEstimate=eugei_sami_PLEs_stimulantsLifetime_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: stimulants (lifetime misuse)
eugei_sami_PLEs_stimulantsmisuse_t=0.45
eugei_sami_PLEs_stimulantsmisuseExtracted=compute.es::tes(eugei_sami_PLEs_stimulantsmisuse_t ,
                                                          eugei_sami_PLEs_n_cases,
                                                          eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_stimulantsmisuse_p=0.654
pEstimate_eugei_sami_PLEs_stimulantsmisuse=data.frame(pEstimate=eugei_sami_PLEs_stimulantsmisuse_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: sedatives (lifetime)
eugei_sami_PLEs_sedativesLifetime_t=1.79
eugei_sami_PLEs_sedativesLifetimextracted=compute.es::tes(eugei_sami_PLEs_sedativesLifetime_t ,
                                                          eugei_sami_PLEs_n_cases,
                                                          eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_sedativesLifetime_p=0.074
pEstimate_eugei_sami_PLEs_sedativesLifetime=data.frame(pEstimate=eugei_sami_PLEs_sedativesLifetime_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: sedatives (lifetime misuse)
eugei_sami_PLEs_sedativesmisuse_t=2.46
eugei_sami_PLEs_sedativesmisuseExtracted=compute.es::tes(eugei_sami_PLEs_sedativesmisuse_t ,
                                                         eugei_sami_PLEs_n_cases,
                                                         eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_sedativesmisuse_p=0.014
pEstimate_eugei_sami_PLEs_sedativesmisuse=data.frame(pEstimate=eugei_sami_PLEs_sedativesmisuse_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: opioids (lifetime)
eugei_sami_PLEs_opioidsLifetime_t=-0.92
eugei_sami_PLEs_opioidsLifetimeExtracted=compute.es::tes(eugei_sami_PLEs_opioidsLifetime_t ,
                                                         eugei_sami_PLEs_n_cases,
                                                         eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_opioidsLifetime_p=0.36
pEstimate_eugei_sami_PLEs_opioidsLifetime=data.frame(pEstimate=eugei_sami_PLEs_opioidsLifetime_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: opioids (lifetime misuse)
eugei_sami_PLEs_opioidsmisuse_t=-1.93
eugei_sami_PLEs_opioidsmisuseExtracted=compute.es::tes(eugei_sami_PLEs_opioidsmisuse_t ,
                                                       eugei_sami_PLEs_n_cases,
                                                       eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_opioidsmisuse_p=0.054
pEstimate_eugei_sami_PLEs_opioidsmisuse=data.frame(pEstimate=eugei_sami_PLEs_opioidsmisuse_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: hallucinogens (lifetime)
eugei_sami_PLEs_hallucinogensLifetime_t=-1.19
eugei_sami_PLEs_hallucinogensLifetimeExtracted=compute.es::tes(eugei_sami_PLEs_hallucinogensLifetime_t ,
                                                               eugei_sami_PLEs_n_cases,
                                                               eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_hallucinogensLifetime_p=0.235
pEstimate_eugei_sami_PLEs_hallucinogensLifetime=data.frame(pEstimate=eugei_sami_PLEs_hallucinogensLifetime_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: hallucinogens (lifetime misuse)
eugei_sami_PLEs_hallucinogensmisuse_t=0.63
eugei_sami_PLEs_hallucinogensmisuseExtracted=compute.es::tes(eugei_sami_PLEs_hallucinogensmisuse_t ,
                                                             eugei_sami_PLEs_n_cases,
                                                             eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_hallucinogensmisuse_p=0.531
pEstimate_eugei_sami_PLEs_hallucinogensmisuse=data.frame(pEstimate=eugei_sami_PLEs_hallucinogensmisuse_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: ketamine (lifetime)
eugei_sami_PLEs_ketamineLifetime_t=-1.03
eugei_sami_PLEs_ketamineLifetimeExtracted=compute.es::tes(eugei_sami_PLEs_ketamineLifetime_t ,
                                                          eugei_sami_PLEs_n_cases,
                                                          eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_ketamineLifetime_p=0.305
pEstimate_eugei_sami_PLEs_ketamineLifetime=data.frame(pEstimate=eugei_sami_PLEs_ketamineLifetime_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: ketamine (lifetime misuse)
eugei_sami_PLEs_ketaminemisuse_t=-0.13
eugei_sami_PLEs_ketaminemisuseExtracted=compute.es::tes(eugei_sami_PLEs_ketaminemisuse_t ,
                                                        eugei_sami_PLEs_n_cases,
                                                        eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_ketaminemisuse_p=0.9
pEstimate_eugei_sami_PLEs_ketaminemisuse=data.frame(pEstimate=eugei_sami_PLEs_ketaminemisuse_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: novelPsychoact (lifetime)
eugei_sami_PLEs_novelPsychoactLifetime_t=-0.1
eugei_sami_PLEs_novelPsychoactLifetimeExtracted=compute.es::tes(eugei_sami_PLEs_novelPsychoactLifetime_t ,
                                                                eugei_sami_PLEs_n_cases,
                                                                eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_novelPsychoactLifetime_p=0.922
pEstimate_eugei_sami_PLEs_novelPsychoactLifetime=data.frame(pEstimate=eugei_sami_PLEs_novelPsychoactLifetime_p, Ntot=eugei_sami_PLEs_n_tot )

# ======= Other drug use: novelPsychoact (lifetime misuse)
eugei_sami_PLEs_novelPsychoactmisuse_t=1.16
eugei_sami_PLEs_novelPsychoactmisuseExtracted=compute.es::tes(eugei_sami_PLEs_novelPsychoactmisuse_t ,
                                                              eugei_sami_PLEs_n_cases,
                                                              eugei_sami_PLEs_n_controls, dig=20)
# Get p-value
eugei_sami_PLEs_novelPsychoactmisuse_p=0.247
pEstimate_eugei_sami_PLEs_novelPsychoactmisuse=data.frame(pEstimate=eugei_sami_PLEs_novelPsychoactmisuse_p, Ntot=eugei_sami_PLEs_n_tot )




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bhattacharyya (2012) ======================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Bhattacharyya, S., Atakan, Z., Martin-Santos, R., Crippa, J.A., Kambeitz, J., Prata, D., Williams, S., Brammer, M., Collier, D.A. and McGuire, P.K., 2012. Preliminary report of biological basis of sensitivity to the effects of cannabis on psychosis: AKT1 and DAT1 genotype modulates the effects of δ-9-tetrahydrocannabinol on midbrain and striatal function. Molecular psychiatry, 17(12), pp.1152-1155.
bhattacharyya_AKT1_psych_change_THC_tot=35
# Outcome: Change in total PANSS score


# AKT1
# Effect of AKT1 genotype (F1,31 = 3.42, P = 0.07; G homozygotes > heterozygotes) on the increase in psychotic symptoms induced by d-9-THC.
# Paper: "the psychotogenic effects of delta-9-THC were significantly greater in individuals carrying the risk genotype of both AKT1 rs1130233 (G homozygotes) "
# Interpretation: Those with G/G had higher psychotic symptoms?
# Coding in meta is different, with AKT (rs1130233, G/G, G/A, A/A) (allele dosage: increasing A)
# Solution => reverse direction of effect estimtate
bhattacharyya_AKT1_psych_change_THC_carrierA_n=9+7 # G homozygotes (AKT1) were contrasted against the A-allele carriers (AKT1)
bhattacharyya_AKT1_psych_change_THC_homozygousG_n=19
bhattacharyya_AKT1_psych_change_THC_f=3.42
fes(bhattacharyya_AKT1_psych_change_THC_f, bhattacharyya_AKT1_psych_change_THC_carrierA_n, bhattacharyya_AKT1_psych_change_THC_homozygousG_n,  dig=20)
pEstimate_bhattacharyya_AKT1_psych_change_THC=data.frame(pEstimate=0.07, Ntot=bhattacharyya_AKT1_psych_change_THC_tot)

# DAT1
# There was a main effect of DAT1 genotype (F1,31 = 7.46, P = 0.01; 9-repeat carriers > 10-repeat homo- zygotes)
# increase in symptoms greater in people with DAT1 3’UTR VNTR (9-repeat carriers)
# The human DAT1 gene has a polymorphic 40bp variable number of tandem repeats (VNTR) in the 30 untranslated region (UTR), with several alleles ranging from 3 to 11 copies of the 40-bp repeats, the 9-repeat and 10-repeat alleles being the most common
# Therefore coded as follows: DAT1 (3’UTR VNTR, 10/10, 9/10, 9/9) (allele dosage: increasing 9-repeat variant)
bhattacharyya_AKT1_psych_change_THC_carrier9repeat_n=3+15 # 9-repeat carriers (DAT1) were contrasted against the 10-repeat homozygotes
bhattacharyya_AKT1_psych_change_THC_homozygous10repeat_n=17
bhattacharyya_DAT1_psych_change_THC_f=7.46
fes(bhattacharyya_DAT1_psych_change_THC_f, bhattacharyya_AKT1_psych_change_THC_carrier9repeat_n, bhattacharyya_AKT1_psych_change_THC_homozygous10repeat_n,  dig=20)
pEstimate_bhattacharyya_DAT1_psych_change_THC=data.frame(pEstimate=0.01, Ntot=bhattacharyya_AKT1_psych_change_THC_tot)

# NO RATES REPORTED


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bhattacharyya (2010) ======================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Bhattacharyya, Sagnik, Paul D. Morrison, Paolo Fusar-Poli, Rocio Martin-Santos, Stefan Borgwardt, Toby Winton-Brown, Chiara Nosarti et al. "Opposite effects of Δ-9-tetrahydrocannabinol and cannabidiol on human brain function and psychopathology." Neuropsychopharmacology 35, no. 3 (2010): 764-774.
# Effects of symptoms arelady extracted
# The effect of D-9-THC in the striatum was inversely correlated with the severity of the psychotic symptoms (as indexed by the PANSS positive symptoms subscale) it concurrently induced: the more it attenuated striatal activation, the more severe were the psychotic symptoms (r 1⁄4 􏰦0.574, p 1⁄4 0.013; after leaving
bhattacharyya2010_10mgTHC_striatum_n=15
bhattacharyya2010_10mgTHC_striatum=res(-0.574,n= bhattacharyya2010_10mgTHC_striatum_n)
pEstimate_bhattacharyya2010_10mgTHC_striatum=data.frame(pEstimate=0.013, Ntot=bhattacharyya2010_10mgTHC_striatum_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bhattacharyya (2010) ======================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This relationship between psychotic symptoms and the effect of 􏱵9-THC in the ventral striatum persisted after excluding an outlier identified using Cook’s D reliabil- ity analysis (r = 0.465, P =.047)
# the relationship between psychotic symptoms and activation in the rostro-anterior cingulate cortex did not remain significant after ex- cluding the outlier (r = 0.388, P =.09). Therefore,

bhattacharyya2009_10mgTHC_fmri_n=15

# ventral striatum
bhattacharyya2009_10mgTHC_fmri_ventral_striatum=res(0.465,n= bhattacharyya2009_10mgTHC_fmri_n)
pEstimate_bhattacharyya2009_10mgTHC_fmri_ventral_striatum=data.frame(pEstimate=0.047, Ntot=bhattacharyya2009_10mgTHC_fmri_n)

# rostro-anterior cingulate cortex
bhattacharyya2009_10mgTHC_fmri_cingulate_cortex=res(0.388,n= bhattacharyya2009_10mgTHC_fmri_n)
pEstimate_bhattacharyya2009_10mgTHC_fmri_cingulate_cortex=data.frame(pEstimate=0.09, Ntot=bhattacharyya2009_10mgTHC_fmri_n)




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bhattacharyya (2012) ======================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Bhattacharyya, Sagnik, José Alexandre Crippa, Paul Allen, Rocio Martin-Santos, Stefan Borgwardt, Paolo Fusar-Poli, Katya Rubia et al. "Induction of psychosis byδ9-tetrahydrocannabinol reflects modulation of prefrontal and striatal function during attentional salience processing." Archives of general psychiatry 69, no. 1 (2012): 27-36.
# Increase in symptoms, indexed by the positive symptoms subscale of the Positive and Negative Syndrome Scale43 (F2,28 = 9.15, P = .001) (eFig- ure 1).
bhattacharyya_10mgTHC_PANSS_pos_n=15

# ====== Effect of THC on pos symptoms (extracted from graph)

# Placebo condition
bhattacharyya_10mgTHC_PANSS_pos_placebo_upperCI=7.80789946140036
bhattacharyya_10mgTHC_PANSS_pos_placebo_lowerCI=7.423698384201078
bhattacharyya_10mgTHC_PANSS_pos_placebo_mean=(bhattacharyya_10mgTHC_PANSS_pos_placebo_upperCI + bhattacharyya_10mgTHC_PANSS_pos_placebo_lowerCI)/2
bhattacharyya_10mgTHC_PANSS_pos_placebo_se=bhattacharyya_10mgTHC_PANSS_pos_placebo_upperCI - bhattacharyya_10mgTHC_PANSS_pos_placebo_mean
bhattacharyya_10mgTHC_PANSS_pos_placebo_sd=bhattacharyya_10mgTHC_PANSS_pos_placebo_se*sqrt(bhattacharyya_10mgTHC_PANSS_pos_n)

# THC condition (one hour post capsule)
bhattacharyya_10mgTHC_PANSS_pos_drug_upperCI=8.91382405745063
bhattacharyya_10mgTHC_PANSS_pos_drug_lowerCI=8.044883303411131
bhattacharyya_10mgTHC_PANSS_pos_drug_mean=(bhattacharyya_10mgTHC_PANSS_pos_drug_upperCI + bhattacharyya_10mgTHC_PANSS_pos_drug_lowerCI)/2
bhattacharyya_10mgTHC_PANSS_pos_drug_se=bhattacharyya_10mgTHC_PANSS_pos_drug_upperCI - bhattacharyya_10mgTHC_PANSS_pos_drug_mean
bhattacharyya_10mgTHC_PANSS_pos_drug_sd=bhattacharyya_10mgTHC_PANSS_pos_drug_se*sqrt(bhattacharyya_10mgTHC_PANSS_pos_n)

# Derive cohen d
extract_bhattacharyya_10mgTHC_PANSS_pos=d_meanDiff_dependent(m1=bhattacharyya_10mgTHC_PANSS_pos_drug_mean, m2=bhattacharyya_10mgTHC_PANSS_pos_placebo_mean, sd1=bhattacharyya_10mgTHC_PANSS_pos_placebo_sd , sd2=bhattacharyya_10mgTHC_PANSS_pos_drug_sd, n=bhattacharyya_10mgTHC_PANSS_pos_n, r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_bhattacharyya_10mgTHC_PANSS_pos=data.frame(pEstimate=0.001, Ntot=bhattacharyya_10mgTHC_PANSS_pos_n)

# NO RATES REPORTED

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bhattacharyya (2015) ======================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

bhattacharyya2015_10mgTHC_PANSS_pos_n=36

# Effect of does
#  delta-9-THCAUC= 25.8376.0, placeboAUC = 21.9671.8; Fig. 1A) and

extract_bhattacharyya2015_10mgTHC_PANSS_pos=d_meanDiff_dependent(m1=25.83,
                                                                 m2=21.96,
                                                                 sd1=6.0 ,
                                                                 sd2=1.8,
                                                                 n=bhattacharyya2015_10mgTHC_PANSS_pos_n,
                                                                 r=r_withinSubj_between_timepoints)

pEstimate_bhattacharyya2015_10mgTHC_PANSS_pos=data.frame(pEstimate=NA, Ntot=bhattacharyya2015_10mgTHC_PANSS_pos_n)


# THC-impaired inhibitory performance
# There was a direct relationship between the severity of transient psychotic-like symptoms and the effect of delta-9-THC on inhibitory processing: the more delta-9-THC impaired inhibitory performance (inhibition errors) the greater was the severity of psychotic-like symptoms (r = 0.472, p = 0.01; Fig. 2C) induced under its influence.
bhattacharyya2015_10mgTHC_inhibit_processing=res(0.472,n= bhattacharyya2015_10mgTHC_PANSS_pos_n)
pEstimate_bhattacharyya2015_10mgTHC_inhibit_processing=data.frame(pEstimate=0.01, Ntot=bhattacharyya2015_10mgTHC_PANSS_pos_n)


# inferior frontal activation
# Furthermore, there was a direct relationship between the severity of psychotic-like symptoms induced by delta-9-THC and the attenuation of inferior frontal activation under its influ- ence, such that they were inversely correlated (ρ=0.378, p=0.03; Fig. 3A).
bhattacharyya2015_10mgTHC_frontal_activation=res(-0.378,n= bhattacharyya2015_10mgTHC_PANSS_pos_n)
pEstimate_bhattacharyya2015_10mgTHC_frontal_activation=data.frame(pEstimate=0.03, Ntot=bhattacharyya2015_10mgTHC_PANSS_pos_n)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Atakan (2013) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Atakan. Cannabis affects people differently: inter-subject variation in the psychotogenic effects of DELTA9-tetrahydrocannabinol: a functional magnetic resonance imaging study with healthy volunteers.
atakan2013_psych_THC_n=21
# THC administration
# 10mg of THC vs placebo


# Correlates associated with sensitivity to cannabis (transiently psychotic [TP] verus non-psychotic)
# => TP = those who scored 3 or more on at least three items of the Positive and Negative Syndrome Scale (PANSS) positive subscale at 2-h measurements following THC
# => Participants who scored below these thresholds were classed as ‘ non-psychotic ’ (NP).
atakan2013_TP_n=11
atakan2013_NP_n=10

# Age of the participants
atakan2013_TP_age_mean=26.76
atakan2013_TP_age_SD=5
atakan2013_NP_age_mean=25.7
atakan2013_NP_age_SD=6.27
# Extract effect estimate
mes(atakan2013_TP_age_mean, atakan2013_NP_age_mean, atakan2013_TP_age_SD, atakan2013_NP_age_SD, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_age=data.frame(pEstimate=0.44, Ntot=atakan2013_TP_n+atakan2013_NP_n) # no significant group differences with respect to age (Mann–Whitney U test : Z=x0.78, p=0.44

# Years of education
atakan2013_TP_education_t=0.79 # no significant differences for years of education (t=0.79, df=16, p=0.44)  [15.33 (3.64) in TP vs in NP 16.78 (4.15)]
atakan2013_TP_educationExtracted=compute.es::tes(atakan2013_TP_education_t, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_education=data.frame(pEstimate=0.44, Ntot=atakan2013_TP_n+atakan2013_NP_n) # no significant group differences with respect to age (Mann–Whitney U test : Z=x0.78, p=0.44

# Employment status (employed versus unemployed)
# Gender
NriskA_case=8
NriskB_case=3
NriskA_control=0
NriskB_control=1
labelA="Unemployed"
labelB="Employed"
extract_atakan2013_TP_employment=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_atakan2013_TP_employment=data.frame(pEstimate=NA, Ntot=extract_atakan2013_TP_employment$N.total)

#  cannabis (p=0.67)
atakan2013_TP_cannabisSeverity_p=0.67 # Experimental cannabis use=less than 10 times vs. Occasional use=10–25 times, in lifetime.
pes(atakan2013_TP_cannabisSeverity_p, atakan2013_TP_n, atakan2013_NP_n, dig=20) # Higher rates of PSE in more frequent users
pEstimate_atakan2013_TP_cannabis=data.frame(pEstimate=0.67, Ntot=atakan2013_TP_n+atakan2013_NP_n)

#  use of cigarette smoking (p=1.00)
atakan2013_TP_cigarette_p=1
pes(atakan2013_TP_cigarette_p, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_cigarette=data.frame(pEstimate=1, Ntot=atakan2013_TP_n+atakan2013_NP_n)

#  alcohol (p=1.00)
atakan2013_TP_alcoholSeverity_p=1 # Occasional alcohol use=drinking at weekends, social events. Moderate use=drinking at least three times per week.
pes(atakan2013_TP_alcoholSeverity_p, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_alcohol=data.frame(pEstimate=1, Ntot=atakan2013_TP_n+atakan2013_NP_n)

#  other drug use (p=1.00)
atakan2013_TP_otherDrugs_p=1 # Occasional alcohol use=drinking at weekends, social events. Moderate use=drinking at least three times per week.
pes(atakan2013_TP_otherDrugs_p, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_otherDrugs=data.frame(pEstimate=1, Ntot=atakan2013_TP_n+atakan2013_NP_n)


#  other drug use (p=1.00)
atakan2013_TP_otherDrugs_p=1 # Occasional alcohol use=drinking at weekends, social events. Moderate use=drinking at least three times per week.
pes(atakan2013_TP_otherDrugs_p, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_otherDrugs=data.frame(pEstimate=1, Ntot=atakan2013_TP_n+atakan2013_NP_n)


# baseline placebo: State-Trait-Anxiety Inventory STAI
atakan2013_TP_baselinePLC_STAI_p=0.76 # TP individuals => higher baseline anxiety (ns)
pes(atakan2013_TP_baselinePLC_STAI_p, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_baselinePLC_STAI=data.frame(pEstimate=0.76, Ntot=atakan2013_TP_n+atakan2013_NP_n)

# baseline placebo: Addiction Research Centre Inventory ARCI
# The Addiction Research Center Inventory is a 550 item multipurpose test measuring the broad range of physical, emotive, cognitive, and subjective effects of drugs.
# Assesses subjective effects to drugs
atakan2013_TP_baselinePLC_ARCI_p=0.14 # TP individuals => higher baseline scores in ARCI (ns)
pes(atakan2013_TP_baselinePLC_ARCI_p, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_baselinePLC_ARCI=data.frame(pEstimate=0.14, Ntot=atakan2013_TP_n+atakan2013_NP_n)

# post THC: AIS score (AIS, Analogue Intoxication Scale)
atakan2013_TP_THC_AIS_p=0.43 # TP individuals => higher levels in AIS following THC
pes(atakan2013_TP_THC_AIS_p, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_baselinePLC_ARCI=data.frame(pEstimate=0.43, Ntot=atakan2013_TP_n+atakan2013_NP_n)

# baseline placebo: VAMS tranquillization subscale (isual Analogue Mood Scale)
# Higher scores in VAMS = poorer outcome in mood
atakan2013_TP_baselinePLC_VAMS_p=0.68 # TP individuals => lower scores in VAMS (better outcome)
pes(atakan2013_TP_baselinePLC_VAMS_p, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_baselinePLC_VAMS=data.frame(pEstimate=0.68, Ntot=atakan2013_TP_n+atakan2013_NP_n)

# baseline PANSS pos
atakan2013_TP_baselinePLC_PANSS_pos_p=0.14 # TP individuals => lower scores in PANSS prior THC administration
pes(atakan2013_TP_baselinePLC_PANSS_pos_p, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_baselinePLC_PANSS_pos=data.frame(pEstimate=0.14, Ntot=atakan2013_TP_n+atakan2013_NP_n)

# baseline PANSS neg
atakan2013_TP_baselinePLC_PANSS_neg_p=0.38 # TP individuals => lower scores in PANSS prior THC administration
pes(atakan2013_TP_baselinePLC_PANSS_neg_p, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_baselinePLC_PANSS_neg=data.frame(pEstimate=0.38, Ntot=atakan2013_TP_n+atakan2013_NP_n)

# baseline PANSS gen
atakan2013_TP_baselinePLC_PANSS_gen_p=0.68 # TP individuals => lower scores in PANSS prior THC administration
pes(atakan2013_TP_baselinePLC_PANSS_gen_p, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_baselinePLC_PANSS_gen=data.frame(pEstimate=0.68, Ntot=atakan2013_TP_n+atakan2013_NP_n)

# baseline PANSS tot
atakan2013_TP_baselinePLC_PANSS_tot_p=0.32 # TP individuals => lower scores in PANSS prior THC administration
pes(atakan2013_TP_baselinePLC_PANSS_tot_p, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_baselinePLC_PANSS_tot=data.frame(pEstimate=0.32, Ntot=atakan2013_TP_n+atakan2013_NP_n)

# Placebo condition:impulse control - Reaction time to Go signals (Go/No-Go task)
atakan2013_TP_recT_mean=0.43
atakan2013_TP_recT_SD=0.05
atakan2013_NP_recT_mean=0.44
atakan2013_NP_recT_SD=0.07
# Extract effect estimate
mes(atakan2013_TP_recT_mean, atakan2013_NP_recT_mean, atakan2013_TP_recT_SD, atakan2013_NP_recT_SD, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_recT=data.frame(pEstimate=NA, Ntot=atakan2013_TP_n+atakan2013_NP_n)

# Placebo condition:  impulse control - Reaction time to Oddball signals (Go/No-Go task)
atakan2013_TP_recT_Odd_mean=0.41
atakan2013_TP_recT_Odd_SD=0.17
atakan2013_NP_recT_Odd_mean=0.48
atakan2013_NP_recT_Odd_SD=0.07
# Extract effect estimate
mes(atakan2013_TP_recT_Odd_mean, atakan2013_NP_recT_Odd_mean, atakan2013_TP_recT_Odd_SD, atakan2013_NP_recT_Odd_SD, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_recT_Odd=data.frame(pEstimate=NA, Ntot=atakan2013_TP_n+atakan2013_NP_n)


# Placebo condition:  impulse control - Inhibition error  (Go/No-Go task)
atakan2013_TP_impulse_mean=2.38
atakan2013_TP_impulse_SD=2.23
atakan2013_NP_impulse_mean=3.20
atakan2013_NP_impulse_SD=4.86
# Extract effect estimate
mes(atakan2013_TP_impulse_mean, atakan2013_NP_impulse_mean, atakan2013_TP_impulse_SD, atakan2013_NP_impulse_SD, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_impulse=data.frame(pEstimate=NA, Ntot=atakan2013_TP_n+atakan2013_NP_n)

# baseline heart rate
# Under the THC condition, there was no evidence of a difference in heart rates between the two groups either at baseline or 2 h after drug administration.
pes(pValueNS_allStudies, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_baselinePLC_heartRate=data.frame(pEstimate=NA, Ntot=atakan2013_TP_n+atakan2013_NP_n)


# Activation in right middle temporal gyrus
# activation in the right middle temporal gyrus is attenuated in the transiently psychotic (TP) group in comparison with the non-psychotic (NP) group (TP <NP, p<0.007)
pes(0.0069, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_MTG_activation=data.frame(pEstimate=0.0069, Ntot=atakan2013_TP_n+atakan2013_NP_n)

# cerebellum
# activation in the vermis of the cerebellum is attenuated in the TP group in comparison with the NP group (TP <NP, p<0.005, corrected for <1 false-positive cluster).
pes(0.0049, atakan2013_TP_n, atakan2013_NP_n, dig=20)
pEstimate_atakan2013_TP_cerebellum_activation=data.frame(pEstimate=0.0049, Ntot=atakan2013_TP_n+atakan2013_NP_n)

# NO RATES REPORTED


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bloomfield (2014) =========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# REF: Bloomfield, Michael AP, Celia JA Morgan, Alice Egerton, Shitij Kapur, H. Valerie Curran, and Oliver D. Howes. "Dopaminergic function in cannabis users and its relationship to cannabis-induced psychotic symptoms." Biological psychiatry 75, no. 6 (2014): 470-478.

# Within the cannabis user group, the mean (SD) increase in PSI psychotic symptom subscale score after consuming cannabis was 9.9 (5.1).

# Outcome: PSI (Psychotomimetic States Inventory) while intoxicated (when compared to non-intoxicated state)

# The relationship between the striatal influx rate constant Ki cer and transient induction of cannabis-induced psychotic-like symptoms in the cannabis users.
# There was no significant relationship between the two variables (r= .32, p= .19).
# Dopamine synthesis capacity  was not associated with cannabis-induced psychotic-like symptoms (r = .32, p =.19)
# we found no relationship between dopaminergic function and cannabis-induced psychotic-like symptoms
bloomberg_dop_synthesis_can_user=19 # => Dopamine synthesis capacity
bloomberg_dop_synthesis_psych_r=0.32
bloomberg_dop_synthesis_psych_p=0.19
# Derive cohen d
res(bloomberg_dop_synthesis_psych_r, n=bloomberg_dop_synthesis_can_user, dig=20)
# Extract p value
pEstimate_bloomberg_dop_synthesis_psych=data.frame(pEstimate=0.19, Ntot=bloomberg_dop_synthesis_can_user)

# NO RATES REPORTED


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bloomfield (2016) =========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Bloomfield, M. A. P., E. Mouchlianitis, C. J. A. Morgan, T. P. Freeman, H. V. Curran, J. P. Roiser, and O. D. Howes. "Salience attribution and its relationship to cannabis-induced psychotic symptoms." Psychological medicine 46, no. 16 (2016): 3383-3395.
# There was a significant relation- ship between cannabis-induced psychotic symptom se- verity and explicit aberrant salience (r = 0.61, p = 0.04;

bloomberg_salience_can_user=17
bloomberg_explicitSalience_psych_r=0.61
bloomberg_explicitSalience_psych_p=0.04

# == Explicit aberrant salience
# Derive cohen d
res(bloomberg_explicitSalience_psych_r, n=bloomberg_salience_can_user, dig=20)
# Extract p value
pEstimate_bloomberg_explicitSalience_psych=data.frame(pEstimate=bloomberg_explicitSalience_psych_p, Ntot=bloomberg_salience_can_user)

# == Implicit aberrant salience
# There were no significant relationships between cannabis-induced psychotic symptoms and the other salience measures (p>0.05),
# Derive cohen d
pes(pValueNS_allStudies, bloomberg_salience_can_user/2, bloomberg_salience_can_user/2, dig=20)
# Extract p value
pEstimate_bloomberg_implicitSalience_psych=data.frame(pEstimate=NA, Ntot=bloomberg_salience_can_user)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Radhakrishnan (2015) =========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Radhakrishnan R, Skosnik PD, Cortes-Briones J, Sewell RA, Carbuto M, Schnakenberg A, Cahill J, Bois F, Gunduz-Bruce H, Pittman B, Ranganathan M, D’Souza DC (2015) GABA deficits enhance the psychotomimetic effects of Δ9-THC. Neuropsychopharmacology 40:2047–2056.

# These assessments were administered at baseline (−60 min), +70 min and +240 min timepoints

# ======= Effect of THC alone (IOM − / THC+)  (Intravenous THC (0.015 mg/kg) vs placebo)
# THC DOSE: THC at a dose of 0.015 mg/kg (1.05 mg in a 70 kg individual) was administered intrave- nously over 10min into a rapidly flowing intravenous infusion of normal saline.
radhakrishnan_PANSS_tot_THC_n = 23
radhakrishnan_PANSS_tot_placebo_mean = 33.26 # IOM−/THC− at +70 min
radhakrishnan_PANSS_tot_placebo_sd = 6.33

radhakrishnan_PANSS_tot_THC_mean =40.83 # IOM−/THC+ at +70 min
radhakrishnan_PANSS_tot_THC_sd = 7.69

# Derive cohen d
extract_radhakrishnan_singledose_THC=d_meanDiff_dependent(m1=radhakrishnan_PANSS_tot_THC_mean,
                                                          m2=radhakrishnan_PANSS_tot_placebo_mean,
                                                          sd1=radhakrishnan_PANSS_tot_THC_sd ,
                                                          sd2=radhakrishnan_PANSS_tot_placebo_sd,
                                                          n=radhakrishnan_PANSS_tot_THC_n,
                                                          r=r_withinSubj_between_timepoints)
# Extract p value
#  Relative to placebo, THC alone (IOM − / THC+) produced significant increases (padj = 0.005) in Total PANSS scores,
pEstimate_radhakrishnan_singledose_THC=data.frame(pEstimate=0.005, Ntot=radhakrishnan_PANSS_tot_THC_n)

# ======= Effect of GABA deficit
# GABA deficit was pharmacologically modeled by the administration of the GABAA inverse agonist, iomazenil
# healthy volunteers received iomazenil followed by THC, placebo iomazenil followed by THC, iomazenil followed by placebo THC, and placebo iomazenil followed by placebo THC
# Iomazenil (Ro 16-0154) is an iodine analog of the benzodiazepine receptor (BZR)-competitive antagonist flu- mazenil.
# When pretreated with IOM, THC induced significantly greater psychosis-relevant symptoms
radhakrishnan_PANSS_tot_THC_IOM_n = 21
radhakrishnan_PANSS_tot_THC_IOM_mean = 46.19 # IOM−/THC− at +70 min
radhakrishnan_PANSS_tot_THC_IOM_sd = 9.79

radhakrishnan_PANSS_tot_THC_mean =40.83 # IOM−/THC+ at +70 min
radhakrishnan_PANSS_tot_THC_sd = 7.69

# Derive cohen d
extract_radhakrishnan_THC_IOM=d_meanDiff_dependent(m1=radhakrishnan_PANSS_tot_THC_IOM_mean,
                                                   m2=radhakrishnan_PANSS_tot_THC_mean,
                                                   sd1=radhakrishnan_PANSS_tot_THC_IOM_sd ,
                                                   sd2=radhakrishnan_PANSS_tot_THC_sd,
                                                   n=radhakrishnan_PANSS_tot_THC_IOM_n,
                                                   r=r_withinSubj_between_timepoints)
# Extract p value
# there were significantly (padj = 0.04) greater increases in Total PANSS scores for the IOM+/THC+ condition compared with IOM − /THC+ (Figure 1a).
pEstimate_radhakrishnan_THC_IOM=data.frame(pEstimate=.04, Ntot=radhakrishnan_PANSS_tot_THC_IOM_n)


# ======== Effect of P300a Amplitude while under the influence of THC
# There were no correlations between the effects of IOM − /THC+ on PANSS outcomes and both P3b and P3a amplitudes
radhakrishnan_PANSS_pos_THC_P300a_n=16
radhakrishnan_PANSS_pos_THC_P300a_r=0.31255
# Derive cohen d
res(radhakrishnan_PANSS_pos_THC_P300a_r, n=radhakrishnan_PANSS_pos_THC_P300a_n, dig=20)
# Extract p value
pEstimate_radhakrishnan_PANSS_pos_THC_P300a=data.frame(pEstimate=0.2386, Ntot=radhakrishnan_PANSS_pos_THC_P300a_n)


# ======== Effect of ERP (P300b Amplitudewhile) on pos PANSS while under the influence of THC
# There were no correlations between the effects of IOM − /THC+ on PANSS outcomes and both P3b and P3a amplitudes
radhakrishnan_PANSS_pos_THC_P300b_n=16
radhakrishnan_PANSS_pos_THC_P300b_r=0.18335
# Derive cohen d
res(radhakrishnan_PANSS_pos_THC_P300b_r, n=radhakrishnan_PANSS_pos_THC_P300b_n, dig=20)
# Extract p value
pEstimate_radhakrishnan_PANSS_pos_THC_P300b=data.frame(pEstimate=0.4967, Ntot=radhakrishnan_PANSS_pos_THC_P300b_n)

# NO RATES REPORTED

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Morrison (2009) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Morrison, P. D., V. Zois, D. A. McKeown, T. D. Lee, D. W. Holt, J. F. Powell, S. Kapur, and R. M. Murray. "The acute effects of synthetic intravenous Δ 9-tetrahydrocannabinol on psychosis, mood and cognitive functioning." Psychological medicine 39, no. 10 (2009): 1607-1616.


# ====== Effect of dose on positive symptoms (PANSS)
# Scores on the PANSS positive subscale were increased from baseline following THC but not placebo admin- istration (Friedman’s x2=62, df=7, n=21, p<0.001).
# Note: extracted from Figure: computed based in 95% CI
n_morrison_2.5mgTHC_PANSS_pos=21
morrison_2.5mgTHC_PANSS_pos_mean=10.398625429553265
morrison_2.5mgTHC_PANSS_pos_upper=12.717067583046965
morrison_2.5mgTHC_PANSS_pos_lower=8.080183276059564
morrison_2.5mgTHC_PANSS_pos_se = (morrison_2.5mgTHC_PANSS_pos_upper-morrison_2.5mgTHC_PANSS_pos_lower)/(2*1.96) # calculate the standard error
morrison_2.5mgTHC_PANSS_pos_sd=morrison_2.5mgTHC_PANSS_pos_se*sqrt(n_morrison_2.5mgTHC_PANSS_pos)

morrison_placebo_PANSS_pos_mean=6.844036697247707
morrison_placebo_PANSS_pos_upper=7.211009174311927
morrison_placebo_PANSS_pos_lower=7.0183486238532105
morrison_placebo_PANSS_pos_se = (morrison_placebo_PANSS_pos_upper-morrison_placebo_PANSS_pos_lower)/(2*1.96) # calculate the standard error
morrison_placebo_PANSS_pos_sd=morrison_placebo_PANSS_pos_se*sqrt(n_morrison_2.5mgTHC_PANSS_pos)

# Derive cohen d
extract_morrison_placebo_PANSS_pos=d_meanDiff_dependent(m1=morrison_2.5mgTHC_PANSS_pos_mean, m2=morrison_placebo_PANSS_pos_mean, sd1=morrison_2.5mgTHC_PANSS_pos_sd , sd2=morrison_placebo_PANSS_pos_sd, n=n_morrison_2.5mgTHC_PANSS_pos, r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_morrison_placebo_PANSS_pos=data.frame(pEstimate=0.009, Ntot=n_morrison_2.5mgTHC_PANSS_pos) # p<0.001


# ====== Effect of dose on positive symptoms (CAPE)
# Similarly, sub- ject-rated positive psychotic symptoms as measured by the CAPE-state increased from baseline following THC but not placebo (Friedman’s x2=20, df=7, n=17, p=0.005).
# Community Assessment of Psychic Experiences (CAPE)
# CAPE-state is a validated 42-item self-reported questionnaire, derived from the Peters Delusions Inventory (PDI), which generates a positive, a negative and a depressive dimension score
# Note: CI for placebo not reported => I therefore took the CI from the CAPE in the same condition
n_morrison_placebo_CAPE_pos=17
morrison_2.5mgTHC_CAPE_pos_mean=1.7979568671963675
morrison_2.5mgTHC_CAPE_pos_upper=1.3620885357548234
morrison_2.5mgTHC_CAPE_pos_lower=0.9353007945516452
morrison_2.5mgTHC_CAPE_pos_se = (morrison_2.5mgTHC_CAPE_pos_upper-morrison_2.5mgTHC_CAPE_pos_lower)/(2*1.96) # calculate the standard error
morrison_2.5mgTHC_CAPE_pos_sd=morrison_2.5mgTHC_CAPE_pos_se*sqrt(n_morrison_placebo_CAPE_pos)

morrison_placebo_CAPE_pos_mean=1.0624290578887623
morrison_placebo_CAPE_pos_upper=1.4937570942111236
morrison_placebo_CAPE_pos_lower=0.6129398410896711
morrison_placebo_CAPE_pos_se = (morrison_placebo_CAPE_pos_upper-morrison_placebo_CAPE_pos_lower)/(2*1.96) # calculate the standard error
morrison_placebo_CAPE_pos_sd=morrison_placebo_CAPE_pos_se*sqrt(n_morrison_placebo_CAPE_pos)

# Derive cohen d
extract_morrison_placebo_CAPE_pos=d_meanDiff_dependent(m1=morrison_2.5mgTHC_CAPE_pos_mean,
                                                       m2=morrison_placebo_CAPE_pos_mean,
                                                       sd1=morrison_2.5mgTHC_CAPE_pos_sd ,
                                                       sd2=morrison_placebo_CAPE_pos_sd,
                                                       n=n_morrison_placebo_CAPE_pos,
                                                       r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_morrison_placebo_CAPE_pos=data.frame(pEstimate=NA, Ntot=n_morrison_2.5mgTHC_PANSS_pos) # p<0.001


# ====== Frequency of cannabis use
# Participants who had taken cannabis more extensively in the past were less likely to exhibit positive psychotic symptoms as rated by the PANSS under THC conditions at 30 min post-injection (Spearman’s r=x0.45, p<0.05).
morrison_placebo_PANSS_pos_THC_frequency_r=-.45
# Derive cohen d
res(morrison_placebo_PANSS_pos_THC_frequency_r, n=n_morrison_2.5mgTHC_PANSS_pos, dig=20) # p<0.05
# Extract p value
pEstimate_morrison_placebo_PANSS_pos_THC_frequency=data.frame(pEstimate=NA, Ntot=n_morrison_2.5mgTHC_PANSS_pos)

# NO RATES REPORTED

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Liem-Moolenaar (2010) =====================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Liem-Moolenaar, Marieke, Erik T. te Beek, Marieke L. de Kam, Kari L. Franson, René S. Kahn, Ron Hijman, Daan Touw, and J. MA van Gerven. "Central nervous system effects of haloperidol on THC in healthy male volunteers." Journal of psychopharmacology 24, no. 11 (2010): 1697-1708.

# All 24 subjects received the treatment combinations ‘THC + placebo’ and ‘THC + haloperidol’, but half of the subjects received ‘haloperidol + placebo’ and the other half ‘placebo + placebo
# Each study day started with a dose of 3 mg haloperidol (or its matching placebo) orally.
# After 3 h, three consecutive doses of THC 2, 4 and 6 mg (or matching placebo) were given intrapulmonary
t1=180 # min
t2=270 # min
t3=360 # min
Liem_n=22 # Thirty-five subjects started the study, but only 22 completed all three study days according to protocol.

# ====== Effect of dose on positive symptoms (PANSS positive) - 2mg
n_liem_PANSS_pos=11 # 11 subjects placebo, 11 subjects haloperidol at baseline
liem_placebo_t1_PANSS_pos_mean=-0.48363636363636386
liem_placebo_t1_PANSS_pos_lower=-1.734545454545455
liem_placebo_t1_PANSS_pos_upper=liem_placebo_t1_PANSS_pos_mean+ (liem_placebo_t1_PANSS_pos_mean-liem_placebo_t1_PANSS_pos_lower)
liem_placebo_t1_PANSS_pos_se = (liem_placebo_t1_PANSS_pos_upper-liem_placebo_t1_PANSS_pos_lower)/(2*1.96) # calculate the standard error
liem_placebo_t1_PANSS_pos_sd=liem_placebo_t1_PANSS_pos_se*sqrt(n_liem_PANSS_pos)

liem_2mgTHC_PANSS_pos_mean=1.6545454545454552
liem_2mgTHC_PANSS_pos_upper=2.614545454545456
liem_2mgTHC_PANSS_pos_lower=liem_2mgTHC_PANSS_pos_mean - (liem_2mgTHC_PANSS_pos_upper- liem_2mgTHC_PANSS_pos_mean)
liem_2mgTHC_PANSS_pos_se = (liem_2mgTHC_PANSS_pos_upper-liem_2mgTHC_PANSS_pos_lower)/(2*1.96) # calculate the standard error
liem_2mgTHC_PANSS_pos_sd=liem_2mgTHC_PANSS_pos_se*sqrt(n_liem_PANSS_pos)

# Derive cohen d
extract_liem_2mgTHC_PANSS_pos=d_meanDiff_dependent(m1=liem_2mgTHC_PANSS_pos_mean, m2=liem_placebo_t1_PANSS_pos_mean, sd1=liem_2mgTHC_PANSS_pos_sd , sd2=liem_placebo_t1_PANSS_pos_sd, n=n_liem_PANSS_pos, r=r_withinSubj_between_timepoints)
# Extract p value
liem_THC_PANSS_pos_p=0.0002 # Note: only one p-value for cumulative effect of THC administration vs placebo reported
pEstimate_liem_2mgTHC_PANSS_pos=data.frame(pEstimate=NA, Ntot=n_liem_PANSS_pos)


# ====== Effect of dose on positive symptoms (PANSS positive) - 4mg
liem_placebo_t2_PANSS_pos_mean=-0.21454545454545482
liem_placebo_t2_PANSS_pos_lower=-1.4509090909090916
liem_placebo_t2_PANSS_pos_upper=liem_placebo_t2_PANSS_pos_mean+ (liem_placebo_t2_PANSS_pos_mean-liem_placebo_t2_PANSS_pos_lower)
liem_placebo_t2_PANSS_pos_se = (liem_placebo_t2_PANSS_pos_upper-liem_placebo_t2_PANSS_pos_lower)/(2*1.96) # calculate the standard error
liem_placebo_t2_PANSS_pos_sd=liem_placebo_t2_PANSS_pos_se*sqrt(n_liem_PANSS_pos)

liem_4mgTHC_PANSS_pos_mean=2.5054545454545467
liem_4mgTHC_PANSS_pos_upper=3.4727272727272727
liem_4mgTHC_PANSS_pos_lower=liem_4mgTHC_PANSS_pos_mean - (liem_4mgTHC_PANSS_pos_upper- liem_4mgTHC_PANSS_pos_mean)
liem_4mgTHC_PANSS_pos_se = (liem_4mgTHC_PANSS_pos_upper-liem_4mgTHC_PANSS_pos_lower)/(2*1.96) # calculate the standard error
liem_4mgTHC_PANSS_pos_sd=liem_4mgTHC_PANSS_pos_se*sqrt(n_liem_PANSS_pos)

# Derive cohen d
extract_liem_4mgTHC_PANSS_pos=d_meanDiff_dependent(m1=liem_4mgTHC_PANSS_pos_mean, m2=liem_placebo_t2_PANSS_pos_mean, sd1=liem_4mgTHC_PANSS_pos_sd , sd2=liem_placebo_t2_PANSS_pos_sd, n=n_liem_PANSS_pos, r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_liem_4mgTHC_PANSS_pos=data.frame(pEstimate=NA, Ntot=n_liem_PANSS_pos)


# ====== Effect of dose on positive symptoms (PANSS positive) - 6mg
liem_placebo_t3_PANSS_pos_mean=-0.4690909090909092
liem_placebo_t3_PANSS_pos_lower=-1.7200000000000004
liem_placebo_t3_PANSS_pos_upper=liem_placebo_t3_PANSS_pos_mean+ (liem_placebo_t3_PANSS_pos_mean-liem_placebo_t3_PANSS_pos_lower)
liem_placebo_t3_PANSS_pos_se = (liem_placebo_t3_PANSS_pos_upper-liem_placebo_t3_PANSS_pos_lower)/(2*1.96) # calculate the standard error
liem_placebo_t3_PANSS_pos_sd=liem_placebo_t3_PANSS_pos_se*sqrt(n_liem_PANSS_pos)

liem_6mgTHC_PANSS_pos_mean=2.1054545454545464
liem_6mgTHC_PANSS_pos_upper=3.0654545454545463
liem_6mgTHC_PANSS_pos_lower=liem_6mgTHC_PANSS_pos_mean - (liem_6mgTHC_PANSS_pos_upper- liem_6mgTHC_PANSS_pos_mean)
liem_6mgTHC_PANSS_pos_se = (liem_6mgTHC_PANSS_pos_upper-liem_6mgTHC_PANSS_pos_lower)/(2*1.96) # calculate the standard error
liem_6mgTHC_PANSS_pos_sd=liem_6mgTHC_PANSS_pos_se*sqrt(n_liem_PANSS_pos)

# Derive cohen d
extract_liem_6mgTHC_PANSS_pos=d_meanDiff_dependent(m1=liem_6mgTHC_PANSS_pos_mean, m2=liem_placebo_t3_PANSS_pos_mean, sd1=liem_6mgTHC_PANSS_pos_sd , sd2=liem_placebo_t3_PANSS_pos_sd, n=n_liem_PANSS_pos, r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_liem_6mgTHC_PANSS_pos=data.frame(pEstimate=NA, Ntot=n_liem_PANSS_pos)


# ====== Dose response effects
# Not formally tested for significance


# ====== Effect of D2 blocker on THC-induced positive symptoms
# In the current study, THC mainly affected the individual positive PANSS items ‘delusions’, ‘conceptual disorganiza- tion’ and ‘hallucinatory behaviour’. ‘Excitement’, ‘grandi- osity’, ‘suspiciousness/persecution’ and ‘hostility’ did not change significantly. Haloperidol completely reversed THC-induced increases in ‘delusions’ and ‘conceptual disor- ganization’ and almost halved the increase in ‘hallucinatory behaviour’.
n_effective_liem_PANSS_pos=n_effective_dependent(n_liem_PANSS_pos, r=r_withinSubj_between_timepoints)
liem_PANSS_pos_THC_vs_haloperidol_p=0.03
pes(p=liem_PANSS_pos_THC_vs_haloperidol_p, n.1=n_effective_liem_PANSS_pos/2, n.2=n_effective_liem_PANSS_pos/2, dig=20)
pEstimate_liem_PANSS_pos_THC_vs_haloperidol=data.frame(pEstimate=liem_PANSS_pos_THC_vs_haloperidol_p, Ntot=n_liem_PANSS_pos)

# NO RATES REPORTED


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Stoke (2009) ==============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Stokes, Paul RA, Mitul A. Mehta, H. Valerie Curran, Gerome Breen, and Paul M. Grasby. "Can recreational doses of THC produce significant dopamine release in the human striatum?." Neuroimage 48, no. 1 (2009): 186-190.

stoke_10mgTHC_n=13

# ==== PSI total score
stoke_10mgTHC_PSI_tot_mean=15.9
stoke_10mgTHC_PSI_tot_sd=11
stoke_placebo_PSI_tot_mean=0.8
stoke_placebo_PSI_tot_sd=3.7
# Derive cohen d
extract_stoke_10mgTHC_PSI_tot=d_meanDiff_dependent(m1=stoke_10mgTHC_PSI_tot_mean,
                                                   m2=stoke_placebo_PSI_tot_mean,
                                                   sd1=stoke_10mgTHC_PSI_tot_sd ,
                                                   sd2=stoke_placebo_PSI_tot_sd,
                                                   n=stoke_10mgTHC_n,
                                                   r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_stoke_10mgTHC_PSI_tot=data.frame(pEstimate=NA, Ntot=stoke_10mgTHC_n)


# ==== PSI delusiobs
stoke_10mgTHC_PSI_delusion_mean=0.4
stoke_10mgTHC_PSI_delusion_sd=1.6
stoke_placebo_PSI_delusion_mean=0.2
stoke_placebo_PSI_delusion_sd=0.6
# Derive cohen d
extract_stoke_10mgTHC_PSI_delusion=d_meanDiff_dependent(m1=stoke_10mgTHC_PSI_delusion_mean,
                                                        m2=stoke_placebo_PSI_delusion_mean,
                                                        sd1=stoke_10mgTHC_PSI_delusion_sd ,
                                                        sd2=stoke_placebo_PSI_delusion_sd,
                                                        n=stoke_10mgTHC_n,
                                                        r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_stoke_10mgTHC_PSI_delusion=data.frame(pEstimate=0.6, Ntot=stoke_10mgTHC_n) # There was no significant effect of THC on the delusory thinking subscale (F1,12 = 0.3, p = 0.6).

# ==== PSI paranoia
stoke_10mgTHC_PSI_paranoia_mean=0.9
stoke_10mgTHC_PSI_paranoia_sd=1.4
stoke_placebo_PSI_paranoia_mean=0.1
stoke_placebo_PSI_paranoia_sd=0.3
# Derive cohen d
extract_stoke_10mgTHC_PSI_paranoia=d_meanDiff_dependent(m1=stoke_10mgTHC_PSI_paranoia_mean,
                                                        m2=stoke_placebo_PSI_paranoia_mean,
                                                        sd1=stoke_10mgTHC_PSI_paranoia_sd ,
                                                        sd2=stoke_placebo_PSI_paranoia_sd,
                                                        n=stoke_10mgTHC_n,
                                                        r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_stoke_10mgTHC_PSI_paranoia=data.frame(pEstimate=NA, Ntot=stoke_10mgTHC_n) # paranoia (F1,12 = 4.3, p < 0.06)

# ==== Plasma levels
# There was no significant correlation between post-dose plasma THC levels and change in either total PSI scores (r and pvalues) or any of the subscales.
n_effective_stoke_10mgTHC=n_effective_dependent(stoke_10mgTHC_n, r=r_withinSubj_between_timepoints)
pes(p=pValueNS_allStudies, n.1=n_effective_stoke_10mgTHC/2, n.2=n_effective_stoke_10mgTHC/2, dig=20)
pEstimate_stoke_10mgTHC_plasma=data.frame(pEstimate=NA, Ntot=stoke_10mgTHC_n)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== D’Souza (2008) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: D’Souza, Deepak Cyril, Gabriel Braley, Rebecca Blaise, Michael Vendetti, Stephen Oliver, Brian Pittman, Mohini Ranganathan et al. "Effects of haloperidol on the behavioral, subjective, cognitive, motor, and neuroendocrine effects of Δ-9-tetrahydrocannabinol in humans." Psychopharmacology 198, no. 4 (2008): 587-603.
# D’Souza et al. used doses of active oral haloperidol (0.057 mg/kg) or placebo in random order followed 90 and 215min later by a fixed-order intravenous administration over 20min of pla- cebo and active (0.0286 mg/kg) THC, respectively.
# no blood concentrations were measured in D’Souza et al.’s study

# 2 test days (random order)
#   => Test day 1: placebo followed by Δ-9-THC (0.0286 mg/kg) administered intravenously
#   => Test day 2: Active (0.057 mg/kg) haloperidol
dsouza_IV_THC_n=28-8 # Healthy subjects (no cannabis abuse) =17; Frequent cannabis users = 11; n=8 subjects (five healthy subjects and three frequent users) did not complete the study


# ==== THC on PANSS positive
#  Δ-9-THC produced significant increases PANSS positive symptoms subscale (ATS = 10.1, num df = 2.8, adjusted P < 0.0001),
dsouza2008_IV_THC_PANSS_pos_mean=9.13
dsouza2008_IV_THC_PANSS_pos_sd=1.39
dsouza2008_placebo_PANSS_pos_mean=7.7
dsouza2008_placebo_PANSS_pos_sd=0.76
# Derive cohen d
extract_dsouza2008_IV_THC_PANSS_pos=d_meanDiff_dependent(m1=dsouza2008_IV_THC_PANSS_pos_mean,
                                                         m2=dsouza2008_placebo_PANSS_pos_mean,
                                                         sd1=dsouza2008_IV_THC_PANSS_pos_sd ,
                                                         sd2=dsouza2008_placebo_PANSS_pos_sd,
                                                         n=dsouza_IV_THC_n, r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_dsouza2008_IV_THC_PANSS_pos=data.frame(pEstimate=0.00009, Ntot=dsouza_IV_THC_n) # P < 0.0001


# ==== Haloperidol pretreatment on PANSS positive following THC
# Subjects completed 2 test days during which they received placebo or active (0.057 mg/kg) haloperidol in random counterbalanced order,
# there was no effect of haloperidol (reported in study)
# While PANSS positive symptoms scores induced by Δ-9-THC were lower on the active vs the placebo haloperidol condition (see Table 4), these effects were not statistically significant.
dsouza2008_haloperidol_PANSS_pos_mean=8.64
dsouza2008_haloperidol_PANSS_pos_sd=1.29
# Derive cohen d
extract_dsouza2008_haloperidol_PANSS_pos=d_meanDiff_dependent(m1=dsouza2008_haloperidol_PANSS_pos_mean,
                                                              m2=dsouza2008_IV_THC_PANSS_pos_mean,
                                                              sd1=dsouza2008_haloperidol_PANSS_pos_sd ,
                                                              sd2=dsouza2008_IV_THC_PANSS_pos_sd,
                                                              n=dsouza_IV_THC_n, r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_dsouza2008_haloperidol_PANSS_pos=data.frame(pEstimate=pValueNS_allStudies, Ntot=dsouza_IV_THC_n) # >0.05


# Effect of haloperidol on THC-induced effects
# Haloperidol pretreatment did not reduce any of the behavioral effects of Δ-9-THC.


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== D’Souza (2008b) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: D'souza, Deepak Cyril, Mohini Ranganathan, Gabriel Braley, Ralitza Gueorguieva, Zoran Zimolo, Thomas Cooper, Edward Perry, and John Krystal. "Blunted psychotomimetic and amnestic effects of Δ-9-tetrahydrocannabinol in frequent users of cannabis." Neuropsychopharmacology 33, no. 10 (2008): 2505-2516.
dsouza2008b_frequ_user_n=30
dsouza2008b_control_n=22

# Cannabis group (Placebo condition)
dsouza_placebo_frequ_user_PANSS_tot_mean=0.14767932489451552
dsouza_placebo_frequ_user_PANSS_tot_se_upper=0.48523206751055004
dsouza_placebo_frequ_user_PANSS_tot_se=dsouza_placebo_frequ_user_PANSS_tot_se_upper-dsouza_placebo_frequ_user_PANSS_tot_mean
dsouza_placebo_frequ_user_PANSS_tot_sd=dsouza_placebo_frequ_user_PANSS_tot_se*sqrt(dsouza2008b_frequ_user_n)
# Control group (Placebo condition)
dsouza_placebo_control_PANSS_tot_mean=0.14767932489451552
dsouza_placebo_control_PANSS_tot_se_upper=0.48523206751055004
dsouza_placebo_control_PANSS_tot_se=dsouza_placebo_control_PANSS_tot_se_upper-dsouza_placebo_control_PANSS_tot_mean
dsouza_placebo_control_PANSS_tot_sd=dsouza_placebo_control_PANSS_tot_se*sqrt(dsouza2008b_control_n)
# Cannabis group (2.5mg THC)
dsouza_IV_2.5mgTHC_frequ_user_PANSS_tot_mean=3.206751054852321
dsouza_IV_2.5mgTHC_frequ_user_PANSS_tot_se_lower=2.341772151898737
dsouza_IV_2.5mgTHC_frequ_user_PANSS_tot_se=dsouza_IV_2.5mgTHC_frequ_user_PANSS_tot_mean-dsouza_IV_2.5mgTHC_frequ_user_PANSS_tot_se_lower
dsouza_IV_2.5mgTHC_frequ_user_PANSS_tot_sd=dsouza_IV_2.5mgTHC_frequ_user_PANSS_tot_se*sqrt(dsouza2008b_frequ_user_n)
# Control group (2.5mg THC)
dsouza_IV_2.5mgTHC_control_PANSS_tot_mean=8.396624472573842
dsouza_IV_2.5mgTHC_control_PANSS_tot_se_lower= 6.118143459915615
dsouza_IV_2.5mgTHC_control_PANSS_tot_se=dsouza_IV_2.5mgTHC_control_PANSS_tot_mean-dsouza_IV_2.5mgTHC_control_PANSS_tot_se_lower
dsouza_IV_2.5mgTHC_control_PANSS_tot_sd=dsouza_IV_2.5mgTHC_control_PANSS_tot_se*sqrt(dsouza2008b_control_n)
# Cannabis group (5mg THC)
dsouza_IV_5mgTHC_frequ_user_PANSS_tot_mean=5.41432584269663
dsouza_IV_5mgTHC_frequ_user_PANSS_tot_se_lower=4.382022471910115
dsouza_IV_5mgTHC_frequ_user_PANSS_tot_se=dsouza_IV_5mgTHC_frequ_user_PANSS_tot_mean-dsouza_IV_5mgTHC_frequ_user_PANSS_tot_se_lower
dsouza_IV_5mgTHC_frequ_user_PANSS_tot_sd=dsouza_IV_5mgTHC_frequ_user_PANSS_tot_se*sqrt(dsouza2008b_frequ_user_n)
# Control group (5mg THC)
dsouza_IV_5mgTHC_control_PANSS_tot_mean=10.660112359550562
dsouza_IV_5mgTHC_control_PANSS_tot_se_upper=13.167134831460675
dsouza_IV_5mgTHC_control_PANSS_tot_se=dsouza_IV_5mgTHC_control_PANSS_tot_se_upper-dsouza_IV_5mgTHC_control_PANSS_tot_mean
dsouza_IV_5mgTHC_control_PANSS_tot_sd=dsouza_IV_5mgTHC_control_PANSS_tot_se*sqrt(dsouza2008b_control_n)


# ====== Effect of dose on positive symptoms (PANSS tot) - frequent user - 2.5mg
extract_dsouza_IV_2.5mgTHC_frequ_user_PANSS_tot=d_meanDiff_dependent(
  m1=dsouza_IV_2.5mgTHC_frequ_user_PANSS_tot_mean,
  m2=dsouza_placebo_frequ_user_PANSS_tot_mean,
  sd1=dsouza_IV_2.5mgTHC_frequ_user_PANSS_tot_sd ,
  sd2=dsouza_placebo_frequ_user_PANSS_tot_sd,
  n=dsouza2008b_frequ_user_n,
  r=r_withinSubj_between_timepoints)
# Extract p-value
pEstimate_dsouza_IV_2.5mgTHC_frequ_user_PANSS_tot=data.frame(pEstimate=NA, Ntot=dsouza2008b_frequ_user_n) # Not reported

# ====== Effect of dose on positive symptoms (PANSS tot) - control - 2.5mg
extract_dsouza_IV_2.5mgTHC_control_PANSS_tot=d_meanDiff_dependent(
  m1=dsouza_IV_2.5mgTHC_control_PANSS_tot_mean,
  m2=dsouza_placebo_control_PANSS_tot_mean,
  sd1=dsouza_IV_2.5mgTHC_control_PANSS_tot_sd ,
  sd2=dsouza_placebo_control_PANSS_tot_sd,
  n=dsouza2008b_control_n,
  r=r_withinSubj_between_timepoints)

# Extract p-value
pEstimate_dsouza_IV_2.5mgTHC_control_PANSS_tot=data.frame(pEstimate=NA, Ntot=dsouza2008b_control_n) # Not reported


# ====== Effect of dose on positive symptoms (PANSS tot) - frequent user - 5mg
extract_dsouza_IV_5mgTHC_frequ_user_PANSS_tot=d_meanDiff_dependent(
  m1=dsouza_IV_5mgTHC_frequ_user_PANSS_tot_mean,
  m2=dsouza_placebo_frequ_user_PANSS_tot_mean,
  sd1=dsouza_IV_5mgTHC_frequ_user_PANSS_tot_sd ,
  sd2=dsouza_placebo_frequ_user_PANSS_tot_sd,
  n=dsouza2008b_frequ_user_n,
  r=r_withinSubj_between_timepoints)
# Extract p-value
pEstimate_dsouza_IV_5mgTHC_frequ_user_PANSS_tot=data.frame(pEstimate=NA, Ntot=dsouza2008b_frequ_user_n) # Not reported

# ====== Effect of dose on positive symptoms (PANSS tot) - control - 5mg
extract_dsouza_IV_5mgTHC_control_PANSS_tot=d_meanDiff_dependent(
  m1=dsouza_IV_5mgTHC_control_PANSS_tot_mean,
  m2=dsouza_placebo_control_PANSS_tot_mean,
  sd1=dsouza_IV_5mgTHC_control_PANSS_tot_sd ,
  sd2=dsouza_placebo_control_PANSS_tot_sd,
  n=dsouza2008b_control_n,
  r=r_withinSubj_between_timepoints)
# Extract p-value
pEstimate_dsouza_IV_5mgTHC_control_PANSS_tot=data.frame(pEstimate=NA, Ntot=dsouza2008b_control_n) # Not reported

# Dose response effects
# Included in Ganesh (2020)

# ========= Frequency cannabis users vs. non-frequenc user in THC-induced PANSS symptoms
# Difference between abusers and controls was significant 5mg dose at the 10min (ATS = 5.76,df = 1, p = 0.016),
# Included in Ganesh (2020)

# no rates reported

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Freeman (2015) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ========= Effect of single dose THC
# Paranoid VAS: 6 visual analogue scales (“Right now I feel suspicious of other people,” “Right now I feel that people want to harm me,” “Right now I feel like peo- ple want to upset me,” “Right now I feel like people are against me,” “Right now I am thinking that others are trying to persecute me,” and “Right now I feel like peo- ple are being hostile towards me”)
# PCA was undertaken (on the correlation matrix) for all 6 measures together (Paranoid VAS post THC administration, Paranoid VAS at end of testing, Social Situation Paranoia, SSPS, VAS VR Hostile, and PANSS Suspiciousness)
# An ANCOVA with the paranoia principal component as the dependent variable, and controlling for baseline paranoia and anxiety levels, showed that THC signifi- cantly increases paranoia, THC coefficient = 0.91, SE = 0.43, t = 2.15, P = .034,
# Get cohen d
freeman_paranoia_1.5mgTHC_t=2.15
freeman_paranoia_1.5mgTHC_n=41
freeman_paranoia_placebo_n=41
freeman_paranoia_1.5mgTHCExtracted=compute.es::tes(freeman_paranoia_1.5mgTHC_t ,
                                                   freeman_paranoia_1.5mgTHC_n ,
                                                   freeman_paranoia_placebo_n, dig=20)
# Get p-value
pEstimate_freeman_paranoia_1.5mgTHC=data.frame(pEstimate=0.034, Ntot=freeman_paranoia_1.5mgTHC_n + freeman_paranoia_placebo_n)  # P = .034

# ========= Effect of cognitive awareness training
# The cognitive awareness condition, given before THC administration, involved a simple 5-minute educational module, explaining the range of effects that the drug can cause (THC was considered synonymous with cannabis for this procedure).
# Cognitive Awareness coef- ficient = 0.51, SE = 0.44, t = 1.16, P = .247
# The awareness condition may have increased sensitivity to paranoid thoughts.
# Get cohen d
freeman_paranoia_cogawareness_1.5mgTHC_t=1.16
freeman_paranoia_cogawareness_n=39
freeman_paranoia_cogawareness_1.5mgTHCExtracted=compute.es::tes(freeman_paranoia_cogawareness_1.5mgTHC_t,
                                                                freeman_paranoia_1.5mgTHC_n,
                                                                freeman_paranoia_cogawareness_n,
                                                                dig=20)
# Get p-value
pEstimate_freeman_paranoia_cogawareness_1.5mgTHC=data.frame(pEstimate=0.247 , Ntot=freeman_paranoia_1.5mgTHC_n + freeman_paranoia_cogawareness_n)  # P = .247

# No rates reported

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Tunbridge (2015) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Tunbridge, Elizabeth M., Graham Dunn, Robin M. Murray, Nicole Evans, Rachel Lister, Katharina Stumpenhorst, Paul J. Harrison, Paul D. Morrison, and Daniel Freeman. "Genetic moderation of the effects of cannabis: catechol-O-methyltransferase (COMT) affects the impact of Δ9-tetrahydrocannabinol (THC) on working memory performance but not on the occurrence of psychotic experiences." Journal of psychopharmacology 29, no. 11 (2015): 1146-1151.

# COMT x THC interaction
#     COMT (rs4680; A/A, A/G, G/G)
#     rs4680 = A = Met
#     rs4680 = G = Val
# Val/Met and Met/Met individuals were pooled into a single ‘Met carrier’ group for analysis purposes
#     COMT (Met carrier vs Val/Val carrier)
# there was no drug*genotype interac- tion (B = −0.038 ± 0.048; 95% CI: −0.137 to 0.053; p=0.521).

tunbridge2015_COMT_CAPEpos_n=78
tunbridge2015_COMT_CAPEpos_beta=-0.038
tunbridge2015_COMT_CAPEpos_se=0.048
tunbridge2015_COMT_CAPEpos_p=0.521
# Get cohen d
extracted_tunbridge2015_COMT_CAPEpos=beta_to_d_independent(beta=tunbridge2015_COMT_CAPEpos_beta,
                                                           se=tunbridge2015_COMT_CAPEpos_se,
                                                           n=tunbridge2015_COMT_CAPEpos_n) # p-value matches study p-value
# Get p-value
pEstimate_tunbridge2015_COMT_CAPEpos=data.frame(pEstimate=tunbridge2015_COMT_CAPEpos_p, Ntot=tunbridge2015_COMT_CAPEpos_n)

# No rates reported


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Peters (2009) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

peters_FEP_cases=52
peters_UHR_cases=17
peters_healthy_cannabis=52


# ===== Self-reported Unusual visual experiences: healthy vs FEP
# FEP
allA=peters_FEP_cases
percA=29
NriskA_case=round((allA/100)*percA,0)
NriskA_control=allA-NriskA_case
# Healthy
allB=peters_healthy_cannabis
percB=8
NriskB_case=round((allB/100)*percB,0)
NriskB_control=allB-NriskB_case
# Get chie square
labelA="FEP"
labelB="Healthy"
extract_peters_FEPvsHealthy_visualH=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_peters_FEPvsHealthy_visualH=data.frame(pEstimate=NA, Ntot=extract_peters_FEPvsHealthy_visualH$N.total)


# ===== Self-reported Unusual visual experiences: healthy vs UHR
# FEP
allA=peters_UHR_cases
percA=25
NriskA_case=round((allA/100)*percA,0)
NriskA_control=allA-NriskA_case
# Healthy
allB=peters_healthy_cannabis
percB=8
NriskB_case=round((allB/100)*percB,0)
NriskB_control=allB-NriskB_case
# Get chie square
labelA="UHR"
labelB="Healthy"
extract_peters_UHRvsHealthy_visualH=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_peters_UHRvsHealthy_visualH=data.frame(pEstimate=NA, Ntot=extract_peters_UHRvsHealthy_visualH$N.total)


# ===== Self-reported Unusual bodily experiences: healthy vs FEP
# FEP
allA=peters_FEP_cases
percA=31
NriskA_case=round((allA/100)*percA,0)
NriskA_control=allA-NriskA_case
# Healthy
allB=peters_healthy_cannabis
percB=21
NriskB_case=round((allB/100)*percB,0)
NriskB_control=allB-NriskB_case
# Get chie square
labelA="FEP"
labelB="Healthy"
extract_peters_FEPvsHealthy_tactileH=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_peters_FEPvsHealthy_tactileH=data.frame(pEstimate=NA, Ntot=extract_peters_FEPvsHealthy_tactileH$N.total)

# ===== Self-reported Unusual bodily experiences:  healthy vs UHR
# FEP
allA=peters_UHR_cases
percA=25
NriskA_case=round((allA/100)*percA,0)
NriskA_control=allA-NriskA_case
# Healthy
allB=peters_healthy_cannabis
percB=21
NriskB_case=round((allB/100)*percB,0)
NriskB_control=allB-NriskB_case
# Get chie square
labelA="UHR"
labelB="Healthy"
extract_peters_UHRvsHealthy_tactileH=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_peters_UHRvsHealthy_tactileH=data.frame(pEstimate=NA, Ntot=extract_peters_UHRvsHealthy_tactileH$N.total)



# === Self-reported hearing voices:  healthy vs FEP
# FEP
allA=peters_FEP_cases
percA=27
NriskA_case=round((allA/100)*percA,0)
NriskA_control=allA-NriskA_case
# Healthy
allB=peters_healthy_cannabis
percB=2
NriskB_case=round((allB/100)*percB,0)
NriskB_control=allB-NriskB_case
# Get chie square
labelA="FEP"
labelB="Healthy"
extract_peters_FEPvsHealthy_auditoryH=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_peters_FEPvsHealthy_auditoryH=data.frame(pEstimate=NA, Ntot=extract_peters_FEPvsHealthy_auditoryH$N.total)


# ===== Self-reported hearing voices: healthy vs UHR
# FE# UHR
allA=peters_UHR_cases
percA=18
NriskA_case=round((allA/100)*percA,0)
NriskA_control=allA-NriskA_case
# Healthy
allB=peters_healthy_cannabis
percB=2
NriskB_case=round((allB/100)*percB,0)
NriskB_control=allB-NriskB_case
# Get chie square
labelA="UHR"
labelB="Healthy"
extract_peters_UHRvsHealthy_auditoryH=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_peters_UHRvsHealthy_auditoryH=data.frame(pEstimate=NA, Ntot=extract_peters_UHRvsHealthy_auditoryH$N.total)



# +# ===== Self-reported suspicion:  healthy vs FEP
# FEP
allA=peters_FEP_cases
percA=52
NriskA_case=round((allA/100)*percA,0)
NriskA_control=allA-NriskA_case
# Healthy
allB=peters_healthy_cannabis
percB=6
NriskB_case=round((allB/100)*percB,0)
NriskB_control=allB-NriskB_case
# Get chie square
labelA="FEP"
labelB="Healthy"
extract_peters_FEPvsHealthy_paranoia=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_peters_FEPvsHealthy_paranoia=data.frame(pEstimate=NA, Ntot=extract_peters_FEPvsHealthy_paranoia$N.total)



# ===== Self-reported suspicion: healthy vs UHR
# UHR
allA=peters_UHR_cases
percA=41
NriskA_case=round((allA/100)*percA,0)
NriskA_control=allA-NriskA_case
# Healthy
allB=peters_healthy_cannabis
percB=6
NriskB_case=round((allB/100)*percB,0)
NriskB_control=allB-NriskB_case
# Get chie square
labelA="UHR"
labelB="Healthy"
extract_peters_UHRvsHealthy_paranoia=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_peters_UHRvsHealthy_paranoia=data.frame(pEstimate=NA, Ntot=extract_peters_UHRvsHealthy_paranoia$N.total)




# RATES
peters2009_canHalDF=data.frame(event=(52/100)*2, n=52)  # rates in healthy subjects only
peters2009_canParDF=data.frame(event=(52/100)*6, n=52)  # rates in healthy subjects only




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Green (2004) ==============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Green, Bob, David Kavanagh, and Ross MCD Young. "Reasons for cannabis use in men with and without psychosis." Drug and alcohol review 23, no. 4 (2004): 445-453.

green_cases_n=45 # The final sample of participants with psychosis consisted of 45 men.
green_healthy_n=47 # resulting in a final sample of 47 men

# ===== Psychotic symptoms - baseline
# Get cohen d
extract_greenNegativePsych_baseline=pes(p=0.05, n.1=green_cases_n, n.2=green_healthy_n) # The only statistically significant difference between the groups on most negative effects at baseline was for ‘psychotic’ symptoms (Fisher’s exact test, p = 0.050).
# Get p-value
pEstimate_green_cases_vs_healthy_symptoms_baseline=data.frame(pEstimate=0.05, Ntot=green_healthy_n+green_cases_n)

# ===== Paranoia
# paranoia, were the most frequently reported negative effect for participants with psychosis (24.4% vs. 8.5%),
allA=green_cases_n
percA=24.4
NriskA_case=round((allA/100)*percA,0)
NriskA_control=allA-NriskA_case
# Healthy
allB=green_healthy_n
percB=8.5
NriskB_case=round((allB/100)*percB,0)
NriskB_control=allB-NriskB_case
# Get chie square
labelA="Case"
labelB="Healthy"
extract_green_cases_vs_healthy_paranoia_baseline=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_green_cases_vs_healthy_paranoia_baseline=data.frame(pEstimate=NA, Ntot=extract_green_cases_vs_healthy_paranoia_baseline$N.total)


# RATES
green2004_canParDF=data.frame(event=(green_healthy_n/100)*8.5, n=green_healthy_n)  # rates in healthy subjects only



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Englund (2013) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Englund, Amir, Paul D. Morrison, Judith Nottage, Dominic Hague, Fergus Kane, Stefania Bonaccorso, James M. Stone et al. "Cannabidiol inhibits THC-elicited paranoid symptoms and hippocampal-dependent memory impairment." Journal of psychopharmacology 27, no. 1 (2013): 19-27.

# ========== Englund (CBD) ===========
# Healthy participants were randomised to receive oral CBD 600 mg (n=22) or placebo (n=26), 210 min ahead of intravenous (IV) THC (1.5 mg).
englund2013_CBD_n=22
englund2013_THC_n=26

# ========= Effect of 1.5mg THC
#  In the placebo group, PANSS positive scores, (mean±sd) increased by 2.4 (±3.1) points following THC
englund2013_THC_placebo_mean_diff=2.4
englund2013_THC_placebo_sd_diff=3.1
# Derive Cohen D
extract_englund2013_1.5THC_placebo_panss_pos=d_meanDiff_dependent(m_diff=englund2013_THC_placebo_mean_diff,
                                                                  sd_diff=englund2013_THC_placebo_sd_diff,
                                                                  n=englund2013_THC_n,
                                                                  r=r_withinSubj_between_timepoints)
# Get p-value
pEstimate_englund2013_1.5THC_placebo_panss_pos=data.frame(pEstimate=NA, Ntot=englund2013_THC_n) # There was a main effect of CONDI- TION (F=27.9, p<0.000)


# ========= Effect of CBD pre-treatment (increase in PANSS pos score)
# Pre-treatment with oral CBD reduced the paranoiainducing effects of intravenously administered THC
# In the placebo group, PANSS positive scores, (mean±sd) increased by 2.4 (±3.1) points following THC, compared with 1.2 (±1.8) in the CBD group, a non-significant difference (t=1.5, p=0.15)
# Placebo condition
englund2013_THC_placebo_panss_pos_increase_mean=2.4
englund2013_THC_placebo_panss_pos_increase_sd=3.1
# CBD condition
englund2013_THC_CBD_panss_pos_increase_mean=1.2
englund2013_THC_CBD_panss_pos_increase_sd=1.8
# Derive Cohen D
extract_englund2013_THC_CBD_panss_pos=mes(englund2013_THC_CBD_panss_pos_increase_mean, englund2013_THC_placebo_panss_pos_increase_mean, englund2013_THC_CBD_panss_pos_increase_sd, englund2013_THC_placebo_panss_pos_increase_sd, englund2013_CBD_n, englund2013_THC_n, dig=20)
# Get p-value
pEstimate_englund2013_THC_CBD_panss_pos_increase=data.frame(pEstimate=0.15, Ntot=englund2013_CBD_n+englund2013_THC_n)

# ========= Effect of CBD pre-treatment (acute psychotic reaction)
# PANSS positive scores of ≥3 points, were more common in the group pre-treated with placebo (11 of 26 cases) compared with the group pre-treated with CBD (3 of 22 cases), (χ2=4.74, p<0.05)
#  the odds of developing a clinically significant acute psychotic (1.5 mg), defined as a ≥3-point increase from baseline on the PANSS positive subscale reaction to IV THC
englund2013_THC_CBD_actue_psychotic_chisqu=4.74
# Get cohen d
extract_chiSqu_englund2013_THC_CBD_panss_pos=chies(englund2013_THC_CBD_actue_psychotic_chisqu, englund2013_CBD_n+englund2013_THC_n, dig=50)
# Get p-value
pEstimate_englund2013_THC_CBD_actue_psychotic=data.frame(pEstimate=0.049, Ntot=englund2013_CBD_n+englund2013_THC_n) # p<0.05

# ========= Effect of CBD pre-treatment (paranoia)
# post-THC paranoia, as rated with the State Social Paranoia Scale (SSPS), was less in the CBD group compared with the placebo group (t=2.28, p<0.05).
# The increase in SSPS scores post-THC, with respect to baseline, was greater in the placebo versus the CBD group (t=2.28, p<0.05)
englund2013_THC_CBD_actue_paranoia_t=2.28
# Get cohen d
englund2013_THC_CBD_actue_paranoiaExtract=compute.es::tes(englund2013_THC_CBD_actue_paranoia_t,
                                                          englund2013_CBD_n,
                                                          englund2013_THC_n, dig=20)
# Get p-value
pEstimate_englund2013_THC_CBD_paranoia=data.frame(pEstimate=0.049, Ntot=englund2013_CBD_n+englund2013_THC_n) # p<0.05

# rates
# Clinically significant positive symptoms following THC, defined as an increase in PANSS positive scores of ≥3 points, were more common in the group pre-treated with placebo (11 of 26 cases)
englund2013_THC_panssposDF=data.frame(event=11, n=26)  

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Morgan (2010) ============================+
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Morgan CJ, Schafer G, Freeman TP, et al. (2010) Impact of cannabidiol on the acute memory and psychotomimetic effects of smoked cannabis: naturalistic study: naturalistic study. Br J Psychiatry 197: 285–290.


morgan2010_lowCBD_n=22
morgan2010_highCBD_n=22
# The low-cannabidiol group (n = 22) comprised individuals whose samples had less than 0.14% cannabidiol and the high-cannabidiol group (n = 22) those whose samples had more than 0.75% cannabidiol

# ======= Low CBD in THC induced changes in PSI
morgan2010_lowCBD_PSI_intoxicated_mean=34.38
morgan2010_lowCBD_PSI_intoxicated_sd=19.99
morgan2010_lowCBD_PSI_sober_mean=18.57
morgan2010_lowCBD_PSI_sober_sd=11.18
# Derive cohen d
extract_morgan2010_lowCBD_PSI=d_meanDiff_dependent(m1=morgan2010_lowCBD_PSI_intoxicated_mean,
                                                   m2=morgan2010_lowCBD_PSI_sober_mean,
                                                   sd1=morgan2010_lowCBD_PSI_intoxicated_sd ,
                                                   sd2=morgan2010_lowCBD_PSI_sober_sd,
                                                   n=morgan2010_lowCBD_n, r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_morgan2010_lowCBD_PSI=data.frame(pEstimate=NA, Ntot=morgan2010_lowCBD_n) # # Not reported


# ======= High CBD in THC induced changes in PSI
morgan2010_highCBD_PSI_intoxicated_mean=25.90
morgan2010_highCBD_PSI_intoxicated_sd=15.16
morgan2010_highCBD_PSI_sober_mean=18.19
morgan2010_highCBD_PSI_sober_sd=15.35
# Derive cohen d
extract_morgan2010_highCBD_PSI=d_meanDiff_dependent(m1=morgan2010_highCBD_PSI_intoxicated_mean,
                                                    m2=morgan2010_highCBD_PSI_sober_mean,
                                                    sd1=morgan2010_highCBD_PSI_intoxicated_sd ,
                                                    sd2=morgan2010_highCBD_PSI_sober_sd,
                                                    n=morgan2010_highCBD_n, r=r_withinSubj_between_timepoints)

pEstimate_morgan2010_highCBD_PSI=data.frame(pEstimate=NA, Ntot=morgan2010_highCBD_n) # Not reported

# Compare THC induced symptoms in high vs low CBD group
extract_morgan2010_highCBD_vs_lowCBD_PSI=compare_2cohenD(d1=extract_morgan2010_highCBD_PSI$d,
                                                         d1.var=extract_morgan2010_highCBD_PSI$var.d,
                                                         d2=extract_morgan2010_lowCBD_PSI$d,
                                                         d2.var=extract_morgan2010_lowCBD_PSI$var.d,
                                                         n=morgan2010_highCBD_n+morgan2010_lowCBD_n,
                                                         method="independent")
# Extract p value
pEstimate_morgan2010_highCBD_vs_lowCBD_PSI=data.frame(pEstimate=NA, Ntot=morgan2010_highCBD_n+morgan2010_lowCBD_n) # >0.05

# rates not reported

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Kleinloog (2012) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Kleinloog, Daniël, Marieke Liem-Moolenaar, Gabriël Jacobs, Erica Klaassen, Marieke de Kam, Ron Hijman, and Joop van Gerven. "Does olanzapine inhibit the psychomimetic effects of Δ9-tetrahydrocannabinol?." Journal of Psychopharmacology 26, no. 10 (2012): 1307-1316.
kleinloog2010_n=49 # A total of 49 subjects were included, 33 (67%) of whom com- pleted all five study days. All subjects who received at least one administration of THC or its placebo were included in the analysis.
kleinloog2010_n_effective=n_effective_dependent(kleinloog2010_n, r=r_withinSubj_between_timepoints)

# ======= Dose of THC on PANSS
# Three consequiteive dosages of 2, 4 and 6 mg
#  The interviews were performed four times during a study day: once before the administration of olanzapine (or placebo) and again after each THC (or placebo) administration.
# Compared with placebo, administration of THC induced an average increase on the positive subscale of the PANSS of 20.6% (95%CI 13.1–28.6%; p<0.001)
kleinloog2010_PANSS_pos_4mgTHC_p=0.0009 # <0.001
pes(p=kleinloog2010_PANSS_pos_4mgTHC_p, n.1=kleinloog2010_n_effective/2, n.2=kleinloog2010_n_effective/2, dig=20)
pEstimate_kleinloog2010_PANSS_pos_4mgTHC=data.frame(pEstimate=NA, Ntot=kleinloog2010_n)
# Subset analysis in responders
# responders were con- servatively defined as subjects who showed at least 1 point increase on the positive subscale compared with baseline in any of the measurements following THC administration. In these 33 (67%) responders,
kleinloog2010_n_responders=33
kleinloog2010_n_effective_responders=n_effective_dependent(kleinloog2010_n_responders, r=r_withinSubj_between_timepoints)
kleinloog2010_PANSS_pos_4mgTHC_responders_p=0.0009 # <0.001
pes(p=kleinloog2010_PANSS_pos_4mgTHC_responders_p, n.1=kleinloog2010_n_effective_responders/2, n.2=kleinloog2010_n_effective_responders/2, dig=20)
pEstimate_kleinloog2010_PANSS_pos_4mgTHC_responders=data.frame(pEstimate=NA, Ntot=kleinloog2010_n_responders)


# ======= Effect of olanzapine pretreatment on THC-induced PANSS change
kleinloog2010_PANSS_pos_4mgTHC_olanzapine_p=0.066
pes(p=kleinloog2010_PANSS_pos_4mgTHC_olanzapine_p, n.1=kleinloog2010_n_effective/2, n.2=kleinloog2010_n_effective/2, dig=20)
pEstimate_kleinloog2010_PANSS_pos_4mgTHC_olanzapine=data.frame(pEstimate=0.066, Ntot=kleinloog2010_n)
# Subset analysis in responders
kleinloog2010_PANSS_pos_4mgTHC_olanzapine_responders_p=0.005
pes(p=kleinloog2010_PANSS_pos_4mgTHC_olanzapine_responders_p, n.1=kleinloog2010_n_effective_responders/2, n.2=kleinloog2010_n_effective_responders/2, dig=20)
pEstimate_kleinloog2010_PANSS_pos_4mgTHC_responders_olanzapine=data.frame(pEstimate=0.005, Ntot=kleinloog2010_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Nottage (2015) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nottage2015_n=17 # n = 13 pairs + 4 THC only
# REF: Nottage, J.F., Stone, J., Murray, R.M., Sumich, A., Bramon-Bosch, E. and Morrison, P.D., 2015. Delta-9-tetrahydrocannabinol, neural oscillations above 20 Hz and induced acute psychosis. Psychopharmacology, 232(3), pp.519-528.

# ===== Effect of resting state
# This effect was correlated with positive symptoms, as assessed on the Positive and Negative Syndrome Scale (PANSS) (r = 0.429, p = 0.042).

# Neural oscillations (resting-state)

#  neuronal oscillations 30 Hz gamma band
# A standard frequency band for high beta is 20–30 Hz, whereas the frequen- cies used for low gamma have varied, usually starting at 30– 35 Hz with an upper bound between 40 and 60 Hz
# general effects of THC on high-frequency oscillations in cortico-striatal circuits
# We also hypothesised that THC-induced effects would correlate with psychopathology as measured using the Positive and Negative Syndrome Scale (PANSS)

# Intravenous (IV) THC (1.25 mg)


# ============ Effect of neuronal oscillations on PANSS pos during THC condition
# neuronal oscillations (27–45-Hz / 21–27-Hz amplitude)

# ratio (27–45-Hz amplitude divided by the 21–27-Hz amplitude) was not correlated with PANSS positive symptom scores.
nottage2015_neur_oscill_ratio1_panss_pos_r=rValueNS_allStudies # not correlated with PANSS positive  (in THC condition?)
res(nottage2015_neur_oscill_ratio1_panss_pos_r, n=nottage2015_n, dig=20)
pEstimate_nottage2015_neur_oscill_ratio1_panss_pos=data.frame(pEstimate=NA, Ntot=nottage2015_n) # NS

#  location effect as well as the frequency effect, the posterior 27–45-Hz amplitude was divided by the anterior 21–27-Hz amplitude
# This ratio did show a positive association with PANSS positive symptom scores (r=0.429, p=0.021)
nottage2015_neur_oscill_ratio2_panss_pos_r=0.429 # THC condition only
res(nottage2015_neur_oscill_ratio2_panss_pos_r, n=nottage2015_n, dig=20)
pEstimate_nottage2015_neur_oscill_ratio2_panss_pos=data.frame(pEstimate=0.021, Ntot=nottage2015_n)

# the strongest association [with ratio of posterior 27–45-Hz amplitude divided by the anterior 21–27-Hz amplitude] was with delusion-associated scores (PANSS items P1 + P5 + P6) (r=0.525, p=0.016)
nottage2015_neur_oscill_ratio2_panss_paranoia_r=0.525 # THC condition only
res(nottage2015_neur_oscill_ratio2_panss_paranoia_r, n=nottage2015_n, dig=20)
pEstimate_nottage2015_neur_oscill_ratio2_panss_paranoia=data.frame(pEstimate=0.016, Ntot=nottage2015_n)

# no rates reported

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Collin (2010) =============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Collin, C., E. Ehler, G. Waberzinek, Z. Alsindi, P. Davies, K. Powell, W. Notcutt et al. "A double-blind, randomized, placebo-controlled, parallel-group study of Sativex, in subjects with symptoms of spasticity due to multiple sclerosis." Neurological research 32, no. 5 (2010): 451-459.
collin2010_sativex_n=167
collin2010_placebo_n=170

# Sativex vs placebo: Hallucinations
NriskA_case=2
NriskA_control=collin2010_sativex_n-NriskA_case
NriskB_case=1
NriskB_control=collin2010_placebo_n-NriskB_case
# Get chi square estimates
labelA="sativex"
labelB="placebo"
extract_collin2010_sativex_hallucinations=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_collin2010_sativex_hallucinations=data.frame(pEstimate=NA, Ntot=extract_collin2010_sativex_hallucinations$N.total)


# Sativex vs placebo: Paranoia
NriskA_case=1
NriskA_control=collin2010_sativex_n-NriskA_case
NriskB_case=1
NriskB_control=collin2010_placebo_n-NriskB_case
# Get chi square estimates
labelA="sativex"
labelB="placebo"
extract_collin2010_sativex_paranoia=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_collin2010_sativex_paranoia=data.frame(pEstimate=NA, Ntot=extract_collin2010_sativex_hallucinations$N.total)

# get rates in treatment group
collin2010_sativex_HallucinationsDF=data.frame(event=2, n=collin2010_sativex_n)
collin2010_sativex_ParanoiaDF=data.frame(event=1, n=collin2010_sativex_n)

# get rates in control group
collin2010_sativex_HallucinationsControl=data.frame(event=2, n=170)
collin2010_sativex_ParanoiaControl=data.frame(event=1, n=170)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Aragona (2009) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Aragona, Massimiliano, Emanuela Onesti, Valentina Tomassini, Antonella Conte, Shiva Gupta, Francesca Gilio, Patrizia Pantano, Carlo Pozzilli, and Maurizio Inghilleri. "Psychopathological and cognitive effects of therapeutic cannabinoids in multiple sclerosis: a double-blind, placebo controlled, crossover study." Clinical neuropharmacology 32, no. 1 (2009): 41-47.
aragona2009_sativex_n=17

# ===== Paranoia ========
aragona2009_sativex_paranoia_mean=3.17
aragona2009_placebo_paranoia_mean=2.88
aragona2009_sativex_paranoia_sd=3.74
aragona2009_placebo_paranoia_sd=3.46
# Extract Cohen D
extract_aragona2009_sativex_paranoia=d_meanDiff_dependent(m1=aragona2009_sativex_paranoia_mean,
                                                          m2=aragona2009_placebo_paranoia_mean,
                                                          sd1=aragona2009_sativex_paranoia_sd ,
                                                          sd2=aragona2009_placebo_paranoia_sd,
                                                          n=aragona2009_sativex_n,
                                                          r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_aragona2009_sativex_paranoia=data.frame(pEstimate=0.72, Ntot=aragona2009_sativex_n)

# ===== Psychosis ========
aragona2009_sativex_psychosis_mean=4.23
aragona2009_placebo_psychosis_mean=5
aragona2009_sativex_psychosis_sd=3.63
aragona2009_placebo_psychosis_sd=3.8
# Extract Cohen D
extract_aragona2009_sativex_psychosis=d_meanDiff_dependent(m1=aragona2009_sativex_psychosis_mean,
                                                           m2=aragona2009_placebo_psychosis_mean,
                                                           sd1=aragona2009_sativex_psychosis_sd ,
                                                           sd2=aragona2009_placebo_psychosis_sd,
                                                           n=aragona2009_sativex_n, r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_aragona2009_sativex_psychosis=data.frame(pEstimate=0.65, Ntot=aragona2009_sativex_n)

# no rates reported

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Duran (2010) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Duran, Marta, Eulàlia Pérez, Sergio Abanades, Xavier Vidal, Cristina Saura, Margarita Majem, Edurne Arriola et al. "Preliminary efficacy and safety of an oromucosal standardized cannabis extract in chemotherapy‐induced nausea and vomiting." British journal of clinical pharmacology 70, no. 5 (2010): 656-663.
#  Seven patients were randomized to CBM and nine to placebo, one patient in the CBM arm was withdrawn due to AEs
# The mean daily dose was 4.8 sprays in both groups.
# The mean number of daily sprays taken during the 4 days after chemotherapy was 4.81 in the CBM group (range 2.7– 5.0, SD = 1.01), equivalent to 12.9 mg of THC and 12 mg of CBD, and 4.78 in the placebo group (range 2.9–5.0, SD = 0.79). The median duration of treatment was 3 days in the CBM group (range 1–5) and 4 (range 3–5) in the placebo group.

duran2010_sativex_n=7
duran2010_placebo_n=9

# Sativex vs placebo: Hallucinations
NriskA_case=1
NriskA_control=duran2010_sativex_n-NriskA_case
NriskB_case=0
NriskB_control=duran2010_placebo_n-NriskB_case
# Get chi square estimates
labelA="sativex"
labelB="placebo"
extract_duran2010_sativex_psychosis=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_duran2010_sativex_psychosis=data.frame(pEstimate=NA, Ntot=extract_duran2010_sativex_psychosis$N.total)

# get rates in treatment group
duran2010_sativex_PsychosisDF=data.frame(event=1, n=duran2010_sativex_n)

# get rates in control group
duran2010_placebo_PsychosisDF=data.frame(event=0, n=9)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Ranganathan (2019) ========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Ranganathan, Mohini, Joao P. De Aquino, Jose A. Cortes-Briones, Rajiv Radhakrishnan, Brian Pittman, Savita Bhakta, and Deepak C. D’Souza. "Highs and lows of cannabinoid-dopamine interactions: effects of genetic variability and pharmacological modulation of catechol-O-methyl transferase on the acute response to delta-9-tetrahydrocannabinol in humans." Psychopharmacology 236, no. 11 (2019): 3209-3219.

# ======= Effect of dose: Panss pos
ranganathan2019_n=74 #74 completed the 2 test days
n_effective_ranganathan2019=n_effective_dependent(ranganathan2019_n, r=r_withinSubj_between_timepoints)
ranganathan2019_PANSS_pos_p=0.00009 # p < 0.0001
pes(p=ranganathan2019_PANSS_pos_p, n.1=n_effective_ranganathan2019/2, n.2=n_effective_ranganathan2019/2, dig=20)
pEstimate_ranganathan2019_PANSS_pos=data.frame(pEstimate=NA, Ntot=ranganathan2019_n)

# ======= Interaction with COMT
# COMT (rs4680; A/A, A/G, G/G)
# rs4680 = A = Met
# rs4680 = G = Val
# COMT (Met/Met; Val/Met; Val/Va)
# Model: genotype (Val/Val, Val/Met, Met/Met) as between-subject factor, drug (THC, placebo) as within- subject factor
# no THC × COMT geno- type interaction was were observed
n_effective_ranganathan2019=n_effective_dependent(ranganathan2019_n, r=r_withinSubj_between_timepoints)
pes(p=pValueNS_allStudies, n.1=n_effective_ranganathan2019/2, n.2=n_effective_ranganathan2019/2, dig=20)
pEstimate_ranganathan2019_PANSS_pos_comt=data.frame(pEstimate=NA, Ntot=ranganathan2019_n)


# ======= Effect of pharmacological inactivation
# Tolcapone is a brain- penetrating, potent, and reversible COMT inhibitor devel- oped for the treatment of Parkinson’s disease (Jorga 1998).
# selective COMT inhibitors now, possible to pharmacologically model high and low COMT enzyme activity
#  conducted in a small sub-sample of subject
# a subset of rs4680 homozygote participants (Val/Val or Met/Met) participated in two extra test days (sub- study II) where they received oral tolcapone followed
ranganathan2019_tolcapone_n=22 # 22 homozygote sub- jects completed the 4 test days
n_effective_ranganathan2019_tolcapone=n_effective_dependent(ranganathan2019_tolcapone_n, r=r_withinSubj_between_timepoints)
# Sub-study II: No main effects of tolcapone or COMT ge- notype
pes(p=pValueNS_allStudies, n.1=n_effective_ranganathan2019_tolcapone/2, n.2=n_effective_ranganathan2019_tolcapone/2, dig=20)
pEstimate_ranganathan2019_PANSS_pos_tolcapone=data.frame(pEstimate=NA, Ntot=ranganathan2019_n)

# rates included in ganesh

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Thaler (2019) =============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Thaler, Avner, Shira Arad, Lihi Bar-Lev Schleider, Judith Knaani, Tali Taichman, Nir Giladi, and Tanya Gurevich. "Single center experience with medical cannabis in Gilles de la Tourette syndrome." Parkinsonism & related disorders 61 (2019): 211-213.
thaler2019_MedCan_n=42
# MC formulation with varying concentrations of THC and CBD
# Medical can vs placebo: Hallucinations

# Hallucinations
thaler2019_MedCan_hallucinationsDF=data.frame(event=4, n=thaler2019_MedCan_n)  # Four patients re-ported hallucinations
thaler2019_MedCan_psychosisDF=data.frame(event=1, n=thaler2019_MedCan_n)  # One patient detailed an acute psychotic episode.


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Balash (2017) =============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Balash, Yacov, Lihi Bar-Lev Schleider, Amos D. Korczyn, Herzel Shabtai, Judith Knaani, Alina Rosenberg, Yehuda Baruch, Ruth Djaldetti, Nir Giladi, and Tanya Gurevich. "Medical cannabis in Parkinson disease: Real-life patients' experience." Clinical neuropharmacology 40, no. 6 (2017): 268-272.
balash2019_MedCan_n=47


# Hallucinations
balash2019_MedCan_hallucinationsDF=data.frame(event=8, n=balash2019_MedCan_n)  # 8 cases with hallucinations
balash2019_MedCan_psychosisDF=data.frame(event=1, n=balash2019_MedCan_n)  # 1 case with acute psychosis



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Serpell (2013) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Serpell, Michael G., William Notcutt, and Christine Collin. "Sativex long-term use: an open-label trial in patients with spasticity due to multiple sclerosis." Journal of neurology 260, no. 1 (2013): 285-295.

serpell2013_Sativex_n=119+20

# Hallucinations
serpell2013_Sativex_hallucinationsDF=data.frame(event=1, n=serpell2013_Sativex_n)  # 1 case
serpell2013_Sativex_paranoiaDF=data.frame(event=1, n=serpell2013_Sativex_n)  # 1 case
serpell2013_Sativex_delusionsDF=data.frame(event=1, n=serpell2013_Sativex_n)  # 1 case


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Lichtman (2018) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Lichtman, Aron H., Eberhard Albert Lux, Robert McQuade, Sandro Rossetti, Raymond Sanchez, Wei Sun, Stephen Wright, Elena Kornyeyeva, and Marie T. Fallon. "Results of a double-blind, randomized, placebo-controlled study of nabiximols oromucosal spray as an adjunctive therapy in advanced cancer patients with chronic uncontrolled pain." Journal of pain and symptom management 55, no. 2 (2018): 179-188.

lichtman2018_nabiximols_n=199
lichtman2018_placebo_n=198

# Sativex - hallucinations
# Serious treatment-emergent adverse events in the trial were one case one case of visual hallucination in the nabiximols group (not in the placebo group)

# nabiximols - hallucinations
lichtman2018_nabiximols_hallucinations=1 # case of visual hallucinations in nabiximols group
lichtman2018_placebo_hallucinations=0  #visual hallucinations in placebo group

labelA="Sativex"
labelB="Placebo"
extract_lichtman2018_sativex_hallucinations=chiSquareCalc2x2(lichtman2018_nabiximols_hallucinations, lichtman2018_placebo_hallucinations, lichtman2018_nabiximols_n-lichtman2018_nabiximols_hallucinations, lichtman2018_placebo_n-lichtman2018_placebo_hallucinations, labelA, labelB)
pEstimate_lichtman2018_sativex_hallucinations=data.frame(pEstimate=NA, Ntot=extract_lichtman2018_sativex_hallucinations$N.total)

# get rates in treatment group
lichtman2018_sativex_HallucinationsDF=data.frame(event=lichtman2018_nabiximols_hallucinations, n=lichtman2018_nabiximols_n)

# get rates in control group
lichtman2018_sativex_HallucinationsControl=data.frame(event=0, n=lichtman2018_placebo_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Abuhasira (2018) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Abuhasira, R., Schleider, L. B.-L., Mechoulam, R. and Novack, V. (2018), ‘Epidemiological characteristics, safety and efficacy of medical cannabis in the elderly’, European Journal of Internal Medicine 49, pp. 44-50.
abuhasira2018_cannabis_n=901
abuhasira2018_cannabisDF=data.frame(event=7, n=abuhasira2018_cannabis_n)  # 7 cases

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Camera (2012) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Camera, Ariella A., Veronica Tomaselli, Jerry Fleming, Gul A. Jabbar, Melissa Trachtenberg, Juan A. Galvez-Buccollini, Ashley C. Proal, Richard N. Rosenthal, and Lynn E. DeLisi. "Correlates to the variable effects of cannabis in young adults: a preliminary study." Harm reduction journal 9, no. 1 (2012): 15.

camera2012_n=100

# === Family history schizophrenia - hallucinations
# Having a family history of schizophrenia was not associated with having hallucinations (chi-square = 0.154; p = 0.695),
extract_camera2012_hallucinations_famhxSchz=chies(0.154, camera2012_n, dig=50)
pEstimate_camera2012_hallucinations_famhxSchz=data.frame(pEstimate=0.695, Ntot=100)

# === Age of onset of cannabis use - factor component (paranoia/hallucinations)
#  Factor 3 was associated with age of onset of cannabis use (Pearson’s r = !0.29, p <0.004) so that the earlier the onset of use the greater the likelihood of feeling paranoid (suspiciousness) and having other delusions with use.
extract_camera2012_psychFac_ageOnsetCan=res(-0.255, n=camera2012_n, dig=50)
pEstimate_camera2012_psychFac_ageOnsetCan=data.frame(pEstimate=0.01, Ntot=100)

# === Alcohol use - hallucinations
# Direction: cannabis users who also used alcohol were significantly more likely to report having hallucinations with cannabis use
extracted_camera2012_hallucinations_alcUse=chies(10.5, camera2012_n, dig=50)
pEstimate_camera2012_hallucinations_alcUse=data.frame(pEstimate=0.001, Ntot=100)

# === Family history of alcoholism - factor component (paranoia/hallucinations)
extract_camera2012_psych_alcFamHx=tes(-2.204 , 68, 32, dig=20)
pEstimate_camera2012_psych_alcFamHx=data.frame(pEstimate=0.03, Ntot=100)

# Rates
camera2012_hallucinationsDF=data.frame(event=7, n=100)
camera2012_paranoiaDF=data.frame(event=40, n=100)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Hindocha (2020) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Hindocha, C., Quattrone, D., Freeman, T.P. et al. Do AKT1, COMT and FAAH influence reports of acute cannabis intoxication experiences in patients with first episode psychosis, controls and young adult cannabis users?. Transl Psychiatry 10, 143 (2020). https://doi.org/10.1038/s41398-020-0823-9


# AKT1
# Note: Main effect of AKT1 included even already estimated in Morgan study. But: Morgan used PSI instead of CEQ
hindocha2020_akt1_psych_n=578
hindocha2020_akt1_psych_se = (4.18 - (-0.13) )/(2*1.96) # calculate the standard error
hindocha2020_akt1_psych_beta=2.02
extract_hindocha2020_akt1_psych = beta_to_d_independent(beta=hindocha2020_akt1_psych_beta,
                                                        se=hindocha2020_akt1_psych_se,
                                                        n=hindocha2020_akt1_psych_n)
pEstimate_hindocha2020_akt1_psych=data.frame(pEstimate=0.066, Ntot=hindocha2020_akt1_psych_n)

# AKT1 x interaction with case : (rs2494732) TT/CT/CC genotype
hindocha2020_akt1_x_case_psych_se = (-0.57 - (-6.06) )/(2*1.96) # calculate the standard error
hindocha2020_akt1_x_case_psych_beta=-3.33
extract_hindocha2020_akt1_x_case_psych = beta_to_d_independent(beta=hindocha2020_akt1_x_case_psych_beta,
                                                               se=hindocha2020_akt1_x_case_psych_se,
                                                               n=hindocha2020_akt1_psych_n)
pEstimate_hindocha2020_akt1_x_case_psych=data.frame(pEstimate=0.02, Ntot=hindocha2020_akt1_psych_n)

# COMT
# Note: Main effect of COMT: (rs4680)  included even already estimated in Morgan study. But: Morgan used PSI instead of CEQ
hindocha2020_comt_psych_n=592
hindocha2020_comt_psych_se = (1.98-(-2.9) )/(2*1.96) # calculate the standard error
hindocha2020_comt_psych_beta= -0.46
extract_hindocha2020_comt_psych = beta_to_d_independent(beta=hindocha2020_comt_psych_beta,
                                                        se=hindocha2020_comt_psych_se,
                                                        n=hindocha2020_comt_psych_n)
pEstimate_hindocha2020_comt_psych=data.frame(pEstimate=0.71, Ntot=hindocha2020_comt_psych_n)
# COMT x interaction with case
hindocha2020_comt_x_case_psych_se = (3.25-(-3.54) )/(2*1.96) # calculate the standard error
hindocha2020_comt_x_case_psych_beta= -0.14
extract_hindocha2020_comt_x_case_psych = beta_to_d_independent(beta=hindocha2020_comt_x_case_psych_beta, se=hindocha2020_comt_x_case_psych_se, n=hindocha2020_comt_psych_n)
pEstimate_hindocha2020_comt_x_case_psych=data.frame(pEstimate=0.93, Ntot=hindocha2020_comt_psych_n)


# FAAH (rs324420) CC/AC/ AA
hindocha2020_faah_psych_n=590
hindocha2020_faah_psych_se = (1.03 - (-2.42) )/(2*1.96) # calculate the standard error
hindocha2020_faah_psych_beta=-0.70
extract_hindocha2020_faah_psych = beta_to_d_independent(beta=hindocha2020_faah_psych_beta,
                                                        se=hindocha2020_faah_psych_se,
                                                        n=hindocha2020_faah_psych_n)
pEstimate_hindocha2020_faah_psych=data.frame(pEstimate=0.43, Ntot=hindocha2020_faah_psych_n)
# FAAH x interaction with case
hindocha2020_faah_x_case_psych_se = (2.96 - (-1.47) )/(2*1.96) # calculate the standard error
hindocha2020_faah_x_case_psych_beta= -0.75
extract_hindocha2020_faah_x_case_psych = beta_to_d_independent(beta=hindocha2020_faah_x_case_psych_beta, se=hindocha2020_faah_x_case_psych_se, n=hindocha2020_faah_psych_n)
pEstimate_hindocha2020_faah_x_case_psych=data.frame(pEstimate=0.51, Ntot=hindocha2020_faah_psych_n)

# No rates reported

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Kaufmann (2010) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Kaufmann RM, Kraft B, Frey R, et al. Acute psychotropic effects of oral cannabis extract with a defined content of Delta9-tetrahydrocannabinol (THC) in healthy volunteers. Pharmacopsychiatry. 2010;43(1):24‐32. doi:10.1055/s-0029-1237397

# Subjects: Sixteen healthy females (mainly university students) aged between 19 and 29 years (mean 23.56 ± 2.7)
# Cannabis extract with stand- ardised Δ9-tetrahydrocannabinol (THC) content (20mg) or active placebo (5mg diazepam)
# THC and cannabidiol (CBD) predomi- nated in a ratio of 2:1,

# In the cannabis condition, subjects scored significantly higher regarding total score compared to diazepam 3 h [t(14) = − 5.277, p < 0.001] after intake.
kaufmann2010_20mg_n=15 # 16 females were included in the trial, but only 15 were submit- ted to statistical analyses, since the development of severe acute psychotic symptoms in one subject made further assessments impossible.

# Total bprs score
extract_kaufmann2010_20mg_bprs_tot=d_meanDiff_dependent(m1=22.1,
                                                        m2=18,
                                                        sd1=3.3,
                                                        sd2=0,
                                                        n=kaufmann2010_20mg_n,
                                                        r=r_withinSubj_between_timepoints )
pEstimate_kaufmann2010_20mg_bprs_tot=data.frame(pEstimate=NA, Ntot=kaufmann2010_20mg_n) # p < 0.001
# Paranoia only
extract_kaufmann2010_20mg_bprs_par=d_meanDiff_dependent(m1=3.4,m2=3.0,sd1=0.7,sd2=0,n=kaufmann2010_20mg_n, r=r_withinSubj_between_timepoints )
pEstimate_kaufmann2010_20mg_bprs_par=data.frame(pEstimate=NA, Ntot=kaufmann2010_20mg_n) # not reported


# One subject, who was excluded from statistical analysis, experienced transient psychotic symptoms (BPRS total score: 3 h post- drug: 84; 6h postdrug: 32) after cannabis intake.
kaufmann2010_psychosisDF=data.frame(event=1, n=16)

kaufmann2010_psychosisControl=data.frame(event=0, n=16)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Hindocha (2017) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Hindocha, Chandni, T. P. Freeman, J. X. Xia, Natacha DC Shaban, and Helen V. Curran. "Acute memory and psychotomimetic effects of cannabis and tobacco both ‘joint’and individually: a placebo-controlled trial." Psychological medicine 47, no. 15 (2017): 2708-2719.

# We compared the effects of (a) active cannabis + active tobacco (CAN-TOB) (b) active cannabis + placebo tobacco (CAN), (c) placebo cannabis+active tobacco (TOB), (d) placebo cannabis + placebo tobacco (no active drug) (PLACEBO). T

hindocha2017_n=24

# Main effect of THC
# A main effect of cannabis (F1,33 = 33.01, showed cannabis (M: 32.04, S.E.: 3.53) markedly increased PSI scores in comparison with placebo (M: 13.85, S.E.: 1.76);

# THC condition: 66.67 mg Bedrobinol (16.1% THC and <1% CBD)
# Estimate dose of THC
THCmg=(66.67/100)*16.1
# Standard unit
# 0.16mg THC => 1g Bedrobinol
unit5mg=5/0.161

# Extract differences between conditions
extract_hindocha2017_10mg_PSI=d_meanDiff_dependent(m1=32.04,
                                                   m2=13.85,
                                                   sd1=3.53*sqrt(hindocha2017_n),
                                                   sd2=1.76*sqrt(hindocha2017_n),
                                                   n=hindocha2017_n,
                                                   r=r_withinSubj_between_timepoints)
pEstimate_hindocha2017_10mg_PSI=data.frame(pEstimate=NA, Ntot=hindocha2017_n) # p<0.001

# No significant interaction effects
n_effective_hindocha2017=n_effective_dependent(hindocha2017_n, r=r_withinSubj_between_timepoints) # effective sample size
pes(pValueNS_allStudies, n.1=n_effective_hindocha2017/2, n.2=n_effective_hindocha2017/2)
pEstimate_hindocha2017_nicotine_x_THC=data.frame(pEstimate=NA, Ntot=hindocha2017_n)

# no rates reported

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Mokrysz (2016) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Mokrysz, Claire, Tom P. Freeman, Saana Korkki, Kirsty Griffiths, and H. Valerie Curran. "Are adolescents more vulnerable to the harmful effects of cannabis than adults? A placebo-controlled study in human males." Translational psychiatry 6, no. 11 (2016): e961-e961.

# We recruited 20 adolescent (aged 16–17 years) and 20 adult (24–28 years) male cannabis users
mokrysz2016_n_ado=20
mokrysz2016_n_adult=20
# PSI subscale not included as psychosis-like outcomes not covered (PSI subscale (PSI subscale: thought distortion, perceptual distortion, cognitive disorganisation, anhedonia, manic experience. paranoia subscale was not included in analyses due to floor effects)


# THC condition: Medicinal-grade active (Bedrobinol; THC 12.0%, ~ 8.0 mg THC per individual)
# participants received 0.89 mg/kg of Bedrobinol (THC 12.0%), corresponding to ~ 8.0 mg THC for an individual weighing 75 kg.

# Adolescents (placebo vs THC)
mokrysz2016_n_ado_placebo_mean=0.9320388349514567
mokrysz2016_n_ado_placebo_se= 1.1844660194174765-0.6213592233009717
mokrysz2016_n_ado_placebo_sd=mokrysz2016_n_ado_placebo_se*sqrt(mokrysz2016_n_ado)

mokrysz2016_n_ado_8mgTHC_mean=2.271844660194175
mokrysz2016_n_ado_8mgTHC_se= (2.9514563106796117-mokrysz2016_n_ado_8mgTHC_mean)*2
mokrysz2016_n_ado_8mgTHC_sd=mokrysz2016_n_ado_8mgTHC_se*sqrt(mokrysz2016_n_ado)

# Extract differences between conditions
extract_mokrysz2016_8mg_paranoia_adolescent=d_meanDiff_dependent(m1=mokrysz2016_n_ado_8mgTHC_mean,
                                                                 m2=mokrysz2016_n_ado_placebo_mean,
                                                                 sd1=mokrysz2016_n_ado_8mgTHC_sd,
                                                                 sd2=mokrysz2016_n_ado_placebo_sd,
                                                                 n=mokrysz2016_n_ado,
                                                                 r=r_withinSubj_between_timepoints)

# Adults (placebo vs THC)
mokrysz2016_n_adult_placebo_mean=0.21359223300970998
mokrysz2016_n_adult_placebo_se= (0.4854368932038834-mokrysz2016_n_adult_placebo_mean)*2
mokrysz2016_n_adult_placebo_sd=mokrysz2016_n_adult_placebo_se*sqrt(mokrysz2016_n_adult)

mokrysz2016_n_adult_8mgTHC_mean=1.41747572815534
mokrysz2016_n_adult_8mgTHC_se= (2.1165048543689315-mokrysz2016_n_adult_placebo_mean)*2
mokrysz2016_n_adult_8mgTHC_sd=mokrysz2016_n_adult_placebo_se*sqrt(mokrysz2016_n_adult)

# Extract differences between conditions
extract_mokrysz2016_8mg_paranoia_adult=d_meanDiff_dependent(m1=mokrysz2016_n_adult_8mgTHC_mean,
                                                            m2=mokrysz2016_n_adult_placebo_mean,
                                                            sd1=mokrysz2016_n_adult_8mgTHC_sd,
                                                            sd2=mokrysz2016_n_adult_placebo_sd,
                                                            n=mokrysz2016_n_adult,
                                                            r=r_withinSubj_between_timepoints)


dat_mokrysz2016=rbind(extract_mokrysz2016_8mg_paranoia_adolescent, extract_mokrysz2016_8mg_paranoia_adult)

# Combine d from both groups
dat_mokrysz2016_rem=rma(d, var.d, method="REML", data=dat_mokrysz2016)
extract_mokrysz2016_8mg_paranoia_tot_sample=data.frame(d=dat_mokrysz2016_rem$beta,
                                                       var.d=dat_mokrysz2016_rem$se^2 ,
                                                       l.d=dat_mokrysz2016_rem$ci.lb,
                                                       u.d=dat_mokrysz2016_rem$ci.ub,
                                                       N.total=mokrysz2016_n_ado+mokrysz2016_n_adult,
                                                       pval.d=dat_mokrysz2016_rem$pval)
# Extract p-value estimate
pEstimate_mokrysz2016_8mg_paranoia_tot_sample=data.frame(pEstimate=NA, Ntot=mokrysz2016_n_ado+mokrysz2016_n_adult) # Not reported

# Compare adolescent can-induced symp to adults
extract_mokrysz2016_paranoia_ado_vs_adult =
  compare_2cohenD(d1=extract_mokrysz2016_8mg_paranoia_adolescent$d,
                  d2=extract_mokrysz2016_8mg_paranoia_adult$d,
                  d1.var = extract_mokrysz2016_8mg_paranoia_adolescent$var.d,
                  d2.var = extract_mokrysz2016_8mg_paranoia_adult$var.d,
                  n=mokrysz2016_n_ado+mokrysz2016_n_adult,
                  method = "independent")
# Extract p-value estimate
pEstimate_mokrysz2016_paranoia_ado_vs_adult=data.frame(pEstimate=NA, Ntot=mokrysz2016_n_ado+mokrysz2016_n_adult) # Not reported

# no rates reported

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Dalzell (1986) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Dalzell, A. M., H. Bartlett, and J. S. Lilleyman. "Nabilone: an alternative antiemetic for cancer chemotherapy." Archives of disease in childhood 61, no. 5 (1986): 502-505.

# Comparison of new synthetic cannabinoid nabilone with oral domperidone
dalzell1986_nabilone_n=18
dalzell1986_nabilone_hallucinations_perc=5
tableChi=chiSquareCalc_repeated(percCan= dalzell1986_nabilone_hallucinations_perc,
                                percPLB = 0,
                                nTot =  dalzell1986_nabilone_n,
                                r=r_withinSubj_between_timepoints)
chiSquEstimate_dalzell1986_nabilone_hallucinations=tableChi[1]
Ntot_dalzell1986_nabilone_hallucinations=tableChi[2]
# Get cohen d
extract_dalzell1986_nabilone_hallucinations=chies(chiSquEstimate_dalzell1986_nabilone_hallucinations, Ntot_dalzell1986_nabilone_hallucinations, dig=50)
pEstimate_dalzell1986_nabilone_hallucinations=data.frame(pEstimate=NA, Ntot=dalzell1986_nabilone_n)

# Get estimates for rates
dalzell1986_nabilone_hallucinationsDF=data.frame(event=1, n=dalzell1986_nabilone_n)

# rates under placebo condition
dalzell1986_nabilone_hallucinationsControl=data.frame(event=0, n=dalzell1986_nabilone_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Laszilo (1981) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: LASZLO, JOHN, VIRGIL S. LUCAS JR, DEAN C. HANSON, CAROL M. CRONIN, and STEPHEN E. SALLAN. "Levonantradol for Chemotherapy‐Induced Emesis: Phase I—II Oral Administration." The Journal of Clinical Pharmacology 21, no. S1 (1981): 51S-56S.
# Administration of levonantradol =>  synthetic cannabinoid analog of dronabinol (Marinol)

# Four starting dosages were studied, 0.5, 1.0, 1.5, and 2.0 mg.
# Placebo condition: anitemetic drugs

# Hallucinations
laszilo1981_dronabinol_n=35
laszilo1981_dronabinol_hallucinationsDF=data.frame(event=11, n=laszilo1981_dronabinol_n)  # 11 cases




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Jain (1981) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: JAIN, A.K., RYAN, J.R., McMAHON, F.G. and SMITH, G., 1981. Evaluation of intramuscular levonantradol and placebo in acute postoperative pain. The Journal of Clinical Pharmacology, 21(S1), pp.320S-326S.
# Levonantradol =>  synthetic cannabinoid analog of dronabinol (Marinol)
jain1981_drabinol_n=40
jain1981_placebo_n=16

# Sativex - hallucinations
jain1981_drabinol_case=3 # number reporting hallucinations as side effect in THC groups (all dose groups combined, mild symptoms)
jain1981_placebo_case=0 # number reporting hallucinations as side effect in placebo condition
labelA="Nabilone"
labelB="Placebo"
extract_jain1981_drabinol_hallucinations=chiSquareCalc2x2(jain1981_drabinol_case, jain1981_placebo_case, jain1981_drabinol_n-jain1981_drabinol_case, jain1981_placebo_n-jain1981_placebo_case, labelA, labelB)
pEstimate_jain1981_drabinol_hallucinations=data.frame(pEstimate=NA, Ntot=extract_jain1981_drabinol_hallucinations$N.total)

# Get estimates for rates
jain1981_drabinol_hallucinationsDF=data.frame(event=3, n=jain1981_drabinol_n)

# Get estimates for rates in control
jain1981_drabinol_hallucinationsControl=data.frame(event=0, n=jain1981_placebo_n)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Herman (1979) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Herman, Terence S., Lawrence H. Einhorn, Stephen E. Jones, Catherine Nagy, Aurelia B. Chester, Judith C. Dean, Becky Furnas et al. "Superiority of nabilone over prochlorperazine as an antiemetic in patients receiving cancer chemotherapy." New England Journal of Medicine 300, no. 23 (1979): 1295-1297.
#  three patients (3 per cent) taking nabilone had side effects (hallucinations) that required medical attention.

# The first antiemetic that the patient received was randomly assigned to be nabilone or prochlorperazine by the pharmaceutical company, and the second was automatically the other agent.
# Of the 113 evaluable patients, three patients taking nabilone had side effects (hallucinations) that required medical attention.
# No hallucinations reported in prochlorperazine arm

# Hallucinations
herman1979_nabilone_n=113
herman1979_nabilone_hallucinations_perc=(100/herman1979_nabilone_n*3) # 3 cases reporting hallucinations

tableChi=chiSquareCalc_repeated(percCan= herman1979_nabilone_hallucinations_perc,
                                percPLB = 0,
                                nTot =  herman1979_nabilone_n,
                                r=r_withinSubj_between_timepoints)
chiSquEstimate_herman1979_nabilone_hallucinations=tableChi[1]
Ntot_herman1979_nabilone_hallucinations=tableChi[2]
# Get cohen d
chies(chiSquEstimate_herman1979_nabilone_hallucinations, Ntot_herman1979_nabilone_hallucinations, dig=50)
pEstimate_herman1979_nabilone_hallucinations=data.frame(pEstimate=NA, Ntot=herman1979_nabilone_n)

# Get estimates for rates
herman1979medCan_hallucinationsDF=data.frame(event=3, n=herman1979_nabilone_n)

# Get estimates for rates in controls
herman1979medCan_hallucinationsControl=data.frame(event=0, n=herman1979_nabilone_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Dittrich (1976) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Dittrich A, Bickel P, Schöpf J, Zimmer D. Vergleich veränderter Bewusstseinszustände unter den Halluzinogenen (--)-delta9-trans-Tetrahydrocannabinol (delta9-THC) und N,N-Dimethyltryptamin (DMT) [Comparison of altered states of consciousness induced by the hallucinogens (--)-delta9-trans-tetrahydrocannabinol (delta9-THC) and N,N-dimethyltryptamine (DMT) (author's transl)]. Arch Psychiatr Nervenkr (1970). 1976;223(1):77-87. doi:10.1007/BF00367455
# [Comparison of altered states of consciousness induced by the hallucinogens (--)-delta9-trans-tetrahydrocannabinol (delta9-THC) and N,N-dimethyltryptamine (DMT) (author's transl)].

# Placebo was given to 12 subjects (for THC comparison)
# A total of 24 subjects received 250μg Δ9-THC p.o./kg body weight

# Measure: Fragebogen APZ (Dittrich, 1975a)
# administration: p.o. => per os => Gabe von Arzneimitteln über den Mund
dittrich1976_THC_n=24
dittrich1976_placebo_n=12

# Visual hallucinations
dittrich1976_THC_hallucinations_visual_m=2.54
dittrich1976_THC_hallucinations_visual_sd=2.84
dittrich1976_placebo_hallucinations_visual_m=0.38
dittrich1976_placebo_hallucinations_visual_sd=0.92
# Extract d and p
mes(dittrich1976_THC_hallucinations_visual_m, dittrich1976_placebo_hallucinations_visual_m, dittrich1976_THC_hallucinations_visual_sd, dittrich1976_placebo_hallucinations_visual_sd, dittrich1976_THC_n, dittrich1976_placebo_n, dig=20)
pEstimate_dittrich1976_THC_hallucinations_visual=data.frame(pEstimate=NA, Ntot=dittrich1976_THC_n+dittrich1976_placebo_n)  # p < 0.001

# auditory hallucinations
dittrich1976_THC_hallucinations_auditory_m=0.75
dittrich1976_THC_hallucinations_auditory_sd=1.19
dittrich1976_placebo_hallucinations_auditory_m=-0.04
dittrich1976_placebo_hallucinations_auditory_sd=0.2
# Extract d and p
mes(dittrich1976_THC_hallucinations_auditory_m, dittrich1976_placebo_hallucinations_auditory_m, dittrich1976_THC_hallucinations_auditory_sd, dittrich1976_placebo_hallucinations_auditory_sd, dittrich1976_THC_n, dittrich1976_placebo_n, dig=20)
pEstimate_dittrich1976_THC_hallucinations_auditory=data.frame(pEstimate=NA, Ntot=dittrich1976_THC_n+dittrich1976_placebo_n)  # p < 0.001

# no rates reported

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Crescioli (2019) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Crescioli, Giada, Niccolò Lombardi, Alessandra Bettiol, Francesca Menniti‐Ippolito, Roberto Da Cas, Maria Parrilli, Martina Del Lungo et al. "Adverse events following cannabis for medical use in Tuscany: An analysis of the Italian Phytovigilance database." British Journal of Clinical Pharmacology 86, no. 1 (2020): 106-120.
crescioli_medTrial_n=53
crescioli_medTrial_hallucinationsDF=data.frame(event=2, n=crescioli_medTrial_n)  # 2 cases


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Milosev (2019) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Milosev, Leonie M., Nikolas Psathakis, Natalia Szejko, Ewgeni Jakubovski, and Kirsten R. Müller-Vahl. "Treatment of Gilles de la Tourette Syndrome with Cannabis-Based Medicine: Results from a Retrospective Analysis and Online Survey." Cannabis and Cannabinoid Research 4, no. 4 (2019): 265-274.
milosev_medTrial_n=86 # (medical can, Nabiximols, Dronabinol)
milosev_medTrial_hallucinationsDF=data.frame(event=3, n=milosev_medTrial_n)  # 3 cases


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Zuurman (2008) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Zuurman, L., C. Roy, R. C. Schoemaker, A. Hazekamp, J. Den Hartigh, J. C. M. E. Bender, R. Verpoorte, J. L. Pinquier, A. F. Cohen, and J. M. A. Van Gerven. "Effect of intrapulmonary tetrahydrocannabinol administration in humans." Journal of psychopharmacology 22, no. 7 (2008): 707-716.

# Rising doses of THC (2, 4, 6 and 8 mg) or vehicle were administered with 90 minutes intervals to twelve healthy males using a Volcano® vaporizer
# Dose THC = 8 mg of THC (>98% purity)
# On each study day, rising doses of THC or placebo were administered by inhalation using a Volcano® vaporizer
# (8 mg THC) of the different parameters of the Visual Analogue Scales (VAS) of psychedelic effects,

zuurman2008_8mgTHCvaped_n=10 # Twelve healthy men were included in the study. BUT: Two of 12 sub- jects experienced side effects severe enough to decide not to administer the last dose of 8 mg THC.

# == Hallucinations
# Convert CI to SD
zuurman2008_8mgTHCvaped_hallucinations_beta=0.144
zuurman2008_8mgTHCvaped_hallucinations_sd=(0.266-0.021)/(2*1.96)*sqrt(zuurman2008_8mgTHCvaped_n)

# Extract estimates
extract_zuurman2008_8mgTHCvaped_hallucinations=d_meanDiff_dependent(m_diff=zuurman2008_8mgTHCvaped_hallucinations_beta,
                                                                    sd_diff=zuurman2008_8mgTHCvaped_hallucinations_sd,
                                                                    n=zuurman2008_8mgTHCvaped_n,
                                                                    r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_zuurman2008_8mgTHCvaped_hallucinations=data.frame(pEstimate=NA, Ntot=zuurman2008_8mgTHCvaped_n)

# == Paranoia
# Convert CI to SD
zuurman2008_8mgTHCvaped_paranoia_beta=0.127
zuurman2008_8mgTHCvaped_paranoia_sd=(0.274 - (-0.127) )/(2*1.96)*sqrt(zuurman2008_8mgTHCvaped_n)

# Extract estimates
extract_zuurman2008_8mgTHCvaped_paranoia=d_meanDiff_dependent(m_diff=zuurman2008_8mgTHCvaped_paranoia_beta,
                                                              sd_diff=zuurman2008_8mgTHCvaped_paranoia_sd,
                                                              n=zuurman2008_8mgTHCvaped_n,
                                                              r=r_withinSubj_between_timepoints)
# Extract p value
pEstimate_zuurman2008_8mgTHCvaped_paranoia=data.frame(pEstimate=NA, Ntot=zuurman2008_8mgTHCvaped_n)

# no rates reported

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Kleinloog (2014) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# REF: Kleinloog, Daniël, Frits Roozen, Willem De Winter, Jan Freijer, and Joop Van Gerven. "Profiling the subjective effects of Δ9‐tetrahydrocannabinol using visual analogue scales." International journal of methods in psychiatric research 23, no. 2 (2014): 245-256.
# All the studies had a randomized, cross-over, placebo-controlled
#  In this analysis, the effects of THC on these VASs were compared within a total of 217 subjects who participated in 10 different studies.
#  To determine possible dose–response relationships for the different VAS items that are sensitive for the effect of THC, the studies that used intrapulmonary administration of THC were selected.
# In nine out of 10 studies (94.5% of subjects) purified THC was inhaled using the VolcanoTM vaporizer (Storz-Bickel, Tuttlingen, Germany).

kleinloogMega_analysis2014_n=round((217/100)*94.5,0)

# Doses (2 mg, 4 mg, 6 mg), administered intrapulmonary

# Get chi square estimates from repreated measure design
kleinloogMega_analysis2014_placebo_hallucinations=3
kleinloogMega_analysis2014_2mgTHC_hallucinations=7
kleinloogMega_analysis2014_4mgTHC_hallucinations=14
kleinloogMega_analysis2014_6mgTHC_hallucinations=19

# ==== Hallucinations: 2mg THC
# Estimate chi square
tableChi=chiSquareCalc_repeated(percCan= kleinloogMega_analysis2014_2mgTHC_hallucinations,
                                percPLB = kleinloogMega_analysis2014_placebo_hallucinations,
                                nTot =  kleinloogMega_analysis2014_n,
                                r=r_withinSubj_between_timepoints)

chiSquEstimate_kleinloogMega_analysis2014_2mgTHC_hallucinations=tableChi[1]
# Get cohen d
chies(chiSquEstimate_kleinloogMega_analysis2014_2mgTHC_hallucinations, kleinloogMega_analysis2014_n, dig=50)
pEstimate_kleinloogMega_analysis2014_2mgTHC_hallucinations=data.frame(pEstimate=NA, Ntot=kleinloogMega_analysis2014_n)

# ==== Hallucinations: 4mg THC
# Estimate chi square
tableChi=chiSquareCalc_repeated(percCan= kleinloogMega_analysis2014_4mgTHC_hallucinations,
                                percPLB = kleinloogMega_analysis2014_placebo_hallucinations,
                                nTot =  kleinloogMega_analysis2014_n, r=r_withinSubj_between_timepoints)

chiSquEstimate_kleinloogMega_analysis2014_4mgTHC_hallucinations=tableChi[1]
# Get cohen d
chies(chiSquEstimate_kleinloogMega_analysis2014_4mgTHC_hallucinations, kleinloogMega_analysis2014_n, dig=50)
pEstimate_kleinloogMega_analysis2014_4mgTHC_hallucinations=data.frame(pEstimate=NA, Ntot=kleinloogMega_analysis2014_n)

# ==== Hallucinations: 6mg THC
# Estimate chi square
tableChi=chiSquareCalc_repeated(percCan= kleinloogMega_analysis2014_6mgTHC_hallucinations,
                                percPLB = kleinloogMega_analysis2014_placebo_hallucinations,
                                nTot =  kleinloogMega_analysis2014_n, r=r_withinSubj_between_timepoints)

chiSquEstimate_kleinloogMega_analysis2014_6mgTHC_hallucinations=tableChi[1]
# Get cohen d
chies(chiSquEstimate_kleinloogMega_analysis2014_6mgTHC_hallucinations, kleinloogMega_analysis2014_n, dig=50)
pEstimate_kleinloogMega_analysis2014_6mgTHC_hallucinations=data.frame(pEstimate=NA, Ntot=kleinloogMega_analysis2014_n)


# ===== PARANOIA
kleinloogMega_analysis2014_placebo_paranoia=4
kleinloogMega_analysis2014_2mgTHC_paranoia=7
kleinloogMega_analysis2014_4mgTHC_paranoia=12
kleinloogMega_analysis2014_6mgTHC_paranoia=16

# ==== Paranoia: 2mg THC
# Estimate chi square
tableChi=chiSquareCalc_repeated(percCan= kleinloogMega_analysis2014_2mgTHC_paranoia,
                                percPLB = kleinloogMega_analysis2014_placebo_paranoia,
                                nTot =  kleinloogMega_analysis2014_n, r=r_withinSubj_between_timepoints)

chiSquEstimate_kleinloogMega_analysis2014_2mgTHC_paranoia=tableChi[1]
# Get cohen d
chies(chiSquEstimate_kleinloogMega_analysis2014_2mgTHC_paranoia, kleinloogMega_analysis2014_n, dig=50)
pEstimate_kleinloogMega_analysis2014_2mgTHC_paranoia=data.frame(pEstimate=NA, Ntot=kleinloogMega_analysis2014_n)

# ==== Paranoia: 4mg THC
# Estimate chi square
tableChi=chiSquareCalc_repeated(percCan= kleinloogMega_analysis2014_4mgTHC_paranoia,
                                percPLB = kleinloogMega_analysis2014_placebo_paranoia,
                                nTot =  kleinloogMega_analysis2014_n, r=r_withinSubj_between_timepoints)

chiSquEstimate_kleinloogMega_analysis2014_4mgTHC_paranoia=tableChi[1]
# Get cohen d
chies(chiSquEstimate_kleinloogMega_analysis2014_4mgTHC_paranoia, kleinloogMega_analysis2014_n, dig=50)
pEstimate_kleinloogMega_analysis2014_4mgTHC_paranoia=data.frame(pEstimate=NA, Ntot=kleinloogMega_analysis2014_n)

# ==== Paranoia: 6mg THC
# Estimate chi square
tableChi=chiSquareCalc_repeated(percCan= kleinloogMega_analysis2014_6mgTHC_paranoia,
                                percPLB = kleinloogMega_analysis2014_placebo_paranoia,
                                nTot =  kleinloogMega_analysis2014_n, r=r_withinSubj_between_timepoints)

chiSquEstimate_kleinloogMega_analysis2014_6mgTHC_paranoia=tableChi[1]
# Get cohen d
chies(chiSquEstimate_kleinloogMega_analysis2014_6mgTHC_paranoia, kleinloogMega_analysis2014_n, dig=50)
pEstimate_kleinloogMega_analysis2014_6mgTHC_paranoia=data.frame(pEstimate=NA, Ntot=kleinloogMega_analysis2014_n)


# Dose response relationship
kleinloogMega_analysis2014_doseResp_nEff=n_effective_dependent(kleinloogMega_analysis2014_n, r=r_withinSubj_between_timepoints) # effective sample size

# Dose–response relationships
# dose response relation using a p-value of 0.05/96 (Bonferroni correction for four times 24 items; the five items that did not differ significantly between placebo and THC were not taken along).
# extract unadjusted p-values

# Paranoia (significant after bonferroni correction)
pvalkleinloogMega_paranoia=0.05/96
extract_kleinloogMega_analysis2014_doseResp_paranoia=pes(pvalkleinloogMega_paranoia,
                                                         n.1=kleinloogMega_analysis2014_doseResp_nEff/2,
                                                         n.2=kleinloogMega_analysis2014_doseResp_nEff/2)
# Hallucinations: ns at adjusted p-value level
# ==> cannot use pValueNS_allStudies
#extract_kleinloogMega_analysis2014_doseResp_hallucinations=pes(pValueNS_allStudies, n.1=kleinloogMega_analysis2014_doseResp_nEff/2, n.2=kleinloogMega_analysis2014_doseResp_nEff/2)


# P-values
pEstimate_kleinloogMega_analysis2014_doseResp_paranoia=data.frame(pEstimate=NA, Ntot=kleinloogMega_analysis2014_n)
pEstimate_kleinloogMega_analysis2014_doseResp_hallucinations=data.frame(pEstimate=NA, Ntot=kleinloogMega_analysis2014_n)


# rates
# CAPS = rates of responders (suspicious / hallucinations)
kleinloogMega_analysis2014_paranoiaDF=data.frame(event=round((kleinloogMega_analysis2014_n/100)*kleinloogMega_analysis2014_6mgTHC_paranoia,0),
                                                 n=kleinloogMega_analysis2014_n)
kleinloogMega_analysis2014_hallucinationsDF=data.frame(event=round((kleinloogMega_analysis2014_n/100)*kleinloogMega_analysis2014_6mgTHC_hallucinations,0),
                                                       n=kleinloogMega_analysis2014_n)

# CAPS =controls
kleinloogMega_analysis2014_paranoiaControl=data.frame(event=round((kleinloogMega_analysis2014_n/100)*kleinloogMega_analysis2014_placebo_paranoia,0),
                                                 n=kleinloogMega_analysis2014_n)
kleinloogMega_analysis2014_hallucinationsControl=data.frame(event=round((kleinloogMega_analysis2014_n/100)*kleinloogMega_analysis2014_placebo_hallucinations,0),
                                                       n=kleinloogMega_analysis2014_n)





# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Ware (2015) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Ware, Mark A., Tongtong Wang, Stan Shapiro, Jean-Paul Collet, Aline Boulanger, John M. Esdaile, Allan Gordon, Mary Lynch, Dwight E. Moulin, and Colleen O'Connell. "Cannabis for the management of pain: assessment of safety study (COMPASS)." The Journal of Pain 16, no. 12 (2015): 1233-1242.

# A standardized herbal cannabis product (12.5% tetrahydrocannabinol) was dispensed to eligible individuals for a 1-year period
# Two hundred and fifteen individuals with chronic pain were recruited to the cannabis group (141 current users and 58 ex-users) and 216 controls (chronic pain but no current cannabis use) from 7 clinics across Canada.

ware2015medCan_treatment_n=215

# Get estimates for rates
ware2015medCan_paranoiaDF=data.frame(event=2, n=ware2015medCan_treatment_n)
ware2015medCan_hallucinationsDF=data.frame(event=1, n=ware2015medCan_treatment_n)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Ware (2010) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Ware, Mark A., Tongtong Wang, Stan Shapiro, Ann Robinson, Thierry Ducruet, Thao Huynh, Ann Gamsa, Gary J. Bennett, and Jean-Paul Collet. "Smoked cannabis for chronic neuropathic pain: a randomized controlled trial." Cmaj 182, no. 14 (2010): E694-E701.

# receive cannabis at four potencies (0%, 2.5%, 6% and 9.4% tetrahydrocannabi- nol), crossover trial

ware2010RCT_n=21
# Hallucinations (effect of single dose, no means/sd reported for this analysis)
tableChi=chiSquareCalc_repeated(percCan= (1/ware2010RCT_n)*100, percPLB = 0, nTot =  ware2010RCT_n, r=r_withinSubj_between_timepoints)
chiSquEstimate_ware2010RCT_paranoia=tableChi[1]
Ntot_ware2010RCT_paranoia=tableChi[2]
# Get cohen d
extract_ware2010RCT_paranoia=chies(chiSquEstimate_ware2010RCT_paranoia, Ntot_ware2010RCT_paranoia, dig=50)
pEstimate_ware2010RCT_paranoia=data.frame(pEstimate=NA, Ntot=ware2010RCT_n)
# Rate
ware2010RCT_paranoiaDF=data.frame(event=1, n=ware2010RCT_n)

# Rate
ware2010RCT_paranoiaControl=data.frame(event=0, n=ware2010RCT_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Cooper (2017) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Cooper, Ruth E., Emma Williams, Seth Seegobin, Charlotte Tye, Jonna Kuntsi, and Philip Asherson. "Cannabinoids in attention-deficit/hyperactivity disorder: A randomised-controlled trial." European Neuropsychopharmacology 27, no. 8 (2017): 795-808.
# 6-week, double-blind, randomised placebo- controlled experimental trial of Sativex Oromucosal Spray, a cannabinoid medication containing a 1:1 ratio of delta-9- tetrahydrocannabinol (Δ9-THC) to cannabidiol (CBD).

cooperADHD_sativex_n=6
cooperADHD_placebo_n=8
cooperADHD_sativex_mean=0
cooperADHD_placebo_mean=0
cooperADHD_sativex_sd=0
cooperADHD_placebo_sd=0

mes(cooperADHD_sativex_mean, cooperADHD_placebo_mean, cooperADHD_sativex_sd, cooperADHD_placebo_sd, cooperADHD_sativex_n, cooperADHD_placebo_n)
# Note: Cannot estimate effect size using mean comparisons => use p-value instead
extractCooperADHD_paranoia=pes(1, cooperADHD_sativex_n, cooperADHD_placebo_n)
pEstimate_cooperADHD_sativex=data.frame(pEstimate=NA, Ntot=cooperADHD_sativex_n+cooperADHD_placebo_n)







# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Ganesh (2020) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ganesh, Suhas, Jose Cortes-Briones, Mohini Ranganathan, Rajiv Radhakrishnan, Patrick D. Skosnik, and Deepak Cyril D’Souza. "Psychosis-relevant effects of intravenous delta-9-tetrahydrocannabinol-A mega analysis of individual participant-data from human laboratory studies." International Journal of Neuropsychopharmacology (2020).
# THC-induced psychosis-relevant effects were examined using a data repository of 10 double-blind, randomized, placebo-controlled, crossover studies with 400 i.v. THC infusions in healthy human volunteers.


# STUDY 1 - THC response in healthy frequent and infrequent
# # REF: D'souza, Deepak Cyril, Mohini Ranganathan, Gabriel Braley, Ralitza Gueorguieva, Zoran Zimolo, Thomas Cooper, Edward Perry, and John Krystal. "Blunted psychotomimetic and amnestic effects of Δ-9-tetrahydrocannabinol in frequent users of cannabis." Neuropsychopharmacology 33, no. 10 (2008): 2505-2516.
# N included = 44 (in mega)
# Extracted estimate (Tabea): exlcudee all (ie effect of frequecy, dose-response) except effect of single dose

# STUDY 2: 2 Interactions between haloperidol and THC
# REF: D’Souza, Deepak Cyril, Gabriel Braley, Rebecca Blaise, Michael Vendetti, Stephen Oliver, Brian Pittman, Mohini Ranganathan et al. "Effects of haloperidol on the behavioral, subjective, cognitive, motor, and neuroendocrine effects of Δ-9-tetrahydrocannabinol in humans." Psychopharmacology 198, no. 4 (2008): 587-603.
# N included = 31 (in mega)
# Extracted estimate (Tabea): nothing to exclude as none of the predicors assessed in the mega-analysis (frequenct cannabis use, dose-response, gender, age, nicotine) was included

# STUDY 3: Genetics of THC response and interaction with tolcapone
# REF: Ranganathan, Mohini, Joao P. De Aquino, Jose A. Cortes-Briones, Rajiv Radhakrishnan, Brian Pittman, Savita Bhakta, and Deepak C. D’Souza. "Highs and lows of cannabinoid-dopamine interactions: effects of genetic variability and pharmacological modulation of catechol-O-methyl transferase on the acute response to delta-9-tetrahydrocannabinol in humans." Psychopharmacology 236, no. 11 (2019): 3209-3219.
# N included = 78 (in mega)
# Extracted estimate (Tabea): nothing to exclude as none of the predictors assessed in the mega-analysis (frequenct cannabis use, dose-response, gender, age, nicotine) was included

# STUDY 4: THC effects in people with family history of alcoholism
# REF: not published
# N included = 30 (in mega)
# Extracted estimate (Tabea): Not extracted

# STUDY 5 - Electrophysiological effects of THC
# Extracted estimate (Tabea): Not extracted

# STUDY 6 - Interactions between iomazenil and THC
# REF: Radhakrishnan R, Skosnik PD, Cortes-Briones J, Sewell RA, Carbuto M, Schnakenberg A, Cahill J, Bois F, Gunduz-Bruce H, Pittman B, Ranganathan M, D’Souza DC (2015) GABA deficits enhance the psychotomimetic effects of Δ9-THC. Neuropsychopharmacology 40:2047–2056.
# N included = 23 (in mega)
# Extracted estimate (Tabea): nothing to exclude as none of the predictors assessed in the mega-analysis (frequenct cannabis use, dose-response, gender, age, nicotine) was included

# STUDY 7 - Interactions between naltrexone and THC
# REF: Ranganathan M, Carbuto M, Braley G, Elander J, Perry E, Pittman B, Radhakrishnan R, Sewell RA, D’Souza DC (2012b) Naltrexone does not attenuate the effects of intra- venous Δ9-tetrahydrocannabinol in healthy humans. Int J Neuropsychopharmacol 15:1251–1264.
# N included = 6 (in mega)
# Extracted estimate (Tabea): not extracted

# STUDY 8 - Interactions between physostigmine and THC
# REF: unpublished
# N included = 6 (in mega)
# Extracted estimate (Tabea): not extracted

# STUDY 9 - Interactions between cannabidiol and THC
# REF: unpublished
# N included = 29 (in mega)
# Extracted estimate (Tabea): not extracted

# STUDY 10 -  THC effects on information processing
# Extracted estimate (Tabea): Not extracted


# cannabis- naïve individuals were not permitted to participate in the studies
# Of the 239 participants, 114 (47.7%) received more than 1 dose of THC (2–6 infusions) on different test days.
# received a total of 400 i.v. THC
# The mean (SD) age of the sample was 25.29 (7.49) years, and only 32 (13.4%) participants were older than 30 years.


# ===== Dose response
# There was a significant effect of dose on the PANSS positive symptom response to THC (beta = 11.13, SE = 4.94, Wald χ2 = 19.88, P < .001).
ganesh2020_megaTHCdose_n=114
n_effective_ganesh2020_megaTHCdose=n_effective_dependent(ganesh2020_megaTHCdose_n, r=r_withinSubj_between_timepoints)
chies(19.88, n_effective_ganesh2020_megaTHCdose, dig=50)
pEstimate_ganesh2020_megaTHCdose=data.frame(pEstimate=NA, Ntot=ganesh2020_megaTHCdose_n) #  P < .001

# ===== Effect of frequency of cannabis use =>  Frequent cannabis use (beta = −0.575, SE = 0.14, Wald χ2 = 18.13, P < .001)
#  associated with a blunted psychotomimetic re- sponse. Psychotomimetic Response (= change in panss pos following THC)
ganesh2020_megaTHC_canFrq_n=239
chies( 18.13, ganesh2020_megaTHC_canFrq_n, dig=50) # higher frequency linked to fewer symptoms
pEstimate_ganesh2020_megaTHCcanFrq=data.frame(pEstimate=NA, Ntot=ganesh2020_megaTHC_canFrq_n) #  P < .001

# ===== Effect of age =>  age (beta = −0.14, SE = 0.006, Wald χ2 = 5.05 P = .025)
#  older age associated with a blunted psychotomimetic response. Psychotomimetic Response (= change in panss pos following THC)
ganesh2020_megaTHC_age_n=287
chies( 5.05, ganesh2020_megaTHC_age_n, dig=50) # older age linked to fewer symptoms
pEstimate_ganesh2020_megaTHCage=data.frame(pEstimate=.025, Ntot=ganesh2020_megaTHC_age_n)


# ===== Gender
#  There was no significant effect of gender on psychotomimetic response. Psychotomimetic Response (= change in panss pos following THC)
ganesh2020_megaTHC_gender_n=287
pes(p=pValueNS_allStudies, n.1=ganesh2020_megaTHC_gender_n/2, n.2=ganesh2020_megaTHC_gender_n/2)
pEstimate_ganesh2020_megaTHCgender=data.frame(pEstimate=NA, Ntot=ganesh2020_megaTHC_age_n)

# ===== Tobacco use
# in a subsample with data about current tobacco smoking (n=269 infusions, 67.25%), smokers had a significantly lower psychotomimetic response. Psychotomimetic Response (= change in panss pos following THC)
ganesh2020_megaTHC_tobacooAll_n=269
ganesh2020_megaTHC_tobacooSmoker_n=round((ganesh2020_megaTHC_tobacooAll_n/100)*67.25,0)
ganesh2020_megaTHC_tobacooNonSmoker_n=ganesh2020_megaTHC_tobacooAll_n-ganesh2020_megaTHC_tobacooSmoker_n

# Extract estimates: Smokers have lower response to THC
mes(3, 4.32,  3.46, 3.46, ganesh2020_megaTHC_tobacooSmoker_n, ganesh2020_megaTHC_tobacooNonSmoker_n, dig = 20)
pEstimate_ganesh2020_megaTHC_tobacoo=data.frame(pEstimate=0.003, Ntot=ganesh2020_megaTHC_tobacooAll_n)


# rates
ganesh2020_mega_delusionsDF=data.frame(event=(239/100)*4, n=239)  # based on Cut-off = 3, as this was also applied by ENGLUND [also cf mean PANSS pos in FEP patients (ref:https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4856312/)
ganesh2020_mega_hallucinationDF=data.frame(event=(239/100)*19, n=239)
ganesh2020_mega_paranoiaDF=data.frame(event=(239/100)*2.5, n=239)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Ranganathan (2012) ========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Ranganathan, Mohini, Michelle Carbuto, Gabriel Braley, Jaqueline Elander, Edward Perry, Brian Pittman, Rajiv Radhakrishnan, Richard A. Sewell, and Deepak C. D'Souza. "Naltrexone does not attenuate the effects of intravenous Δ9-tetrahydrocannabinol in healthy humans." International Journal of Neuropsychopharmacology 15, no. 9 (2012): 1251-1264.

# NOTE: Effects of THC on PANSS positive not included as included in the mega analysis by Ganesh (2020)

# ==== Effect of naltrexone
# THC naltrexone interactions on psychotomimetic effects
# As expected, naltrexone had no significant main effects on any of these measures. Further, there were no significant THC rnaltrexone interactions on psychotomimetic effects.
Ranganathan_PANSSpos_naltrexone_n=30
# No significant effects
Ranganathan_PANSSpos_naltrexone_n=n_effective_dependent(Ranganathan_PANSSpos_naltrexone_n, r=r_withinSubj_between_timepoints) # effective sample size
extract_Ranganathan_PANSSpos_naltrexone=pes(pValueNS_allStudies, n.1=Ranganathan_PANSSpos_naltrexone_n/2, n.2=Ranganathan_PANSSpos_naltrexone_n/2)
# Extract p-value
pEstimate_Ranganathan_PANSSpos_naltrexone=data.frame(pEstimate=NA, Ntot=Ranganathan_PANSSpos_naltrexone_n)

# Rates => extracted from ganesh

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Amerongen (2018) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# van Amerongen, G., P. Siebenga, M. L. de Kam, J. L. Hay, and G. J. Groeneveld. "Effect profile of paracetamol, Δ9‐THC and promethazine using an evoked pain test battery in healthy subjects." European Journal of Pain 22, no. 7 (2018): 1331-1342.
# three subjects (12%) mild audi- tory hallucinations. => no such effects were reported in placebo condition
Amerongen2018_n=23 # that completed study participation

# rates
# three subjects (12%) mild auditory hallucinations
amerongen_oralTHC10mg_hallucinationsDF=data.frame(event=3, n=Amerongen2018_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Patti (2016) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Patti, Francesco, Silvia Messina, Claudio Solaro, Maria Pia Amato, Roberto Bergamaschi, Simona Bonavita, R. Bruno Bossio et al. "Efficacy and safety of cannabinoid oromucosal spray for multiple sclerosis spasticity." Journal of Neurology, Neurosurgery & Psychiatry 87, no. 9 (2016): 944-951.

patti2016Sativex_n=1615 # 3 discontinued treatment due to the emergence of hallucinations
patti2016Sativex_hallucinationsDF=data.frame(event=3, n=patti2016Sativex_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Freeman (2020) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Study is a mega analysis, reporting results already incldued in previous studies
# Extracted are only those estimates that are not yet included in the meta analysis
# Outcome measure changes in PSI: Psychotomimetic States Inventory (PSI): The PSI was admin- istered following drug administration on both test days. The PSI is a 48-item scale designed to measure drug-induced changes in psychosis-like experiences (Mason et al., 2008).


# Hindocha, Freeman, et al. (2015) = Morgan_Curran_Experiment
FreemanMega_Hindocha2015_n=48
FreemanMega_Hindocha2015_effective=n_effective_dependent(n=FreemanMega_Hindocha2015_n, r=r_withinSubj_between_timepoints)
des(0.147, n.1=FreemanMega_Hindocha2015_effective/2, n.2=FreemanMega_Hindocha2015_effective/2)
pEstimate_FreemanMega_Hindocha2015_PSI=data.frame(pEstimate=NA, Ntot=FreemanMega_Hindocha2015_n)

# Hindocha et al. (2017) = Curran_Hindocha
FreemanMega_Hindocha2017_n=48
FreemanMega_Hindocha2017_effective=n_effective_dependent(n=FreemanMega_Hindocha2017_n, r=r_withinSubj_between_timepoints)
des(0.520, n.1=FreemanMega_Hindocha2017_effective/2, n.2=FreemanMega_Hindocha2017_effective/2)
pEstimate_FreemanMega_Hindocha2017_PSI=data.frame(pEstimate=NA, Ntot=FreemanMega_Hindocha2017_n)

# Lawn et al. (2016) = Lawn_experiment
FreemanMega_Lawn2016_n=17
FreemanMega_Lawn2016_effective=n_effective_dependent(n=FreemanMega_Lawn2016_n, r=r_withinSubj_between_timepoints)
des(0.313, n.1=FreemanMega_Lawn2016_effective/2, n.2=FreemanMega_Lawn2016_effective/2)
pEstimate_FreemanMega_Lawn2016_PSI=data.frame(pEstimate=NA, Ntot=FreemanMega_Lawn2016_n)

# Mokrysz et al. (2016)
FreemanMega_Mokrysz_n=40
FreemanMega_Mokrysz_effective=n_effective_dependent(n=FreemanMega_Mokrysz_n, r=r_withinSubj_between_timepoints)
des(0.658, n.1=FreemanMega_Mokrysz_effective/2, n.2=FreemanMega_Mokrysz_effective/2)
pEstimate_FreemanMega_Mokrysz_PSI=data.frame(pEstimate=NA, Ntot=FreemanMega_Mokrysz_n)

# Effect of frequency of cannabis use
# direction of efffects: For each additional day of cannabis use per month, there was a decrease in the difference between post-drug placebo and THC stoned ratings
FreemanMega_PSI_freq_all_n=128
FreemanMega_PSI_freq_all_effective=n_effective_dependent(n=FreemanMega_PSI_freq_all_n, r=r_withinSubj_between_timepoints)
pes(0.816, n.1=FreemanMega_PSI_freq_all_effective/2, n.2=FreemanMega_PSI_freq_all_effective/2)
pEstimate_FreemanMega_PSI_freq=data.frame(pEstimate=0.816, Ntot=FreemanMega_PSI_freq_all_n)


# tolerance
pes(0.522, n.1=FreemanMega_PSI_freq_all_effective/2, n.2=FreemanMega_PSI_freq_all_effective/2)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bonn-Miller (2021) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Bonn-Miller, Marcel O., Sue Sisley, Paula Riggs, Berra Yazar-Klosinski, Julie B. Wang, Mallory JE Loflin, Benjamin Shechet et al. "The short-term impact of 3 smoked cannabis preparations versus placebo on PTSD symptoms: A randomized cross-over clinical trial." Plos one 16, no. 3 (2021): e0246990.

# smoked cannabis containing three different concentrations of THC and CBD, and placebo.
# Placebo controlled only at stage 1 ["As placebo was not an option in Stage 2, placebo participants were randomized 1:1 between High THC and High CBD,"]
# Four concentrations of cannabis from NIDA included: High THC = approximately 12% THC and < 0.05% CBD); High CBD = 11% CBD and 0.50% THC; THC+-CBD = approximately 7.9% THC and 8.1% CBD, and placebo = < 0.03% THC and < 0.01% CBD.
#  treatment delivery = smoked
# Adverse Events (AEs) were assessed at baseline, during the introductory session, self-administration session, end of treatment, and before/after cessation in each stage by asking participants to self-report any side effects experienced over the past week.
# The study physician then rated all AEs by severity (mild, moderate, severe) and study relatedness (i.e., possibly related, probably related, not related).

# THC conditions: high THC and THC + CBD  (stage 1)
BM_nTot_highTHC=40 # THC condiction
BM_nTot_PLC=20 # Placebo condition
# Hallucinations
NriskA_case=1
NriskB_case=0
NriskA_control=BM_nTot_highTHC-NriskA_case
NriskB_control=BM_nTot_PLC-NriskB_case
labelA="THC"
labelB="Placebo"
extract_EstBonn_HAL=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pvalEstBonn_HAL=data.frame(pEstimate=NA, Ntot=extract_EstBonn_HAL$N.total)


# Paranoia
NriskA_case=5
NriskB_case=0
NriskA_control=BM_nTot_highTHC-NriskA_case
NriskB_control==BM_nTot_PLC-NriskB_case
labelA="THC"
labelB="Placebo"
extract_EstBonn_PAR=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pvalEstBonn_PAR=data.frame(pEstimate=NA, Ntot=extract_EstBonn_PAR$N.total)



# Get estimates for rates
bonn2021HighTC_hallucinationsDF=data.frame(event=1, n=BM_nTot_highTHC)  # 1 case
bonn2021HighTC_paranoiaDF=data.frame(event=5, n=BM_nTot_highTHC)  # 5 cases

# Get estimates for rates
bonn2021HighTC_hallucinationsControl=data.frame(event=0, n=BM_nTot_PLC)  
bonn2021HighTC_paranoiaControl=data.frame(event=0, n=BM_nTot_PLC)  # 5 cases

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Aviram (2021) ========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aviram, Joshua, Dorit Pud, Tamar Gershoni, Bareket Schiff‐Keren, Miriam Ogintz, Simon Vulfsons, Tamar Yashar et al. "Medical cannabis treatment for chronic pain: Outcomes and prediction of response." European Journal of Pain 25, no. 2 (2021): 359-374.
# get rates in treatment group
aviramMedCanT1_n=829

aviramMedCanT1_HallucinationsDF=data.frame(event=20, n=aviramMedCanT1_n)
aviramMedCanT1_ParanoiaDF=data.frame(event=15, n=aviramMedCanT1_n)
aviramMedCanT1_PsychosisDF=data.frame(event=8, n=aviramMedCanT1_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Gustavsen (2021) ========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Gustavsen, S., H. B. Søndergaard, K. Linnet, R. Thomsen, B. S. Rasmussen, P. S. Sorensen, F. Sellebjerg, and A. B. Oturai. "Safety and efficacy of low-dose medical cannabis oils in multiple sclerosis." Multiple sclerosis and related disorders 48 (2021): 102708.
# Mean doses of THC and CBD were 4.0 mg and 7.0 mg, respectively, and primarily administered as a once-daily evening dose.

# Assessed AE: dry mouth, cognitive impairment, dizziness, nausea, drowsiness, headache, excessive thoughts, confusion, feeling subdued, depression, affected sensory impressions (hallucina- tions), felt persecuted (paranoia),
gustavsen2021MedCanMS_n=28 # 28 patients with MS

gustavsen2021MedCanMS_ParanoiaDF=data.frame(event=0, n=gustavsen2021MedCanMS_n)
gustavsen2021MedCanMS_HallucinationsDF=data.frame(event=0, n=gustavsen2021MedCanMS_n)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Theunissen (2021) ========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# inhaled vapour of placebo or 75μg/kg bodyweight JWH-018.
# The average dose of JWH-018 administered was 5.52 mg.

theunissen2021_JWH_018_paranoia_n=24
theunissen2021_JWH_018_paranoia_n_perc=(1/theunissen2021_JWH_018_paranoia_n)*100 # 1 case reporting paranoia
tableChi=chiSquareCalc_repeated(percCan= theunissen2021_JWH_018_paranoia_n_perc, percPLB = 0, nTot =  theunissen2021_JWH_018_paranoia_n, r=r_withinSubj_between_timepoints)
chiSquEstimate_theunissen2021_JWH_018_paranoia=tableChi[1]
Ntot_theunissen2021_JWH_018_paranoia=tableChi[2]
# Get cohen d
extracted_theunissen2021_JWH_018_paranoia=chies(chiSquEstimate_theunissen2021_JWH_018_paranoia, Ntot_theunissen2021_JWH_018_paranoia, dig=50)
pEstimate_theunissen2021_JWH_018_paranoia=data.frame(pEstimate=NA, Ntot=theunissen2021_JWH_018_paranoia_n)

# rate
# After JWH-018 administration, one participant reported paranoid feelings.
theunissen2021_JWH_018_paranoiaDF=data.frame(event=1, n=theunissen2021_JWH_018_paranoia_n)
theunissen2021_JWH_018_paranoiaControl=data.frame(event=0, n=theunissen2021_JWH_018_paranoia_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Aran (2021) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Aran, Adi, Moria Harel, Hanoch Cassuto, Lola Polyansky, Aviad Schnapp, Nadia Wattad, Dorit Shmueli, Daphna Golan, and F. Xavier Castellanos. "Cannabinoid treatment for autism: a proof-of-concept randomized trial." Molecular autism 12, no. 1 (2021): 1-11.
# Treatments were: (1) oral placebo, (2) whole-plant cannabis extract containing CBD and THC at a 20:1 ratio, and (3) pure CBD and pure THC at the same ratio and concentration.


# Hallucinations
aran2021_medCan_autism_hallucination_n=93
aran2021_medCan_autism_hallucination_perc=6 #  pure THC (moderate severity)
aran2021_medCan_autism_hallucination_perc_placebo=6
tableChi=chiSquareCalc_repeated(percCan= aran2021_medCan_autism_hallucination_perc, percPLB = aran2021_medCan_autism_hallucination_perc_placebo, nTot =  aran2021_medCan_autism_hallucination_n, r=r_withinSubj_between_timepoints)
#chiSquEstimate_aran2021_medCan_autism_hallucination=tableChi[1]

chiSquEstimate_aran2021_medCan_autism_hallucination=tableChi[1]
chiSquEstimate_aran2021_medCan_autism_hallucination=ifelse(chiSquEstimate_aran2021_medCan_autism_hallucination==0, 0.0000000000000000001, chiSquEstimate_aran2021_medCan_autism_hallucination)
Ntot_aran2021_medCan_autism_hallucination=tableChi[2]

# Get cohen d
extractedAran2021_medCan_autism_hallucination=chies(chiSquEstimate_aran2021_medCan_autism_hallucination, Ntot_aran2021_medCan_autism_hallucination, dig=50)
pEstimate_aran2021_medCan_autism_hallucination=data.frame(pEstimate=NA, Ntot=aran2021_medCan_autism_hallucination_n)

# rates in case group
aran2021_medCan_autism_hallucination_cases=round((aran2021_medCan_autism_hallucination_n/100)*aran2021_medCan_autism_hallucination_perc,0)
aran2021_medCan_autism_hallucination_HallucinationsDF=data.frame(event=aran2021_medCan_autism_hallucination_cases, n=aran2021_medCan_autism_hallucination_n)

# rates in control group
aran2021_medCan_autism_hallucination_control=round((aran2021_medCan_autism_hallucination_n/100)*aran2021_medCan_autism_hallucination_perc_placebo,0)
aran2021_medCan_autism_HallucinationsControl=data.frame(event=aran2021_medCan_autism_hallucination_control, n=aran2021_medCan_autism_hallucination_n)



# Delusions
aran2021_medCan_autism_delusions_n=93 # subject included in within-subject comparison (comparing pure cannabinoid condition to placebo condition)
aran2021_medCan_autism_delusions_perc=10 #  percentage of patients reporting delusions (any) when taking pure THC (supplement table Table S2B)
aran2021_medCan_autism_delusions_perc_placebo=7.5 #   percentage of patients reporting delusions (any) when placebot (supplement table Table S2B)
tableChi=chiSquareCalc_repeated(percCan= aran2021_medCan_autism_delusions_perc, percPLB = aran2021_medCan_autism_delusions_perc_placebo, nTot =  aran2021_medCan_autism_delusions_n, r=r_withinSubj_between_timepoints)
chiSquEstimate_aran2021_medCan_autism_delusions=tableChi[1]
Ntot_aran2021_medCan_autism_delusions=tableChi[2]

# Get cohen d
extractedAran2021_medCan_autism_delusions=chies(chiSquEstimate_aran2021_medCan_autism_delusions, Ntot_aran2021_medCan_autism_delusions, dig=50)
pEstimate_aran2021_medCan_autism_delusions=data.frame(pEstimate=NA, Ntot=aran2021_medCan_autism_delusions_n)

# rates in case group
aran2021_medCan_autism_delusions_cases=round((aran2021_medCan_autism_delusions_n/100)*aran2021_medCan_autism_delusions_perc,0)
aran2021_medCan_autism_delusionsDF=data.frame(event=aran2021_medCan_autism_delusions_cases, n=aran2021_medCan_autism_delusions_n)

# rates in control group
aran2021_medCan_autism_delusions_control=round((aran2021_medCan_autism_delusions_n/100)*aran2021_medCan_autism_delusions_perc_placebo,0)
aran2021_medCan_autism_delusionsControl=data.frame(event=aran2021_medCan_autism_delusions_control, n=aran2021_medCan_autism_delusions_n)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Schmidt (2021) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
schmidtMedCanGermany2021_n=10010

Halluzinationen_perc=0.8
schmidtMedCanGermany2021HALL_cases=round((schmidtMedCanGermany2021_n/100)*Halluzinationen_perc,0)

Wahnvorstellungen_perc=0.4
schmidtMedCanGermany2021WAHN_cases=round((schmidtMedCanGermany2021_n/100)*Wahnvorstellungen_perc,0)

schmidtMedCanGermany202_HallucinationsDF=data.frame(event=schmidtMedCanGermany2021HALL_cases, n=schmidtMedCanGermany2021_n)
schmidtMedCanGermany202_ParanoiaDF=data.frame(event=schmidtMedCanGermany2021WAHN_cases, n=schmidtMedCanGermany2021_n)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Fairhurst (2020) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Fairhurst, Charlie, Ram Kumar, Daniel Checketts, Bola Tayo, and Susie Turner. "Efficacy and safety of nabiximols cannabinoid medicine for paediatric spasticity in cerebral palsy or traumatic brain injury: a randomized controlled trial." Developmental Medicine & Child Neurology 62, no. 9 (2020): 1031-1039.

fairhurstNabiximols_n=47
fairhurstPlacebo_n=25

# hallucinations
NriskA_case=3
NriskB_case=0
NriskA_control=fairhurstNabiximols_n-NriskA_case
NriskB_control=fairhurstPlacebo_n-NriskB_case
labelA="THC"
labelB="Placebo"
extract_EstfairhurstNabiximols_HAL=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pvalEstfairhurstNabiximols_HAL=data.frame(pEstimate=NA, Ntot=extract_EstfairhurstNabiximols_HAL$N.total)

# Get estimates for rates
fairhurstNabiximols_HALDF=data.frame(event=3, n=fairhurstNabiximols_n)  # 3 cases

# Get estimates for contrl
fairhurstNabiximols_HALControl=data.frame(event=0, n=fairhurstPlacebo_n)  # 0 cases


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Habib (2020) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Habib, G., & Levinger, U. (2020). CHARACTERISTICS OF MEDICAL CANNABIS USAGE AMONG PATIENTS WITH FIBROMYALGIA. Harefuah, 159(5), 343-348.
Habib_medCan_n=101

# Get estimates for rates
Habib_medCan_psychDF=data.frame(event=1, n=Habib_medCan_n)  # ne patient developed a psychotic attack (was consuming 70 gram of MC monthly).


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Okey (2020) ========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Okey, Sarah A., and Madeline H. Meier. "A within-person comparison of the subjective effects of higher vs. lower-potency cannabis." Drug and alcohol dependence 216 (2020): 108225.
# Ns for the within-person analyses ranged from 545 to 566 depending on the scale.

# ple
# marijuana versus cannabis concentration
# Marijuana refers to the dried buds (flower) of the cannabis plant and contains, on average, 9 %–20 % THC (Chandra et al., 2019; Smart et al., 2017). Concentrates refers to an extract of cannabis plant material, with THC content that can exceed 80 % (DEA, 2017; Chandra et al., 2019; Raber et al., 2015; Smart et al., 2017).
okey2020_within_CEQ_n=545
extract_okey2020_within_CEQ=d_meanDiff_dependent(m1=1.1,
                                                 m2=1.2,
                                                 sd1=1.3 ,
                                                 sd2=1.2,
                                                 n=okey2020_within_CEQ_n,
                                                 r=r_withinSubj_between_measures)
pEstimate_okey2020_within_CEQ=data.frame(pEstimate=NA, Ntot=okey2020_within_CEQ_n) # < .001

# no rates reported

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Safakish (2020) ========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Safakish, Ramin, Gordon Ko, Vahid Salimpour, Bryan Hendin, Imrat Sohanpal, Gena Loheswaran, and Sun Young Rosalia Yoon. "Medical Cannabis for the Management of Pain and Quality of Life in Chronic Pain Patients: A Prospective Observational Study." Pain Medicine 21, no. 11 (2020): 3073-3086.

# extracted at first follow up after baseline (i.e., baseline = before MC was given)  (= largest n)
safakish_medCan_n=628

safakish_medCan_paranoia=round((safakish_medCan_n/100)*3.5,0)
safakish_medCan_hallucinations=round((safakish_medCan_n/100)*0.8,0)

# Get estimates for rates
safakish_medCan_paranoiaDF=data.frame(event=safakish_medCan_paranoia, n=safakish_medCan_n)
safakish_medCan_hallucinationsDF=data.frame(event=safakish_medCan_hallucinations, n=safakish_medCan_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Abrams (2020) ========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Abrams, Donald I., Paul Couey, Niharika Dixit, Varun Sagi, Ward Hagar, Elliott Vichinsky, Mary Ellen Kelly, John E. Connett, and Kalpna Gupta. "Effect of Inhaled Cannabis for Pain in Adults With Sickle Cell Disease: A Randomized Clinical Trial." JAMA network open 3, no. 7 (2020): e2010874-e2010874.

abramsMedCan_n=23

# Extract differences between conditions
extract_abramsMedCan_paranoia=d_meanDiff_dependent(m1=0.04,
                                                   m2=0.09,
                                                   sd1=0.04*sqrt(abramsMedCan_n),
                                                   sd2=0.09*sqrt(abramsMedCan_n),
                                                   n=abramsMedCan_n,
                                                   r=r_withinSubj_between_timepoints)
pEstimate_abramsMedCan_paranoia=data.frame(pEstimate=NA, Ntot=abramsMedCan_n) # p<0.001


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Grimison (2020) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Grimison, P., A. Mersiades, A. Kirby, N. Lintzeris, R. Morton, P. Haber, I. Olver et al. "Oral THC: CBD cannabis extract for refractory chemotherapy-induced nausea and vomiting: a randomised, placebo-controlled, phase II crossover trial." Annals of Oncology 31, no. 11 (2020): 1553-1560.# 19 patients received either nabilone (median dose = 0.75 mg) or pla- cebo
grimison2020Cancer_n=78

# Extract differences between conditions
tableChi=chiSquareCalc_repeated(percCan= 0, percPLB = 0, nTot =  grimison2020Cancer_n, r=r_withinSubj_between_timepoints)
grimison2020Cancer_hallucination=0
Ntot_grimison2020Cancer=tableChi[2]
# Note: cannot use chi square estimate (NA), use p=1 inestead

# Get cohen d
extract_grimison2020Cancer_hallucinations=pes(1, Ntot_grimison2020Cancer/2, Ntot_grimison2020Cancer/2)
# extract p
pEstimate_grimison2020Cancer_hallucinations=data.frame(pEstimate=NA, Ntot=grimison2020Cancer_n) # p<0.001

# Get estimates for rates (cases)
grimison2020Cancer_hallucinationsDF=data.frame(event=0, n=grimison2020Cancer_n)

# Get estimates for rates (controls)
grimison2020Cancer_hallucinationsControls=data.frame(event=0, n=grimison2020Cancer_n)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Martin-Santos (2012) ======================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Martin-Santos, R., J. a Crippa, A. Batalla, S. Bhattacharyya, Z. Atakan, S. Borgwardt, P. Allen et al. "Acute effects of a single, oral dose of d9-tetrahydrocannabinol (THC) and cannabidiol (CBD) administration in healthy volunteers." Current pharmaceutical design 18, no. 32 (2012): 4966-4979.

marSan2012acute_n=16

# effect of dose not extracted as already extracted (Bhattacharyya, Sagnik, José Alexandre Crippa, Paul Allen, Rocio Martin-Santos, Stefan Borgwardt, Paolo Fusar-Poli, Katya Rubia et al. "Induction of psychosis byδ9-tetrahydrocannabinol reflects modulation of prefrontal and striatal function during attentional salience processing." Archives of general psychiatry 69, no. 1 (2012): 27-36.)

# the level the PANSS total score (PANSS-TS) was correlated with THC-COOH levels at 1 hour post drug administration (rho=0.687; p=0.007)
# THC-COOH levels
cohen_marSan2012THC_COOH_panssTS=res(0.687, n=marSan2012acute_n, dig=20)
# extract p
pEstimate_marSan2012THC_COOH_panssTS=data.frame(pEstimate=0.007, Ntot=marSan2012acute_n) # p=0.007

# extract rates
# Some volun- teers, 5 (33%) showed severe effects and became markedly paranoid and anxious, but there was a wide inter-subject variability,
marSan2012acute_paranoiaDF=data.frame(event=5, n=marSan2012acute_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Favrat (2005) =============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Favrat, Bernard, Annick Ménétrey, Marc Augsburger, Laura E. Rothuizen, Monique Appenzeller, Thierry Buclin, Marie Pin, Patrice Mangin, and Christian Giroud. "Two cases of" cannabis acute psychosis" following the administration of oral cannabis." BMC psychiatry 5, no. 1 (2005): 1-6.
# [demographic info taken from: Effects of oral cannabis and dronabinol on driving capacity]
# data for effect size estimation not reported
# Rates:
favrat2005oralTHC_psychosisDF=data.frame(event=2, n=8)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Ménétrey (2005) =============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Ménétrey, Annick, Marc Augsburger, Bernard Favrat, Marie A. Pin, Laura E. Rothuizen, Monique Appenzeller, Thierry Buclin, Patrice Mangin, and Christian Giroud. "Assessment of driving capability through the use of clinical and psychomotor tests in relation to blood cannabinoids levels following oral administration of 20 mg dronabinol or of a cannabis decoction made with 20 or 60 mg Δ9-THC." Journal of analytical toxicology 29, no. 5 (2005): 327-338.
# The first participant experienced strong anxiety with paranoid feelings after drinking a milk decoction con- taining 16.5 mg THC
menetrey2005oralTHC_paranoiaDF=data.frame(event=1, n=8)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Cavazos-Rehg (2018) =======================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Cavazos-Rehg, Patricia A., Melissa J. Krauss, Shaina J. Sowles, Glennon M. Floyd, Elizabeth S. Cahn, Veronica L. Chaitan, and Marisel Ponton. "Leveraging user perspectives for insight into cannabis concentrates." The American journal of drug and alcohol abuse 44, no. 6 (2018): 628-641.
# rates only
# severe paranoia (6.9%)
cavazos_rehgCanConcentrates_paranoiaDF=data.frame(event=16, n=234)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Barratt (2013) =======================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#REF: Barratt, Monica J., Vince Cakic, and Simon Lenton. "Patterns of synthetic cannabinoid use in A ustralia." Drug and alcohol review 32, no. 2 (2013): 141-146.
#  paranoia (18%) and psychosis (4%).
barratt2016_synthetic_n=291 # (valid n)
barratt2016_synthetic_paranoiaDF=data.frame(event=(barratt2016_synthetic_n/100)*18, n=barratt2016_synthetic_n)
barratt2016_synthetic_psychosisDF=data.frame(event=(barratt2016_synthetic_n/100)*4, n=barratt2016_synthetic_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Contreras (2016) =======================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Contreras, Alexandra Elyse, Katelyn E. Hall, Daniel I. Vigil, Allison Rosenthal, Alejandro Azofeifa, and Michael Van Dyke. "Results from the Colorado Cannabis Users Survey on Health (CUSH), 2016." International Journal of Mental Health and Addiction 18, no. 1 (2020): 1-13.
contreras2020Colorado_n=1252
contreras2020Colorado_paranoiaDF=data.frame(event=261, n=contreras2020Colorado_n)
contreras2020Colorado_hallucinationsDF=data.frame(event=43, n=contreras2020Colorado_n)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Morvan (2009) =============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Morvan, Y., J. Rouvier, J-P. Olié, H. Lôo, and M-O. Krebs. "Consommations de substances illicites chez les étudiants: une enquête en service de médecine préventive." L'Encéphale 35 (2009): S202-S208.
# n = 880, mean age 20 years, 44% with data on cannabis-related experiences
#  26 % of the student sample had felt at least one of these last four « psychotic-like » effects.
morvan2009_PLE_n=(800/100)*44
# PLE
morvan2009_PLEDF=data.frame(event=(morvan2009_PLE_n/100)*26, n=morvan2009_PLE_n)
# hallucinations [visual (10 %) and auditory (6 %) hallucinations => 8%]
morvan2009_hallucinationDF=data.frame(event=(morvan2009_PLE_n/100)*8, n=morvan2009_PLE_n)
# paranoia [mistrust or feelings of persecution (11 %)]
morvan2009_paranoiaDF=data.frame(event=(morvan2009_PLE_n/100)*11, n=morvan2009_PLE_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Costiniuk (2019) ==========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Costiniuk, Cecilia T., Zahra Saneei, Syim Salahuddin, Joseph Cox, Jean-Pierre Routy, Sergio Rueda, Sara J. Abdallah et al. "Cannabis consumption in people living with HIV: Reasons for use, secondary effects, and opportunities for health education." Cannabis and cannabinoid research 4, no. 3 (2019): 204-213.
costiniuk2019_HIVparanoiaDF=data.frame(event=23, n=104)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== LaFrance (2020) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: LaFrance, Emily M., Amanda Stueber, Nicholas C. Glodosky, Dakota Mauzay, and Carrie Cuttler. "Overbaked: assessing and predicting acute adverse reactions to Cannabis." Journal of Cannabis Research 2, no. 1 (2020): 1-10.

laFrance2020arc_n=999
laFrance2020arc_paranoiaDF=data.frame(event=(laFrance2020arc_n/100)*50.3, n=laFrance2020arc_n)
laFrance2020arc_hallucinationDF=data.frame(event=(laFrance2020arc_n/100)*17, n=laFrance2020arc_n)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Naef (2003) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Naef, Myrtha, Michele Curatolo, Steen Petersen-Felix, Lars Arendt-Nielsen, Alex Zbinden, and Rudolf Brenneisen. "The analgesic effect of oral delta-9-tetrahydrocannabinol (THC), morphine, and a THC-morphine combination in healthy subjects under experimental pain conditions." Pain 105, no. 1-2 (2003): 79-88.

# THC (20 mg), morphine (30 mg), THC-morphine (20 mg THC þ 30 mg morphine), or placebo were given orally and as single doses.
naef2003analgesic_n=12


# Hallucinations (effect of single dose, no means/sd reported for this analysis)
naef2003analgesic_THC_hallucinations_n_perc=(6/naef2003analgesic_n)*100
tableChi=chiSquareCalc_repeated(percCan= naef2003analgesic_THC_hallucinations_n_perc, percPLB = 0, nTot =  naef2003analgesic_n, r=r_withinSubj_between_timepoints)
chiSquEstimate_naef2003analgesic_THC_hallucinations=tableChi[1]
Ntot_naef2003analgesic_THC_hallucinations=tableChi[2]
# Get cohen d
extract_naef2003analgesic_THC_hallucinations=chies(chiSquEstimate_naef2003analgesic_THC_hallucinations, Ntot_naef2003analgesic_THC_hallucinations, dig=50)
pEstimate_naef2003analgesic_THC_hallucinations=data.frame(pEstimate=NA, Ntot=naef2003analgesic_n)

# rate
naef2003analgesic_THC_hallucinationsDF=data.frame(event=6, n=naef2003analgesic_n)
naef2003analgesic_THC_hallucinationsControl=data.frame(event=0, n=naef2003analgesic_n)


# Mean comparison in level of hallucinations: THC alone vs THC combineed with morphine
extract_naef2003analgesic_THC_morphine_hallucinations=d_meanDiff_dependent(m1=39,
                                                                           m2=64,
                                                                           sd1=37*sqrt(naef2003analgesic_n),
                                                                           sd2=29*sqrt(naef2003analgesic_n),
                                                                           n=naef2003analgesic_n,
                                                                           r=r_withinSubj_between_timepoints)

pEstimate_naef2003analgesic_THC_morphine_hallucinations=data.frame(pEstimate=NA, Ntot=naef2003analgesic_n) # p<0.001


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Zeiger (2021) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Zeiger, Joanna S., William S. Silvers, Tonya A. Winders, Mary K. Hart, and Robert S. Zeiger. "Cannabis attitudes and patterns of use among followers of the Allergy & Asthma Network." Annals of Allergy, Asthma & Immunology 126, no. 4 (2021): 401-410.

zeiger2021_paranoiaDF=data.frame(event=(88/100)*21.6, n=88)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Zeiger (2021) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Mathews, Eva M., Emily Jeffries, Chenen Hsieh, Glenn Jones, and Julia D. Buckner. "Synthetic cannabinoid use among college students." Addictive behaviors 93 (2019): 219-224.
# n total, 1140,  with n = 90 users of synthetic

mathews2019synthetic_n=90
mathews2019synthetic_paranoiaDF=data.frame(event=57, n=mathews2019synthetic_n)

# auditory hallucinations (n = 31), visual hallucinations (n = 27) => (31+27)/2 = 29
mathews2019synthetic_hallucinationDF=data.frame(event=29, n=mathews2019synthetic_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Gilman (2019) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Gilman, Jodi M., Meryem A. Yücel, Gladys N. Pachas, Kevin Potter, Nina Levar, Hannah Broos, Eve M. Manghis, Randi M. Schuster, and A. Eden Evins. "Delta-9-tetrahydrocannabinol intoxication is associated with increased prefrontal activation as assessed with functional near-infrared spectroscopy: A report of a potential biomarker of intoxication." Neuroimage 197 (2019): 575-585.
# single dose of 5–50 mg of dronabinol
# Fifty-four participants (31 males, 23 females, mean age 26.7.41 years) completed pre and peak

#  The most common were vomiting (4 participants), nausea, paranoia, sedation, tachycardia, anxiety, dizziness, and bradycardia/ syncope (2 participants each).
gilman2019_dronabinol_n=54
gilman2019_dronabinol_paranoiaDF=data.frame(event=2, n=gilman2019_dronabinol_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Li (2019) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Li, Xiaoxue, Jacob M. Vigil, Sarah S. Stith, Franco Brockelman, Keenan Keeling, and Branden Hall. "The effectiveness of self-directed medical cannabis treatment for pain." Complementary therapies in medicine 46 (2019): 123-130.
li2019_ReleafApp_n=2987
# data from between 06/06/2016 and 10/24/2018
li2019_ReleafApp_paranoiaDF=data.frame(event=(li2019_ReleafApp_n/100)*2, n=li2019_ReleafApp_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bhattacharyya (2018) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Bhattacharyya, Sagnik, Thomas Sainsbury, Paul Allen, Chiara Nosarti, Zerrin Atakan, Vincent Giampietro, Michael Brammer, and P. K. McGuire. "Increased hippocampal engagement during learning as a marker of sensitivity to psychotomimetic effects of δ-9-THC." Psychological medicine 48, no. 16 (2018): 2748-2756.

bhattacharyya2018_THCsens_n=14
bhattacharyya2018_THCnotsens_n=22

# hippocampal activation
# The level of hippocampal activation was directly correlated (Spearman’s ρ = 0.44, p = 0.008) with the severity of transient psychotic symptoms induced by THC.
bhattacharyya2018_THCsens_hippoActivation=res(0.44, n=bhattacharyya2018_THCsens_n+bhattacharyya2018_THCnotsens_n,  dig=20)
pEstimate_bhattacharyya2018_THCsens_hippoActivation=data.frame(pEstimate=0.008, Ntot=bhattacharyya2018_THCsens_n+bhattacharyya2018_THCnotsens_n)

# age
bhattacharyya2018_THCsens_age=mes(m.1=25.86, sd.1=5.14, m.2=26.05, sd.2=5.96, n.1=bhattacharyya2018_THCsens_n, n.2=bhattacharyya2018_THCnotsens_n,  dig=20)
pEstimate_bhattacharyya2018_THCsens_age=data.frame(pEstimate=0.92, Ntot=bhattacharyya2018_THCsens_n+bhattacharyya2018_THCnotsens_n)

# intellectual functioning (National Adult Reading Test)
bhattacharyya2018_THCsens_IF=mes(m.1=97.07, sd.1=8.32, m.2=98.24, sd.2=5.37, n.1=bhattacharyya2018_THCsens_n, n.2=bhattacharyya2018_THCnotsens_n,  dig=20)
pEstimate_bhattacharyya2018_THCsens_IF=data.frame(pEstimate=0.65, Ntot=bhattacharyya2018_THCsens_n+bhattacharyya2018_THCnotsens_n)

# years in education
bhattacharyya2018_THCsens_education=mes(m.1=16.58, sd.1=4.06, m.2=17.41, sd.2=4.40, n.1=bhattacharyya2018_THCsens_n, n.2=bhattacharyya2018_THCnotsens_n,  dig=20)
pEstimate_bhattacharyya2018_THCsens_education=data.frame(pEstimate=0.59, Ntot=bhattacharyya2018_THCsens_n+bhattacharyya2018_THCnotsens_n)

# number of cigarettes/day
bhattacharyya2018_THCsens_cigarettes=mes(m.1=1.36, sd.1=3.97, m.2=1.09, sd.2=2.66, n.1=bhattacharyya2018_THCsens_n, n.2=bhattacharyya2018_THCnotsens_n,  dig=20)
pEstimate_bhattacharyya2018_THCsens_cigarettes=data.frame(pEstimate=0.81, Ntot=bhattacharyya2018_THCsens_n+bhattacharyya2018_THCnotsens_n)

# Cannabis (number of times lifetime)
bhattacharyya2018_THCsens_cannabis=mes(m.1=12.28, sd.1=7.67, m.2=14.16, sd.2=7.02, n.1=bhattacharyya2018_THCsens_n, n.2=bhattacharyya2018_THCnotsens_n,  dig=20)
pEstimate_bhattacharyya2018_THCsens_cannabis=data.frame(pEstimate=0.47, Ntot=bhattacharyya2018_THCsens_n+bhattacharyya2018_THCnotsens_n)

# Caffeine (number of cups of coffee)
bhattacharyya2018_THCsens_caffeine=mes(m.1=2.45, sd.1=2.18, m.2=2.40, sd.2=1.68, n.1=bhattacharyya2018_THCsens_n, n.2=bhattacharyya2018_THCnotsens_n,  dig=20)
pEstimate_bhattacharyya2018_THCsens_caffeine=data.frame(pEstimate=0.95, Ntot=bhattacharyya2018_THCsens_n+bhattacharyya2018_THCnotsens_n)



# Alcohol use
# coded as 0: Drinks only at weekends (moderate amounts) ; Drinks occasionally
# coded as 1: Drinks everyday in moderate amounts; Drinks everyday moderately, some days is drunk
NriskA_case=3
NriskB_case=11
NriskA_control=6
NriskB_control=16
labelA="Alc every day"
labelB="Alc not every day"
extract_bhattacharyya2018_THCsens_alc=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_bhattacharyya2018_THCsens_alc=data.frame(pEstimate=NA, Ntot=extract_bhattacharyya2018_THCsens_alc$N.total)


# Amphetamine use
# coded as 0: No use
# coded as 1: Experimental use / Occasional use (small quantities from time to time)
NriskA_case=4
NriskB_case=10
NriskA_control=1
NriskB_control=21
labelA="Experimental use"
labelB="No use"
extract_bhattacharyya2018_THCsens_amphetamine=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_bhattacharyya2018_THCsens_amphetamine=data.frame(pEstimate=NA, Ntot=extract_bhattacharyya2018_THCsens_amphetamine$N.total)


# LSD use
# coded as 0: No use
# coded as 1: Experimental use / Occasional use (small quantities from time to time)
NriskA_case=3
NriskB_case=11
NriskA_control=7
NriskB_control=15
labelA="Experimental use"
labelB="No use"
extract_bhattacharyya2018_THCsens_LSD=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_bhattacharyya2018_THCsens_LSD=data.frame(pEstimate=NA, Ntot=extract_bhattacharyya2018_THCsens_LSD$N.total)



# cocaine use
# coded as 0: No use
# coded as 1: Experimental use / Occasional use (small quantities from time to time)
NriskA_case=1
NriskB_case=13
NriskA_control=2
NriskB_control=20
labelA="Experimental use"
labelB="No use"
extract_bhattacharyya2018_THCsens_cocaine=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_bhattacharyya2018_THCsens_cocaine=data.frame(pEstimate=NA, Ntot=extract_bhattacharyya2018_THCsens_cocaine$N.total)


# mdma use
# coded as 0: No use
# coded as 1: Experimental use / Occasional use (small quantities from time to time)
NriskA_case=4
NriskB_case=10
NriskA_control=7
NriskB_control=15
labelA="Experimental use"
labelB="No use"
extract_bhattacharyya2018_THCsens_mdma=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_bhattacharyya2018_THCsens_mdma=data.frame(pEstimate=NA, Ntot=extract_bhattacharyya2018_THCsens_mdma$N.total)

# opioid use
# coded as 0: No use
# coded as 1: Experimental use / Occasional use (small quantities from time to time)
NriskA_case=1
NriskB_case=13
NriskA_control=1
NriskB_control=21
labelA="Experimental use"
labelB="No use"
extract_bhattacharyya2018_THCsens_opioid=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_bhattacharyya2018_THCsens_opioid=data.frame(pEstimate=NA, Ntot=extract_bhattacharyya2018_THCsens_opioid$N.total)



# Rates
bhattacharyya2018_THCsens_paranoiaDF=data.frame(event=bhattacharyya2018_THCsens_n, n=bhattacharyya2018_THCsens_n+bhattacharyya2018_THCnotsens_n)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Abdala (2018) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Abdallah, Sara J., Benjamin M. Smith, Mark A. Ware, Michelle Moore, Pei Zhi Li, Jean Bourbeau, and Dennis Jensen. "Effect of vaporized cannabis on exertional breathlessness and exercise endurance in advanced chronic obstructive pulmonary disease. A randomized controlled trial." Annals of the American Thoracic Society 15, no. 10 (2018): 1146-1158.

# 35 mg of inhaled vaporized cannabis (18.2% D9- tetrahydrocannabinol, ,0.1% cannabidiol) versus 35 mg of a placebo control cannabis (CTRL; 0.33% D9-tetrahydrocannabinol, ,0.99% cannabidiol)
#  adults with advanced chronic obstructive pulmonary disease (COPD)
abdallah2018_COPD_cannabis_n=8
abdallah2018_COPD_placebo_n=8
abdallah2018_COPD_n=16


# Derive cohen d  (placebo)
abdallah2018_COPD_placebo_paranoiaChange=d_meanDiff_dependent(m2=94,
                                                              m1=92.8,
                                                              sd2 =6.5 ,
                                                              sd1=8,
                                                              n=abdallah2018_COPD_placebo_n, r=r_withinSubj_between_timepoints)# Derive cohen d
# Derive cohen d  (cannabis)
abdallah2018_COPD_cannabis_paranoiaChange=d_meanDiff_dependent(m2=90.4,
                                                               m1=91.6,
                                                               sd2=12.8 ,
                                                               sd1=11.4,
                                                               n=abdallah2018_COPD_cannabis_n, r=r_withinSubj_between_timepoints)

# Compare the 2 Cohen's d
abdallah2018_COPD_paranoiaChange=compare_2cohenD(d1=abdallah2018_COPD_cannabis_paranoiaChange$d,
                                                 d1.var=abdallah2018_COPD_cannabis_paranoiaChange$var.d,
                                                 d2=abdallah2018_COPD_placebo_paranoiaChange$d,
                                                 d2.var=abdallah2018_COPD_placebo_paranoiaChange$var.d,
                                                 n=abdallah2018_COPD_n,
                                                 method="independent")

# Extract p value
pEstimate_abdallah2018_COPD_paranoiaChange=data.frame(pEstimate=NA, Ntot=abdallah2018_COPD_n) # >0.05


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Cortes-Briones (2015) =====================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Cortes-Briones, Jose A., John D. Cahill, Patrick D. Skosnik, Daniel H. Mathalon, Ashley Williams, R. Andrew Sewell, Brian J. Roach, Judith M. Ford, Mohini Ranganathan, and Deepak Cyril D’Souza. "The psychosis-like effects of Δ9-tetrahydrocannabinol are associated with increased cortical noise in healthy humans." Biological psychiatry 78, no. 11 (2015): 805-813.

#  subjects received Δ9-THC (vehicle (ethanol), 0.015mg/kg, or 0.03mg/kg) over 10 min by IV route.
#, a total of 24 subjects in the placebo condition and 23 in both the 0.015mg/kg and 0.03mg/kg conditions were included in the analyses.
cortes_PANSS_pos_THC_n=23

# Derive cohen d (0.015 mg/kg)
extract_cortes_singledose_THC15mg=d_meanDiff_dependent(m1=9.435,
                                                       m2=6.833,
                                                       sd1=2.936,
                                                       sd2=2.334,
                                                       n=cortes_PANSS_pos_THC_n,
                                                       r=r_withinSubj_between_timepoints)
# Extract p value
#  Relative to placebo, THC alone (IOM − / THC+) produced significant increases (padj = 0.005) in Total PANSS scores,
pEstimate_cortes_singledose_THC15mg=data.frame(pEstimate=NA, Ntot=cortes_PANSS_pos_THC_n)



# Derive cohen d (0.03 mg/kg)
extract_cortes_singledose_THC30mg=d_meanDiff_dependent(m1=11.217,
                                                       m2=6.833,
                                                       sd1=2.999,
                                                       sd2=2.334,
                                                       n=cortes_PANSS_pos_THC_n,
                                                       r=r_withinSubj_between_timepoints)
# Extract p value
#  Relative to placebo, THC alone (IOM − / THC+) produced significant increases (padj = 0.005) in Total PANSS scores,
pEstimate_cortes_singledose_THC30mg=data.frame(pEstimate=NA, Ntot=cortes_PANSS_pos_THC_n)


# neural noise
extract_cortes_neuralNoise=res(0.442, n=cortes_PANSS_pos_THC_n)
# Extract p value
pEstimate_extract_cortes_neuralNoise=data.frame(pEstimate=0.015, Ntot=cortes_PANSS_pos_THC_n) # >0.05


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== D'souza (2012) =====================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#REF: D'souza, Deepak Cyril, Daniel J. Fridberg, Patrick D. Skosnik, Ashley Williams, Brian Roach, Nagendra Singh, Michelle Carbuto et al. "Dose-related modulation of event-related potentials to novel and target stimuli by intravenous Δ 9-THC in humans." Neuropsychopharmacology 37, no. 7 (2012): 1632-1646.

# Note: dose effects taken from: Cortes-Briones (2015)
dsouza_eeg2012_n=26
# P3b amplitude
extract_dsouza_eeg2012_P3bamplitude=res(-0.259, n=dsouza_eeg2012_n)
# P3a amplitude
extract_dsouza_eeg2012_P3aamplitude=res(-0.242, n=dsouza_eeg2012_n)
# P3b latency
extract_dsouza_eeg2012_P3alatency=res(-0.112, n=dsouza_eeg2012_n)
# P3a latency
extract_dsouza_eeg2012_P3blatency=res(-0.029, n=dsouza_eeg2012_n)


# Extract p value (same for all as not reported)
pEstimate_dsouza_eeg2012=data.frame(pEstimate=NA, Ntot=dsouza_eeg2012_n) # >0.05


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Flachenecker (2014) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ref: Flachenecker, Peter, Thomas Henze, and Uwe K. Zettl. "Long-term effectiveness and safety of nabiximols (tetrahydrocannabinol/cannabidiol oromucosal spray) in clinical practice." European neurology 72, no. 1-2 (2014): 95-102.
flachenecker2014_medCan_n=104
flachenecker2014_medCan_paranoiaDF=data.frame(event=1, n=flachenecker2014_medCan_n)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Kleinloog (2014) =======================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Kleinloog, Daniël, Jasper Stevens, Jules Heuberger, Philip Spinhoven, and Joop van Gerven. "The influence of personality on the sensitivity to subjective effects of Δ9-tetrahydrocannabinol." Psychiatry research 220, no. 3 (2014): 945-953.
# Note: effects of dose not included as already extracted from Kleinloog mega analysis
# included outcome: ‘dysphoria’ (which occurs in only a subset of subjects) consists of hearing voices, the idea that events, objects or people have a different meaning and suspiciousness

kleinloog2014THCpersonality_n=127

# Novelty seeking
extract_kleinloog2014THC_noveltyseeking=pes(0.1995, n.1=kleinloog2014THCpersonality_n/2, n.2=kleinloog2014THCpersonality_n/2)
pEstimate_kleinloog2014THC_noveltyseeking=data.frame(pEstimate=0.1995, Ntot=kleinloog2014THCpersonality_n)
# Harm avoidance
extract_kleinloog2014THC_harmavoidance=pes(0.2462, n.1=kleinloog2014THCpersonality_n/2, n.2=kleinloog2014THCpersonality_n/2)
pEstimate_kleinloog2014THC_harmavoidance=data.frame(pEstimate=0.2462, Ntot=kleinloog2014THCpersonality_n)
# Reward dependence
extract_kleinloog2014THC_rewarddependence=pes(0.3598, n.1=kleinloog2014THCpersonality_n/2, n.2=kleinloog2014THCpersonality_n/2)
pEstimate_kleinloog2014THC_rewarddependence=data.frame(pEstimate=0.3598, Ntot=kleinloog2014THCpersonality_n)
# Persistence
extract_kleinloog2014THC_persistence=pes(0.1663, n.1=kleinloog2014THCpersonality_n/2, n.2=kleinloog2014THCpersonality_n/2)
pEstimate_kleinloog2014THC_persistence=data.frame(pEstimate=0.1663, Ntot=kleinloog2014THCpersonality_n)
# Self-directedness
extract_kleinloog2014THC_selfdirectedness=pes(0.3756, n.1=kleinloog2014THCpersonality_n/2, n.2=kleinloog2014THCpersonality_n/2)
pEstimate_kleinloog2014THC_selfdirectedness=data.frame(pEstimate=0.3756, Ntot=kleinloog2014THCpersonality_n)
# Cooperativeness
extract_kleinloog2014THC_cooperativeness=pes(0.6143, n.1=kleinloog2014THCpersonality_n/2, n.2=kleinloog2014THCpersonality_n/2)
pEstimate_kleinloog2014THC_cooperativeness=data.frame(pEstimate=0.6143, Ntot=kleinloog2014THCpersonality_n)
# Self-transcendence
extract_kleinloog2014THC_selftranscendence=pes(0.0168, n.1=kleinloog2014THCpersonality_n/2, n.2=kleinloog2014THCpersonality_n/2)
pEstimate_kleinloog2014THC_selftranscendence=data.frame(pEstimate=0.0168, Ntot=kleinloog2014THCpersonality_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Cameron (2014) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ref: Cameron, C., Watson, D., & Robinson, J. (2014). Use of a Synthetic Cannabinoid in a Correctional Population for Posttraumatic Stress Disorder–Related Insomnia and Nightmares, Chronic Pain, Harm Reduction, and Other Indications. Journal of Clinical Psychopharmacology, 34(5), 559–564.

# retrospective study of 104 male inmates with serious mental illness prescribed nabilone analyzes the indications
# Psychosis was the most serious adverse effect, but occurred in only 2 patients (1.9%); both of which had a preexisting psychotic illness
cameron2014_medCanNabilone_n=104
cameron2014_medCanNabilone_psychosisDF=data.frame(event=2, n=cameron2014_medCanNabilone_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Ahmed (2014) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ref: Ahmed, Amir IA, Geke AH van den Elsen, Angela Colbers, Marjolein A. van der Marck, David M. Burger, Ton B. Feuth, Marcel GM Olde Rikkert, and Cornelis Kramers. "Safety and pharmacokinetics of oral delta-9-tetrahydrocannabinol in healthy older subjects: a randomized controlled trial." European Neuropsychopharmacology 24, no. 9 (2014): 1475-1482.

# cross-over trial
ahmed2014namisol_n=11 #n individuals taking part in both experimental conditions (placebo, 6.5mg THC)

# Hallucinations (effect of single dose, 6.5mg THC)
ahmednumber6_5mgTHC_hallucinations= 1 # number of individuals in  6.5mg THC condition reporting hallucinations
ahmednumberPlacebo_hallucinations= 0 # number of individuals in  placebo condition reporting hallucinations
tableChi=chiSquareCalc_repeated(percCan= (ahmednumber6_5mgTHC_hallucinations/ahmed2014namisol_n)*100, percPLB = (ahmednumberPlacebo_hallucinations/ahmed2014namisol_n)*100, nTot =  ahmed2014namisol_n, r=r_withinSubj_between_timepoints)
chiSquEstimate_ahmed2014namisol_hallucinations=tableChi[1]
Ntot_ahmed2014namisol_hallucinations=tableChi[2]
# Get cohen d
extract_ahmed2014namisol_hallucinations=chies(chiSquEstimate_ahmed2014namisol_hallucinations, Ntot_ahmed2014namisol_hallucinations, dig=50)
pEstimate_ahmed2014namisol_hallucinations=data.frame(pEstimate=NA, Ntot=ahmed2014namisol_n)

# Rate (cases)
ahmed2014namisol_hallucinationsDF=data.frame(event=ahmednumber6_5mgTHC_hallucinations, n=ahmed2014namisol_n)

# Rate (controls)
ahmed2014namisol_hallucinationsControls=data.frame(event=ahmednumberPlacebo_hallucinations, n=ahmed2014namisol_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Fernández (2014) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Fernández, L. Lorente, E. Monte Boquet, F. Perez-Miralles, I. Gil Gomez, M. Escutia Roig, I. Bosca Blasco, JL Poveda Andrés, and B. Casanova-Estruch. "Clinical experiences with cannabinoids in spasticity management in multiple sclerosis." Neurología (English Edition) 29, no. 5 (2014): 257-260.
fernandezSativex_MS_n=50
fernandezSativex_paranoiaDF=data.frame(event=1, n=fernandezSativex_MS_n)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Portenoy (2012) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Portenoy, R.K., Ganae-Motan, E.D., Allende, S., Yanagihara, R., Shaiova, L., Weinstein, S., McQuade, R., Wright, S. and Fallon, M.T., 2012. Nabiximols for opioid-treated cancer patients with poorly-controlled chronic pain: a randomized, placebo-controlled, graded-dose trial. The Journal of Pain, 13(5), pp.438-449.
#  The maximum duration was 9 weeks
portenoyNabiximols_n=268
portenoyPlacebo_n=91

# hallucinations
NriskA_case=8
NriskB_case=5
NriskA_control=portenoyNabiximols_n-NriskA_case
NriskB_control=portenoyPlacebo_n-NriskB_case
labelA="THC"
labelB="Placebo"
tableChi=chiSquareCalc(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
chiSquEstimateportenoyNabiximols_HAL=tableChi[1]
NtotSquEstimateportenoyNabiximols_HAL=tableChi[2]

# Get cohen d
extract_portenoyNabiximols_hallucinations=chies(chiSquEstimateportenoyNabiximols_HAL, NtotSquEstimateportenoyNabiximols_HAL, dig=50)
# Get p-value
pvalportenoyNabiximols_HAL=data.frame(pEstimate=NA, Ntot=NtotSquEstimateportenoyNabiximols_HAL)

# Get estimates for rates
portenoyNabiximols_HALDF=data.frame(event=8, n=portenoyNabiximols_n)

# Get estimates for rates
portenoyNabiximols_HALControl=data.frame(event=5, n=portenoyPlacebo_n)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Morrison (2011) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Morrison PD, Nottage J, Stone JM, et al. (2011) Disruption of frontal theta coherence by Delta9-tetrahydrocannabinol is associated with positive psychotic symptoms. Neuropsychopharmacology 36: 827–836.

morrisonEEG2011_n=16

# theta power
# there was no correlation between change in theta power and change-in positive PANSS scores
extract_morrison_theta_PLE=res(0, n=morrisonEEG2011_n, dig=20)
# Extract p-value
pEstimate_morrison_theta_PLE=data.frame(pEstimate=NA, Ntot=morrisonEEG2011_n)

# theta coherence
# The change-in theta coherence (averaged over bi-frontal, and left and right fronto-parietal) under THC conditions was strongly associated with the change-in positive PANSS scores
extract_morrison_theta_coherence_PLE=res(0.75, n=morrisonEEG2011_n, dig=20)
# Extract p-value
pEstimate_morrison_theta_coherence_PLE=data.frame(pEstimate=0.001, Ntot=morrisonEEG2011_n)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Cooper (2005) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Cooper, Ziva D., and Margaret Haney. "Comparison of subjective, pharmacokinetic, and physiological effects of marijuana smoked as joints and blunts." Drug and alcohol dependence 103, no. 3 (2009): 107-113.
# Marijuana blunt smokers (12 women and 12 men) were recruited and participated in a 6-session outpatient study. Par- ticipants were blindfolded and smoked three puffs from either a blunt or a joint containing marijuana with varying 􏱬9-tetrahydrocannabinol (THC) concentrations (0.0, 1.8, and 3.6%).
cooper2005joints_n=24
# Get estimates for rates
cooper2005joints_ParanoiaDF=data.frame(event=1, n=cooper2005joints_n)  # 1 became paranoid after marijuana smoking.

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Abrams (2007) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Abrams, Donald I., C. A. Jay, S. B. Shade, H. Vizoso, H. Reda, S. Press, M. E. Kelly, M. C. Rowbotham, and K. L. Petersen. "Cannabis in painful HIV-associated sensory neuropathy: a randomized placebo-controlled trial." Neurology 68, no. 7 (2007): 515-521.
extract_abrams_HIVmed_can_n=27
abrams_sd_can=  ((0.45 - 0.03) / 3.92) * sqrt(extract_abrams_HIVmed_can_n)
extract_abrams_HIVmed_placebo_n=27
abrams_sd_plc=  ((0.14 - 0.01) / 3.92) * sqrt(extract_abrams_HIVmed_placebo_n)

extract_abrams_HIVmedcan_paranoia=mes(m.1=0.13,
                                      m.2=0.04,
                                      sd.1=abrams_sd_can ,
                                      sd.2=abrams_sd_plc,
                                      n.1=extract_abrams_HIVmed_can_n,
                                      n.2=extract_abrams_HIVmed_placebo_n)

pEstimate_abrams_HIVmedcan_paranoia=data.frame(pEstimate=NA, Ntot=extract_abrams_HIVmed_can_n + extract_abrams_HIVmed_placebo_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Haney (2007) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Haney, Margaret. "Opioid antagonism of cannabinoid effects: differences between marijuana smokers and nonmarijuana smokers." Neuropsychopharmacology 32, no. 6 (2007): 1391-1403.

# Included: Nonmarijuana smokers (Study 2) [study 1 does not report CAPS]
# the doses of THC (0, 2.5, 5, 10 mg) and methadone (7.5mg)
haney2007THCopioid_n=21

haney2007THCopioid_nEffective=n_effective_dependent(haney2007THCopioid_n,
                                                    r=r_withinSubj_between_timepoints) # 0 in placebo vs 0 in 2.5mh THC

# 2.5mgTHC (cannot estmiate d)
tableChi=chiSquareCalc_repeated(percCan= 0, percPLB = 0, nTot =  haney2007THCopioid_n, r=r_withinSubj_between_timepoints)
# 5mgTHC
tableChi=chiSquareCalc_repeated(percCan= (1/haney2007THCopioid_n)*100, percPLB = 0, nTot =  haney2007THCopioid_n, r=r_withinSubj_between_timepoints)
chiSquEstimate_haney2007THCopioid_5mg=tableChi[1]
extract_haney2007THCopioid_5mgTHC=chies(chiSquEstimate_haney2007THCopioid_5mg, haney2007THCopioid_nEffective, dig=50)
# 10mgTHC (cannot estmiate d)
tableChi=chiSquareCalc_repeated(percCan= 0, percPLB = 0, nTot =  haney2007THCopioid_n, r=r_withinSubj_between_timepoints)

# 10mgTHC vs. 12mg Naltrexone
# higher rates of paranoia in the naltrexone group
tableChi=chiSquareCalc_repeated(percCan= 0, percPLB = (2/haney2007THCopioid_n)*100, nTot =  haney2007THCopioid_n, r=r_withinSubj_between_timepoints)
chiSquEstimate_haney2007THCopioid_10mgTHC_naltrexone=tableChi[1]
extract_haney2007THCopioid_10mgTHC_naltrexone=chies(chiSquEstimate_haney2007THCopioid_10mgTHC_naltrexone,
                                                    haney2007THCopioid_nEffective, dig=50)

# p-estimate for all studies
pEstimate_haney2007THCopioid_paranoia=data.frame(pEstimate=NA, Ntot=haney2007THCopioid_n)

# Rate
haney2007THCopioid_10mgTHC_paranoiaDF=data.frame(event=0, n=haney2007THCopioid_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Brady (2004) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Brady, C. M., R. DasGupta, C. Dalton, O. J. Wiseman, K. J. Berkley, and C. J. Fowler. "An open-label pilot study of cannabis-based extracts for bladder dysfunction in advanced multiple sclerosis." Multiple Sclerosis Journal 10, no. 4 (2004): 425-433.
brady2004MedCanMS_n=21
brady2004MedCanMS_HallucinationsDF=data.frame(event=3, n=brady2004MedCanMS_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Beaulieu (2006) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##REF: Beaulieu, Pierre. "Effects of nabilone, a synthetic cannabinoid, on postoperative pain." Canadian Journal of Anesthesia 53, no. 8 (2006): 769-775.
# No psychotic episodes were recorded.
beaulieu2006medPain_psychosisDF=data.frame(event=0, n=41)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Holdcroft (2006) ==========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Holdcroft, Anita, Mervyn Maze, Caroline Dore, Susan Tebbs, and Simon Thompson. "A multicenter dose-escalation study of the analgesic and adverse effects of an oral cannabis extract (Cannador) for postoperative pain management." The Journal of the American Society of Anesthesiologists 104, no. 5 (2006): 1040-1046.
holdcroft2006medPain_paranoiaDF=data.frame(event=2, n=24)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Buggy (2003) ==========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Buggy, Donal J., Lynn Toogood, Shelagh Maric, Paul Sharpe, David G. Lambert, and David J. Rowbotham. "Lack of analgesic efficacy of oral δ-9-tetrahydrocannabinol in postoperative pain." Pain 106, no. 1-2 (2003): 169-172.
#  d-9-THC 5 mg ðn 1⁄4 20Þ or placebo ðn 1⁄4 20Þ

# hallucinations (=> one person in placebo arm with hallucinations)
NriskA_case=0
NriskB_case=1
NriskA_control=20
NriskB_control=19
labelA="THC"
labelB="Placebo"
extract_Buggy200_HAL=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pvalBuggy200_HAL=data.frame(pEstimate=NA, Ntot=extract_Buggy200_HAL$N.total)


# Get estimates for rates
buggy200_HAL_DF=data.frame(event=0, n=20)

# Get estimates for rates (control)
buggy200_HAL_Control=data.frame(event=1, n=19)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Niederle (1986)  ==========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Niederle, N., J. Schütte, and C. G. Schmidt. "Crossover comparison of the antiemetic efficacy of nabilone and alizapride in patients with nonseminomatous testicular cancer receiving cisplatin therapy." Klinische Wochenschrift 64, no. 8 (1986): 362-365.
niederleNabilone1986_n=20
niederleNabilone1986_hallucinationsDF=data.frame(event=1, n=niederleNabilone1986_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Pomeroy (1986)  ==========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Pomeroy, Mauve, James J. Fennelly, and Mark Towers. "Prospective randomized double-blind trial of nabilone versus domperidone in the treatment of cytotoxic-induced emesis." Cancer chemotherapy and pharmacology 17, no. 3 (1986): 285-288.
pomeroyNabilone1986_n=19
pomeroyNabilone1986_hallucinationsDF=data.frame(event=0, n=pomeroyNabilone1986_n)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Niiranen (1985)  ==========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Niiranen, Aila, and Karin Mattson. "A cross-over comparison of nabilone and prochlorperazine for emesis induced by cancer chemotherapy." American journal of clinical oncology 8, no. 4 (1985): 336-340.
niiranenNabilone1985_n=27
niiranenNabilone1985_hallucinationsDF=data.frame(event=3, n=niiranenNabilone1985_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Ahmedzai (1983)  ==========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Ahmedzai, S., D. L. Carlyle, I. T. Calder, and F. Moran. "Anti-emetic efficacy and toxicity of nabilone, a synthetic cannabinoid, in lung cancer chemotherapy." British Journal of Cancer 48, no. 5 (1983): 657-663.
ahmedzaiNabilone1983_n=28
ahmedzaiNabilone1983_hallucinationsDF=data.frame(event=0, n=ahmedzaiNabilone1983_n)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Sweet (1981)  =============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Sweet, Donald L., Nancy J. Miller, William Wellington, Edward Senay, and Lisa Sushelsky. "Δ9‐tetrahydrocannabinol as an antiemetic for patients receiving cancer chemotherapy. A pilot study." The Journal of Clinical Pharmacology 21, no. S1 (1981): 70S-75S.
sheetTHC1981_n=25
sheetTHC1981_hallucinationsDF=data.frame(event=1, n=sheetTHC1981_n)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Noyes (1976)  =============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Noyes, Russell, S. Fred Brunk, David H. Avery, and Arthur Canter. "Psychologic effects of oral delta-9-tetrahydrocannabinol in advanced cancer patients." Comprehensive psychiatry (1976).

#  placebo, 10 and 20 mgofTHC, and
noyesCancerTHC_n=44
# Hallucinations (effect of single dose, no means/sd reported for this analysis)
tableChi=chiSquareCalc_repeated(percCan= (6/noyesCancerTHC_n)*100, percPLB = 0,
                                nTot =  noyesCancerTHC_n,
                                r=r_withinSubj_between_timepoints)
chiSquEstimate_noyesCancerTHC_hallucinations=tableChi[1]
Ntot_noyesCancerTHC_hallucinations=tableChi[2]
# Get cohen d
extract_noyesCancerTHC_hallucinations=chies(chiSquEstimate_noyesCancerTHC_hallucinations, Ntot_noyesCancerTHC_hallucinations, dig=50)
pEstimate_noyesCancerTHC_hallucinations=data.frame(pEstimate=NA, Ntot=noyesCancerTHC_n)
# Rate
noyesCancerTHC_hallucinationsDF=data.frame(event=6, n=noyesCancerTHC_n)
# Rate (controls)
noyesCancerTHC_hallucinationsControl=data.frame(event=0, n=noyesCancerTHC_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Marcus (1974)  ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Marcus, Anthony M., Harry Klonoff, and Morton Low. "Psychiatric status of the marihuana user." Canadian Psychiatric Association Journal 19, no. 1 (1974): 31-39.

marcus1974CanUserQuest_n=43 + 38 # females plus males
marcus1974CanUserQuest_nFemale=43

marcus1974CanUserQuest_hallucinations=1 # only assessed in femals
marcus1974CanUserQuest_paranoid=2 + 2 # assessed in males and females

# Rate
marcus1974_hallucinationsDF=data.frame(event=marcus1974CanUserQuest_hallucinations, n=marcus1974CanUserQuest_nFemale)
# Rate
marcus1974_paranoiaDF=data.frame(event=marcus1974CanUserQuest_paranoid, n=marcus1974CanUserQuest_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Kassim (2022)  ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Healthy participants receiving nabilone (1–2mg, PO) or placebo 
# randomized, double-blind, counterbalanced, crossover manner. 
#A subset of participants completed a short battery of schizotypy measures (n = 25).
# The first seven participants were administered 2mg, but two of them terminated the WM tasks and schizotypy measure because of nausea and emesis.
# The remaining 23 participants were administered 1mg nabilone.

kassimNabilone_n=24
n_effective_kassimNabilone=n_effective_dependent(kassimNabilone_n, r=r_withinSubj_between_timepoints)

# For the remaining 24 participants, a Wilcoxon rank-sum test with continuity correction failed to show a significant effect of nabilone on the combined schizotypy score (V = 176, p = 0.5).
kassimNabilone_p=0.5 # <.001
extract_kassimNabilone=pes(kassimNabilone_p, n_effective_kassimNabilone/2, n_effective_kassimNabilone/2, dig=50)
pEstimate_kassimNabilone=data.frame(pEstimate=NA, Ntot=kassimNabilone_n)
# An overall schizotypy measure was calculated as the unweighted average of the total z-scores from all psychological scales.


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bassir (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Bassir Nia, Anahita, Maria J. Orejarena, Leigh Flynn, Christina Luddy, Deepak Cyril D’Souza, Patrick D. Skosnik, Brian Pittman, and Mohini Ranganathan. "Sex differences in the acute effects of intravenous (IV) delta-9 tetrahydrocannabinol (THC)." Psychopharmacology 239, no. 5 (2022): 1621-1628.
n_bassir2022=42
n_bassir2022_male=22
n_bassir2022_female=20
# Estimate effective sample size
n_effective_bassir2022=n_effective_dependent(n_bassir2022, r=r_withinSubj_between_timepoints)

# main effect THC
# THC induced an increase in psychotomimetic symptoms as measured by the PSI in both males and females, as revealed by main effect of dose (F2,41 = 39.79, p < 0.001).
mainTHC_bassir2022=fes(39.79, n_effective_bassir2022/2, n_effective_bassir2022/2,  dig=20)
pEstimate_mainTHC_bassir2022=data.frame(pEstimate=NA, Ntot=n_bassir2022)

# effect of sex
# THC had
# no significant main effects on sex or sex × dose interactions on psychotomimetic effects
sexTHC_bassir2022=pes(p=pValueNS_allStudies, n.1=n_bassir2022_male, n.2=n_bassir2022_female)
pEstimate_sexTHC_bassir2022=data.frame(pEstimate=NA, Ntot=n_bassir2022)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bassir (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# effect of CBD
# Post-hoc (THC vs CBD 7.5 mg) => use highest dose of administered CBD
cbdTHC_ganesh2022_n=10
# Estimate effective sample size
n_effective_ganesh2022=n_effective_dependent(cbdTHC_ganesh2022_n, r=r_withinSubj_between_timepoints)
extraxted_cbdTHC_ganesh2022=pes(p=0.26, n.1=n_effective_ganesh2022/2, n.2=n_effective_ganesh2022/2)
pEstimate_cbdTHC_ganesh2022=data.frame(pEstimate=0.26, Ntot=cbdTHC_ganesh2022_n)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Rub (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Cannabis and Mental Health: Adverse Outcomes and Self-Reported Impact of Cannabis Use by Mental Health Status

# Those who reported using cannabis in the past 12 months were asked “In the past 12 months, did you seek medical help for any adverse or negative health effects?” Response options included: yes/no/don’t know/refuse to answer.

# The final analytic sample included 6,413 respondents.
rupCanUserAvderseEvents=446 # number of individuals seeking medicak help because of averse events related to cannabis
rupHallucinations=37 # individuals reporting hallucinations
rupSurvey2022all=6344

# Rate
rupSurvey2022_hallucinationsDF=data.frame(event=rupHallucinations, n=rupSurvey2022all)


# Anxiety
NriskA_case=12 
n_Acontrol=2474 #n axniety
NriskB_case=24
n_Bcontrol=3939 # no anxiety
labelA="anxiety"
labelB="noMH"
extract_RupSurvey2022_anxiety=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_RupSurvey2022_anxiety=data.frame(pEstimate=NA, Ntot=extract_RupSurvey2022_anxiety$N.total)


# Depression
NriskA_case=13 
n_Acontrol=2069 #n dep
NriskB_case=24
n_Bcontrol=4344 # no dep
labelA="depression"
labelB="noMH"
extract_RupSurvey2022_depression=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_RupSurvey2022_depression=data.frame(pEstimate=NA, Ntot=extract_RupSurvey2022_depression$N.total)


# ptsd
NriskA_case=30 
n_Acontrol=580 #n ptsd
NriskB_case=7
n_Bcontrol=5833 # no dep
labelA="ptsd"
labelB="noMH"
extract_RupSurvey2022_ptsd=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_RupSurvey2022_ptsd=data.frame(pEstimate=NA, Ntot=extract_RupSurvey2022_ptsd$N.total)


# bipolar
NriskA_case=30 
n_Acontrol=358 #n bipolar
NriskB_case=7
n_Bcontrol=6055 # no bipolar
labelA="bipolar"
labelB="noMH"
extract_RupSurvey2022_bipolar=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_RupSurvey2022_bipolar=data.frame(pEstimate=NA, Ntot=extract_RupSurvey2022_bipolar$N.total)

# psychosis
NriskA_case=31 
n_Acontrol=164 #n psychosis
NriskB_case=5
n_Bcontrol=6249 # no psychosis
labelA="psychosis"
labelB="noMH"
extract_RupSurvey2022_psychosis=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_RupSurvey2022_psychosis=data.frame(pEstimate=NA, Ntot=extract_RupSurvey2022_psychosis$N.total)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Ueberall (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
UeberallDrabinol_n=1145 # received DRO (53.8% female, mean6standard deviation age: 56.9610.6 years),

# Rate hallucinations
UeberallDrabinol_hallucinationsDF=data.frame(event=round(0.016*UeberallDrabinol_n, 0), n=UeberallDrabinol_n)

# Rate delusions
UeberallDrabinol_delusionsDF=data.frame(event=round(0.01*UeberallDrabinol_n, 0), n=UeberallDrabinol_n)

# Rate paranoia
UeberallDrabinol_paranoiaDF=data.frame(event=round(0.008*UeberallDrabinol_n, 0), n=UeberallDrabinol_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Melén (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Melén, Christopher M., Magali Merrien, Agata M. Wasik, Georgios Panagiotidis, Olof Beck, Kristina Sonnevi, Henna-Riikka Junlén, Birger Christensson, Birgitta Sander, and Björn Engelbrekt Wahlin. "Clinical effects of a single dose of cannabinoids to patients with chronic lymphocytic leukemia." Leukemia & Lymphoma 63, no. 6 (2022): 1387-1397.
melenMedCan_n=23
# Rate hallucinations
melenMedCan_hallucinationsDF=data.frame(event=7, n=melenMedCan_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Abi_Jaoude (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Abi_Jaoude_tourette_n=9
Abi_Jaoude_tourette_hallucinationsDF=data.frame(event=0, n=Abi_Jaoude_tourette_n) # adverse ievents in condiction with highest THC dose


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Peters (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Peters, Erica N., Irina Mosesova, Laura MacNair, Ryan Vandrey, M. Hunter Land, Mark A. Ware, Cynthia Turcotte, and Marcel O. Bonn-Miller. "Safety, Pharmacokinetics and Pharmacodynamics of Spectrum Red Softgels in Healthy Participants." Journal of Analytical Toxicology 46, no. 5 (2022): 528-539.
# Mixed THC condictiions: aTreatment A: 5 mg total THC and 0.06 mg CBD daily; B: 10 mg total THC and 0.12 mg total CBD daily; C: 15 mg total THC and 0.18 mg total CBD daily; D: 20 mg total THC and 0.24 mg total CBD daily.

petersMedTrialHealthy_THC=33
petersMedTrialHealthy_placebo=8

# paranoia 
NriskA_case=2 
n_Acontrol=33 #THC groups combined
NriskB_case=0
n_Bcontrol=8 # Placebo groups
labelA="paranoia"
labelB="noMH"
extract_petersMedTrialHealthy_paranoia=chiSquareCalc2x2(NriskA_case, NriskB_case, NriskA_control, NriskB_control, labelA, labelB)
pEstimate_petersMedTrialHealthy_paranoia=data.frame(pEstimate=NA, Ntot=extract_petersMedTrialHealthy_paranoia$N.total)

# rates
petersMedTrialHealthy_paranoiaDF=data.frame(event=2, n=petersMedTrialHealthy_THC) # adverse ievents in condiction with highest THC dose

# rates
petersMedTrialHealthy_paranoiaControl=data.frame(event=0, n=petersMedTrialHealthy_THC) # adverse ievents in condiction with highest THC dose


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Peters (Santarossa) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Santarossa, Talia M., Randy So, Penelope Smyth, Stefan Gustavsen, and Ross T. Tsuyuki. "Medical cannabis use in Canadians with multiple sclerosis." Multiple Sclerosis and Related Disorders 59 (2022): 103638.
nMS_Santarossa=215
nMS_Santarossa_hallucinationsDF=data.frame(event=2, n=nMS_Santarossa) # individuals reporting moderate or severe hallucinations
nMS_Santarossa_paranoiaDF=data.frame(event=11, n=nMS_Santarossa) # individuals reporting moderate or severe hallucinations


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Tumati (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ref: Tumati, Shankar, Krista L. Lanctôt, RuoDing Wang, Abby Li, Andrew Davis, and Nathan Herrmann. "Medical cannabis use among older adults in Canada: self-reported data on types and amount used, and perceived effects." Drugs & aging 39, no. 2 (2022): 153-163.
# At follow-up, among older adults using cannabis oils (n =3009)
TumatiCanOil_n=3009

# hallucinations by 0.5%
TumatiCanOil_hallucinationsDF=data.frame(event=round(TumatiCanOil_n*0.005,0), n=TumatiCanOil_n)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Ergisi (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Ergisi, Mehmet, Simon Erridge, Michael Harris, Michal Kawka, Devaki Nimalan, Oliver Salazar, Katerina Loupasaki et al. "UK Medical Cannabis Registry: an analysis of clinical outcomes of medicinal cannabis therapy for generalized anxiety disorder." Expert review of clinical pharmacology (2022): 1-9.

# adverse events were reported by 25 patients (Table 5). 
# The most reported adverse events were ... paranoia (n = 1, 1.5%) 
ErgisiCanMedPTSD_n=25
ErgisiCanMedPTSD_paranoiaDF=data.frame(event=1, n=ErgisiCanMedPTSD_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Tofthagen (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ref: Tofthagen, Cindy, Adam Perlman, Pooja Advani, Brenda Ernst, Judith Kaur, Winston Tan, Katharine Sheffield, John Crump, Joshua Henry, and Jason Starr. "Medical Marijuana Use for Cancer-Related Symptoms among Floridians: A Descriptive Study." Journal of Palliative Medicine 25, no. 10 (2022): 1563-1570.
# Have you experienced any side effects or adverse reactions from the use of medical marijuana? (n = 155)

TofthagenMedCanCancer_n=155
TofthagenMedCanCancer_paranoiaDF=data.frame(event=5, n=TofthagenMedCanCancer_n)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Gibson (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ref: Gibson, Laurel P., Hollis C. Karoly, Jarrod M. Ellingson, Jost Klawitter, Cristina Sempio, Julia E. Squeri, Angela D. Bryan, L. Cinnamon Bidwell, and Kent E. Hutchison. "Effects of cannabidiol in cannabis flower: implications for harm reduction." Addiction Biology 27, no. 1 (2022): e13092.

# Participants were randomly 
Gibson2022_THCdom_n= 57# THC-dominant: 24% THC, 1% CBD; 
Gibson2022_THC_CBD_n= 51 # THC+CBD: 9% THC, 10% CBD; 
Gibson2022_CBDdom_n= 51 # CBD-dominant: 1% THC, 23% CBD);
Gibson2022_total=Gibson2022_THCdom_n+Gibson2022_THC_CBD_n+Gibson2022_CBDdom_n
# plasma concentrations (THC)
  
Gibson2022plasmaTHC_paranoia=res(0.02, n=Gibson2022_total, dig=20) # first time point at at the acute post-use assessment time point
pEstimate_Gibson2022plasmaTHC_paranoia=data.frame(pEstimate=NA, Ntot=Gibson2022_total)


# Simple effects tests indicated that participants in the THC condition reported higher levels of paranoia compared to those in the CBD and THC + CBD conditions at both the acute post-use assessment (p = .034) and the 1-h post-use assessment (p = .002).
Gibson2022CBD_paranoia=pes(0.034, n.1=Gibson2022_THCdom_n, n.2=Gibson2022_THC_CBD_n+Gibson2022_CBDdom_n, dig=20) 
pEstimate_Gibson2022CBD_paranoia=data.frame(pEstimate=0.034, Ntot=Gibson2022_total)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Drennan (2021) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# REF Drennan, M. L., H. C. Karoly, A. D. Bryan, K. E. Hutchison, and L. C. Bidwell. "Acute objective and subjective intoxication effects of legal-market high potency THC-dominant versus CBD-dominant cannabis concentrates." Scientific reports 11, no. 1 (2021): 1-10.

# Fifty-four participants who took part in both baseline and experimental sessions were included in analyses
# 28 participants (51.9%) were assigned to the THC-dominant concentrate condition (mean [SD] age 29.86 [8.7] years; 42.9%women) and 26 were assigned to the CBD-dominant concentrate condition (mean [SD] 29.88 [10.5] years; 53.8% women).
drennanColoradoCBD_n=26
drennanColoradoTHC_n=28

drennanColoradoTHC_CBD=pes(p=0.049, n.1=drennanColoradoCBD_n, n.2=drennanColoradoTHC_n) # p<0.05 difference between CBD and THC condition
pEstimate_drennanColoradoTHC_CBD=data.frame(pEstimate=NA, Ntot=drennanColoradoCBD_n+drennanColoradoTHC_n)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Carlyle (2021) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ref: Carlyle, Molly, Toby Constable, Zoe C. Walter, Joanna Wilson, Grace Newland, and Leanne Hides. "Cannabis-induced dysphoria/paranoia mediates the link between childhood trauma and psychotic-like experiences in young cannabis users." Schizophrenia Research 238 (2021): 178-184.


#1269/(1269+1332) =  0.4878893 (proportion of males)

# Childhood trauma was significantly positively associated with dysphoria/paranoia (b = 0.06, BootSE = 0.01, 95%CI = 0.04, 0.07, p < .001) (path ae),
library(compute.es)
# Predictor: childhood trauma (standardized beta)
carlyleSurveyCT_n=2090
carlyleSurveyCT_trauma=res(0.18, n=carlyleSurveyCT_n, dig=20)
pEstimate_carlyleSurveyCT_trauma=data.frame(pEstimate=NA, Ntot=carlyleSurveyCT_n) 

# Predictor: age (standardized beta)
carlyleSurveyCT_age=res(-0.05, n=carlyleSurveyCT_n, dig=20)
pEstimate_carlyleSurveyCT_age=data.frame(pEstimate=NA, Ntot=carlyleSurveyCT_n) 


# Predictor: Indigenous Status (standardized beta)
carlyleSurveyCT_indigenous=res(0.01, n=carlyleSurveyCT_n, dig=20)
pEstimate_carlyleSurveyCT_indigenous=data.frame(pEstimate=NA, Ntot=carlyleSurveyCT_n)


# Predictor: GAD Anxiety (standardized beta)
carlyleSurveyCT_anxiety=res(0.51, n=carlyleSurveyCT_n, dig=20)
pEstimate_carlyleSurveyCT_anxiety=data.frame(pEstimate=NA, Ntot=carlyleSurveyCT_n)

# Predictor:  PHQ-9 - Patient Health Questionnaire-9 (depression severity)
carlyleSurveyCT_depression=res(0.51, n=carlyleSurveyCT_n, dig=20)
pEstimate_carlyleSurveyCT_depression=data.frame(pEstimate=NA, Ntot=carlyleSurveyCT_n)

# Predictor:  Mental Health Liability (Family History of Mental Illness)
carlyleSurveyCT_MHliability=res(0.09, n=carlyleSurveyCT_n, dig=20)
pEstimate_carlyleSurveyCT_MHliability=data.frame(pEstimate=NA, Ntot=carlyleSurveyCT_n)


# Predictor:  AddictionLiability (Family History of Drug Dependence)
carlyleSurveyCT_AddictionLiability=res(0.12, n=carlyleSurveyCT_n, dig=20)
pEstimate_carlyleSurveyCT_AddictionLiability=data.frame(pEstimate=NA, Ntot=carlyleSurveyCT_n)


# Predictor: gender
carlyleSurveyCT_gender=mes(9, 10, 5, 7, carlyleSurveyCT_n*0.4878893, carlyleSurveyCT_n*(1-0.4878893), dig=20)
pEstimate_carlyleSurveyCT_gender=data.frame(pEstimate=0.01, Ntot=carlyleSurveyCT_n)  



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Sainz-Cort (2021) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Sainz-Cort, Alberto, Daniel Jimenez-Garrido, Elena Muñoz-Marron, Raquel Viejo-Sobera, Joost Heeroma, and Jose Carlos Bouso. "Opposite Roles for Cannabidiol and δ-9-Tetrahydrocannabinol in Psychotomimetic Effects of Cannabis Extracts: A Naturalistic Controlled Study." Journal of Clinical Psychopharmacology 41, no. 5 (2021): 561-570.

bousoExpTHC_CBD_n=18 # Two participants were dropped because they did not show up at the experimental sessions.

# THC extract = 66.64% THC
# 100mg of THC extract administered, where THC extract comes with 66.64% THC

# Hallucinations (VAS)
extract_bousoExpTHC_CBD_VAS_hallucinations=d_meanDiff_dependent(m2=0.42,
                                                 m1=17.08,
                                                 sd2=1.77 ,
                                                 sd1=40.35,
                                                 n=bousoExpTHC_CBD_n,
                                                 r=r_withinSubj_between_measures)
pEstimate_bousoExpTHC_CBD_VAS_hallucinations=data.frame(pEstimate=NA, Ntot=bousoExpTHC_CBD_n) 


# Paranoia (VAS)
extract_bousoExpTHC_CBD_VAS_paranoia=d_meanDiff_dependent(m2=2.78,
                                                                m1=20.39,
                                                                sd2=10.23 ,
                                                                sd1=36.99,
                                                                n=bousoExpTHC_CBD_n,
                                                                r=r_withinSubj_between_measures)
pEstimate_bousoExpTHC_CBD_VAS_paranoia=data.frame(pEstimate=NA, Ntot=bousoExpTHC_CBD_n) 

# Paranoia (PSI)
extract_bousoExpTHC_CBD_PSI_paranoia=d_meanDiff_dependent(m2=0.56,
                                                          m1=1.89,
                                                          sd2=0.92 ,
                                                          sd1=1.78,
                                                          n=bousoExpTHC_CBD_n,
                                                          r=r_withinSubj_between_measures)
pEstimate_bousoExpTHC_CBD_PSI_paranoia=data.frame(pEstimate=NA, Ntot=bousoExpTHC_CBD_n) 


# Delusions (PSI)
extract_bousoExpTHC_CBD_PSI_delusions=d_meanDiff_dependent(m2=2.44,
                                                          m1=4.39,
                                                          sd2=3.47 ,
                                                          sd1=4.42,
                                                          n=bousoExpTHC_CBD_n,
                                                          r=r_withinSubj_between_measures)
pEstimate_bousoExpTHC_CBD_PSI_delusions=data.frame(pEstimate=NA, Ntot=bousoExpTHC_CBD_n) 



# Hallucinations (VAS) => CBD:THC versus THC
extract_bousoExpTHC_CBD_VAS_hallucinations_cbdAdmin=d_meanDiff_dependent(m2=0,
                                                                m1=17.08,
                                                                sd2=0 ,
                                                                sd1=40.35,
                                                                n=bousoExpTHC_CBD_n,
                                                                r=r_withinSubj_between_measures)
pEstimate_bousoExpTHC_CBD_VAS_hallucinations_cbdAdmin=data.frame(pEstimate=NA, Ntot=bousoExpTHC_CBD_n) 


# Paranoia (VAS) => CBD:THC versus THC
extract_bousoExpTHC_CBD_VAS_paranoia_cbdAdmin=d_meanDiff_dependent(m2=1.39,
                                                          m1=20.39,
                                                          sd2=3.89 ,
                                                          sd1=36.99,
                                                          n=bousoExpTHC_CBD_n,
                                                          r=r_withinSubj_between_measures)
pEstimate_bousoExpTHC_CBD_VAS_paranoia_cbdAdmin=data.frame(pEstimate=NA, Ntot=bousoExpTHC_CBD_n) 


# Paranoia (PSI) => CBD:THC versus THC
extract_bousoExpTHC_CBD_PSI_paranoia_cbdAdmin=d_meanDiff_dependent(m2=1.06,
                                                          m1=1.89,
                                                          sd2=1.30 ,
                                                          sd1=1.78,
                                                          n=bousoExpTHC_CBD_n,
                                                          r=r_withinSubj_between_measures)
pEstimate_bousoExpTHC_CBD_PSI_paranoia_cbdAdmin=data.frame(pEstimate=NA, Ntot=bousoExpTHC_CBD_n) 



# Delusions (PSI) => CBD:THC versus THC
extract_bousoExpTHC_CBD_PSI_delusions_cbdAdmin=d_meanDiff_dependent(m2=2.89,
                                                           m1=4.39,
                                                           sd2=3.89 ,
                                                           sd1=4.42,
                                                           n=bousoExpTHC_CBD_n,
                                                           r=r_withinSubj_between_measures)
pEstimate_bousoExpTHC_CBD_PSI_delusions_cbdAdmin=data.frame(pEstimate=NA, Ntot=bousoExpTHC_CBD_n) 



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bloomfield (2021) =========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Bloomfield, Michael AP, Katherine Petrilli, Rachel Lees, Chandni Hindocha, Katherine Beck, Ryan J. Turner, Ellis Chika Onwordi et al. "The effects of acute Δ9-tetrahydrocannabinol on striatal glutamatergic function: A proton magnetic resonance spectroscopy study." Biological Psychiatry: Cognitive Neuroscience and Neuroimaging 6, no. 6 (2021): 660-667.

bloomfieldGlutamateTHC_n=20
bloomfieldGlutamateTHC_nEff=n_effective_dependent(bloomfieldGlutamateTHC_n, r=r_withinSubj_between_timepoints)

# There was a main effect of drug (F1,18 = 9.557,p = .006,2 = 0.347)
bloomfieldGlutamateTHC_dose=fes(9.557, bloomfieldGlutamateTHC_nEff/2, bloomfieldGlutamateTHC_nEff/2, dig=20)
pEstimate_bloomfieldGlutamateTHC_dose=data.frame(pEstimate=0.006, Ntot=bloomfieldGlutamateTHC_n) 

# There were no correlations between striatal Glx/Cre measures and PSI values 150 minutes after drug administration (rs = 0.206, p = .48),
bloomfieldGlutamateTHC_Gl=res(0.206, n=bloomfieldGlutamateTHC_nEff,dig=20)
pEstimate_bloomfieldGlutamateTHC_Gl=data.frame(pEstimate=0.48, Ntot=bloomfieldGlutamateTHC_n) 



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Walsh (2021) =========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ref: Walsh, Jennifer H., Kathleen J. Maddison, Tim Rankin, Kevin Murray, Nigel McArdle, Melissa J. Ree, David R. Hillman, and Peter R. Eastwood. "Treating insomnia symptoms with medicinal cannabis: a randomized, crossover trial of the efficacy of a cannabinoid medicine compared with placebo." Sleep 44, no. 11 (2021): zsab149.
# ZTL-101 contained THC 20 mg/mL, CBN 2 mg/mL

walshInsomnia_2021_n=24
walshInsomnia_2021_hallucinations_perc=(1/24)*100
walshInsomnia_2021_hallucinationsChi=chiSquareCalc_repeated(percCan= walshInsomnia_2021_hallucinations_perc,
                                percPLB = 0,
                                nTot =  walshInsomnia_2021_n,
                                r=r_withinSubj_between_timepoints)

chiSquEstimate_dalzell1986_nabilone_hallucinations=Ntot_dalzell1986_nabilone_hallucinations=tableChi[2]
# Get cohen d
extractWalshInsomnia_2021_hallucinations=chies(walshInsomnia_2021_hallucinationsChi[1], walshInsomnia_2021_hallucinationsChi[2], dig=50)
pEstimate_walshInsomnia_2021_hallucinations=data.frame(pEstimate=NA, Ntot=walshInsomnia_2021_n)

# rate
WalshInsomnia_2021_hallucinationsDF=data.frame(event=1, n=walshInsomnia_2021_n)
WalshInsomnia_2021_hallucinationsControls=data.frame(event=0, n=23)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Halikas (1971) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Halikas, James A., Donald W. Goodwin, and Samuel B. Guze. "Marihuana effects: a survey of regular users." Jama 217, no. 5 (1971): 692-694.

regularCan_Halikas1971_n=100
regularCan_Halikas1971_hallucinationsDF=data.frame(event=27, n=regularCan_Halikas1971_n) # individuals reporting moderate or severe hallucinations


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Greywoode (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Greywoode, Ruby, Chinazo Cunningham, Maegan Hollins, and Olga Aroniadis. "Medical Cannabis Use Patterns and Adverse Effects in Inflammatory Bowel Disease." Journal of Clinical Gastroenterology (2022): 10-1097.
greywoodeIBD_n=236
greywoodeIBD_paranoiaDF=data.frame(event=8, n=greywoodeIBD_n) 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Stith (2023) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Stith, Sarah See, Xiaoxue Li, Franco Brockelman, Keenan Keeling, Branden Hall, and Jacob Miguel Vigil. "Understanding Feeling" High" and Its Role in Medical Cannabis Patient Outcomes." Frontiers in Pharmacology 14: 1380.
# other study using ReleafApp: until 2018 => therefore considered as independent samples
# 6/5/2016 and 3/11/2021
StithReleaf_n=1882 
# 3% reporting paranoid symptoms
StithReleaf_paranoiaDF=data.frame(event=round(1882*0.03,0), n=StithReleaf_n) 
StithReleaf_paranoiaCor=res(0.0727, n=StithReleaf_n, dig=4)
pEstimate_StithReleaf_paranoia=data.frame(pEstimate=NA, Ntot=StithReleaf_n) 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Aladeen (2023) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Aladeen, Traci S., Anna G. Mattle, Kory Zelen, Moustafa Mesha, Michelle M. Rainka, Tanya Geist, Bennett Myers, and Laszlo Mechtler. "Medical Cannabis in the Treatment of Parkinson’s Disease." Clinical neuropharmacology (2023): 10-1097.
AladeenPD_n=69
AladeenPD_psychosisDF=data.frame(event=2, n=AladeenPD_n) 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Arkell (2023) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Arkell, Thomas R., Luke A. Downey, Amie C. Hayley, and Sebastian Roth. "Assessment of medical cannabis and health-related quality of life." JAMA network open 6, no. 5 (2023): e2312522-e2312522.
ArkellMed2023_n=3148
ArkellMed2023_hallucinationsDF=data.frame(event=45, n=ArkellMed2023_n) 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Kliuk (2023) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Kliuk-Ben Bassat, Orit, Meir Schechter, Natalia Ashtamker, Ilan Yanuv, Aliza Rozenberg, Boaz Hirshberg, Ayelet Grupper, Nachum Vaisman, Silviu Brill, and Ofri Mosenzon. "Medical cannabis for pain management in patients undergoing chronic hemodialysis: randomized, double-blind, cross-over, feasibility study." Clinical Kidney Journal 16, no. 4 (2023): 701-710.
#  BOL-DP-o-04-WPE whole-plant extract (WPE), BOL-DP-o-04 cannabinoid extraction
# Serious AEs considered related to study drug included one episode of accidental overdose (WPE) leading to hallucinations
KliukWPE_n=9
KliukWPE_hallucinationsDF=data.frame(event=1, n=KliukWPE_n) 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Olsson (2023) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#REF: Olsson, F., Erridge, S., Tait, J., Holvey, C., Coomber, R., Beri, S., Hoare, J., Khan, S., Weatherall, M.W., Platt, M. and Rucker, J.J., 2023. An observational study of safety and clinical outcome measures across patient groups in the United Kingdom Medical Cannabis Registry. Expert Review of Clinical Pharmacology, 16(3), pp.257-266.
OlssonMCR_n=2833
OlssonMCR_paranoiaDF=data.frame(event=9, n=OlssonMCR_n) 
OlssonMCR_psychosisDF=data.frame(event=2, n=OlssonMCR_n) 
OlssonMCR_delusionsDF=data.frame(event=1, n=OlssonMCR_n) 
OlssonMCR_hallucinationsDF=data.frame(event=1, n=OlssonMCR_n) 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Lawn (2023) ============================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Lawn, W., Trinci, K., Mokrysz, C., Borissova, A., Ofori, S., Petrilli, K., Bloomfield, M., Haniff, Z.R., Hall, D., Fernandez‐Vinson, N. and Wang, S., 2023. The acute effects of cannabis with and without cannabidiol in adults and adolescents: A randomised, double‐blind, placebo‐controlled, crossover experiment. Addiction.

lawn2023Adolescents_n=24
lawn2023Adults_n=24
lawn2023Tot=lawn2023Adults_n+lawn2023Adolescents_n
#We administered three vaporised cannabis flower preparations: 
# ‘THC’ (8 mg THC for 75 kg person); 
# ‘THC + CBD’ (8 mg THC and 24 mg CBD for 75 kg person);
# ‘PLA’ (matched placebo).

# Effects of THC
extract_lawn2023AdolAdultsTHC_PSI=d_meanDiff_dependent(m2=13.646, 
                                                       m1=21.417,
                                                       sd2=12.036, 
                                                       sd1=12.551, 
                                                       n=lawn2023Adolescents_n+lawn2023Adults_n, 
                                                       r=r_withinSubj_between_measures)

# Effects of CBD
extract_lawn2023AdolAdultsTHC_CBD_PSI=d_meanDiff_dependent(m1=24.438, 
                                                       m2=21.417,
                                                       sd1=11.355, 
                                                       sd2=12.551, 
                                                       n=lawn2023Adolescents_n+lawn2023Adults_n, 
                                                       r=r_withinSubj_between_measures)


# Age x THC interaction
# there was no significant interaction between drug and age group (F[2,92] = 0.932; P = 0.398; η2p = 0.020). 
extract_lawn2023AdolAdultsTHCxAge=fes(0.932, lawn2023Adolescents_n, lawn2023Adults_n, dig=20)
pEstimate_lawn2023AdolAdultsTHCxAge=data.frame(pEstimate=0.398, Ntot=lawn2023Adolescents_n+lawn2023Adults_n) 


# PANSS positive (drug effect)
extract_lawn2023AdolAdultsTHC_main_PANSS=d_meanDiff_dependent(m1=10.06, 
                                                             m2=7.40,
                                                             sd1=3.28, 
                                                             sd2=1.01, 
                                                             n=lawn2023Adolescents_n+lawn2023Adults_n, 
                                                             r=r_withinSubj_between_measures)



# PANSS positive (CBD)
extract_lawn2023AdolAdultsTHC_CBD_PANSS=d_meanDiff_dependent(m1=10.81, 
                                                           m2=10.06,
                                                           sd1=2.63, 
                                                           sd2=3.28, 
                                                           n=lawn2023Adolescents_n+lawn2023Adults_n, 
                                                           r=r_withinSubj_between_measures)

# Paranois (drug effect)
extract_lawn2023AdolAdultsTHC_main_Paranoia=d_meanDiff_dependent(m1=0.81, 
                                                                m2=0.48,
                                                                sd1=1.57, 
                                                                sd2=1.05, 
                                                                n=lawn2023Adolescents_n+lawn2023Adults_n, 
                                                                r=r_withinSubj_between_measures)



# Paranois (CBD effect)
extract_lawn2023AdolAdultsTHC_CBD_Paranoia=d_meanDiff_dependent(m1=1.10, 
                                                              m2=0.81,
                                                              sd1=1.69, 
                                                              sd2=1.57, 
                                                              n=lawn2023Adolescents_n+lawn2023Adults_n, 
                                                              r=r_withinSubj_between_measures)



# Delusions (drug effect)
extract_lawn2023AdolAdultsTHC_main_Delusions=d_meanDiff_dependent(m1=1.92, 
                                                                 m2=1.52,
                                                                 sd1=2.33, 
                                                                 sd2=2.70, 
                                                                 n=lawn2023Adolescents_n+lawn2023Adults_n, 
                                                                 r=r_withinSubj_between_measures)

# Delusions (CBD effect)
extract_lawn2023AdolAdultsTHC_CBD_Delusions=d_meanDiff_dependent(m1=2.19, 
                                                                  m2=1.92,
                                                                  sd1=2.47, 
                                                                  sd2=2.33, 
                                                                  n=lawn2023Adolescents_n+lawn2023Adults_n, 
                                                                  r=r_withinSubj_between_measures)

pEstimate_lawn2023AdolAdults=data.frame(pEstimate=NA, Ntot=lawn2023Adolescents_n+lawn2023Adults_n) 



# rates
# PANSS positive
lawn2023AdolAdultsTHC_pseDF=data.frame(event=27, n=lawn2023Tot)


# CAPS =controls
# PANSS positive
lawn2023AdolAdultsTHC_pseControlDF=data.frame(event=21, n=lawn2023Tot)






# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Schnell (2023) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Schnell, Thomas, Christina-Marie Grömm, and Nils Klöckner. "Predictive impact of different acute cannabis intoxication effects with regard to abstinence motivation and cessation of use." Scientific reports 13, no. 1 (2023): 709.
# Get cohen d
#  (1) “psychotic-like experiences/loss of reality” (feeling threatened, hearing voices, losing ones sense of reality, feeling like going crazy); (2) “paranoia/dysphoria” (feeling paranoid, anxious, nervous, depressed); 
Schnell2023_n=441
# Get cohen d

#  Predictor:  (3) “taking steps” (indicator for the concrete efforts to remain abstinent)
extract_Schnell2023_takingSteps_paranoia=beta_to_d_independent(beta=0.422, se=0.071, N1=Schnell2023_n/2, N2=Schnell2023_n/2) # 
pEstimate_Schnell2023_takingSteps_paranoia=data.frame(pEstimate=NA, Ntot=Schnell2023_n) 

#  # Predictor: “recognition” (indicator for awareness of problem)
extract_Schnell2023_recognition_paranoia=beta_to_d_independent(beta=0.311, se=0.075, N1=Schnell2023_n/2, N2=Schnell2023_n/2) # 
pEstimate_Schnell2023_recognition_paranoia=data.frame(pEstimate=NA, Ntot=Schnell2023_n) 

extract_Schnell2023_recognition_PLE=beta_to_d_independent(beta=0.283, se=0.098, N1=Schnell2023_n/2, N2=Schnell2023_n/2) # 
pEstimate_Schnell2023_recognition_PLE=data.frame(pEstimate=0.004, Ntot=Schnell2023_n) 

# Predictor:  (1) “ambivalence” (indicator for the process of balancing two options—namely, to remain or become abstinent versus continued substance use),
extract_Schnell2023_ambivalence_paranoia=beta_to_d_independent(beta=0.239, se=0.077, N1=Schnell2023_n/2, N2=Schnell2023_n/2) # 
pEstimate_Schnell2023_ambivalence_paranoia=data.frame(pEstimate=0.002, Ntot=Schnell2023_n) 


# intention to quit
extract_Schnell2023_quit_paranoia=mes(2.31, 1.92, 0.65, 0.66, 186, 150, dig=20)
pEstimate_Schnell2023_quit_paranoia=data.frame(pEstimate=NA, Ntot=extract_Schnell2023_quit_paranoia$N.total) # p < 0.001

extract_Schnell2023_quit_PLE=mes(m.1=1.51, m.2=1.38, sd.1=0.53, sd.2=0.52, 186, 150, dig=20)
pEstimate_Schnell2023_quit_paranoia=data.frame(pEstimate=NA, Ntot=extract_Schnell2023_quit_paranoia$N.total) # p < 0.001


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Englund (2023) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Englund, Amir, Dominic Oliver, Edward Chesney, Lucy Chester, Jack Wilson, Simina Sovi, Andrea De Micheli et al. "Does cannabidiol make cannabis safer? A randomised, double-blind, cross-over trial of cannabis with four different CBD: THC ratios." Neuropsychopharmacology 48, no. 6 (2023): 869-876.

# Each cannabis dose consisted of 10 mg of THC (two standard THC units [16]) and either 0 mg, 10 mg, 20 mg, or 30 mg of CBD. 
# here was an initial baseline visit followed by four drug administration visits, in which participants inhaled vaporised cannabis containing 
# 1) 0 mg CBD and 10 mg THC (0:1 CBD:THC) 
# 2) 10 mg CBD and 10 mg THC (1:1), 
# 3) 20 mg CBD and 10 mg THC (2:1)
# 4) 30 mg CBD and 10 mg THC (3:1) CBD

# effect of cannabis (THC) not included as not placeno controlled

Englund2023CBD_n=46

# effect of CBD (PANSS)
extract_lEnglund2023CBD_PANSS=d_meanDiff_dependent(m2=8.96, # 0 mg CBD and 10 mg THC (0:1 CBD:THC) 
                                                   m1=8.70, # 30 mg CBD and 10 mg THC (3:1) CBD
                                                   sd2=0.317*sqrt(Englund2023CBD_n), 
                                                   sd1=0.317*sqrt(Englund2023CBD_n), 
                                                   n=Englund2023CBD_n, 
                                                   r=r_withinSubj_between_measures)
pEstimate_lEnglund2023CBD_PANSS=data.frame(pEstimate=NA, Ntot=Englund2023CBD_n) 

# effect of CBD (SSPS) -State social paranoia scale  
extract_lEnglund2023CBD_SSPS=d_meanDiff_dependent(m2=10.2, 
                                                   m1=10.1,
                                                   sd2=0.116*sqrt(Englund2023CBD_n), 
                                                   sd1=0.116*sqrt(Englund2023CBD_n), 
                                                   n=Englund2023CBD_n, 
                                                   r=r_withinSubj_between_measures)
pEstimate_lEnglund2023CBD_SSPS=data.frame(pEstimate=NA, Ntot=Englund2023CBD_n) 

# effect of CBD (CAPE)
extract_lEnglund2023CBD_CAPE=d_meanDiff_dependent(m2=4.5, 
                                                  m1=5.17,
                                                  sd2=0.674*sqrt(Englund2023CBD_n), 
                                                  sd1=0.669*sqrt(Englund2023CBD_n), 
                                                  n=Englund2023CBD_n, 
                                                  r=r_withinSubj_between_measures)
pEstimate_lEnglund2023CBD_CAPE=data.frame(pEstimate=NA, Ntot=Englund2023CBD_n) 

# effect of CBD (PSI)
extract_lEnglund2023CBD_PSI=d_meanDiff_dependent(m2=21.91, 
                                                  m1=24.1,
                                                  sd2=1.9*sqrt(Englund2023CBD_n), 
                                                  sd1=1.88*sqrt(Englund2023CBD_n), 
                                                  n=Englund2023CBD_n, 
                                                  r=r_withinSubj_between_measures)
pEstimate_lEnglund2023CBD_PSI=data.frame(pEstimate=NA, Ntot=Englund2023CBD_n) 



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Johnson (2023) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF Johnson, Emma C., Sarah MC Colbert, Paul W. Jeffries, Rebecca Tillman, Tim B. Bigdeli, Nicole R. Karcher, Grace Chan et al. "Associations between cannabis use, polygenic liability for schizophrenia, and cannabis-related experiences in a sample of cannabis users." Schizophrenia bulletin 49, no. 3 (2023): 778-787.

johnson2023total_n=4832
johnson2023casesHallucinations_n=564
johnson2023casesParanoia_n=846
johnson2023casesAny_n=2210


# Any PLE - age of onset of cannabis use
extract_johnson2023AnyPLE_age_onset=beta_to_d_independent(beta=-0.320, se=0.038, N1=johnson2023casesAny_n, N2=johnson2023total_n-johnson2023casesAny_n) # p-value matches study p-value
pEstimate_johnson2023AnyPLE_age_onset=data.frame(pEstimate=2.20e-17, Ntot=johnson2023total_n)
# Hallucinations - age of onset of cannabis use
extract_johnson2023Hallucinations_age_onset=beta_to_d_independent(beta=-0.205, se=0.055, N1=johnson2023casesHallucinations_n, N2=johnson2023total_n-johnson2023casesHallucinations_n) # p-value matches study p-value
pEstimate_johnson2023Hallucinations_age_onset=data.frame(pEstimate=2.22e-4, Ntot=johnson2023total_n)
# Paranoia - age of onset of cannabis use
extract_johnson2023Paranoia_age_onset=beta_to_d_independent(beta=-0.270, se=0.055, N1=johnson2023casesParanoia_n, N2=johnson2023total_n-johnson2023casesParanoia_n) # p-value matches study p-value
pEstimate_johnson2023Paranoia_age_onset=data.frame(pEstimate=3.26e-8, Ntot=johnson2023total_n)


# Any PLE - duration of daily use
extract_johnson2023AnyPLE_duration_daily=beta_to_d_independent(beta=0.215, se=0.035, N1=johnson2023casesAny_n, N2=johnson2023total_n-johnson2023casesAny_n) # p-value matches study p-value
pEstimate_johnson2023AnyPLE_duration_daily=data.frame(pEstimate=1.25e-9, Ntot=johnson2023total_n)
# Hallucinations - duration of daily use
extract_johnson2023Hallucinations_duration_daily=beta_to_d_independent(beta=-0.001, se=0.050, N1=johnson2023casesHallucinations_n, N2=johnson2023total_n-johnson2023casesHallucinations_n) # p-value matches study p-value
pEstimate_johnson2023Hallucinations_duration_daily=data.frame(pEstimate=0.985, Ntot=johnson2023total_n)
# Paranoia - duration of daily use
extract_johnson2023Paranoia_duration_daily=beta_to_d_independent(beta=0.099, se=0.051, N1=johnson2023casesParanoia_n, N2=johnson2023total_n-johnson2023casesParanoia_n) # p-value matches study p-value
pEstimate_johnson2023Paranoia_duration_daily=data.frame(pEstimate=0.050, Ntot=johnson2023total_n)


# Any PLE - cannabis use disorder
extract_johnson2023AnyPLE_CUD=beta_to_d_independent(beta=3.226, se=0.124, N1=johnson2023casesAny_n, N2=johnson2023total_n-johnson2023casesAny_n) # p-value matches study p-value
pEstimate_johnson2023AnyPLE_CUD=data.frame(pEstimate=4.93e-149, Ntot=johnson2023total_n)
# Hallucinations - cannabis use disorder
extract_johnson2023Hallucinations_CUD=beta_to_d_independent(beta=2.125, se=0.202, N1=johnson2023casesHallucinations_n, N2=johnson2023total_n-johnson2023casesHallucinations_n) # p-value matches study p-value
pEstimate_johnson2023Hallucinations_CUD=data.frame(pEstimate=8.34e-26, Ntot=johnson2023total_n)
# Paranoia - cannabis use disorder
extract_johnson2023Paranoia_CUD=beta_to_d_independent(beta=3.086, se=0.239, N1=johnson2023casesParanoia_n, N2=johnson2023total_n-johnson2023casesParanoia_n) # p-value matches study p-value
pEstimate_johnson2023Paranoia_CUD=data.frame(pEstimate=2.79e-38, Ntot=johnson2023total_n)


# Any PLE - schizophrenia PRS
extract_johnson2023AnyPLE_schzPRS=beta_to_d_independent(beta=0.135, se=0.041, N1=johnson2023casesAny_n, N2=johnson2023total_n-johnson2023casesAny_n) # p-value matches study p-value
pEstimate_johnson2023AnyPLE_schzPRS=data.frame(pEstimate=0.001, Ntot=johnson2023total_n)
# Hallucinations - cannabis use disorder
extract_johnson2023Hallucinations_schzPRS=beta_to_d_independent(beta=0.039, se=0.054, N1=johnson2023casesHallucinations_n, N2=johnson2023total_n-johnson2023casesHallucinations_n) # p-value matches study p-value
pEstimate_johnson2023Hallucinations_schzPRS=data.frame(pEstimate=0.472, Ntot=johnson2023total_n)
# Paranoia - cannabis use disorder
extract_johnson2023Paranoia_schzPRS=beta_to_d_independent(beta=0.163, se=0.048, N1=johnson2023casesParanoia_n, N2=johnson2023total_n-johnson2023casesParanoia_n) # p-value matches study p-value
pEstimate_johnson2023Paranoia_schzPRS=data.frame(pEstimate=6.77e-4, Ntot=johnson2023total_n)


### RATES 
rates_johnson2023AnyPLE=data.frame(event=johnson2023casesAny_n, n=johnson2023total_n) 
rates_johnson2023Hallucinations=data.frame(event=johnson2023casesHallucinations_n, n=johnson2023total_n) 
rates_johnson2023Paranoia=data.frame(event=johnson2023casesParanoia_n, n=johnson2023total_n) 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Schubert (2023) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Schubert, Elise A., Johannes C. Alffenaar, Masego T. Johnstone, John W. Barlow, and Nial J. Wheate. "Medicinal cannabis for patients with chronic non-cancer pain: analysis of safety and concomitant medications." International Journal of Pharmacy Practice 31, no. 1 (2023): 70-79.

SchubertMedCan2023THCproduct_n=123 # n using med cannabis containing THC

rates_SchubertMedCan2023THCproductParanoia=data.frame(event=1, n=SchubertMedCan2023THCproduct_n) 
rates_SchubertMedCan2023THCproductHallucinations=data.frame(event=2, n=SchubertMedCan2023THCproduct_n) 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Horsted (2023) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Horsted, Tina, Karoline Lichon Hesthaven, and Peter Derek Christian Leutscher. "Safety and effectiveness of cannabinoids to Danish patients with treatment refractory chronic pain—a retrospective observational real‐world study." European Journal of Pain 27, no. 2 (2023): 234-247.
Horsted2023_THCgroup_n=284+47
rates_Horsted2023_THCgroupHallucination=data.frame(event=1, n=Horsted2023_THCgroup_n) 



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bouassa (2023) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Mboumba Bouassa, R.S., Needham, J., Nohynek, D., Singer, J., Lee, T., Bobeuf, F., Samarani, S., Del Balso, L., Paisible, N., Vertzagias, C. and Sebastiani, G., 2022. Safety and Tolerability of Oral Cannabinoids in People Living with HIV on Long-Term ART: A Randomized, Open-Label, Interventional Pilot Clinical Trial (CTNPT 028). Biomedicines, 10(12), p.3168.
BouassaTHC_2022_n=5
rates_BouassaTHC_2022_paranoia=data.frame(event=1, n=BouassaTHC_2022_n) 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Clarke (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Clarke, Stephen, Belinda E. Butcher, Andrew J. McLachlan, Jeremy D. Henson, David Rutolo, Sean Hall, and Luis Vitetta. "Pilot clinical and pharmacokinetic study of Δ9-Tetrahydrocannabinol (THC)/Cannabidiol (CBD) nanoparticle oro-buccal spray in patients with advanced cancer experiencing uncontrolled pain." Plos one 17, no. 10 (2022): e0270543.
Clarke_cancer2022_n=25
rates_Clarke_cancer2022_hallucination=data.frame(event=3, n=Clarke_cancer2022_n)  # 1 mild, 2 moderate


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Kimless (2022) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Kimless, Debra, Matthew Caloura, Virginia Markos, Jennie Ryan, Sally Abbonizio, and Sharon Janicki. "An Observational Cross-Sectional Survey Exploring the Indications for and Responses to Medical Marijuana Use in Certified Patients in Pennsylvania." Journal of Primary Care & Community Health 13 (2022): 21501319221129734.
KimlessMedCan2022_n=202
rates_KimlessMedCan2022_paranoia=data.frame(event=4, n=KimlessMedCan2022_n) 




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Melges (1974) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
melges1974_n=6
rates_melges1974_20mgTHC_fast_paranoia=data.frame(event=4, n=melges1974_n) # During fast hashish, four of the six subjects described as 'paranoid'
  


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Bloomfield (2012) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Bloomfield, Michael AP, Katherine Petrilli, Rachel Lees, Chandni Hindocha, Katherine Beck, Ryan J. Turner, Ellis Chika Onwordi et al. "The effects of acute Δ9-tetrahydrocannabinol on striatal glutamatergic function: A proton magnetic resonance spectroscopy study." Biological Psychiatry: Cognitive Neuroscience and Neuroimaging 6, no. 6 (2021): 660-667.





# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Barkus (2011) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Barkus E, Morrison PD, Vuletic D, et al. Does intravenous Δ9-tetrahydrocannabinol increase dopamine release? A SPET study. J Psychopharmacol 2011; 25: 1462–68.


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== EXCLUDED ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Bossong, Matthijs G., Bart Nm Van Berckel, Ronald Boellaard, Lineke Zuurman, Robert C. Schuit, Albert D. Windhorst, Joop MA Van Gerven, Nick F. Ramsey, Adriaan A. Lammertsma, and René S. Kahn. "Δ9-tetrahydrocannabinol induces dopamine release in the human striatum." Neuropsychopharmacology 34, no. 3 (2009): 759-766.
# ==> REASON: Already included in mega analysis (Kleinloog, Daniël, Frits Roozen, Willem De Winter, Jan Freijer, and Joop Van Gerven. "Profiling the subjective effects of Δ9‐tetrahydrocannabinol using visual analogue scales." International journal of methods in psychiatric research 23, no. 2 (2014): 245-256.)

# Bossong, M.G.; Jansma, J.M.; van Hell, H.H.; Jager, G.; Kahn, R.S.; Ramsey, N.F. Default Mode Network in the Effects of ∆9-Tetrahydrocannabinol (THC) on Human Executive Function. PLoS ONE 2013, 8, e70074
# ==> REASON: No CAPS reported

# Klumpers, Linda E., Tim L. Beumer, Johan GC van Hasselt, Astrid Lipplaa, Lennard B. Karger, H. Daniël Kleinloog, Jan I. Freijer, Marieke L. de Kam, and Joop MA van Gerven. "Novel Δ9‐tetrahydrocannabinol formulation Namisol® has beneficial pharmacokinetics and promising pharmacodynamic effects." British journal of clinical pharmacology 74, no. 1 (2012): 42-53.
# ==> REASON: No CAPS reported + Already included in mega analysis (Kleinloog, Daniël, Frits Roozen, Willem De Winter, Jan Freijer, and Joop Van Gerven. "Profiling the subjective effects of Δ9‐tetrahydrocannabinol using visual analogue scales." International journal of methods in psychiatric research 23, no. 2 (2014): 245-256.)

# Klumpers, Linda E., David M. Cole, Najmeh Khalili-Mahani, Roelof P. Soeter, Erik T. te Beek, Serge ARB Rombouts, and Joop MA van Gerven. "Manipulating brain connectivity with δ9-tetrahydrocannabinol: a pharmacological resting state FMRI study." Neuroimage 63, no. 3 (2012): 1701-1711.
# ==> REASON: No CAPS reported + Already included in mega analysis (Kleinloog, Daniël, Frits Roozen, Willem De Winter, Jan Freijer, and Joop Van Gerven. "Profiling the subjective effects of Δ9‐tetrahydrocannabinol using visual analogue scales." International journal of methods in psychiatric research 23, no. 2 (2014): 245-256.)

# Klumpers, Linda E., Christine Roy, Geraldine Ferron, Sandrine Turpault, Franck Poitiers, Jean‐Louis Pinquier, Johan GC van Hasselt, Lineke Zuurman, Frank AS Erwich, and Joop MA van Gerven. "Surinabant, a selective cannabinoid receptor type 1 antagonist, inhibits Δ9‐tetrahydrocannabinol‐induced central nervous system and heart rate effects in humans." British journal of clinical pharmacology 76, no. 1 (2013): 65-77.
# ==> REASON: No CAPS reported + Already included in mega analysis (Kleinloog, Daniël, Frits Roozen, Willem De Winter, Jan Freijer, and Joop Van Gerven. "Profiling the subjective effects of Δ9‐tetrahydrocannabinol using visual analogue scales." International journal of methods in psychiatric research 23, no. 2 (2014): 245-256.)


# Klumpers, Linda E., Marianne Fridberg, Marieke L. de Kam, Paul B. Little, Niels Ole Jensen, Hendrik D. Kleinloog, Christian E. Elling, and Joop MA van Gerven. "Peripheral selectivity of the novel cannabinoid receptor antagonist TM 38837 in healthy subjects." British Journal of Clinical Pharmacology 76, no. 6 (2013): 846-857.
# ==> REASON: No CAPS reported + Already included in mega analysis (Kleinloog, Daniël, Frits Roozen, Willem De Winter, Jan Freijer, and Joop Van Gerven. "Profiling the subjective effects of Δ9‐tetrahydrocannabinol using visual analogue scales." International journal of methods in psychiatric research 23, no. 2 (2014): 245-256.)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Hjorthoj (2020) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF:Annual incidence of cannabis-induced psychosis, other substance-induced psychoses and dually diagnosed schizophrenia and cannabis use disorder in Denmark from 1994 to 2016.



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Morrison (2010) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REF: Morrison PD, Murray RM and Kapur S (2010) In healthy subjects Canna- bidiol inhibits delta-9-tetrahydrocannabinol induced acute psychosis and tachycardia via a non-pharmacokinetic mechanism. J Psycho- pharmacol 24: A61.
# Previously we reported preliminary findings that pre-treatment with intravenous (IV) CBD (5 mg) inhibited IV THC (1.25 mg) evoked positive psychotic symptoms, as measured by the Positive & Negative Syndrome Scale (PANSS), although the small sample size (crossover, n=6) prevents definitive conclusions (Morrison et al., 2010).




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ========================== Dittrich (1975) ===========================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Dittrich,A.,Bickel,P.,Zimmer,D.:Effektevon(--)A9-trans-Tetrahydrocannabin(ozxl9-THC) auf Psychotizismus-TestsE.ine UntersuchungdesEysenckschenDrogenpostulatetsiber Be- ziehungen zwischen Halluzinogenwirkungund Psychotizismus.Psychopharmacologia (Berl.) 40, 351--358 (1975)


#Tunbridge EM, Dunn G, Murray RM, Evans N, Lister R, Stumpenhorst K, Harrison PJ, Morrison PD, Freeman D (2015) Genetic modera- tion of the effects of cannabis: catechol-O-methyltransferase (COMT) affects the impact of Δ9-tetrahydrocannabinol (THC) on working memory performance but not on the occurrence of psychot- ic experiences. J Psychopharmacol 29:1146–1151
# In contrast, the effect of THC on psychotic experiences, measured using the Community Assessment of Psychic Experiences (CAPE) positive dimension, was unaffected by COMT genotype.




#OTHER TO INCLUDE




# Additionally, there may be a role of terpenoids such as limonene, myrcene, α-pinene and linalool [56] in moderating the effects of THC.
# 56. Russo E. B. Taming THC: potential cannabis synergy and phytocannabinoid-terpenoid entourage effects. Br J Pharmacol 2011; 163: 1344–64.


