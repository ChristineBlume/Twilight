##############################################################################
## This Code evaluates the screening questionnaires used for screening of
## participants in the Twilight study
##############################################################################


## --- set here the path to the data (p) and the name of the file you have downloaded (d)
p <- "C:/Users/chris/switchdrive/Twilight-Studie/Screening/REDCap_Exports"
d <- "BLUMETwilightScreeni_DATA_2021-10-18_2143.csv"

setwd(p)

#----- check if pacman is installed - if not install it
if(!require(pacman)) install.packages("pacman")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(stringr)) install.packages("stringr")

#----- use pacman function p_load to check all packages that you are using in this script
pacman::p_load(ggplot2, stringr)

#----- load files
data_all <- read.csv(paste(p,d, sep = "/"), header = T, stringsAsFactors = F, sep = ";") ## ignore warning messages

# remove incomplete rows, i.e. rows with missing data (colour vision test is the last test, if it is not complete,
# the screening is not complete)
pb_na <- c(which(data_all$color_blind_test_complete != 2)) # 2 == complete
data <- data_all[-c(pb_na),]

# data reduction
pb_info <- data[,c(3,9,12:16,18,21, 24:27, 30, 31, 34:48)]
psqi <- data[,c(50:75)]
mctq <- data[,c(77:114)]
bsi <- data[,c(116:168)]
colourvis <- data[,c(170:181)]


### -----------------------------------------------------------------------------------------------------------------------------
### BMI calculation
pb_info$BMI <- round((pb_info$vp_weight/(pb_info$vp_height/100)^2), digits = 1)


### -----------------------------------------------------------------------------------------------------------------------------
### ---- Calculate components of the PSQI and determine sum score
#component 1 (Sleep Quality)
# no calculation necessary, corresponds to psqi$psqi_sleepqual  

#component 2 (Sleep Latency)
psqi$psqi_sleeptime[psqi$psqi_sleeptime<=15] <- 0
psqi$psqi_sleeptime[psqi$psqi_sleeptime>=16 & psqi$psqi_sleeptime<=30] <- 1
psqi$psqi_sleeptime[psqi$psqi_sleeptime>31 & psqi$psqi_sleeptime<=60] <- 2
psqi$psqi_sleeptime[psqi$psqi_sleeptime>60] <- 3

psqi$psqi_sleeplatency <- psqi$psqi_sleeptime+psqi$psqi_notfallasleep 

#component 3 (Sleep Duration)
psqi_sleephrs <- as.numeric(unlist(str_split(psqi$psqi_sleephrs, pattern = ":")))
psq_sleephrs_hrs <- psqi_sleephrs[seq(1, length(psqi_sleephrs),2)]
psq_sleephrs_mins <- psqi_sleephrs[seq(2, length(psqi_sleephrs),2)]
psqi$psqi_sleephrs <- round(as.numeric(psq_sleephrs_hrs+psq_sleephrs_mins/60), digits = 1)
psqi$psqi_sleephrs[psqi$psqi_sleephrs<5] <- 3
psqi$psqi_sleephrs[psqi$psqi_sleephrs>=5 & psqi$psqi_sleephrs<=6] <- 2
psqi$psqi_sleephrs[psqi$psqi_sleephrs>6 & psqi$psqi_sleephrs<=7] <- 1
psqi$psqi_sleephrs[psqi$psqi_sleephrs>7] <- 0

psqi$psqi_sleepduration <- psqi$psqi_sleephrs

#component 4 (Sleep Efficiency)
psqi_sleepdur <- round(as.numeric(psq_sleephrs_hrs+psq_sleephrs_mins/60), digits = 1)
psqi$psqi_bedtime[psqi$psqi_bedtime=="00:00"] <- "23:59"
bedtime <- as.POSIXct(psqi$psqi_bedtime, format = "%H:%M")
getuptime <- as.POSIXct(psqi$psqi_getuptime, format = "%H:%M")+24*60*60
tib <- round(as.numeric(getuptime-bedtime), digits = 2)
rm(bedtime, getuptime)

psqi$psqi_SEFF = (psqi_sleepdur/tib)*100
psqi$psqi_SEFF[psqi$psqi_SEFF<65] <- 3
psqi$psqi_SEFF[psqi$psqi_SEFF>=65 & psqi$psqi_SEFF<=74] <- 2
psqi$psqi_SEFF[psqi$psqi_SEFF>=75 & psqi$psqi_SEFF<=84] <- 1
psqi$psqi_SEFF[psqi$psqi_SEFF>=85] <- 0

#component 5 (Sleep disorders)
psqi$sleepdis <- psqi$psqi_earlyawake+psqi$psqi_wc+psqi$psqi_breath+psqi$psqi_snore+psqi$psqi_cold+psqi$psqi_warm+psqi$psqi_dream+psqi$psqi_pain+psqi$psqi_other
psqi$sleepdis[psqi$sleepdis>1 & psqi$sleepdis<=9] <- 1
psqi$sleepdis[psqi$sleepdis>9 & psqi$sleepdis<=18] <- 2
psqi$sleepdis[psqi$sleepdis>18] <- 3

#component 6 (sleep meds)
# psqi_sleepmed

#component 7 (day sleepiness)
psqi$psqi_sleepinessday <- psqi$psqi_staywake +psqi$psqi_dailytasks
psqi$psqi_sleepinessday[psqi$psqi_sleepinessday>=1 & psqi$psqi_sleepinessday<=2] <- 1
psqi$psqi_sleepinessday[psqi$psqi_sleepinessday>=3 & psqi$psqi_sleepinessday<=4] <- 2
psqi$psqi_sleepinessday[psqi$psqi_sleepinessday>5] <- 3

# PSQI sum score
pb_info$psqi_sum <- psqi$psqi_sleepqual + psqi$psqi_sleeplatency + psqi$psqi_sleepduration + psqi$psqi_SEFF + psqi$sleepdis + psqi$psqi_sleepmed + psqi$psqi_sleepinessday

rm(psqi_sleephrs, psq_sleephrs_hrs, psq_sleephrs_mins, psqi_sleepdur, tib)

### -----------------------------------------------------------------------------------------------------------------------------
### ---- BSI (calculate subscores and global scores)
bsi$gender <- data$vp_gender

bsi_somatisierung <- rowMeans(data.frame(bsi$bsi_q2, bsi$bsi_q7, bsi$bsi_q23, bsi$bsi_q29, bsi$bsi_q30, bsi$bsi_q33, bsi$bsi_q37))
bsi_somatisierung2 <- rowSums(data.frame(bsi$bsi_q2>0, bsi$bsi_q7>0, bsi$bsi_q23>0, bsi$bsi_q29>0, bsi$bsi_q30>0, bsi$bsi_q33>0, bsi$bsi_q37>0))
bsi_somatisierung3 <- rowSums(data.frame(bsi$bsi_q2, bsi$bsi_q7, bsi$bsi_q23, bsi$bsi_q29, bsi$bsi_q30, bsi$bsi_q33, bsi$bsi_q37))

bsi_zwanghaftigkeit <- rowMeans(data.frame(bsi$bsi_q5, bsi$bsi_q15, bsi$bsi_q26, bsi$bsi_q27, bsi$bsi_q32, bsi$bsi_q36))
bsi_zwanghaftigkeit2 <- rowSums(data.frame(bsi$bsi_q5>0, bsi$bsi_q15>0, bsi$bsi_q26>0, bsi$bsi_q27>0, bsi$bsi_q32>0, bsi$bsi_q36>0))
bsi_zwanghaftigkeit3 <- rowSums(data.frame(bsi$bsi_q5, bsi$bsi_q15, bsi$bsi_q26, bsi$bsi_q27, bsi$bsi_q32, bsi$bsi_q36))

bsi_unsicherheit <- rowMeans(data.frame(bsi$bsi_q20, bsi$bsi_q21, bsi$bsi_q22, bsi$bsi_q42))
bsi_unsicherheit2 <- rowSums(data.frame(bsi$bsi_q20>0, bsi$bsi_q21>0, bsi$bsi_q22>0, bsi$bsi_q42>0))
bsi_unsicherheit3 <- rowSums(data.frame(bsi$bsi_q20, bsi$bsi_q21, bsi$bsi_q22, bsi$bsi_q42))

bsi_depression <- rowMeans(data.frame(bsi$bsi_q9, bsi$bsi_q16, bsi$bsi_q17, bsi$bsi_q18, bsi$bsi_q35, bsi$bsi_q50))
bsi_depression2 <- rowSums(data.frame(bsi$bsi_q9>0, bsi$bsi_q16>0, bsi$bsi_q17>0, bsi$bsi_q18>0, bsi$bsi_q35>0, bsi$bsi_q50>0))
bsi_depression3 <- rowSums(data.frame(bsi$bsi_q9, bsi$bsi_q16, bsi$bsi_q17, bsi$bsi_q18, bsi$bsi_q35, bsi$bsi_q50))

bsi_angst <- rowMeans(data.frame(bsi$bsi_q1, bsi$bsi_q12, bsi$bsi_q19, bsi$bsi_q38, bsi$bsi_q45, bsi$bsi_q49))
bsi_angst2 <- rowSums(data.frame(bsi$bsi_q1>0, bsi$bsi_q12>0, bsi$bsi_q19>0, bsi$bsi_q38>0, bsi$bsi_q45>0, bsi$bsi_q49>0))
bsi_angst3 <- rowSums(data.frame(bsi$bsi_q1, bsi$bsi_q12, bsi$bsi_q19, bsi$bsi_q38, bsi$bsi_q45, bsi$bsi_q49))

bsi_aggression <- rowMeans(data.frame(bsi$bsi_q6, bsi$bsi_q13, bsi$bsi_q40, bsi$bsi_q41, bsi$bsi_q46))
bsi_aggression2 <- rowSums(data.frame(bsi$bsi_q6>0, bsi$bsi_q13>0, bsi$bsi_q40>0, bsi$bsi_q41>0, bsi$bsi_q46>0))
bsi_aggression3 <- rowSums(data.frame(bsi$bsi_q6, bsi$bsi_q13, bsi$bsi_q40, bsi$bsi_q41, bsi$bsi_q46))

bsi_phobie <- rowMeans(data.frame(bsi$bsi_q8, bsi$bsi_q28, bsi$bsi_q31, bsi$bsi_q43, bsi$bsi_q47))
bsi_phobie2 <- rowSums(data.frame(bsi$bsi_q8>0, bsi$bsi_q28>0, bsi$bsi_q31>0, bsi$bsi_q43>0, bsi$bsi_q47>0))
bsi_phobie3 <- rowSums(data.frame(bsi$bsi_q8, bsi$bsi_q28, bsi$bsi_q31, bsi$bsi_q43, bsi$bsi_q47))

bsi_paranoia <- rowMeans(data.frame(bsi$bsi_q4, bsi$bsi_q10, bsi$bsi_q24, bsi$bsi_q48, bsi$bsi_q51))
bsi_paranoia2 <- rowSums(data.frame(bsi$bsi_q4>0, bsi$bsi_q10>0, bsi$bsi_q24>0, bsi$bsi_q48>0, bsi$bsi_q51>0))
bsi_paranoia3 <- rowSums(data.frame(bsi$bsi_q4, bsi$bsi_q10, bsi$bsi_q24, bsi$bsi_q48, bsi$bsi_q51))

bsi_psychtizismus <- rowMeans(data.frame(bsi$bsi_q3, bsi$bsi_q14, bsi$bsi_q34, bsi$bsi_q44, bsi$bsi_q53))
bsi_psychtizismus2 <- rowSums(data.frame(bsi$bsi_q3>0, bsi$bsi_q14>0, bsi$bsi_q34>0, bsi$bsi_q44>0, bsi$bsi_q53>0))
bsi_psychtizismus3 <- rowSums(data.frame(bsi$bsi_q3, bsi$bsi_q14, bsi$bsi_q34, bsi$bsi_q44, bsi$bsi_q53))

bsi_zusatz3<- rowSums(data.frame(bsi$bsi_q11, bsi$bsi_q25, bsi$bsi_q39, bsi$bsi_q52))
bsi_zusatz2 <- rowSums(data.frame(bsi$bsi_q11>0, bsi$bsi_q25>0, bsi$bsi_q39>0, bsi$bsi_q52>0))

bsi_scales <- data.frame(bsi$gender, bsi_somatisierung3, bsi_zwanghaftigkeit3, bsi_unsicherheit3, 
                         bsi_depression3, bsi_angst3, bsi_aggression3, bsi_phobie3, bsi_paranoia3, bsi_psychtizismus3)

GS = bsi_somatisierung3 + bsi_zwanghaftigkeit3 + bsi_unsicherheit3 + bsi_depression3 + bsi_angst3 + bsi_aggression3 + bsi_phobie3 + bsi_paranoia3 + bsi_psychtizismus3 + bsi_zusatz3
GSI = GS/53
PST = bsi_somatisierung2 + bsi_zwanghaftigkeit2 + bsi_unsicherheit2 + bsi_depression2 + bsi_angst2 + bsi_aggression2 + bsi_phobie2 + bsi_paranoia2 + bsi_psychtizismus2 + bsi_zusatz2
PSDI = GS/PST


### -----------------------------------------------------------------------------------------------------------------------------
### ---- MCTQ (calculate chronotype)
SPrep_f <- as.POSIXct(mctq$mctq_sleeptime_fd, format = "%H:%M") # bereit zum Einschlafen
SLat_f <- mctq$mctq_fallasleepmin_fd # sleep latency
SPrep_w <- as.POSIXct(mctq$mctq_sleeptime_wd, format = "%H:%M")
SLat_w <- mctq$mctq_fallasleepmin_wd
SE_f <- as.POSIXct(mctq$mctq_wakeuptime_fd, format = "%H:%M")+24*60*60 # sleep end free days
SE_w <- as.POSIXct(mctq$mctq_wakeuptime_wd, format = "%H:%M")+24*60*60
SO_f <- SPrep_f + SLat_f*60 # sleep onset free days
for (i in 1:length(SO_f))
  if (SO_f[i] < as.POSIXct("20:00", format = "%H:%M")){
    SO_f[i]<- SO_f[i] + 24*60*60
  }

SO_w <- SPrep_w + SLat_w*60 # sleep onset
for (i in 1:length(SO_w))
  if (SO_w[i] < as.POSIXct("20:00", format = "%H:%M")){
    SO_w[i]<- SO_w[i] + 24*60*60
  }
SI_w <- mctq$mctq_getupmin_wd # sleep inertia work days
SI_f <- mctq$mctq_getupmin_fd

SD_f <- as.numeric(SE_f - SO_f) #Sleep duration
SD_w <- as.numeric(SE_w - SO_w)

MSW <- SO_w + ((SD_w)/2)*60*60
MSF <- SO_f + ((SD_f)/2)*60*60

for (i in 1:length(SO_w)){
  if (SD_f[i] <= SD_w[i]){
    pb_info$mctq_ChronoT[i] <- as.character(as.POSIXct(MSF[i]))
  }else{
    pb_info$mctq_ChronoT[i] <- as.character(as.POSIXct(MSF[i] -(((SD_f[i])*60*60)-((SD_w[i])*60*60))/2))
  }
}

pb_info$mctq_SDur_f <- SD_f
pb_info$mctq_SDur_w <- SD_w

pb_info$alarm <- mctq$mctq_alarm_fd

## Evaluation
# writes "ok" if a score is within the inclusion criteria and "!" if it is not

# BSI (relies on T values for adults (m,f separately); sums equaling T-values >=60 are defined as "clinically relevant" according to the BSI manual)
pb_info$bsi_GS <- GS
for (i in 1:nrow(pb_info)){
  if (pb_info$bsi_GS[i] >=33){
    pb_info$bsi_GS_clin[i] <- "!"
  }else{
    pb_info$bsi_GS_clin[i] <- "ok"
  }
}

for (i in 1:nrow(bsi_scales)){
  if (bsi_scales$bsi.gender[i] == 1){ # 1 = female
    if (bsi_scales$bsi_somatisierung3[i]>=6 & bsi_scales$bsi_zwanghaftigkeit3[i]>=7 & bsi_scales$bsi_unsicherheit3[i]>=5 & bsi_scales$bsi_depression3[i]>=5 &
        bsi_scales$bsi_angst3[i]>=6 & bsi_scales$bsi_aggression3[i]>=4 & bsi_scales$bsi_phobie3[i]>=3 & bsi_scales$bsi_paranoia3[i]>=5 & bsi_scales$bsi_psychtizismus3[i]>=3){
      pb_info$bsi_scales_clin[i] <- "!"
    }else{
      pb_info$bsi_scales_clin[i] <- "ok"
    }
  }else{
    if (bsi_scales$bsi_somatisierung3[i]>=5 & bsi_scales$bsi_zwanghaftigkeit3[i]>=7 & bsi_scales$bsi_unsicherheit3[i]>=4 & bsi_scales$bsi_depression3[i]>=5 &
        bsi_scales$bsi_angst3[i]>=5 & bsi_scales$bsi_aggression3[i]>=4 & bsi_scales$bsi_phobie3[i]>=3 & bsi_scales$bsi_paranoia3[i]>=5 & bsi_scales$bsi_psychtizismus3[i]>=4){
      pb_info$bsi_scales_clin[i] <- "!"
    }else{
      pb_info$bsi_scales_clin[i] <- "ok"
    }
  }
}


# BMI (exclude if BMI < 17.5 or >25)
for (i in 1:nrow(pb_info)){
  if (pb_info$BMI[i] <17.5 | pb_info$BMI[i] >25){
    pb_info$BMI_state[i] <- "!"
  }else{
    pb_info$BMI_state[i] <- "ok"
  }
}

# PSQI (exclude if PSQI > 5)
for (i in 1:nrow(pb_info)){
  if (pb_info$psqi_sum[i] >5){
    pb_info$psqi_state[i] <- "!"
  }else{
    pb_info$psqi_state[i] <- "ok"
  }
}

# Overall state (checks if any of the exclusion criteria is met including smoking, age, drugs, etc.)
pb_info$vp_pregnancy[is.na(pb_info$vp_pregnancy)] <- 0
for (i in 1:nrow(pb_info)){
  if (pb_info$vp_age[i] >30 | pb_info$vp_smoke[i] == 1 | pb_info$vp_drugs[i] == 1 | pb_info$vp_neuro[i] == 1 | pb_info$vp_braininj[i] == 1 | pb_info$vp_psycho[i] == 1 |
      pb_info$vp_pregnancy[i] == 1 | pb_info$vp_hearingprobs[i] == 1 | pb_info$vp_shiftwork[i] == 1 | pb_info$vp_travel[i] == 1 |
      pb_info$bsi_GS_clin[i] == "!" | pb_info$bsi_scales_clin[i] == "!" | pb_info$meq_state[i] == "!" | pb_info$oldf_state[i] == "!" |
      pb_info$BMI_state[i] == "!" | pb_info$psqi_state[i] == "!"){
    pb_info$OVERALL_state[i] <- "!"
  }else{
    pb_info$OVERALL_state[i] <- "ok"
  }
}
pb_info$vp_firstname2 <- pb_info$vp_firstname
rownames(pb_info) <- NULL
pb_info$index <- seq(1,nrow(pb_info))

