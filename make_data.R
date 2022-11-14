# Preprocessing
preprocess_raw <- function(file) {

  # Read data
  data<- read.csv(file)

  if (!"final_results" %in% data$screen) {
    print(paste0("Warning: Incomplete data for ", file))
    return(data.frame())
  }

  # Get Demographics and Browser Info
  dem <- data[data$screen=='demographics' & !is.na(data$screen), "response"]
  info <- data[data$screen == 'browser_info' & !is.na(data$screen),  ]
  
  # Session 1
  if ("condition" %in% colnames(data)){
    
    # Sort by condition (for Session 1 only)
    if ('1' %in% data$condition){
      # Illusion task data without practice trials
      if ("break1" %in% data$screen){
        illusion_data <- data[which(data$screen == "practice_debrief"):which(data$screen == 'break1'),]
      }
      illusion_trials <- illusion_data[illusion_data$screen == "Trial", ]
      illusion_trials$Task <- 'Illusion'
  
      # Perceptual task data
      if ("perceptual_instructions" %in% data$screen){
        perceptual_data <- data[which(data$screen =='perceptual_instructions'):nrow(data), ]
      }
      perceptual_trials <- perceptual_data[perceptual_data$screen =='Trial',]
      perceptual_trials$Task <- 'Perceptual'
  
    } else if ('2' %in% data$condition){
    
      # Perceptual task data
      if ("break1" %in% data$screen){
      perceptual_data <- data[which(data$screen=='perceptual_instructions'):which(data$screen == 'break1'),]
      }
      perceptual_trials <- perceptual_data[perceptual_data$screen == "Trial", ]
      perceptual_trials$Task <- 'Perceptual'
    
      # Illusion task data without practice trials
      if ("practice_debrief" %in% data$screen) {
        illusion_data <- data[which(data$screen == "practice_debrief"):nrow(data), ]
      }
      illusion_trials <- illusion_data[illusion_data$screen =='Trial',]
      illusion_trials$Task <- 'Illusion'
  } 
    trial_data<- rbind(perceptual_data, illusion_data)
    trials <- rbind(illusion_trials, perceptual_trials)
    trials$Session <- 1
    
  } else if (is.null(data$condition)){
    # Session 2  
    # Illusion task data
      trial_data <- data[which(data$screen == "practice_debrief"):which(data$screen == 'break1'),]
      trials <- trial_data[trial_data$screen == "Trial", ]
      trials$Task <- 'Illusion'
      trials$Session <- 2
      trials$condition <- NA
  }
  
    # Get fixation cross position
    fixation<- trial_data[trial_data$screen=='fixation', c('stimulus', 'trial_index') ]
    fixation<- fixation[order(fixation$trial_index),]
    temp <- gsub(";", '>', (gsub("<p style=\"color: black; font-size: 80px; ", "", fixation$stimulus)))
    fixation_position<- tidyr::separate(data.frame(position=temp), col=position, into=c('Padding_Left', 'Padding_Right', 'Padding_Top', 'Padding_Bottom'), sep='>')
    fixation_position$Padding_Bottom<-as.numeric(gsub("\\D+", "", fixation_position$Padding_Bottom))
    fixation_position$Padding_Bottom<-as.numeric(gsub('\\D+','', fixation_position$Padding_Bottom))
    fixation_position$Padding_Left<-as.numeric(gsub('\\D+','', fixation_position$Padding_Left))
    fixation_position$Padding_Right<-as.numeric(gsub('\\D+','', fixation_position$Padding_Right))
    fixation_position$Padding_Top<-as.numeric(gsub('\\D+','', fixation_position$Padding_Top))
    fixation_position$Fixation_Horizontal<- fixation_position$Padding_Left - fixation_position$Padding_Right
    fixation_position$Fixation_Vertical<- fixation_position$Padding_Top - fixation_position$Padding_Bottom
    fixation_position['fixation_index'] <- fixation$trial_index
    
    # Combine trial and fixation data
    trials<- trials[order(trials$trial_index),]
    trials<- cbind(trials, fixation_position)
  
    df <- data.frame(
      Participant = trials$participant_id,
      Age = as.numeric(jsonlite::fromJSON(dem[1])$age),
      Sex = jsonlite::fromJSON(dem[2])$sex,
      Education = jsonlite::fromJSON(dem[2])$education,
      Ethnicity = tools::toTitleCase(jsonlite::fromJSON(dem[1])$ethnicity),
      Date = ifelse(is.null(info$date), NA, info$date),
      Time = ifelse(is.null(info$time), NA, info$time),
      Duration = as.numeric(data[data$screen == "final_results", "time_elapsed"]) / 1000 / 60,
      Break_Duration = as.numeric(data[data$screen %in% c("break", "break2") & !is.na(data$screen), "rt"]) / 1000 / 60,
      Screen_Resolution = paste0(trials$screen_width, "x", trials$screen_height),
      Screen_Size = (as.numeric(trials$screen_width) / 1000) * (as.numeric(trials$screen_height) / 1000),
      Screen_Refresh = trials$vsync_rate,
      Browser = trials$browser,
      Browser_Version = trials$browser_version,
      Device = ifelse(trials$mobile == TRUE, "Mobile", "Desktop"),
      Device_OS = trials$os,
      Session = trials$Session,
      Condition = trials$condition,
      Illusion_Type = trials$type,
      Block = trials$Task,
      Block_Order = as.numeric(trials$block_number),
      Trial = as.numeric(trials$trial_number),
      Stimulus = gsub(".png", "", gsub("stimuli/", "", trials$stimulus)),
      Fixation_VerticalDist = trials$Fixation_Vertical,
      Fixation_HorizontalDist = trials$Fixation_Horizontal,
      Illusion_Strength = as.numeric(trials$illusion_strength),
      # Illusion_Effect = ifelse(sign(as.numeric(trials$illusion_strength)) == -1, "Congruent", ifelse(sign(as.numeric(trials$illusion_strength)) == 0, "Null", "Incongruent")),
      Illusion_Effect = ifelse(sign(as.numeric(trials$illusion_strength)) == -1, "Congruent", "Incongruent"),
      Illusion_Side = as.factor(sign(as.numeric(trials$illusion_difference))),
      Illusion_Difference = abs(as.numeric(trials$illusion_difference)),
      Answer = trials$response,
      Error = as.integer(!as.logical(trials$correct)),
      ISI = as.numeric(trial_data[trial_data$screen == "fixation" & !is.na(trial_data$screen), "trial_duration"]),
      RT = as.numeric(trials$rt)
    )
  
    # Add Scores from psychometric scales 
    # IPIP6
    if("IPIP6" %in% data$screen) { 
      ipip6 <- as.data.frame(jsonlite::fromJSON(data[data$screen == "IPIP6", "response"]))
      df<- merge(df, ipip6)
      ipip6[grepl("_R", names(ipip6))] <- 1 - ipip6[grepl("_R", names(ipip6))]
      df$IPIP6_Extraversion <- rowMeans(ipip6[grepl("Extraversion", names(ipip6))])
      df$IPIP6_Conscientiousness <- rowMeans(ipip6[grepl("Conscientiousness", names(ipip6))])
      df$IPIP6_Neuroticism <- rowMeans(ipip6[grepl("Neuroticism", names(ipip6))])
      df$IPIP6_Openness <- rowMeans(ipip6[grepl("Openness", names(ipip6))])
      df$IPIP6_HonestyHumility <- rowMeans(ipip6[grepl("HonestyHumility", names(ipip6))])
      df$IPIP6_Agreeableness <- rowMeans(ipip6[grepl("Agreeableness", names(ipip6))])
      df$IPIP6_SD <- mean(c(sd(ipip6[grepl("Extraversion", names(ipip6))]),
                            sd(ipip6[grepl("Conscientiousness", names(ipip6))]),
                            sd(ipip6[grepl("Neuroticism", names(ipip6))]),
                            sd(ipip6[grepl("Openness", names(ipip6))]),
                            sd(ipip6[grepl("HonestyHumility", names(ipip6))]),
                            sd(ipip6[grepl("Agreeableness", names(ipip6))])))
    
      # PID5
      pid5 <- as.data.frame(jsonlite::fromJSON(data[data$screen == "PID5_SF", "response"]))
      df<- merge(df, pid5)
      df$PID5_Disinhibition <- rowMeans(pid5[grepl("Disinhibition", names(pid5))])
      df$PID5_Detachment <- rowMeans(pid5[grepl("Detachment", names(pid5))])
      df$PID5_NegativeAffect <- rowMeans(pid5[grepl("NegativeAffect", names(pid5))])
      df$PID5_Antagonism <- rowMeans(pid5[grepl("Antagonism", names(pid5))])
      df$PID5_Psychoticism <- rowMeans(pid5[grepl("Psychoticism", names(pid5))])
      df$PID5_SD <- mean(c(sd(pid5[grepl("Disinhibition", names(pid5))]),
                           sd(pid5[grepl("Detachment", names(pid5))]),
                           sd(pid5[grepl("NegativeAffect", names(pid5))]),
                           sd(pid5[grepl("Antagonism", names(pid5))]),
                           sd(pid5[grepl("Psychoticism", names(pid5))])))
    
      # PHQ-4
      phq4 <- as.data.frame(jsonlite::fromJSON(data[data$screen=='PHQ_4', 'response']))
      df<- merge(df, phq4)
      df$PHQ4_Anxiety<- rowMeans(phq4[grepl("Anxiety", names(phq4))])
      df$PHQ4_Depression<- rowMeans(phq4[grepl("Depression", names(phq4))])
      df$PHQ4_SD <- mean(c(sd(phq4[grepl("Anxiety", names(phq4))]),
                           sd(phq4[grepl("Depression", names(phq4))])))
    
      # ASQ-28
      asq28<- as.data.frame(jsonlite::fromJSON(data[data$screen=='AQ28', 'response']))
      df<- merge(df, asq28)
      df$ASQ28_SocialSkills<- rowMeans(asq28[grepl("SocialSkills", names(asq28))])
      df$ASQ28_Routine<- rowMeans(asq28[grepl("Routine", names(asq28))])
      df$ASQ28_Imagination<- rowMeans(asq28[grepl("Imagination", names(asq28))])
      df$ASQ28_Switching<- rowMeans(asq28[grepl("Switching", names(asq28))])
      df$ASQ28_Patterns<- rowMeans(asq28[grepl("Patterns", names(asq28))])
      df$ASQ28_SD <- mean(c(sd(asq28[grepl("SocialSkills", names(asq28))]),
                           sd(asq28[grepl("Routine", names(asq28))]),
                           sd(asq28[grepl("Imagination", names(asq28))]),
                           sd(asq28[grepl("Switching", names(asq28))]),
                           sd(asq28[grepl("Patterns", names(asq28))])))
    
      # GCBS (can also be measured as a unidimensional construct)
      gcbs<- as.data.frame(jsonlite::fromJSON(data[data$screen=='GCBS', 'response']))
      df<- merge(df, gcbs)
      df$GCBS_GovernmentMalfeasance<-rowMeans(gcbs[grepl("GM", names(gcbs))])
      df$GCBS_ETCoverUp<-rowMeans(gcbs[grepl("ET", names(gcbs))])
      df$GCBS_MalevolentGlobalConspiracies<-rowMeans(gcbs[grepl("MG", names(gcbs))])
      df$GCBS_PersonalWellBeing<-rowMeans(gcbs[grepl("PW", names(gcbs))])
      df$GCBS_ControlofInformation<-rowMeans(gcbs[grepl("CI", names(gcbs))])
      df$GCBS_Total<-rowMeans(gcbs)
      df$GCBS_SD<- mean(c(sd(gcbs[grepl("GM", names(gcbs))]),
                          sd(gcbs[grepl("ET", names(gcbs))]),
                          sd(gcbs[grepl("MG", names(gcbs))]),
                          sd(gcbs[grepl("PW", names(gcbs))]),
                          sd(gcbs[grepl("CI", names(gcbs))])))
    
      # SPQ
      spq<- as.data.frame(jsonlite::fromJSON(data[data$screen=='SPQ_BRU', 'response']))
      df<- merge(df, spq)
      df$SPQ_MagicalThinking<- rowMeans(spq[grepl("MagicalThinking", names(spq))])
      df$SPQ_UnusualPerceptions<- rowMeans(spq[grepl("UnusualPerceptions", names(spq))])
      df$SPQ_Eccentric<- rowMeans(spq[grepl("Eccentric", names(spq))])
      df$SPQ_OddSpeech<- rowMeans(spq[grepl("OddSpeech", names(spq))])
      df$SPQ_ConstrictedAffect<- rowMeans(spq[grepl("ConstrictedAffect", names(spq))])
      df$SPQ_Reference<- rowMeans(spq[grepl("Reference", names(spq))])
      df$SPQ_NoCloseFriends<- rowMeans(spq[grepl("NoCloseFriends", names(spq))])
      df$SPQ_SocialAnxiety<- rowMeans(spq[grepl("SocialAnxiety", names(spq))])
      df$SPQ_Suspiciousness<- rowMeans(spq[grepl("Suspiciousness", names(spq))])
      df$SPQ_SD<- mean(c(sd(spq[grepl("MagicalThinking", names(spq))]),
                         sd(spq[grepl("UnusualPerceptions", names(spq))]),
                         sd(spq[grepl("Eccentric", names(spq))]),
                         sd(spq[grepl("OddSpeech", names(spq))]),
                         sd(spq[grepl("ConstrictedAffect", names(spq))]),
                         sd(spq[grepl("Reference", names(spq))]),
                         sd(spq[grepl("NoCloseFriends", names(spq))]),
                         sd(spq[grepl("SocialAnxiety", names(spq))]),
                         sd(spq[grepl("Suspiciousness", names(spq))])))
    
      # IAS
      ias<- as.data.frame(jsonlite::fromJSON(data[data$screen=='IAS_R', 'response']))
      df<- merge(df, ias)
      df$IAS_Nociception<- rowMeans(ias[grepl("Nociception", names(ias))])
      df$IAS_Skin<- rowMeans(ias[grepl("Skin", names(ias))])
      df$IAS_Elimination<- rowMeans(ias[grepl("Elimination", names(ias))])
      df$IAS_Interoception<- rowMeans(ias[grepl("Interoception", names(ias))])
      df$IAS_Expulsion<- rowMeans(ias[grepl("Expulsion", names(ias))])
      df$IAS_SD<- mean(c(sd(ias[grepl("Nociception", names(ias))]),
                         sd(ias[grepl("Skin", names(ias))]),
                         sd(ias[grepl("Elimination", names(ias))]),
                         sd(ias[grepl("Interoception", names(ias))]),
                         sd(ias[grepl("Expulsion", names(ias))])))
    
      # LIE
      lie<- as.data.frame(jsonlite::fromJSON(data[data$screen=='LIE', 'response']))
      df<- merge(df, lie)
      df$LIE_Contextuality<- rowMeans(lie[grepl("Contextuality", names(lie))])
      df$LIE_Ability<- rowMeans(lie[grepl("Ability", names(lie))])
      df$LIE_Negativity<- rowMeans(lie[grepl("Negativity", names(lie))])
      df$LIE_Frequency<- rowMeans(lie[grepl("Frequency", names(lie))])
      df$LIE_SD<- mean(c(sd(lie[grepl("Contextuality", names(lie))]),
                         sd(lie[grepl("Ability", names(lie))]),
                         sd(lie[grepl("Negativity", names(lie))]),
                         sd(lie[grepl("Frequency", names(lie))])))
    
      # Attention Check
      # IPIP6, IAS and LIE responses are measured on a visual analog scale
      # NOTE: jsPSych records Likert values from 0 (not 1)
      df$Attention_Check1<- ipip6$Attention_Check_1
      df$Attention_Check2<- ifelse(pid5$Attention_Check_2==3,1,0)
      df$Attention_Check3<- ifelse(spq$Attention_Check_3==4,1,0)
      df$Attention_Check4<- ifelse(asq28$Attention_Check_4==0,1,0)
      df$Attention_Check5<- ias$Attention_Check_5
      df$Attention_Check6<- ifelse(gcbs$Attention_Check_6==0,1,0)
      df$Attention_Check7<- lie$Attention_Check_7
  
  
    # } else {
    #   df$IPIP6_Extraversion <- df$IPIP6_Conscientiousness <- df$IPIP6_Neuroticism <- df$IPIP6_Openness <- df$IPIP6_HonestyHumility <- df$IPIP6_Agreeableness <- df$IPIP6_SD <- NA
    #   df$PID5_Disinhibition <- df$PID5_Detachment <- df$PID5_NegativeAffect <- df$PID5_Antagonism <- df$PID5_Psychoticism <- df$PID5_SD <- NA
    #   df$PHQ4_Anxiety <- df$PHQ4_Depression <- df$PHQ4_SD <- NA
    #   df$ASQ28_SocialSkills <- df$ASQ28_Routine <- df$ASQ28_Imagination <- df$ASQ28_Switching <- df$ASQ28_Patterns <- df$ASQ28_SD <- NA
    #   df$GCBS_GovernmentMalfeasance <-df$GCBS_ETCoverUp <-df$GCBS_MalevolentGlobalConspiracies <- df$GCBS_PersonalWellBeing <-df$GCBS_ControlofInformation <-df$GCBS_Total <-df$GCBS_SD <- NA
    #   df$SPQ_MagicalThinking<- df$SPQ_UnusualPerceptions<- df$SPQ_Eccentric <-df$SPQ_OddSpeech <-df$SPQ_ConstrictedAffect<-df$SPQ_Reference<- df$SPQ_NoCloseFriends<-df$SPQ_SocialANxiety<- df$SPQ_Suspiciousness<- df$SPQ_SD<- NA
    #   df$IAS_Nociception <- df$IAS_Skin <-  df$IAS_Elimination <-  df$IAS_Interoception <-  df$IAS_Expulsion <-  df$IAS_SD<- NA
    #   df$LIE_Contextuality<- df$LIE_Ability<-df$LIE_Negativity<-df$LIE_Frequency<- df$LIE_SD<- NA
   
    } else if("MAIA" %in% data$screen) { 
      
      # MAIA
      maia <- as.data.frame(jsonlite::fromJSON(data[data$screen == "MAIA", "response"]))
      df<- merge(df, maia)
      maia[grepl("_R", names(maia))] <- 1 - maia[grepl("_R", names(maia))]
      df$MAIA_Noticing <- rowMeans(maia[grepl("Noticing", names(maia))])
      df$MAIA_NotDistracting <- rowMeans(maia[grepl("NotDistracting", names(maia))])
      df$MAIA_NotWorrying <- rowMeans(maia[grepl("NotWorrying", names(maia))])
      df$MAIA_AttentionRegulation <- rowMeans(maia[grepl("AttentionRegulation", names(maia))])
      df$MAIA_EmotionalAwareness <- rowMeans(maia[grepl("EmotionalAwareness", names(maia))])
      df$MAIA_SelfRegulation <- rowMeans(maia[grepl("SelfRegulation", names(maia))])
      df$MAIA_BodyListening<- rowMeans(maia[grepl('BodyListening'), names(maia)])
      df$MAIA_Trusting<- rowMeans(maia[grepl('Trusting'), names(maia)])
      df$MAIA_SD <- mean(c(sd(maia[grepl("Noticing", names(maia))]),
                            sd(maia[grepl("NotDistracting", names(maia))]),
                            sd(maia[grepl("NotWorrying", names(maia))]),
                            sd(maia[grepl("AttentionRegulation", names(maia))]),
                            sd(maia[grepl("EmotionalAwareness", names(maia))]),
                            sd(maia[grepl("SelfRegulation", names(maia))]),
                            sd(maia[grepl('BodyListening', names(maia))]),
                            sd(maia[grepl('Trusting', names(maia))])))
      
      # BPD
      bpd<- as.data.frame(jsonlite::fromJSON(data[data$screen =='MSI_BPD', 'response']))
      df$BPD <-rowMeans(bpd[grepl("MSI_", names(bpd))])
      df$BPD_SD <- sd(bpd[grepl("MSI_", names(bpd))])
      
      # PI-18
      pi18<- as.data.frame(jsonlite::fromJSON(data[data$screen =='PI_18', 'response']))
      pi18[grepl("_R", names(pi18))] <- 6 - pi18[grepl("_R", names(pi18))]
      df$PI_Enticing<- rowMeans(pi18[grepl('GE_', names(pi18))])
      df$PI_Alive<- rowMeans(pi18[grepl('A_', names(pi18))])
      df$PI_Safe<- rowMeans(pi18[grepl('GS_', names(pi18))])
      df$PI_Good<- rowMeans(pi18[grepl('G', names(pi18))])
      df$PI_Changing<- rowMeans(pi18[grepl('Changing', names(pi18))])
      df$PI_Hierarchical<- rowMeans(pi18[grepl('Hierarchical', names(pi18))])
      df$PI18_Understandable<- rowMeans(pi18[grepl('Understandable', names(pi18))])
      df$PI18_SD<- mean(c(sd(pi18[grepl('GE_', names(pi18))]),
                          df(pi18[grepl('A_', names(pi18))]),
                          sd(pi18[grepl('GS_', names(pi18))]),
                          sd(pi18[grepl('Changing', names(pi18))]),
                          sd(pi18[grepl('Hierarchical', names(pi18))]),
                          df(pi18[grepl('Understandable', names(pi18))])))
      
      # Attention Check 
      df$Attention_Check8 <- maia$Attention_Check_1
      df$Attention_Check9 <- bpd$Attention_Check_2
      df$Attention_Check_10 <- ifelse(pi18$Attention_Check_3==5, 1, 0) 
    }
    df
}

# Find participants
rawdata_folder <- "C:/Users/anshu/Dropbox/IllusionGameReliability/"
participants <- list.files(paste0(rawdata_folder, "session1/"), pattern='IllusionGameReliability')
participants_2<- list.files(paste0(rawdata_folder, "session2/"), pattern='IllusionGameReliability')


df <- data.frame()
# Session 1 Data
for (ppt in participants_2) {
  df <- rbind(df, preprocess_raw(file = paste0(paste0(rawdata_folder, "session1/"), ppt)))
}

# Session 2 Data
for (ppt in participants){
  id<- substr(ppt, 33, 62)
  if (id %in% df$Participant){
    df<- merge(df, preprocess_raw(file=paste0(paste0(rawdata_folder, "session2/"), ppt)), by ='Participant')
  }
}


write.csv(df, "data/data.csv", row.names = FALSE)


