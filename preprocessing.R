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
  
  # Filter practice trials
  if ("practice_debrief" %in% data$screen) {
    data <- data[which(data$screen == "practice_debrief"):nrow(data), ]
  }
  
  # Sort by random condition
  if ('1' %in% data$condition){
    # Illusion task data
    if ("break1" %in% data$screen){
      illusion_data <- data[0:which(data$screen == 'break1'),]
    }
    illusion_trials <- illusion_data[illusion_data$screen == "Trial", ]
    illusion_trials$Task <- 'Illusion'
    
    # Perceptual task data
    if ("perceptual_instructions" %in% data$screen){
      perceptual_data <- data[which(data$screen =='perceptual_instructions'):nrow(data), ]
    }
    perceptual_trials <- perceptual_data[perceptual_data$screen =='Trial',]
    perceptual_trials$Task <- 'Perceptual'
    
  } else {
    # Perceptual task data
    if ("break1" %in% data$screen){
      perceptual_data <- data[0:which(data$screen == 'break1'),]
    }
    perceptual_trials <- perceptual_data[perceptual_data$screen == "Trial", ]
    perceptual_trials$Task <- 'Perceptual'
    
    # Illusion task data
    if ("illusion_instructions" %in% data$screen){
      illusion_data <- data[which(data$screen =='illusion_instructions'):nrow(data), ]
    }
    illusion_trials <- illusion_data[illusion_data$screen =='Trial',]
    illusion_trials$Task <- 'Illusion'
  }
  
  
  # Get fixation cross position
  fixation<- data[data$screen=='fixation', 'stimulus']
  df <- gsub("<p style=\"color: black; font-size: 80px; ", '', (gsub("</p>", "", fixation)))
  fixation_position<- tidyr::separate(data.frame(position=df), col=position, into=c('Padding_Left', 'Padding_Right', 'Padding_Top', 'Padding_Bottom'), sep=';')
  
  fixation_position$Padding_Bottom<-gsub("[[:punct:]]", "", fixation_position$Padding_Bottom)
  fixation_position$Padding_Bottom<-as.numeric(gsub('\\D+','', fixation_position$Padding_Bottom))
  fixation_position$Padding_Left<-as.numeric(gsub('\\D+','', fixation_position$Padding_Left))
  fixation_position$Padding_Right<-as.numeric(gsub('\\D+','', fixation_position$Padding_Right))
  fixation_position$Padding_Top<-as.numeric(gsub('\\D+','', fixation_position$Padding_Top))
  fixation_position$Fixation_Horizontal<- fixation_position$Padding_Left - fixation_position$Padding_Right
  fixation_position$Fixation_Vertical<- fixation_position$Padding_Top - fixation_position$Padding_Bottom
  
  # Combine trial and fixation data
  trials <- rbind(illusion_trials, perceptual_trials)
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
    ISI = as.numeric(data[data$screen == "fixation" & !is.na(data$screen), "trial_duration"]),
    RT = as.numeric(trials$rt)
  )
  
  # 
  # if("IPIP6" %in% data$screen) {
  # IPIP6
  ipip6 <- as.data.frame(jsonlite::fromJSON(data[data$screen == "IPIP6", "response"]))
  ipip6[grepl("_R", names(ipip6))] <- 100 - ipip6[grepl("_R", names(ipip6))]
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
  df$PHQ4_Anxiety<- rowMeans(phq4[grepl("Anxiety", names(phq4))])
  df$PHQ4_Depression<- rowMeans(phq4[grepl("Depression", names(phq4))])
  df$PHQ4_SD <- mean(c(sd(phq4[grepl("Anxiety", names(phq4))]),
                       sd(phq4[grepl("Depression", names(phq4))])))
  
  # ASQ-28
  asq28<- as.data.frame(jsonlite::fromJSON(data[data$screen=='AQ28', 'response']))
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
  df$LIE_Contextuality<- rowMeans(lie[grepl("Contextuality", names(lie))])
  df$LIE_Ability<- rowMeans(lie[grepl("Ability", names(lie))])
  df$LIE_Negativity<- rowMeans(lie[grepl("Negativity", names(lie))])
  df$LIE_Frequency<- rowMeans(lie[grepl("Frequency", names(lie))])
  df$LIE_SD<- mean(c(sd(lie[grepl("Contextuality", names(lie))]),
                     sd(lie[grepl("Ability", names(lie))]),
                     sd(lie[grepl("Negativity", names(lie))]),
                     sd(lie[grepl("Frequency", names(lie))])))
  
  # Attention Check 
  # IPIP6, IAS and LIE responses are made on a visual analog scale
  # Note jsPSych records Likert values from 0 (not 1)
  df$Attention_Check1<- ipip6$Attention_Check_1
  df$Attention_Check2<- ifelse(pid5$Attention_Check_2==3,1,0)
  df$Attention_Check3<- ifelse(spq$Attention_Check_3==4,1,0)
  df$Attention_Check4<- ifelse(asq28$Attention_Check_4==0,1,0)
  df$Attention_Check5<- ias$Attention_Check_5
  df$Attention_Check6<- ifelse(gcbs$Attention_Check_6==0,1,0)
  df$Attention_Check7<- lie$Attention_Check_7
    
  #   
  # } else {
  #   df$IPIP6_Extraversion <- df$IPIP6_Conscientiousness <- df$IPIP6_Neuroticism <- df$IPIP6_Openness <- df$IPIP6_HonestyHumility <- df$IPIP6_Agreeableness <- df$IPIP6_SD <- NA
  #   df$PID5_Disinhibition <- df$PID5_Detachment <- df$PID5_NegativeAffect <- df$PID5_Antagonism <- df$PID5_Psychoticism <- df$PID5_SD <- NA
  # }
  df
}

participants <- list.files("data/rawdata/")

df <- data.frame()
for (ppt in participants) {
  df <- rbind(df, preprocess_raw(file = paste0("data/rawdata/", ppt)))
}


write.csv(df, "data/data.csv", row.names = FALSE)
  
  
