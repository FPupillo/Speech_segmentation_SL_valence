#------------------------------------------------------------------------------#
# preprocess the files 
#------------------------------------------------------------------------------#
rm(list=ls())

# get the participants files
part_files<-list.files("raw_data_2", pattern = "stat_learn_task")

# take the participant numbers
parts<-sub("_.*", "", part_files)

parts<-unique(parts)

VoI<-c("age", "gender", "handedness", "accuracy", "RTms", "syllable_pos", "repetition", "sequence_type", 
       "trialN", "slider_valence.response","slider_arousal.response", "slider_arousal_1.response" ,
       "slider_valence_2.response", "participant", "syllable", 
       "target_pos","target_rep", "key_resp.keys",   "target", "trialN", "slider.response","is_target" )

feedback<-rep(c(1,1, 0, 0), times = 100 )

# for some participants, we do not have the motivation - because the question 
# was introduced later on. But we have it on the log book 
# participants 02
part_02<-read.csv(list.files("raw_data_2", pattern = paste0(paste0("^02"), "_stat_learn_task"), 
                             full.names = T))
part_02$slider.response<-NA

write.csv(part_02,list.files("raw_data_2", pattern = paste0(paste0("^02"), "_stat_learn_task"), 
                             full.names = T) )
# participant 100
part_100<-read.csv(list.files("raw_data_2", pattern = paste0(paste0("^100"), "_stat_learn_task"), 
                             full.names = T))
part_100$slider.response<-NA

write.csv(part_100,list.files("raw_data_2", pattern = paste0(paste0("^100"), "_stat_learn_task"), 
                             full.names = T) )

# participants 176
part_176<-read.csv(list.files("raw_data_2", pattern = "^176_stat_learn_task", 
                              full.names = T))
part_176$slider.response<-2.5

write.csv(part_176,list.files("raw_data_2", pattern = "^176_stat_learn_task", 
                              full.names = T) )

# participants 180
part_180<-read.csv(list.files("raw_data_2", pattern = "^180_stat_learn_task", 
                              full.names = T))

part_180$slider.response<-2

write.csv(part_180,list.files("raw_data_2", pattern = "^180_stat_learn_task", 
                              full.names = T) )


# participant 52
part_52<-read.csv(list.files("raw_data_2", pattern = "^52_stat_learn_task", 
                             full.names = T))

part_52$slider.response<-NA

write.csv(part_52,list.files("raw_data_2", pattern = "^52_stat_learn_task", 
                              full.names = T) )

# participant 95
part_95<-read.csv(list.files("raw_data_2", pattern = "^95_stat_learn_task", 
                             full.names = T))

part_95$slider.response<-NA

write.csv(part_95,list.files("raw_data_2", pattern = "^95_stat_learn_task", 
                              full.names = T) )

# participant 96
part_96<-read.csv(list.files("raw_data_2", pattern = "^96_stat_learn_task", 
                             full.names = T))

part_96$slider.response<-NA

write.csv(part_96,list.files("raw_data_2", pattern = "^96_stat_learn_task", 
                              full.names = T) )

# select those 
full_df<-vector()

for (f in parts){
  
  tryCatch({
    
    #get the learning file
    stat_learn_file<-list.files("raw_data_2", pattern = paste0(paste0("^",f), "_stat_learn_task"))
    
      c_file<-read.csv(paste0("raw_data_2/", stat_learn_file))
      
      # for some participants, we do not have the motivation variable
      # check it 
      
      # select variables
      c_file<-c_file[, VoI]
      
      # create the RT for each word presentation
      c_file$RT_1<-as.numeric(NA)
      c_file$RT_2<-as.numeric(NA)
      c_file$RT_3<-as.numeric(NA)
      c_file$RT_4<-as.numeric(NA)
      
      # difference bweteen 1 and 2,3, 4th repetition
      c_file$RTdiff_2<-as.numeric(NA)
      c_file$RTdiff_3<-as.numeric(NA)
      c_file$RTdiff_4<-as.numeric(NA)
      
      # add the beta coefficient of repetition
      c_file$beta_repetition<-as.numeric(NA)
      c_file$repet_contr<-as.numeric(NA)
      
      
      # same difference but log
      c_file$log_RTdiff_2<-as.numeric(NA)
      c_file$log_RTdiff_3<-as.numeric(NA)
      c_file$log_RTdiff_4<-as.numeric(NA)
      
      c_file$trialN<-NA
      
      # valence difference with the previous assessment (subtracting previous assessment)
      c_file$val_diff<-NA
      c_file$arous_diff<-NA
      
      # valence difference with the baseline (subtracting thebaseline)
      c_file$val_baseline_diff<-NA
      
      # reverse valence and arrousal
      c_file$slider_valence.response<-101-c_file$slider_valence.response
      c_file$slider_arousal.response<-101-c_file$slider_arousal.response
      c_file$slider_arousal_1.response<-101-c_file$slider_arousal_1.response
      c_file$slider_valence_2.response<-101-c_file$slider_valence_2.response
      
      # get baseline mood
      baseline_val<-c_file$slider_valence.response[!is.na(c_file$slider_valence.response)]
      
      baseline_arous<-c_file$slider_arousal_1.response[!is.na(c_file$slider_arousal_1.response)]
      
      # current arousal
      c_file$c_arousal<-NA
      
      # get variables for SAM
      count_t<-1
      for (n in 1:nrow(c_file)){
        if (!is.na(c_file$slider_valence_2.response[n])){
          
          sub_file<-c_file[(n-48):(n-1),]
          
          # get RT
          RT<-sub_file[!is.na(sub_file$RTms) & sub_file$accuracy==1,] #we are selecting only correct trials
           
          if (nrow(RT)>=3){
            
            c_file$RTdiff_2[n]<-RT$RTms[RT$target_rep ==2][1]- 
              RT$RTms[RT$target_rep ==1][1]
            
            c_file$log_RTdiff_2[n]<-log(RT$RTms[RT$target_rep ==2][1])-
              log(RT$RTms[RT$target_rep ==1][1])
            
            c_file$RTdiff_3[n]<-RT$RTms[RT$target_rep ==3][1]- 
              RT$RTms[RT$target_rep ==1][1]
            
            c_file$log_RTdiff_3[n]<-log(RT$RTms[RT$target_rep ==3][1])- 
              log(RT$RTms[RT$target_rep ==1][1])
            
            c_file$RTdiff_4[n]<-RT$RTms[RT$target_rep ==4][1]- 
              RT$RTms[RT$target_rep ==1][1]
            
            c_file$log_RTdiff_4[n]<-log(RT$RTms[RT$target_rep ==4][1])- 
              log(RT$RTms[RT$target_rep ==1][1])
            
            # run the regression
            RT$target_rep<-as.factor(RT$target_rep)
            
            reg<-lm(RTms~target_rep, data = RT)
            
            c_file$beta_repetition[n]<-as.numeric(reg$coefficients["target_rep"])
            
          }
          

          # get all the RT's 
          for (rep in 1:4){
            
            c_rep<- sub_file$RTms[sub_file$target_rep==rep & !is.na(sub_file$RTms)]
            
            if (length(c_rep)>0){
              
              c_file[[paste0("RT_", rep)]][n]<-c_rep
              
            }
            
          }
          
          # sequence type and target position depends on the previous one
          c_file$sequence_type[n]<-c_file$sequence_type[n-1]
          
          # get all the variables
          c_file$target_pos[n]<- c_file$target_pos[n-1]
          
          # get the difference with the previous valence
          # and the difference with the baseline
          
          if(count_t==1){ # if this is the first trial
            
            c_file$val_diff[n]<-c_file$slider_valence_2.response[n]-baseline_val
            
            c_file$arous_diff[n]<-c_file$slider_arousal.response[n]-baseline_arous
            
            c_valence<-c_file$slider_valence_2.response[n]
            
            c_arousal<-c_file$slider_arousal.response[n]
            
            
          }else{
            # if this is not the first trial, subtract the previous valence
            c_file$val_diff[n]<-  c_file$slider_valence_2.response[n] - c_valence
            
            # if this is not the first trial, subtract the previous valence
            c_file$arous_diff[n]<-  c_file$slider_arousal.response[n] - c_arousal
            
            # store the current valence
            c_valence<-c_file$slider_valence_2.response[n]
            
            # store the current valence
            c_arousal<-c_file$slider_arousal.response[n]
          }
          
          c_file$trialN<-count_t
          count_t<-count_t+1
        }
        
        # difference between c_valence and the baseline
        c_file$val_baseline_diff[n]<-
          c_file$slider_valence_2.response[n]-baseline_val
        
        c_file$c_arousal[n]<-c_file$slider_arousal.response[n]
      }
      
    
      # select only structured and unstructured
      c_file<-c_file[c_file$sequence_type ==  "structured" |
                       c_file$sequence_type == "unstructured",]
      
      # now create a variable for the false alarms
      c_file$FA<-NA
      for (n in 1:nrow(c_file)){
        if (n>1){
        if ( c_file$key_resp.keys[n]=="[\"space\"]"){
          
          # check when the last target occurred
          c_target<-c_file[1:n,]
          
          last_target<-tail(which(c_target$is_target==1),1)
          
          # if there are more than 4 trials between the key press and the 
          # last target (2100), then it is a false alarm
         if (n<4 | length(last_target)==0){ # if this is a very early target or 
                                            # there were no targets
           c_file$FA[n]<-1 
          } else if ((n-last_target)>4){
            c_file$FA[n]<-1
          }
          
        }
        }
      }
      
      #------------------------------------------------------------------------#
      # create centered valence - subtract the mean of the valence of the 
      # unstructured from the structured 
      #------------------------------------------------------------------------#
      
      # first, select valence in the unstructured streams
      c_unstr_valence<-
        c_file$slider_valence_2.response[c_file$sequence_type=="unstructured"]
      c_unstr_arousal<-
        c_file$slider_arousal.response[c_file$sequence_type=="unstructured"]
      
      # delete the naas
      c_unstr_valence<-c_unstr_valence[!is.na(c_unstr_valence)]
      c_unstr_arousal<-c_unstr_arousal[!is.na(c_unstr_arousal)]
      
      # get the mean
      c_file$valence_unstructured<-mean(c_unstr_valence)
      c_file$arousal_unstructured<-mean(c_unstr_arousal)
      
      # create the centered valence
      c_file$valence_c<-c_file$slider_valence_2.response-c_file$valence_unstructured 
      # CAREFUL! We are subtracting the unstructured valence from all the responses!
      
      # create the centered arousal
      c_file$arousal_c<-c_file$slider_arousal.response-c_file$arousal_unstructured 
      # CAREFUL! We are subtracting the unstructured valence from all the responses!
      
      # remove the unstructured from this valence
      c_file$valence_c[c_file$sequence_type=="unstructured"]<-NA
      c_file$arousal_c[c_file$sequence_type=="unstructured"]<-NA
      
      #------------------------------------------------------------------------#
      
      
      # get part
      part<-c_file$participant[1]
      
      c_file$feedback<-feedback[part]
      
      c_file$feedback2<-
        ifelse(is.null(c_file$feedb_Instr.started[
          !is.na(c_file$feedb_Instr.started)]), 0, 1)
    
      # get motivation
      motiv<-c_file$slider.response[!is.na(c_file$slider.response)]==0
      
      if (is.logical(motiv) && length(motiv) == 0 ){
        c_file$motivation<-NA
      }else{
      c_file$motivation<-c_file$slider.response[!is.na(c_file$slider.response)]
      }
      
      c_file$baseline_valence<-baseline_val
      c_file$baseline_arousal<-baseline_arous

      full_df<-rbind(full_df, c_file)
    }, error = function(e){print(paste0("error with file", f))})
  }
  
# some checks
# count the number of trials
a<-full_df %>%
  group_by(participant, sequence_type) %>%
  dplyr::summarise(n_trials = sum(!is.na(slider_valence_2.response)),
            .groups = "drop")


# save the files
write.csv(full_df, "group_data/long_df_stat.csv")

# extract info for wide data
summary_structured<-full_df %>% 
  filter(sequence_type == "structured" & accuracy ==1 & target_pos ==3) %>%
  group_by(participant, target_rep) %>%
 dplyr:: summarise(age = first(age), 
            gender = first(gender),
            handedness = first(handedness),
            motivation = first(motivation),
            Mean_RT = mean(RTms, na.rm = TRUE)) %>%
  pivot_wider(names_from = target_rep, values_from = Mean_RT, names_prefix = 
                "RT_syllpos3_rep_st_")%>%
  mutate(
    # Compute difference between Rep_3 and Rep_1
    RT_diff_3_1_st = RT_syllpos3_rep_st_3 - RT_syllpos3_rep_st_1,  
    # Compute difference between Rep_4 and Rep_1
    RT_Diff_4_1_st = RT_syllpos3_rep_st_4 - RT_syllpos3_rep_st_1   
  )
  

summary_unstructured<-full_df %>% 
  filter(sequence_type == "unstructured" & accuracy ==1) %>%
  group_by(participant, target_rep) %>%
  dplyr::summarise(Mean_RT = mean(RTms, na.rm = TRUE)) %>%
  pivot_wider(names_from = target_rep, values_from = Mean_RT, 
              names_prefix = "RT_rep_un_")%>%
  mutate(
    # Compute difference between Rep_3 and Rep_1
    RT_diff_3_1_un = RT_rep_un_3 - RT_rep_un_1,  
    # Compute difference between Rep_4 and Rep_1
    RT_Diff_4_1_un = RT_rep_un_4 - RT_rep_un_1   
  )
  

# select structured and unstructured only
  full_df<-full_df[full_df$sequence_type=="structured" | 
                     full_df$sequence_type=="unstructured",]
  
  # valence and arousal
  full_df_val<-full_df[!is.na(full_df$slider_valence_2.response),]
  
  # calculate mean and standard deviation
  valence_summary_val<- summarySEwithin(full_df_val,
                                        measurevar = c("slider_valence_2.response"),
                                        withinvars = c("target_pos", 
                                                       "sequence_type" ), 
                                        betweenvars = c("feedback", "participant"),
                                        idvar = "participant", 
                                        na.rm = T)
  
  
  valence_summary_val_diff<- summarySEwithin(full_df_val,
                                             measurevar = c("val_baseline_diff"),
                                             withinvars = c("target_pos", 
                                                            "sequence_type" ), 
                                             betweenvars = c("feedback", "participant"),
                                             idvar = "participant", 
                                             na.rm = T)
  
  
  
  # rename some variables
  names(valence_summary_val)[ names(valence_summary_val)=="slider_valence_2.response"]<-"valence"
  
  # delete confidence interval and number
  valence_summary_val<-valence_summary_val[, c("feedback", "participant", "target_pos",
                                       "sequence_type", "valence", "sd")]
  
  # Reshape the dataset so each participant has one row
  wide_data_val <- valence_summary_val %>%
    pivot_wider(
      names_from = c(target_pos, sequence_type),  # Create new columns based on 'target_pos'
      values_from = c(valence, sd)  # Keep all relevant variables
    )
  
  names(wide_data_val)[c(3:6)]<-c("1_valence_structured", "2_valence_structured", 
                                       "3_valence_structured", "val_unstructured")
  
  # delete confidence interval and number
  valence_summary_diff<-valence_summary_val_diff[, c( "participant", "target_pos",
                                       "sequence_type", "val_baseline_diff")]
  
  # Reshape the dataset so each participant has one row
  wide_data_val_diff <- valence_summary_diff %>%
    pivot_wider(
      names_from = c(target_pos, sequence_type),  # Create new columns based on 'target_pos'
      values_from = c(val_baseline_diff)  # Keep all relevant variables
    )
  
  # rename
 names(wide_data_val_diff)[c(2:5)]<-c("1_valence_diff_structured", "2_valence_diff_structured", 
                                      "3_valence_diff_structured", "val_diff_unstructured")
  
 
  #----------------------------------------------------------------------------#
  # now merge
  # mearge learning
 
  summary_learning<-merge(summary_structured, summary_unstructured, by="participant")
  
  # merge with valence
  summary_val<-merge(wide_data_val, wide_data_val_diff, by="participant")
  
 # merge all
  data_all<-merge(summary_learning, summary_val, by="participant")
  
  # save
  write.csv(data_all, paste0("group_data/", "learning_valence_df.csv"), row.names = F)
  #----------------------------------------------------------------------------#

  