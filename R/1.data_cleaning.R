#-----------------------------------------------------------------------------#
# preprocess data
#-----------------------------------------------------------------------------#

# get the root folder
root<-dirname(getwd())

# Create a copy of the raw data folder
if(!exists("raw_data_2")){
 dir.create("raw_data_2")
}

# remove all the files in the newly created "raw_data_2" folder
file.remove(list.files("raw_data_2", full.names = T))

# Get the list of files in the directory
stat_learn_files <- list.files( "raw_data" ,pattern = "stat_learn_task.*\\.csv$", full.names = T)

# Get file sizes in bytes
file_sizes <- file.info(stat_learn_files)$size

# Select files that are larger than 40 KB (500 KB = 500 * 1024 bytes)
large_files <- stat_learn_files[file_sizes > 500 * 1024]

# now the quest data
quest_files <- list.files( "raw_data" ,pattern = "stat_learn_quest.*\\.csv$", full.names = F)

# delete all  the files which we cannot use
files_to_delete <- quest_files[grepl("^PARTICIPANT|^[3-9][0-9][0-9]|^[4-9][0-9]{2}|^300|^_|^stat",quest_files)]

quest_files_to_keep<-quest_files[!quest_files %in% files_to_delete]

#-----------------------------------------------------------------------------#
# move the files to the new raw_data folder
for (file in large_files){
  
  file.copy(file, 
            paste0("raw_data_2/", substr(file, 10, nchar(file)))
  )
  
}

for (file in quest_files_to_keep){
  
  file.copy( paste0("raw_data/",file), 
            paste0("raw_data_2/",file))
  
  
}
#------------------------------------------------------------------------------#
# some preprocessing
# remove all the files that are below 40kb
c_files<-list.files("raw_data_2", full.names = T)


file_sizes <- file.info(c_files)$size

# Select files that are larger than 5 KB (40 KB = 40 * 1024 bytes)
large_files <- c_files[file_sizes > 40 * 1024]

small_files<-c_files[!c_files %in% large_files]

# remove them
for (f in small_files){
  
  file.remove(f)
}


#------------------------------------------------------------------------------#

# remove the 062
file_062<-list.files("raw_data_2", pattern = "^062_*", full.names = T)

file.remove(file_062)

#------------------------------------------------------------------------------#
# rename the questionnaire 01-19-02 to 101
quest_01<-read.csv("raw_data_2/01_stat_learn_task_2025-02-18_11h29.09.560.csv")

quest_01$participant<-101

write.csv(quest_01, "raw_data_2/101_stat_learn_task_2025-02-18_11h29.09.560.csv", 
          row.names = F)


# delete the previous one
file.remove("raw_data_2/01_stat_learn_task_2025-02-18_11h29.09.560.csv")

#------------------------------------------------------------------------------#
# we have some repeated files - the 01s for example
# remove them
files_01s<-list.files("raw_data_2", pattern = "^01_*")

for (file in files_01s){
file.remove(paste0("raw_data_2/", file))
}


#------------------------------------------------------------------------------#
# we have two questionnaires for participant 21 - however, the are two days
# of difference with one of the two - delete it - for the task, however, 
# we have two participants with the same number thested at one hour difference to
# each other - I am deleting all the files 
files_21s<-list.files("raw_data_2", pattern = "^21_*")

# delete the first one
file.remove(c("raw_data_2/21_stat_learn_quest_2025-02-12_17h07.52.547.csv",
            "raw_data_2/21_stat_learn_task_2025-02-14_14h35.22.872.csv"  ,
            "raw_data_2/21_stat_learn_task_2025-02-14_15h44.30.665.csv"))

#------------------------------------------------------------------------------#
# we have FOUR  files for participant 58 - and no for 59
# therefore, we can assign to the latter ones the number 59
files_58s<-list.files("raw_data_2", pattern = "^58_*")

# the latter ones are the ones that start at a quarter to 8
file_58_q<-read.csv("raw_data_2/58_stat_learn_quest_2025-04-15_19h43.19.930.csv")

# change the participant's name
file_58_q$participant<-59

# write it
write.csv(file_58_q,"raw_data_2/59_stat_learn_quest_2025-04-15_19h43.19.930.csv", row.names = F )

# delete the previous file
file.remove(("raw_data_2/58_stat_learn_quest_2025-04-15_19h43.19.930.csv"))

# now the stat-learn_task - righrt before eight
file_58_s<-read.csv("raw_data_2/58_stat_learn_task_2025-04-15_19h57.22.480.csv")
file_58_s$participant<-59

write.csv(file_58_s,"raw_data_2/59_stat_learn_task_2025-04-15_19h57.22.480.csv", row.names = F )

# delete the previous file
file.remove("raw_data_2/58_stat_learn_task_2025-04-15_19h57.22.480.csv")
#------------------------------------------------------------------------------#
# we have two participants 72 - one tested on the 20/04 - one on the 22/04
# since we are missing the participant 71, the ones on the 22nd belong to 
# participant 71
files_72s<-list.files("raw_data_2", pattern = "^72_*")

# renamae and change particiopants 72 tested on the 20th of april
file_72_q<-read.csv("raw_data_2/72_stat_learn_quest_2025-04-20_10h10.40.731.csv")
file_72_q$participant<-71

write.csv(file_72_q,"raw_data_2/71_stat_learn_quest_2025-04-20_10h10.40.731.csv", row.names = F )

file.remove("raw_data_2/72_stat_learn_quest_2025-04-20_10h10.40.731.csv")

# now the stat-learn_task - righrt before eight
file_72_s<-read.csv("raw_data_2/72_stat_learn_task_2025-04-20_10h23.29.180.csv")
file_72_s$participant<-71

write.csv(file_72_s,"raw_data_2/71_stat_learn_task_2025-04-20_10h23.29.180.csv", row.names = F )

# delete the previous file
file.remove("raw_data_2/72_stat_learn_task_2025-04-20_10h23.29.180.csv")
#------------------------------------------------------------------------------#
# finally, deelete all the files with IDs above 300
cd<-getwd()

# set wd to raw_data_2
setwd("raw_data_2")

all_files<-list.files()

file_nums <- as.numeric(sub("^([0-9]+).*", "\\1", all_files))

# Find files that start with a number > 280
files_to_delete <- all_files[!is.na(file_nums) & file_nums > 300]

# delete them
file.remove(files_to_delete)

setwd(cd)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# finally, check if there are duplicates in the files' ID
# get the lists of the files for the questionniares
quest_files<-list.files('raw_data_2', pattern = "quest")

# extract IDs
quest_IDs<-sub("_.*", "", quest_files)

#Convert to numeric (optional, useful for sorting)
quest_IDs <- as.numeric(quest_IDs)

# Find duplicates
dup_ids_quest <- quest_IDs[duplicated(quest_IDs)]

#------------------------------------------------------------------------------#
# thee 213 is a duplicate. I have checked and they seem to be done by the same
# participant. The second one is more complete. Therefore, deleting the first one

files_213<-list.files("raw_data_2", pattern = "^213_*")


# delete the previous file
file.remove("raw_data_2/213_stat_learn_quest_2025-06-12_09h31.48.291.csv")


#------------------------------------------------------------------------------#
# for the 253, we have multiple files
files_253<-list.files("raw_data_2", pattern ="^253")

# according to the log book, participant 253 was tested on the 23/05. 
# I could not find any correspondents for the other files, and thus I am deleting them

file.remove(c("raw_data_2/253_stat_learn_quest_2025-06-03_12h43.10.091.csv",
              "raw_data_2/253_stat_learn_quest_2025-06-10_16h34.54.524.csv"))
#------------------------------------------------------------------------------#

# now do the same for the learning task

#------------------------------------------------------------------------------#
stat_learn_files<-list.files('raw_data_2', pattern = "task")

# extract IDs
quest_ID_learn<-sub("_.*", "", stat_learn_files)

#Convert to numeric (optional, useful for sorting)
quest_ID_learn <- as.numeric(quest_ID_learn)

# Find duplicates
dup_ids_learn <- quest_ID_learn[duplicated(quest_ID_learn)]
#------------------------------------------------------------------------------#
# we have a duplicated statisticalk  learning task for participant 281
# the wone that corresponds to the questionnaire is the one tested on the seventh of june. 
# delete the other
files_281<-list.files("raw_data_2", pattern = "^281_")
file.remove("raw_data_2/281_stat_learn_task_2025-06-19_11h43.28.508.csv" )
#------------------------------------------------------------------------------#
# now there are no more duplicates!
#------------------------------------------------------------------------------#


VoI<-c("accuracy", "RTms", "syllable_pos", "repetition", "sequence_type", 
       "trialN",  "slider_valence_2.response", "participant", "syllable", 
       "target_pos", "target")

feedback<-rep(c(1,1, 0, 0), times = 100 )

# select those 
full_df<-vector()

# get the stat_learn_files 
files_stat_learn<-list.files( "raw_data_2" ,pattern = "stat_learn_task.*\\.csv$", full.names = T)

for (f in files_stat_learn){
  
  tryCatch({
  c_file<-read.csv(f)
  
  # select files
  c_file<-c_file[, VoI]
  
  c_file$RTdiff<-as.numeric(NA)
  c_file$trialN<-NA
  # get variables for SAM
  count_t<-1
  for (n in 1:nrow(c_file)){
  if (!is.na(c_file$slider_valence_2.response[n])){
    
    sub_file<-c_file[(n-48):(n-1),]
    
    # get RT
    RT<-sub_file[!is.na(sub_file$RTms),]
    
    # clean na
    #RT<-RT[!is.na(RT)]
    
    # creating rt difference
    if (nrow(RT)>3){
    c_file$RTdiff[n]<-RT$RTms[RT$repetition==3][1]- RT$RTms[RT$repetition==1][1]
    }
    
    c_file$sequence_type[n]<-c_file$sequence_type[n-1]
      
    # get all the variables
    c_file$target_pos[n]<- c_file$target_pos[n-1]
    
    c_file$trialN<-count_t
    count_t<-count_t+1
  }

    
  }
  
  # get part
  part<-c_file$participant[1]
  
  c_file$feedback<-feedback[part]
  
  full_df<-rbind(full_df, c_file)
}, error = function(e){print(paste0("error with file", f))})
}


# select structured and unstructured only
full_df<-full_df[full_df$sequence_type=="structured" | 
                   full_df$sequence_type=="unstructured",]

# valence and arousal
full_df_val<-full_df[!is.na(full_df$slider_valence_2.response),]

# count by participant
count_SL<-full_df%>%
  group_by(participant, sequence_type, syllable_pos, repetition)%>%
  tally()

count_feedb<-full_df %>%
  group_by(participant, feedback) %>%
  slice(1)%>%
  ungroup()

count_val<-full_df_val%>%
  group_by(participant, target_pos, repetition)%>%
  tally()

# select only accuracy =1
full_df<-full_df[full_df$accuracy==1,]

