#------------------------------------------------------------------------------#
# random effect structure for structure type analysis
#------------------------------------------------------------------------------#

mod_val_diff_full_sequence_inter<-lmer(val_diff~
                                         (sequence_type*trial_new_c| participant), 
                                       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), 
                                       data =full_df_val[is.na(full_df_val$target_pos)|full_df_val$target_pos==3, ] ,REML=F)

mod_val_diff_full_sequence<-lmer(val_diff~
                                   (sequence_type+trial_new_c| participant), 
                                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), 
                                 data =full_df_val[is.na(full_df_val$target_pos)|full_df_val$target_pos==3, ] ,REML=F)

anova(mod_val_diff_full_sequence_inter,mod_val_diff_full_sequence)


mod_val_diff_sequence_no_trial<-lmer(val_diff~
                                       (sequence_type| participant), 
                                     control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), 
                                     data =full_df_val[is.na(full_df_val$target_pos)|full_df_val$target_pos==3, ] ,REML=F)

mod_val_diff_no_sequence<-lmer(val_diff~
                                 (trial_new_c| participant), 
                               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), 
                               data =full_df_val[is.na(full_df_val$target_pos)|full_df_val$target_pos==3, ] ,REML=F)

anova(mod_val_diff_full_sequence,mod_val_diff_sequence_no_trial)

anova(mod_val_diff_full_sequence,mod_val_diff_no_sequence)

mod_val_diff_sequence_part<-lmer(val_diff~
                                   (1| participant), 
                                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), 
                                 data =full_df_val ,REML=F)

