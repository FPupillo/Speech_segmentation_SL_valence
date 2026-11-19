#------------------------------------------------------------------------------#
mod_val_diff_full_target_pos_inter<-lmer(val_diff~
                                           (target_pos*trial_new_c| participant), 
                                         control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), 
                                         data =full_df_val[full_df_val$sequence_type=="structured",],REML=F)

mod_val_diff_full_target_pos_no_inter<-lmer(val_diff~
                                              (target_pos+trial_new_c| participant), 
                                            control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), 
                                            data =full_df_val[full_df_val$sequence_type=="structured",],REML=F)

anova(mod_val_diff_full_target_pos_inter,mod_val_diff_full_target_pos_no_inter)

mod_val_diff_full_target_pos_no_trial<-lmer(val_diff~
                                              (target_pos| participant),
                                            control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                            data =full_df_val[full_df_val$sequence_type=="structured",],REML=F)

mod_val_diff_full_target_pos_no_target_pos<-lmer(val_diff~
                                                   (trial_new_c| participant),
                                                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                                 data =full_df_val[full_df_val$sequence_type=="structured",],REML=F)


anova(mod_val_diff_full_target_pos_no_inter,mod_val_diff_full_target_pos_no_trial)

anova(mod_val_diff_full_target_pos_no_inter,mod_val_diff_full_target_pos_no_target_pos)