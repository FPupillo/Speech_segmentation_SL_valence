#------------------------------------------------------------------------------#
# model selection for random effects structure for arousal difference and 
# target position
#------------------------------------------------------------------------------#

mod_target_pos_ar_full_diff<-lmer(arous_diff~1+
                                    (target_pos*trial_new_c|participant), 
                                  data =full_df_val[full_df_val$sequence_type=="structured",], REML = F )


# higher arousal for structured

# check on the target pos
mod_target_pos_ar_diff_no_inter<-lmer(arous_diff~1+
                                        (target_pos+trial_new_c|participant), 
                                      data =full_df_val[full_df_val$sequence_type=="structured",], REML = F )

anova(mod_target_pos_ar_full_diff,mod_target_pos_ar_diff_no_inter)

mod_target_pos_ar_diff_no_trial<-lmer(arous_diff~1+
                                        (target_pos|participant),
                                      data =full_df_val[full_df_val$sequence_type=="structured",], REML = F )


anova(mod_target_pos_ar_diff_no_inter,mod_target_pos_ar_diff_no_trial)

mod_target_pos_ar_diff_no_sequence<-lmer(arous_diff~1+
                                           (trial_new_c|participant),
                                         data =full_df_val[full_df_val$sequence_type=="structured",])

anova(mod_target_pos_ar_diff_no_inter,mod_target_pos_ar_diff_no_sequence)


