#------------------------------------------------------------------------------#
# random effect model determination for arousal difference regressed on sequence_type
#------------------------------------------------------------------------------#
mod_sequence_pos_ar_diff_full<-lmer(arous_diff~1+
                                      (sequence_type*trial_new_c|participant), 
                                    data =full_df_val, REML = F )


# higher arousal for structured

# check on the target pos
mod_sequence_pos_ar_diff_no_inter<-lmer(arous_diff~1+
                                          (sequence_type+trial_new_c|participant), 
                                        data =full_df_val, REML = F )

anova(mod_sequence_pos_ar_diff_full,mod_sequence_pos_ar_diff_no_inter)

mod_seqeunce_ar_diff_no_trial<-lmer(arous_diff~1+
                                      (sequence_type|participant),
                                    data =full_df_val, REML = F )


anova(mod_sequence_pos_ar_diff_no_inter,mod_seqeunce_ar_diff_no_trial)
# 
mod_sequence_ar_diff_no_sequence<-lmer(arous_diff~1+
                                         (trial_new_c|participant),
                                       data =full_df_val , REML = F)

anova(mod_sequence_pos_ar_diff_no_inter,mod_sequence_ar_diff_no_sequence)
