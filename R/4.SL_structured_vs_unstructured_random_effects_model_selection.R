#------------------------------------------------------------------------------#
# Dertermine random structure for the comparison structured vs unstructured
#------------------------------------------------------------------------------#


stat_learn_mod_comp_str_full_rand_wcov<-lmer(RTms ~ 1+
                                               (1+sequence_type+syllable_order_n_z+trial_num|participant ) ,
                                             data = full_df[is.na(full_df$target_pos) |full_df$target_pos==3, ],
                                             control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), 
                                             REML=F)


summary(stat_learn_mod_comp_str_full_rand_wcov)

stat_learn_mod_comp_str_no_syllaborder_wcov<-lmer(RTms ~ 1+
                                                    (1+sequence_type+trial_num|participant ) ,
                                                  data = full_df[is.na(full_df$target_pos) |full_df$target_pos==3, ],
                                                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), 
                                                  REML=F)

anova(stat_learn_mod_comp_str_full_rand_wcov,stat_learn_mod_comp_str_no_syllaborder_wcov)

stat_learn_mod_comp_str_no_trial_n_wcov<-lmer(RTms ~ 1+
                                                (1+sequence_type+syllable_order_n_z|participant ) ,
                                              data = full_df[is.na(full_df$target_pos) |full_df$target_pos==3, ],
                                              control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), 
                                              REML=F)

anova(stat_learn_mod_comp_str_full_rand_wcov,stat_learn_mod_comp_str_no_trial_n_wcov)



stat_learn_mod_comp_str_no_sequence_w_cov<-lmer(RTms ~ 1+
                                                  (1+syllable_order_n_z+trial_num|participant ) ,
                                                data = full_df[is.na(full_df$target_pos) |full_df$target_pos==3, ],
                                                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)), 
                                                REML=F)

anova(stat_learn_mod_comp_str_full_rand_wcov,stat_learn_mod_comp_str_no_sequence_w_cov)

contrasts(full_df$feedback)

full_df$feedback2<-as.factor(full_df$feedback2)
contrasts(full_df$feedback2)<-contr.sum(2)

full_df$sequence_type_f<-as.factor(full_df$sequence_type)
contrasts(full_df$sequence_type_f)<-contr.sum(2)