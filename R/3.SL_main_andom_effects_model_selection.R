#------------------------------------------------------------------------------#
# Statistical learning model comparison
#------------------------------------------------------------------------------#

# get trial number 
full_df$trial_num<-full_df$trialN.1

# turn target repetition to factor
full_df$target_rep<-as.factor(full_df$target_rep)

# full random model
stat_learn_mod_full_rand<-lmer(RTms ~ 1+
                                 (target_pos*target_rep+syllable_order_n_z||participant )  ,
                               data = full_df[ 
                                 full_df$sequence_type=="structured" ,], 
                               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                               REML=F)

# add the covariance
stat_learn_mod_full_wcov<-lmer(RTms ~ 1+
                                 (target_pos*target_rep+syllable_order_n_z|participant )  ,
                               data = full_df[ 
                                 full_df$sequence_type=="structured" ,], 
                               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                               REML=F)

anova(stat_learn_mod_full_wcov,stat_learn_mod_full_rand)

stat_learn_mod_no_syll_num<-lmer(RTms ~ 1+
                                   (target_pos*target_rep||participant ) ,
                                 data = full_df[ 
                                   full_df$sequence_type=="structured" ,], 
                                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                 REML=F)

stat_learn_mod_no_syll_num_wcov<-lmer(RTms ~ 1+
                                        (target_pos*target_rep||participant ) ,
                                      data = full_df[ 
                                        full_df$sequence_type=="structured" ,], 
                                      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                      REML=F)

test_syll<-anova(stat_learn_mod_full_rand,stat_learn_mod_no_syll_num)



stat_learn_mod_no_inter<-lmer(RTms ~ 1+
                                (target_pos+target_rep+syllable_order_n_z||participant ) ,
                              data = full_df[ 
                                full_df$sequence_type=="structured" ,], 
                              control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                              REML=F)

stat_learn_mod_no_inter_wcov<-lmer(RTms ~ 1+
                                     (target_pos+target_rep+syllable_order_n_z|participant ) ,
                                   data = full_df[ 
                                     full_df$sequence_type=="structured" ,], 
                                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                   REML=F)


test_inter_wcov<-anova(stat_learn_mod_full_wcov,stat_learn_mod_no_inter_wcov)


# now take out the syllnum
stat_learn_mod_no_inter_no_syll<-lmer(RTms ~ 1+
                                        (target_pos+target_rep||participant ) ,
                                      data = full_df[ 
                                        full_df$sequence_type=="structured" ,], 
                                      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                      REML=F)

anova(stat_learn_mod_no_inter_no_syll, stat_learn_mod_no_inter)

stat_learn_mod_no_inter_no_syll_wcov<-lmer(RTms ~ 1+
                                             (target_pos+target_rep|participant ) ,
                                           data = full_df[ 
                                             full_df$sequence_type=="structured" ,], 
                                           control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                           REML=F)

test_syll_n_wcov<-anova(stat_learn_mod_no_inter_no_syll_wcov,stat_learn_mod_no_inter_wcov)


# now take the target pos
stat_learn_no_target_pos<- lmer(RTms ~ 1+
                                  (target_rep+syllable_order_n_z||participant ) ,
                                data = full_df[ 
                                  full_df$sequence_type=="structured" ,], 
                                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),REML=F)

stat_learn_no_target_pos_wcov<- lmer(RTms ~ 1+
                                       (target_rep+syllable_order_n_z|participant ) ,
                                     data = full_df[ 
                                       full_df$sequence_type=="structured" ,], 
                                     control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),REML=F)

anova(stat_learn_mod_no_inter,stat_learn_no_target_pos_wcov)

anova(stat_learn_no_target_pos_wcov,stat_learn_mod_no_inter_wcov)

# now take the target rep
stat_learn_no_target_rep<- lmer(RTms ~ 1+
                                  (target_pos+syllable_order_n_z||participant ) ,
                                data = full_df[ 
                                  full_df$sequence_type=="structured" ,], 
                                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),REML=F)

stat_learn_no_target_rep_wcov<- lmer(RTms ~ 1+
                                       (target_pos+syllable_order_n_z|participant ) ,
                                     data = full_df[ 
                                       full_df$sequence_type=="structured" ,], 
                                     control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),REML=F)

anova(stat_learn_mod_no_inter_wcov,stat_learn_no_target_rep_wcov)


stat_learn_mod_w_cov_no_syll_num_no_inter<-lmer(RTms ~ 1+
                                                  (target_pos+target_rep|participant ) ,
                                                data = full_df[ 
                                                  full_df$sequence_type=="structured" ,], 
                                                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                                REML=F)

stat_learn_mod_no_int<-lmer(RTms ~ target_pos*target_rep+syllable_order_n+
                              (target_pos+target_rep||participant ) ,
                            data = full_df[ 
                              full_df$sequence_type=="structured" ,], 
                            control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

Anova(stat_learn_mod, type = 3)

