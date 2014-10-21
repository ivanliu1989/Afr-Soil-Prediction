submit <- read.csv('submission_new/11OCT_2.csv', sep=',')
submit_new_row <- read.csv('ivanliu1989_AfSIS_model_submission/2014-10-22_[feature_engineer_SavitzkyGolay]_[fit_method_svmRadial]_[cv_repeats_10]_[cv_numbers_10]_[cv_method_row].csv', sep=',')
submit_new_loc <- read.csv('ivanliu1989_AfSIS_model_submission/2014-10-22_[feature_engineer_SavitzkyGolay]_[fit_method_svmRadial]_[cv_repeats_10]_[cv_numbers_10]_[cv_method_location].csv', sep=',')

head(submit); head(submit_new_row); head(submit_new_loc); 

submit_df <- submit
submit_df$Ca <- submit_new_loc$Ca
submit_df$P <- submit_new_loc$P
submit_df$pH <- submit_new_row$pH
submit_df$SOC <- submit_new_loc$SOC
submit_df$Sand <- submit_new_loc$Sand

submit_df$SOC <- submit$SOC
head(submit_df)

fileName <- 'ivanliu1989_AfSIS_model_submission/row_loc_submit.csv'
write.csv(submit_df, fileName, row.names=FALSE)
