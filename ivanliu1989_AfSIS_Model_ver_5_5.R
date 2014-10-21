#2 Ca - SG / Row
#1 P - FD / Row / log
#3 pH - SG / Loc 
#3 SOC - SG / Loc
#2 Sand - SG / Row

# P => Ca, Sand => pH, SOC

P <- p_test_df[,1]
Ca <- p_test_df[,2]
Sand <- p_test_df[,3]
pH <- p_test_df[,4]
SOC <- p_test_df[,5]

submit <- read.csv('submission_new/11OCT_2.csv', sep=',')
submit_df <- submit 
submit_df$Ca <- Ca
submit_df$P <- P
submit_df$pH <- pH
submit_df$SOC <- SOC
submit_df$Sand <- Sand

head(submit); head(submit_df)

fileName <- 'C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/AfSIS/ivanliu1989_AfSIS_model_submission/ivanliu1989_model_submit.csv'
write.csv(submit_df, fileName, row.names=FALSE)