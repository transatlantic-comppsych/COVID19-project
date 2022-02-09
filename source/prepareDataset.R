prepareDataSet=function(dataset, columnNamePrefix){
  
  smfq_tminus1_long=dataset
  smfq_tminus1_long$SDAN=as.factor(smfq_tminus1_long$SDAN)
  
  smfq_tminus1_long$measureName <- smfq_tminus1_long[,paste0(columnNamePrefix,"_tot")]
  smfq_tminus1_long$measureDate <- smfq_tminus1_long[,paste0(columnNamePrefix,"_date")]
  smfq_tminus1_long$measureDate=as.Date(smfq_tminus1_long$measureDate)
  
  num_variables <- smfq_tminus1_long %>% select(matches("_tot"), "measureName") %>% colnames()
  #date_variables <- smfq_tminus1_long %>% select(matches("_date"), "measureDate") %>% colnames()
  
  smfq_tminus1_long[num_variables] <- lapply(smfq_tminus1_long[num_variables], as.numeric)
  #smfq_tminus1_long[date_variables] <- lapply(smfq_tminus1_long[date_variables], as.Date)
  
  names(smfq_tminus1_long)[names(smfq_tminus1_long) == "SEX"] <- "Gender"
  smfq_tminus1_long$Gender=as.factor(smfq_tminus1_long$Gender)
  
  split1 <- colsplit(smfq_tminus1_long$measureDate, "-", names = c("year", "month", "day"))
  smfq_tminus1_long <- cbind(smfq_tminus1_long, split1)
  #smfq_tminus1_long <- smfq_tminus1_long %>% mutate(day2 = ifelse(day < 7, 1, ifelse((day >= 7 & day <14), 7, ifelse((day >=14 & day < 21), 14, ifelse(day >=21, 21, 0)))))
  smfq_tminus1_long <- smfq_tminus1_long %>% mutate(day2 = ifelse(day < 15, 7.5, 22.5))
  
  smfq_tminus1_long$measureDate3c <- as.Date(paste(smfq_tminus1_long$year, smfq_tminus1_long$month, smfq_tminus1_long$day2, sep="-"))
  
  smfq_tminus1_long_before <- smfq_tminus1_long %>% filter(measureDate >= date1 & measureDate < covidTime) %>% group_by(SDAN) 
  smfq_tminus1_long_after <- smfq_tminus1_long %>% filter(measureDate >= covidTime) %>% group_by(SDAN) 
  
  print("Before Pandemic")
  print(paste("N observations: ",dim(smfq_tminus1_long_before)[1]))
  print(paste("N subjects: ", length(unique(smfq_tminus1_long_before$SDAN))))
  
  mfqDataBeforePandemic = smfq_tminus1_long_before %>% mutate(numberOfMonths=as.numeric(measureDate - date1)/30.5)
  mfqDataAfterPandemic = smfq_tminus1_long_after %>% mutate(numberOfMonths=as.numeric(measureDate - covidTime)/30.5)
  
  #I think Payton has added this to the important_dataframe_maker.R so it might not be needed here anymore
  mfqDataAfterPandemic2 = mfqDataAfterPandemic %>% filter(SDAN %in% mfqDataBeforePandemic$SDAN) #include only the ones that we have prepandemic data - this is the same since Payton alreadey filtered that
  print("Number of observations after pandemic -- only the ones with before pandemic data")
  print(paste("N observations: ",dim(mfqDataAfterPandemic2)[1]))
  print(paste("N subjects: ", length(unique(mfqDataAfterPandemic2$SDAN))))
  
  beforeAndAfter=rbind(mfqDataBeforePandemic, mfqDataAfterPandemic2)
  beforeAndAfter2=beforeAndAfter %>% mutate(year=ifelse(measureDate < spring2020, "2019","2020")) #spring2020 same as covidTime
  beforeAndAfter2$year=as.factor(beforeAndAfter2$year)
  
  names(beforeAndAfter2)=str_replace(names(beforeAndAfter2),"Participant_Type2","Group")
  names(beforeAndAfter2)=str_replace(names(beforeAndAfter2),"SDAN","Participant")
  names(beforeAndAfter2)=str_replace(names(beforeAndAfter2),"numberOfMonths","Time")
  names(beforeAndAfter2)=str_replace(names(beforeAndAfter2),"year","Year")
  return(beforeAndAfter2)
}