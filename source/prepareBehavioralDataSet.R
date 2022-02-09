prepareBehavioralDataSet= function(dataset){
  
  date_variables <- dataset %>% select(matches("_date")) %>% colnames()
  dataset[date_variables] <- lapply(dataset[date_variables], as.Date)

  ct2=dataset %>% mutate(s_worry_total=s_crisis_6_worry_self_infected+ s_crisis_7_worry_family_infected  
                     + s_crisis_8_worry_phys_health + s_crisis_9_worry_ment_health +
                       s_crisis_10_reading) %>% mutate(p_worry_total=p_crisis_6_worry_self_infected+ p_crisis_7_worry_family_infected  
                                                      + p_crisis_8_worry_phys_health + p_crisis_9_worry_ment_health +
                                                        p_crisis_10_reading)
  
  ct2$s_crisis_19_relationships_family <- dplyr :: recode(ct2$s_crisis_19_relationships_family, 
                                                          `0` = 4,
                                                          `1` = 3, 
                                                          `2` = 2,
                                                          `3` = 1,
                                                          `4` = 0
  )


  ct2$s_crisis_21_relationships_friends <- dplyr :: recode(ct2$s_crisis_21_relationships_friends, 
                                                           `0` = 4,
                                                           `1` = 3, 
                                                           `2` = 2,
                                                           `3` = 1,
                                                           `4` = 0
  )
  

  #binarise sleep to 0 and 0
  ct2=ct2 %>% mutate (s_crisis_28_sleep1=recode(ct2$s_crisis_28_sleep,
                                                `0` = 1,
                                                `1` = 0, 
                                                `2` = 0,
                                                `3` = 0
  ))

  ct2=ct2 %>% mutate(s_crisis_28_sleep2=recode(ct2$s_crisis_28_sleep,
                                               `0` = 0,
                                               `1` = 0, 
                                               `2` = 0,
                                               `3` = 1
  ))
  
  ct2$p_crisis_19_relationships_family <- dplyr :: recode(ct2$p_crisis_19_relationships_family, 
                                                          `0` = 4,
                                                          `1` = 3, 
                                                          `2` = 2,
                                                          `3` = 1,
                                                          `4` = 0
  )
  
  
  ct2$p_crisis_21_relationships_friends <- dplyr :: recode(ct2$p_crisis_21_relationships_friends, 
                                                           `0` = 4,
                                                           `1` = 3, 
                                                           `2` = 2,
                                                           `3` = 1,
                                                           `4` = 0
  )
  
  #had recoded exercise and time spent outside to be in sync with other measurements, but made it harder to discuss it in the recoded terms
  
  #binarise sleep to 0 and 0
  ct2=ct2 %>% mutate (p_crisis_28_sleep1=recode(ct2$p_crisis_28_sleep,
                                                `0` = 1,
                                                `1` = 0, 
                                                `2` = 0,
                                                `3` = 0
  ))
  
  ct2=ct2 %>% mutate(p_crisis_28_sleep2=recode(ct2$p_crisis_28_sleep,
                                               `0` = 0,
                                               `1` = 0, 
                                               `2` = 0,
                                               `3` = 1
  ))


  someData=ct2 %>% select(SDAN,SEX, Age_at_visit, c_ksadsdx_primary_dx,Participant_Type2, s_crisis_date, s_mfq_tot,s_mfq_date,s_crisis_19_relationships_family, 
                          s_crisis_21_relationships_friends, s_worry_total,s_scared_combined_tot, s_crisis_28_sleep, s_crisis_28_sleep1, s_crisis_28_sleep2, s_crisis_29_exercise, 
                          s_crisis_30_outdoors, s_crisis_43_video_games, s_crisis_42_soc_media, s_crisis_41_tv, p_crisis_date, p_mfq_tot, p_scared_combined_tot, p_worry_total,
                          p_crisis_19_relationships_family, p_crisis_21_relationships_friends, s_worry_total,s_scared_combined_tot, p_crisis_28_sleep, p_crisis_28_sleep1,
                          p_crisis_28_sleep2, p_crisis_29_exercise, p_crisis_30_outdoors, p_crisis_43_video_games, p_crisis_42_soc_media, p_crisis_41_tv)
  
  idsThatChangedClass=someData %>% group_by(SDAN, c_ksadsdx_primary_dx) %>% dplyr::summarize(count=n()) %>% ungroup %>% group_by(SDAN) %>% dplyr::summarize(count=n()) %>% filter(count != 1)
  someData=someData%>%filter(c_ksadsdx_primary_dx=="MDD" | c_ksadsdx_primary_dx=="Healthy")
  someData = someData %>% filter(!SDAN %in% idsThatChangedClass$SDAN)%>%select(-c_ksadsdx_primary_dx)
  someData = someData %>% filter(!is.na(s_crisis_date))
  someData$SEX=toupper(someData$SEX)
  
  names(someData)=str_replace(names(someData),"Participant_Type2","Group")
  names(someData)=str_replace(names(someData),"SDAN","Participant")
  names(someData)=str_replace(names(someData),"numberOfMonths","Time")
  names(someData)=str_replace(names(someData),"year","Year")
  names(someData)=str_replace(names(someData),"SEX","Gender")
  
  return(someData)
}

