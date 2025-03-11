
library(survivoR)
library(dplyr)
library(tidyr)
library(stringr)
library(DescTools)

votes = vote_history
cast = castaways

seasons = unique(votes$version_season)
us_seasons = seasons[grepl("US",seasons)]


#Calculate Gini Impurity for Starting Tribes
found = 0
stack = NA
for(i in seq(from=1,to=length(us_seasons),by=1)){
  temp = votes %>% filter(version_season == us_seasons[i])
  orig_tribe = cast %>% filter(version_season == us_seasons[i]) %>% select(castaway,original_tribe) %>% distinct()
  
  temp2 = temp %>% filter(tribe_status %in% c("Merged","Mergatory"))
  
  distincts = temp2 %>% filter(!is.na(vote)) %>% group_by(episode, order, vote_order) %>% summarise(count=n()) %>% ungroup()
  
  for(j in seq(from=1,to=dim(distincts)[1],by=1)){
    cat(j)
    temp3 = temp2 %>% filter(episode == unlist(distincts[j,"episode"]) &
                             order == unlist(distincts[j,"order"]) &
                             vote_order == unlist(distincts[j,"vote_order"]) &
                             !is.na(vote)) %>%
                      select(castaway,vote,split_vote) %>%
                      left_join(.,orig_tribe,by="castaway") %>%
                      mutate(vote_final = ifelse(!is.na(split_vote),split_vote,vote)) %>%
                      ungroup()
    
    gini_calc = temp3 %>% group_by(vote_final,original_tribe) %>% 
                          summarise(count=n()) %>% 
                          spread(original_tribe,count) %>%
                          replace(is.na(.), 0) %>%
                          mutate(total = rowSums(across(where(is.numeric)))) %>%
                          ungroup()
    
    max_tribes = dim(gini_calc)[2] - 2
    
    gini_calc$purity = NA
    
    for(k in seq(from=1,to=dim(gini_calc)[1],by=1)){
      if(max_tribes == 1){
        gini_calc[k,"purity"] = 1 
      } else if(max_tribes == 2){
        gini_calc[k,"purity"] = 1 - (gini_calc[k,2]/(gini_calc[k,2]+gini_calc[k,3]))^2 - (gini_calc[k,3]/(gini_calc[k,2]+gini_calc[k,3]))^2
      } else if(max_tribes == 3){
        gini_calc[k,"purity"] = 1 - (gini_calc[k,2]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]))^2 - (gini_calc[k,3]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]))^2 - (gini_calc[k,4]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]))^2
      } else if(max_tribes == 4){
        gini_calc[k,"purity"] = 1 - (gini_calc[k,2]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]))^2 - (gini_calc[k,3]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]))^2 - (gini_calc[k,4]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]))^2 - (gini_calc[k,5]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]))^2
      } else {
        cat("Error: Number of Tribes")
      }
    }
    
    if(max_tribes == 1) {
      totals = gini_calc$total
      totals2 = (totals / sum(totals))^2
      gini = 1 - sum(totals2)
    } else {
      gini = weighted.mean(gini_calc$purity,gini_calc$total)
    }
    
    #Pile-On Indicator
    poi_data = temp2 %>% filter(episode == unlist(distincts[j,"episode"]) &
                                  order == unlist(distincts[j,"order"]) &
                                  vote_order == unlist(distincts[j,"vote_order"]) &
                                  !is.na(vote)) %>%
      select(castaway,vote) %>%
      group_by(vote) %>%
      summarise(count=n()) %>%
      select(count)
    
    if(dim(poi_data)[1]<2 | (dim(poi_data)[1]==2 & min(poi_data$count)==1)) {
      pile_on = 1
    } else {
      pile_on = 0
    }
    
    obs = as.data.frame(cbind(us_seasons[i],distincts[j,"episode"],distincts[j,"order"],distincts[j,"vote_order"],gini,pile_on))
    
    if(found == 0){
      stack = obs
      found = 1
    } else {
      stack = rbind(stack,obs)
    }
  }
}

colnames(stack) = c("Season","Episode","Order","Vote_Order","Gini","Pile_On_Indicator")
summary = stack %>% group_by(Season) %>% summarise(count=n(),avg_gini = mean(Gini))
summary2 = stack %>% filter(Pile_On_Indicator == 0) %>% group_by(Season) %>% summarise(count=n(),avg_gini = mean(Gini))


#Calculate Gini Impurity for vote-to-vote
found2 = 0
stack2 = NA
for(i in seq(from=1,to=length(us_seasons),by=1)){
  temp = votes %>% filter(version_season == us_seasons[i])
  orig_tribe = cast %>% filter(version_season == us_seasons[i]) %>% select(castaway,original_tribe) %>% distinct()
  
  temp2 = temp %>% filter(tribe_status %in% c("Merged","Mergatory"))
  
  distincts = temp2 %>% filter(!is.na(vote)) %>%
                        group_by(episode, order, vote_order) %>% 
                        summarise(count=n()) %>% 
                        ungroup() %>%
                        group_by(episode,order) %>%
                        filter(vote_order == max(vote_order)) %>%
                        ungroup() %>%
                        mutate(players_left = dim(orig_tribe)[1]-order+1) %>%
                        mutate(players_left = ifelse(us_seasons[i]=="US40" & episode >= 14,players_left + 2,
                                                     ifelse(us_seasons[i]=="US40" & episode >= 8, players_left + 1,
                                                            ifelse(us_seasons[i]=="US38" & episode >= 13, players_left + 2,
                                                                   ifelse(us_seasons[i]=="US38" & episode >= 6, players_left + 1,
                                                                          ifelse(us_seasons[i]=="US27" & episode >= 14, players_left + 3,
                                                                                 ifelse(us_seasons[i]=="US27" & episode >= 8, players_left + 2,
                                                                                        ifelse(us_seasons[i]=="US23" & episode >= 15, players_left + 2,
                                                                                               ifelse(us_seasons[i]=="US23" & episode >= 8, players_left + 1,
                                                                                                      ifelse(us_seasons[i]=="US22" & episode >= 14, players_left + 2,
                                                                                                             ifelse(us_seasons[i]=="US22" & episode >= 8, players_left + 1,
                                                                                                                    ifelse(us_seasons[i]=="US07" & episode >= 8, players_left + 2,
                                                                                                                           players_left)))))))))))) %>% #Edge of Extinction or Battle Back Adjustments
                        left_join(.,temp %>% mutate(adjustment = ifelse(vote_event_outcome %in% c("Extra vote"),1,
                                                                        ifelse(vote_event %in% c("Vote blocker","Lost vote","Removal from tribal","Safe","Not safe","No vote"),-1,0))) %>%
                                             group_by(episode, order, vote_order) %>% 
                                             summarise(people_at_tribal = n()-sum(adjustment),
                                                       adj = sum(adjustment)) %>%
                                             select(episode,order,vote_order,people_at_tribal,adj),
                                  by = c("episode","order","vote_order")) %>%
                        filter(count > 1 & (players_left == people_at_tribal)) #Remove final 3 votes and split tribals
  
  for(j in seq(from=2,to=dim(distincts)[1],by=1)){
    cat(j)
    temp3 = temp2 %>% filter(episode == unlist(distincts[j,"episode"]) &
                               order == unlist(distincts[j,"order"]) &
                               vote_order == unlist(distincts[j,"vote_order"]) &
                               !is.na(vote)) %>%
                      select(castaway,vote,split_vote) %>%
                      mutate(vote_final = ifelse(!is.na(split_vote),split_vote,vote)) %>%
                      select(castaway,vote_final) %>%
                      inner_join(.,temp2 %>% filter(episode == unlist(distincts[j-1,"episode"]) &
                                                    order == unlist(distincts[j-1,"order"]) &
                                                    vote_order == unlist(distincts[j-1,"vote_order"]) &
                                                    !is.na(vote)) %>%
                                             select(castaway,vote,split_vote) %>%
                                             mutate(vote_final_prev = ifelse(!is.na(split_vote),split_vote,vote)) %>%
                                             select(castaway,vote_final_prev),
                                 by="castaway")
    
    gini_calc = temp3 %>% group_by(vote_final,vote_final_prev) %>% 
      summarise(count=n()) %>% 
      spread(vote_final_prev,count) %>%
      replace(is.na(.), 0) %>%
      mutate(total = rowSums(across(where(is.numeric)))) %>%
      ungroup()
    
    max_vote_prev = dim(gini_calc)[2] - 2
    
    gini_calc$purity = NA
    
    for(k in seq(from=1,to=dim(gini_calc)[1],by=1)){
      if(max_vote_prev == 1){
        gini_calc[k,"purity"] = 1 
      } else if(max_vote_prev == 2){
        gini_calc[k,"purity"] = 1 - (gini_calc[k,2]/(gini_calc[k,2]+gini_calc[k,3]))^2 - (gini_calc[k,3]/(gini_calc[k,2]+gini_calc[k,3]))^2
      } else if(max_vote_prev == 3){
        gini_calc[k,"purity"] = 1 - (gini_calc[k,2]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]))^2 - (gini_calc[k,3]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]))^2 - (gini_calc[k,4]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]))^2
      } else if(max_vote_prev == 4){
        gini_calc[k,"purity"] = 1 - (gini_calc[k,2]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]))^2 - (gini_calc[k,3]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]))^2 - (gini_calc[k,4]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]))^2 - (gini_calc[k,5]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]))^2
      } else if(max_vote_prev == 5){
        gini_calc[k,"purity"] = 1 - (gini_calc[k,2]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]))^2 - (gini_calc[k,3]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]))^2 - (gini_calc[k,4]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]))^2 - (gini_calc[k,5]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]))^2 - (gini_calc[k,6]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]))^2
      } else if(max_vote_prev == 6){
        gini_calc[k,"purity"] = 1 - (gini_calc[k,2]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]+gini_calc[k,7]))^2 - (gini_calc[k,3]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]+gini_calc[k,7]))^2 - (gini_calc[k,4]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]+gini_calc[k,7]))^2 - (gini_calc[k,5]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]+gini_calc[k,7]))^2 - (gini_calc[k,6]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]+gini_calc[k,7]))^2 - (gini_calc[k,7]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]+gini_calc[k,7]))^2
      } else if (max_vote_prev == 7){
        gini_calc[k,"purity"] = 1 - (gini_calc[k,2]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]+gini_calc[k,7]+gini_calc[k,8]))^2 - (gini_calc[k,3]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]+gini_calc[k,7]+gini_calc[k,8]))^2 - (gini_calc[k,4]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]+gini_calc[k,7]+gini_calc[k,8]))^2 - (gini_calc[k,5]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]+gini_calc[k,7]+gini_calc[k,8]))^2 - (gini_calc[k,6]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]+gini_calc[k,7]+gini_calc[k,8]))^2 - (gini_calc[k,7]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]+gini_calc[k,7]+gini_calc[k,8]))^2 - (gini_calc[k,8]/(gini_calc[k,2]+gini_calc[k,3]+gini_calc[k,4]+gini_calc[k,5]+gini_calc[k,6]+gini_calc[k,7]+gini_calc[k,8]))^2
      } else {
        cat("Error: Number of Tribes")
      }
    }
    
    if(max_vote_prev == 1) {
      totals = gini_calc$total
      totals2 = (totals / sum(totals))^2
      gini = 1 - sum(totals2)
    } else {
      gini = weighted.mean(gini_calc$purity,gini_calc$total)
    }
    
    #Pile-On Indicator
    poi_data = temp2 %>% filter(episode == unlist(distincts[j,"episode"]) &
                                  order == unlist(distincts[j,"order"]) &
                                  vote_order == unlist(distincts[j,"vote_order"]) &
                                  !is.na(vote)) %>%
      select(castaway,vote) %>%
      group_by(vote) %>%
      summarise(count=n()) %>%
      select(count)
    
    if(dim(poi_data)[1]<2 | (dim(poi_data)[1]==2 & min(poi_data$count)==1)) {
      pile_on = 1
    } else {
      pile_on = 0
    }
    
    obs = as.data.frame(cbind(us_seasons[i],distincts[j,"episode"],distincts[j,"order"],distincts[j,"vote_order"],gini,pile_on))
    
    if(found2 == 0){
      stack2 = obs
      found2 = 1
    } else {
      stack2 = rbind(stack2,obs)
    }
  }
}

colnames(stack2) = c("Season","Episode","Order","Vote_Order","Gini","Pile_On_Indicator")
summary3 = stack2 %>% group_by(Season) %>% summarise(count=n(),avg_gini = mean(Gini))
summary4 = stack2 %>% filter(Pile_On_Indicator == 0) %>% group_by(Season) %>% summarise(count=n(),avg_gini = mean(Gini))

