#Test Markov

set.seed(1234)

Short_List <- Rate_List %>% 
  mutate(impact_r = round(effect_rate)) %>% 
  select(share,impact_r)

effect <- Rate_List %>%  
  mutate(effect = 1.5*(((Rate_List$effect_rate - min(Rate_List$effect_rate)) / 
                                             (max(Rate_List$effect_rate) - min(Rate_List$effect_rate))))) %>% 
  select(effect)

sy <- Short_List %>% filter(share == 1)
sn <- Short_List %>% filter(share == 0)
  
SLY <- as.tibble(sy)
SLN <- as.tibble(sn)
i <- 1
max <- 100
dr <- runif(10000,0.05,0.15)

for (i in  1:max) {
  
  days <- paste("day", 1:max, sep = "")
  #q <- matrix(days,1,max)
  #t <- as.character(q[1,i])
  
  SLN <- print(SLN %>% add_column(!!(days[i]) := 0))
  
  mutate(SLN, day1 = impact_r)
  
  if (i >= 1){
    
    SLN[2+i] <- SLN[(2+i)-1] + round(rt(n = 10000, df = 10), 1)
    SLN[2+i] <- replace(SLN[2+i], SLN[2+i] < 0, 0)
    SLN[2+i] <- SLN[2+i] + ifelse(SLN[[2+i]] >= 15, 1.2, 0)
    SLN[2+i] <- SLN[2+i] - dr
    
  }
  
  print(SLN)
  
}

for (i in  1:max) {
  
  days <- paste("day", 1:max, sep = "")
  #q <- matrix(days,1,max)
  #t <- as.character(q[1,i])
  
  SLY <- print(SLY %>% add_column(!!(days[i]) := 0))
  
  mutate(SLY, day1 = impact_r)
  
  if (i >= 1){
    
    SLY[2+i] <- SLY[(2+i)-1] + round(rt(n = 10000, df = 10),1)
    SLY[2+i] <- replace(SLY[2+i], SLY[2+i] < 0, 0)
    SLY[2+i] <- SLY[2+i] + ifelse(SLY[[2+i]] >= 15, 1.1, 0)
    SLY[2+i] <- SLY[2+i] + ifelse(SLY[[2+i]] >= 10, as.numeric(sample_n(effect,1)), 0)
    SLY[2+i] <- SLY[2+i] - dr
    
  }
  
  print(SLY)
  
}


# Combining the Results of SLN and SLY ------------------------------------
SLN[102] <- replace(SLN[102], SLN[102] < 0, 0)
SLY[102] <- replace(SLY[102], SLY[102] < 0, 0)

tbl1 <- head(select(SLN, day100), 4000)
tbl2 <- head(select(SLY, day100), 4000)
tbl2 <- rename(tbl2, day100_2 = day100)
join <- bind_cols(tbl1, tbl2)
join <- gather(join, day100, day100_2, key= "source", value = "cases")
join <- join %>% mutate(cases = round(cases))
join <- as_tibble(join)

# Creating Plots ------------------------------------


join %>% ggplot(aes(x = cases, fill = source)) +
  geom_histogram(alpha = .4, binwidth = 5) +
  geom_freqpoly(mapping = aes(color = source), size = .5, alpha = .7) +
  labs(x = "Number of Citations", fill = "Sharers vs non-Sharers", 
       colour = "Sharers vs non-Sharers") +
  scale_fill_discrete(name = "Sharers vs non-Sharers", labels = c("non-sharers", "sharers")) +
  scale_color_discrete(name = "Sharers vs non-Sharers", labels = c("non-sharers", "sharers"))
  


join %>% ggplot(aes(x = cases, y = ..density.., color = source)) +
  geom_freqpoly(alpha = .7, size = .6) +
  labs(x = "Number of Citations") +
  scale_color_discrete(name = "Sharers vs non-Sharers", labels = c("non-sharers", "sharers"))

summary(join)

write.csv(SLN,"/Users/mircobazzani/Desktop/Literatur/FS20/Replication Seminar/Bazzani_Replication_FS20/replication_fs20/Replication_2/Game-Theoretic-Modelling_Mirco/output/model 2_nonsharers.csv", row.names = TRUE)
write.csv(SLY,"/Users/mircobazzani/Desktop/Literatur/FS20/Replication Seminar/Bazzani_Replication_FS20/replication_fs20/Replication_2/Game-Theoretic-Modelling_Mirco/output/model 2_sharers.csv", row.names = TRUE)
write.csv(join,"/Users/mircobazzani/Desktop/Literatur/FS20/Replication Seminar/Bazzani_Replication_FS20/replication_fs20/Replication_2/Game-Theoretic-Modelling_Mirco/output/model 2_day_100.csv", row.names = TRUE)
