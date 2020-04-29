#Test Markov

Short_List <- Rate_List %>% 
  mutate(impact_r = round(impact)) %>% 
  select(share,impact_r)

sy <- Short_List %>% filter(share == 1)
sn <- Short_List %>% filter(share == 0)
  
SLY <- as.tibble(head(sy))
SLN <- as.tibble(sn)
i <- 1
max <- 100

for (i in  1:max) {
  
 days <- paste("day", 1:max, sep = "")
 q <- matrix(days,1,max)
 t <- as.character(q[1,i])
 
 SLN <- print(SLN %>% add_column(!!(days[i]) := 0))
 
 mutate(SLN, day1 = impact_r)
 
 if (i >= 1){
  
  SLN[2+i] <- SLN[(2+i)-1] + round(rnorm(n = 10000, mean = 0, sd= 1))
  SLN[2+i] <- SLN[2+i] + ifelse(SLN[[2+i]] >= 20, 1, 0)

  }

print(SLN)
       
}

SLN[100] <- replace(SLN[100], SLN[100] < 0, 0)

SLN[100] %>% ggplot(aes(x = day98)) +
  geom_histogram()

max(SLN[100])


