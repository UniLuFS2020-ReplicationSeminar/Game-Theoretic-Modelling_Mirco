#########################################################
# This is my try on recreating and possibly changing    #
# the base code of the gametheoretic model made by      #
# Tessa Ponk                                            #
#########################################################


# Setuo -------------------------------------------------------------------

library(tidyverse)
library(ggplot2)

# Basic Parameters --------------------------------------------------------

set.seed(03071996) # setting the seed in order to make my model reproducible 

N_Total_Researchers <- 10000 # I'll keep the total amount of researchers proposed by T.P.
t_paper <- tibble(t_paper = round(rnorm(10000, 47, 5))) %>% 
  mutate(t_paper, t_pap = t_paper / 365)
t_data <- tibble(t_data = round(rnorm(10000, 73, 7))) %>% 
  mutate(t_data, t_dat = t_data / 365)
t_search <- round(runif(1,0.5,3), digits = 1) # This is not included in the original paper 
# t_search assumes that one spends at least 0.5-3 Days searching for an appripriate Dataset.

#Normally distributed amoubt; i chose the proposed time +/- ten percent
qx <- runif(1,0.1,0.2) #randomized decay rate¨
y_nonShare <- 80 #I'll keep this one constant too, as I do not want to make the model more complex
# I've changed the parameter accoriding to the following statistics: https://ec.europa.eu/info/research-and-innovation/strategy/goals-research-and-innovation-policy/open-science/open-science-monitor/facts-and-figures-open-research-data_en

# Visualize to show that our parameters are normally distributed:

distribution_paper <- ggplot(t_paper, aes(x = t_pap)) +
  geom_bar()

distribution_data <- ggplot(t_data, aes(x = t_dat)) +
  geom_bar()


# Defining the Function ---------------------------------------------------

time_cost_share <- 30
time_cost_know <- 5
correction <- 100
plotmainlabel <- "test"

#function_break_even <- function (t_c,t_r,Y_ns,correction,plotmainlabel)  {

y_share <- 100 - y_nonShare
probability <- c(y_share, y_nonShare)
time_cost_share <- time_cost_share / 365          
time_cost_know <- time_cost_know / 365            

ii<-1             
steps<- 50 #I've not changed this because I do not know how this influences the results     

results_matrix <- matrix(,steps,7)

# This correction is needed so the plots are within the correct range 
# where the break-even point is visible. 

f <- 0.00000001 / correction        #Tessa Ponk does not explain this variable any further.
# we therefore do not know where she takes these numbers from. I assume that she just
# took a very small number.

#for (ii in 1:steps) {  
  
  f <- f + 0.0000004 / correction #this makes even less sense to me.
  
  # steady state calculation of pool of available datasets X
  N_sharers <-(y_nonShare/100)*N_Total_Researchers # total sharing researchers
  time_collect <- t_paper + time_cost_share + t_data  # This line includes my
  #new variable and my randomly generated datset
  time_no_collect <- t_paper + time_cost_share + time_cost_know + t_search
  
  X <- (-(qx*time_collect-y_share*f)+sqrt((qx*time_collect-y_share* f)^2 -
            4*(qx*f*time_no_collect)*-y_share))/(2*(qx*f*time_no_collect))

  pap_research <- rexp(N_Total_Researchers, 
                       rate=mean(t_paper$t_pap, na.rm = TRUE)+
                         mean(t_data$t_dat, na.rm = T))
  # I've had to use the means of the two tibbles "t_paper" and "t_data" because 
  # the rexp funciton does not work with tibbles
  
  pub_research <- 1/pap_research
  share_10 <- sample(c(0,1),N_Total_Researchers,replace=TRUE,probability)
  effect_rate <- pub_research * runif(10000,1,20) #The Effect Rate in the official
  #Paper is constant and equal to pub_research. I randomized it
  
  Rate_List <- data.frame(pap_research, pub_research,share_10,effect_rate)
  xy <- (1-(1/(1+(f*X))))
  lapply(pap_research,rbinom, size = 1, prob = xy)





