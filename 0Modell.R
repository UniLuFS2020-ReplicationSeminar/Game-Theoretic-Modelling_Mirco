
# Setup -------------------------------------------------------------------



library(tidyverse)
library(ggplot2)


# Basic Parameters - Complexity 1  --------------------------------------------------------

# setting the seed in order to make my model reproducible 

N_Total_Researchers <- 10000 
t_pap <- 47/365
t_dat<- 73/365
qx <- 0.1 


# Defining the Function - Complexity 1 ---------------------------------------------------

#time_cost_share = 1
#time_cost_know = 1
#y_nonShare = 50
#correction=150
#plotmainlabel="Scenario A"


function_break_even <- function (time_cost_share,time_cost_know,y_nonShare,correction,plotmainlabel)  {
  
  y_share <- 100 - y_nonShare #The percentage of researchers which do not share their data
  probability <- c(y_share, y_nonShare)
  time_cost_share <- time_cost_share / 365 #This gets defined by the upper funktion         
  time_cost_know <- time_cost_know / 365  #This gets defined by the upper funktion    
  
  ii<-1 #is going to be needed for a for-loop
  steps<- 50 #How often the for-loop gets activated  
  
  res_matrix <- matrix(,steps,7)
  
  # This correction is needed so the plots are within the correct range 
  # where the break-even point is visible. 
  f <- 0.000000000001 / correction        #Tessa Ponk does not explain this variable any further.
  # we therefore do not know where she takes these numbers from. I assume that she just
  # took a very small number.
  
  for (ii in 1:steps) {  
    
    #The formula below does not work with data-frames. This is why I have to reduce my
    #random sample to numeric variables. In a first test, I only used the mean. But the goal
    #of this reproduction is the increased randomness and complexity; this is why i chose
    #to use one random number out of my dataset instead.
    
    
    f <- f +  0.0000004 / correction #this makes even less sense to me.
    
    # steady state calculation of pool of available datasets X
    N_sharers <- (y_share/100)*N_Total_Researchers # total sharing researchers
    time_collect <-  t_pap  + time_cost_share + t_dat  # This line includes my
    #new variable and my randomly generated datset
    time_no_collect <-  t_pap  + time_cost_share + time_cost_know 
    
    Xmb <- (-(qx*time_collect-N_sharers*f)+sqrt((qx*time_collect-N_sharers* f)^2 - 4*(qx*f*time_no_collect)*-N_sharers))/(2*(qx*f*time_no_collect))
    pap_research <- rexp(N_Total_Researchers, rate= (t_pap + t_dat))
    pub_research <- 1/pap_research
    share <- sample(c(0,1),N_Total_Researchers,replace=TRUE,probability)
    effect_rate <- pap_research  
    
    Rate_List <- data.frame(pap_research, pub_research, effect_rate, share)
    xy <- (1-(1/(1+(f*Xmb))))
    chance_appropriate_set <- lapply(pap_research,rbinom, 1, (1-(1/(1+(f*Xmb))))) #Chance of finding appropriate set
    Rate_List$use_opp <- sapply(chance_appropriate_set, sum)
    summary(Rate_List)
    #Defining the cost benefits per researcher
    
    Rate_List$benefits<- round(t_dat*(Rate_List$use_opp/Rate_List$pap_research),3)     # per researcher saved time by reused datasets
    Rate_List$reusecost<-round(time_cost_know*(Rate_List$use_opp/Rate_List$pap_research),3)     # per researcher used time to process datasets for reuse
    Rate_List$cost<-round(time_cost_share*Rate_List$share,3)                  # per researcher cost 
    Rate_List$time<-round((t_pap+t_dat-Rate_List$benefits+Rate_List$cost+Rate_List$reusecost)*(pub_research/(t_pap+t_dat)),3)        # ta+td and costs and benefits. Normalize for speed researcher (Tr to t_a+t_d).
    Rate_List$Publ<-round(1/Rate_List$time,3)                                                              # publications, with costs benefits from reuse and sharing
    Rate_List$impact<-round((Rate_List$Publ),3) # impact with costs benefits from reuse and sharing
    
    head(Rate_List)
    sum(Rate_List$use_opp)/sum(Rate_List$Publ) 
    
    res_matrix[ii,1]<-round(Xmb,0)
    res_matrix[ii,2]<-sum(Rate_List$use_opp)
    res_matrix[ii,3]<-round((sum(Rate_List$use_opp)/sum(Rate_List$Publ))*100,2) # reuse publications per year 
    res_matrix[ii,4]<-round((sum(Rate_List$use_opp)/Xmb)*100,2) # reuse datasets per year
    res_matrix[ii,5]<-sum(Rate_List$impact-Rate_List$effect_rate)/N_Total_Researchers
    res_matrix[ii,6]<-f
    res_matrix[ii,7]<-((sum(Rate_List$impact-Rate_List$effect_rate)/N_Total_Researchers)/mean(Rate_List$impact))*100
    
  }
  
  colnames(res_matrix)<-c("totalPool","reuse","%reuse/publ","%_reuse/pool","av.imp/res","f_param","%impactinc")
  
  f_approx<-res_matrix[which(abs(res_matrix[,5])==min(abs(res_matrix[,5]))),6]
  publperc_approx<-as.numeric(res_matrix[which(abs(res_matrix[,5])==min(abs(res_matrix[,5]))),3])
  dataperc_approx<-as.numeric(res_matrix[which(abs(res_matrix[,5])==min(abs(res_matrix[,5]))),4])
  
  perc5_prod <-as.numeric(res_matrix[which(abs(res_matrix[,5])==min(abs(res_matrix[,5]))),7])
  
  
  plot(res_matrix[,7],res_matrix[,3],pch=15,main=plotmainlabel,cex.axis=1.4,cex.lab=1.4,
       col="orange",xlab="Average % added productivity per researcher",ylab="% Reuse")
  abline(h=publperc_approx,col="grey",lty=2,lwd=2)
  abline (h=dataperc_approx, col="grey",lty=2, lwd=2)
  points(res_matrix[,7],res_matrix[,3],pch=15,col="orange",cex=1.6)
  points(res_matrix[,7],res_matrix[,4],pch=16,col="red",cex=1.6)
  points(0,dataperc_approx,col="red",cex=4,pch=16)
  points(0,publperc_approx,col="orange",cex=4,pch=15)
  abline(v=0)
  
  res_ma_tibb <- as_tibble(res_matrix)
  
  res_ma_tibb <- res_ma_tibb %>% 
    gather(`%_reuse/pool`, `%reuse/publ`, key = "form", value = "percent")
  
  print(ggplot(data = res_ma_tibb, mapping = aes(`%impactinc`, percent)) + 
          geom_point(aes(color = form)) +
          geom_smooth(aes(color = form), se = FALSE) +
          labs(title = "Efficacy benefits in reusage of datasets", subtitle = plotmainlabel, x = "% added productivity", y = "% reusage"))
  
  assign('Rate_List', Rate_List, globalenv())
  assign('Results Matrix', res_matrix, globalenv())
  
  if (plotmainlabel=="Scenario A"){
    legend(-0.4,200/correction,cex=1.6,legend=c("Publication", "Dataset pool"),pch=c(15,16),col=c("orange","red")) 
  } # end of if statement
  
}


function_break_even(time_cost_share = 1,time_cost_know = 1,y_nonShare = 50, correction=100,plotmainlabel="Scenario A")
function_break_even(time_cost_share = 1,time_cost_know = 15,y_nonShare = 50,correction=125,plotmainlabel="Scenario B")
function_break_even(time_cost_share = 15,time_cost_know = 1,y_nonShare = 50,correction=9,plotmainlabel="Scenario C")
function_break_even(time_cost_share = 15,time_cost_know = 15,y_nonShare = 50,correction=6.5,plotmainlabel="Scenario D")








