###################################################################################################
### Script from the manuscript "The time efficiency gain in sharing and reuse of research data" ###
### by Tessa Pronk,  Utrecht University Library, KWR watercycle research institute              ###
###                                                                                             ###
### A game theoretical model to assess the reuse needed to break even in terms of               ###         ###
### time efficiency in a community with time costs for research data sharing.                   ###
###                                                                                             ###
### Specifically, this script produces Figure 1 in the manuscript.                              ###
### Via this script also numbers can be generated                                               ###
### for Table 2 and Figure 2 (see 'res_matrix' below)                                           ###
### Written by Tessa Pronk                                                                      ###
###                                                                                             ###
### Version after revisions required Data Science Journal, Januari 2019                         ###
###################################################################################################

# set parameter values
Y_tot_res <- 10000    # number of researchers in the research community 
t_a<-47/365             # time to write a paper
t_d<-73/365 
qx<-0.1               # decay rate of datasets
Y_ns<-50              # this is the proportion not sharing researchers

# Below a function {} is defined to run the simulation for different scenarios
# This function needs to be run once every R session 
# and can be called after that with different parameters without running it again.

# It needs as parameters 
# t_c (days invested in sharing) 
# t_r (days invested for reuse)
# Y_ns (percentage of non-sharing researchers) 
# correction (a correction factor to align the plot to show the break-even point)
# plotmainlabel (the name of the scenario as label for the plot)

#######################################################
# Define the function 
#######################################################

#t_c=1
#t_r=1
#Y_ns=50
#correction=150
#plotmainlabel="Scenario A"

function_break_even <- function (t_c,t_r,Y_ns,correction,plotmainlabel)  {

Y_s <- 100-Y_ns           # this is the proportion sharing researchers 
probab <-c(Y_ns,Y_s)
t_c <- t_c / 365            # time cost associated with sharing a dataset
t_r <- t_r / 365            # time to 'get to know' a dataset for reuse

ii<-1                   # initialisation for the loop 
steps<-50#200              # intitalisation for the loop

# make a matrix to hold results of the function
res_matrix <- matrix(,steps,7)

# This correction is needed so the plots are within the correct range 
# where the break-even point is visible. 
correct <- correction

f <- 0.000000000001 / correct        # probability of finding an appropriate external dataset

for (ii in 1:steps) {  
  
f <- f + 0.0000004 / correct

# steady state calculation of pool of available datasets X
Ys<-(probab[2]/100)*Y_tot_res # total sharing researchers
T1 <- t_a + t_c + t_d
T2 <- t_a + t_c + t_r 
X<-(-(qx*T1-Ys*f)+sqrt((qx*T1-Ys*f)^2-4*(qx*f*T2)*-Ys))/(2*(qx*f*T2)) # the common pool of datasets; 'resource'.

# Personalize the publication rate per researcher to a exponental distribution, based on no cost benefit (t_a+t_d for one publication).
  Pr<-rexp(Y_tot_res,rate=t_a+t_d)                           # papers per researcher
  Tr<-1/Pr                                                   # publication rate per researcher
  share_rate<-sample(c(0,1),Y_tot_res,replace=TRUE,probab)   # 0= no sharing 1= sharing
  Er<- Pr                                          # citations to produced publications per researcher
  
  Rlist<-data.frame(Pr,Tr,Er,share_rate)                     # Researcherslist with metrics for all reasearchers
  
  chance_appr_set<-lapply(Pr,rbinom,1,(1-(1/(1+(f*X)))))     # the chance of finding an appropriate set for any publication
  
  Rlist$use_opp<-sapply(chance_appr_set, sum)                 # per researcher publications with a reused dataset
  summary(Rlist)
  
# Costs or benefits per researcher:
  Rlist$benefits<- round(t_d*(Rlist$use_opp/Rlist$Pr),3)     # per researcher saved time by reused datasets
  Rlist$reusecost<-round(t_r*(Rlist$use_opp/Rlist$Pr),3)     # per researcher used time to process datasets for reuse
  Rlist$cost<-round(t_c*Rlist$share_rate,3)                  # per researcher cost 
  Rlist$time<-round((t_a+t_d-Rlist$benefits+Rlist$cost+Rlist$reusecost)*(Tr/(t_a+t_d)),3)        # ta+td and costs and benefits. Normalize for speed researcher (Tr to t_a+t_d).
  Rlist$Publ<-round(1/Rlist$time,3)                                                              # publications, with costs benefits from reuse and sharing
  Rlist$impact<-round((Rlist$Publ),3) # impact with costs benefits from reuse and sharing
  
  head(Rlist)
  sum(Rlist$use_opp)/sum(Rlist$Publ)                         
  
    res_matrix[ii,1]<-round(X,0)
    res_matrix[ii,2]<-sum(Rlist$use_opp)
    res_matrix[ii,3]<-round((sum(Rlist$use_opp)/sum(Rlist$Publ))*100,2) # reuse publications per year 
    res_matrix[ii,4]<-round((sum(Rlist$use_opp)/X)*100,2) # reuse datasets per year
    res_matrix[ii,5]<-sum(Rlist$impact-Rlist$Er)/Y_tot_res
    res_matrix[ii,6]<-f
    res_matrix[ii,7]<-((sum(Rlist$impact-Rlist$Er)/Y_tot_res)/mean(Rlist$impact))*100
}

colnames(res_matrix)<-c("totalPool","reuse","%reuse/publ","%reuse/pool","av.imp/res","f_param","%impactincr")

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
  gather(`%reuse/pool`, `%reuse/publ`, key = "form", value = "percent")

print(ggplot(data = res_ma_tibb, mapping = aes(`%impactincr`, percent)) + 
        geom_point(aes(color = form)) +
        geom_smooth(aes(color = form), se = FALSE) +
        labs(title = "Efficacy benefits in reusage of datasets", subtitle = plotmainlabel, x = "% added productivity", y = "% reusage"))

assign('Rate_List', Rlist, globalenv())
assign('Results Matrix', res_matrix, globalenv())

if (plotmainlabel=="Scenario A"){
legend(-0.4,200/correct,cex=1.6,legend=c("Publication", "Dataset pool"),pch=c(15,16),col=c("orange","red")) 
} # end of if statement

# From the below matrix 'res_matrix' can be read (in approximation because of stochastic results):
# The value for the f parameter at 0 % impactincrease (break-even point)
# The value for the f parameter at 5 % impactincrease 
# The % reuse publications needed at that point
# The % reused datasets needed at that point

#res_matrix

} # end of function

#######################################################################
# Run the function for different parameters 
#######################################################################

# Set the plot window
windows(20,12)
par(mfrow = c(2,2),mar = c(6, 6, 2, 1))

# TcTr 15,15: correct = 6.5, Tc15Tr1: correct is 9 Tc1Tr15: correct is 125 Tc1Tr1: correct is 150
# the correction numbers were obtained by trial and error.
# Here are the simulations to run Figure 1:

function_break_even(t_c=1,t_r=1,Y_ns=50,correction=150,plotmainlabel="Scenario A")
function_break_even(t_c=1,t_r=15,Y_ns=50,correction=125,plotmainlabel="Scenario B")
function_break_even(t_c=15,t_r=1,Y_ns=50,correction=9,plotmainlabel="Scenario C")
function_break_even(t_c=15,t_r=15,Y_ns=50,correction=6.5,plotmainlabel="Scenario D")

#######################################################################
# End of script
#######################################################################

