load(url("http://bit.ly/dasi_gss_data"))
library(descr)
library(knitr)

setwd('D:/Documents/coursera/Duke')

 #codebook: https://d396qusza40orc.cloudfront.net/statistics%2Fproject%2Fgss1.html
#Do you think a person has the right to end his or her own life if this person: d. Is tired of living and ready to die?
gss<- data.frame(military = gss$vetyears ,suicide = gss$suicide4)
gss <- subset(gss,gss$military != 'NA' & gss$suicide != 'NA')



#Calculate the proportion of each group who responded yes

g1 = nrow(gss[gss$military == 'None' & gss$suicide == 'Yes',])/nrow(gss[gss$military == 'None' ,]) 
g2 = nrow(gss[gss$military == 'Less Than 2 Yrs' & gss$suicide == 'Yes',])/nrow(gss[gss$military == 'Less Than 2 Yrs' ,])
g3 = nrow(gss[gss$military == '2 To 4 Years' & gss$suicide == 'Yes',])/nrow(gss[gss$military == '2 To 4 Years' ,])
g4 = nrow(gss[gss$military == 'More Than 4 Yrs' & gss$suicide == 'Yes',])/nrow(gss[gss$military == 'More Than 4 Yrs' ,])
percent = c(g1,g2,g3,g4)
group = c("None","Less Than 2 Yrs","2 To 4 Years","More Than 4 Yrs")
frame = (cbind(group,Percent))
barplot(percent,names.arg = group,maim="Percentage of military experience  that said 'yes' ",xlab="military experience",ylib="Percent")


# fun for get n random sampel rows from data.frame
randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}

X1 = rep(NA,100)    # number of nan military group
N1 = rep(NA,100)    # nan military group in  i sample [yes for suicide] -> N1
X2 = rep(NA,100)    # a military group   in  i sample [yes for suicide] -> N2

for (i in 1:100) {
  samp = randomRows(gss,1000)
  
  X1[i] = nrow(samp[samp$military == 'None' & samp$suicide == 'Yes',])
  N1[i] = nrow(samp[samp$military == 'None' ,])
  
  X2[i] = nrow(samp[samp$military != 'None' & samp$suicide == 'Yes',])
}

n1 = round(mean(N1))
x1 = round(mean(X1))
p1 = x1/n1
n2 = 1000- n1 
x2 = round(mean(X2))
p2 = x2/n2
 
#   H0: p1 - p2 = 0    H1: p1 -p2 < 0    
#significance level (0.05)   one thil 

P= (x1+x2)/(n1+n2)
SE = sqrt(P*(1-P)*(1/n1+1/n2))   # SE = sqrt{ p * ( 1 - p ) * [ (1/n1) + (1/n2) ] }

z = (p1-p2)/SE 
Pvalue = pnorm(z)  #0.26

#Interpret results: Since the P-value (0.26) is greater than the significance level (0.05), we cannot reject the null hypothesis.





