Does military service can affect suicidal?
========================================================

* Note: I am not an expert in statistics so If you find a mistake I would love to know it. In addition, English is not my native language so anticipated errors in writing.

Military service often requires sacrifice  whether it's aservice away from home, work under pressure, physiological and mental effort. There is no need to expand the importance of skilled and professional army to maintain security and maintaining freedom of choice.

The data were obtained from the General Social Survey (GSS): A sociological survey used to collect data on demographic characteristics and attitudes of residents of the United States. The codebook** below lists all variables, the values they take, and the survey questions associated with them. There are a total of 57,061 cases and 114 variables in this dataset. Note that this is a cumulative data file for surveys conducted between 1972 - 2012 and that not all respondents answered all questions in all years


To answer the research question, only two variables were collected: VETYEARS & SUICIDE4.

1) VETYEARS.  The variable appearance of military experience:  VETYEARS "Have you ever been on active duty for military training or service for two consecutive months or more? IF YES: What was your total time on active duty?"

Please notes that it can not tell from this variable whether the respondent had experienced combat or served in the headquarters roles . 
When the assumption is that people who have served in battles and stress will have a greater tendency to suicide.

2) SUICIDE4.  The variable appearance of suicidal.  "Do you think a person has the right to end his or her own life if this person:  Is tired of living and  ready to die?".

It is also notes thet the question asked does not address the subject directly to the research question. The subject was asked Is the generally opinion  on this issue and  not explicitly to himself. So the assumption is if the  examined  that says "yes" to this question, has a mor tendency to suicide

codebook**: https://d396qusza40orc.cloudfront.net/statistics%2Fproject%2Fgss1.html

## Loading and preprocessing the data

```r
load(url("http://bit.ly/dasi_gss_data"))

gss<- data.frame(military = gss$vetyears ,suicide = gss$suicide4)
gss <- subset(gss,gss$military != 'NA' & gss$suicide != 'NA')  # subset NA valus
```

Calculate the proportion of each group who responded yes

```r
g1 = nrow(gss[gss$military == 'None' & gss$suicide == 'Yes',])/nrow(gss[gss$military == 'None' ,]) 
g2 = nrow(gss[gss$military == 'Less Than 2 Yrs' & gss$suicide == 'Yes',])/nrow(gss[gss$military == 'Less Than 2 Yrs' ,])
g3 = nrow(gss[gss$military == '2 To 4 Years' & gss$suicide == 'Yes',])/nrow(gss[gss$military == '2 To 4 Years' ,])
g4 = nrow(gss[gss$military == 'More Than 4 Yrs' & gss$suicide == 'Yes',])/nrow(gss[gss$military == 'More Than 4 Yrs' ,])
percent = c(g1,g2,g3,g4)
group = c("None","Less Than 2 Yrs","2 To 4 Years","More Than 4 Yrs")
frame = (cbind(group,percent))
```

```r
barplot(percent,names.arg = group,xlab="military experience")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

With a quick glance you can see that there is a difference between groups of military experience,
but whether the difference is significant?  (.05)

In order to test the null hypothesis was constructed following experiment: 
100 samples were performed, each sample were drawn randomly thousand people. 
The data collected are the number of people who voted "YES" in relation to the subject of military experience

The experiment can be described as a Bernoulli trial, where success is considered when tested answers "yes" in the context of the  suicidal question. 
P is proportion of the number of people who answer "yes" in the subjects group 
## X ~ B(p,n) 
## x ~ N(np,sqrt(npq))      ,n=1000  > 30 

```r
# declaration for a function that pull random # Records
randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}

X1 = rep(NA,100)    # number of nan military group
N1 = rep(NA,100)    # nan military group in  i sample [yes for suicide] 
X2 = rep(NA,100)    # a military group   in  i sample [yes for suicide] 

#Loop 100 experiments carried out with each experiment  icloud  1000 participants
for (i in 1:100) {
  samp = randomRows(gss,1000) 
  X1[i] = nrow(samp[samp$military == 'None' & samp$suicide == 'Yes',])
  N1[i] = nrow(samp[samp$military == 'None' ,])
  X2[i] = nrow(samp[samp$military != 'None' & samp$suicide == 'Yes',])  
}
```
Calculate the average proportion for all experiments

```r
n1 = round(mean(N1))
x1 = round(mean(X1))
p1 = x1/n1
n2 = 1000- n1 
x2 = round(mean(X2))
p2 = x2/n2
```


# H0: p1 - p2 = 0   
# H1: p1 -p2 < 0    
## significance level (0.05)   one thil 


```r
P= (x1+x2)/(n1+n2)
SE = sqrt(P*(1-P)*(1/n1+1/n2))   # SE = sqrt{ p * ( 1 - p ) * [ (1/n1) + (1/n2) ] }

z = (p1-p2)/SE 
Pvalue = pnorm(z)  
sprintf("P value: %s",Pvalue)
```

```
## [1] "P value: 0.227537753956947"
```

## Interpret results:
Since the P-value (~0.2) is greater than the significance level (0.05), we cannot reject the null hypothesis. So according to the above-mentioned data of military service does not affect willingness to suicide,  H0 is True.

As mentioned initially, the data do not relate directly to the research question. Because this issue is important 
is necessary to conduct a survey with more specific questions asked so it would be possible to tell if the patient served in combat service, Which could show that these soldiers need more resources for have normal and happe life.





