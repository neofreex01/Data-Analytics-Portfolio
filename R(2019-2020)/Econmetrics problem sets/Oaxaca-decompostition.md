---
title: "Oaxa Decompostion and reweighting"
author: "Hung-Hsiang(Kevin), CHIEN"
date: "2021年1月18日"
output:
  html_document:
    keep_md: true
    df_print: paged
    toc: yes
editor_options:
  chunk_output_type: inline
---






# 1. Oaxaca decomposition and Autor, Katz and Kearney (2008)
Brief introduction: This paper try to find the trend of wage in US. Also, they use the census data in 1979 and 1997 to look for what is the main reason for the change in wages in US, the change of demographic or the erosion of minimum wage?

## (a) Process the data
### Step1
I added lnhr_wage and limited our  attenetion to the sample with "lhhr_wage" in both datasets. Then, selected the charateristecs we would like to use in this part. Also, I combined these two datasets as df_b

### Step2
Then, in the linear model, I first used the as.factor function to create a series of dummy variables of regions, occupations and year. Then, I decided to run the regression for male and female separately. 



## (b) oaxaca decompostion


```
## 
## =====================
##                 Value
## ---------------------
## Gender Wage Gap 0.299
## Explained       0.015
## Unexplained     0.284
## ---------------------
```
This table tells us that most of the wage gap between genders is unexplained, which could be supposed as discrimination. The explained gender is due to the different characteristic of women and men.

## (c) Wage distribution shiftment
This figure shows the distribution of wages in 1979 and 1997.We can
see the erosion of the minimum wage: the 1979 distribution has a "spike" while
the 1997 distribution does not. The 1997 distribution has more dispersal overal,
with more mass in the lower and upper tail.
<img src="./figure/oaxaca/figureunnamed-chunk-6-1.png" style="display: block; margin: auto;" />



## (d) How the distribution changes from 1979 to 1997 in different groups
<img src="./figure/oaxaca/figureunnamed-chunk-7-1.png" style="display: block; margin: auto;" /><img src="./figure/oaxaca/figureunnamed-chunk-7-2.png" style="display: block; margin: auto;" />

The log hourly wage distribution in 1979 and 1997 for black men is shown in
the firt graph.The distribution remarkably shifted to the left. The log hourly wage distribution for women is shown in the second fiture, and shows a similar pattern to the overall distribution of increasing spread and erosion of the minimum wage.

## (e) demographic change from 1979 to 1997

```
## 
## =============================================
##                       Dependent variable:    
##                   ---------------------------
##                              year            
## ---------------------------------------------
## female                     0.197***          
##                             (0.008)          
##                                              
## black                      -0.064***         
##                             (0.015)          
##                                              
## others                     0.529***          
##                             (0.022)          
##                                              
## age                        0.142***          
##                             (0.002)          
##                                              
## I(age2)                    -0.002***         
##                            (0.00003)         
##                                              
## Constant                   -3.001***         
##                             (0.038)          
##                                              
## ---------------------------------------------
## Observations                247,784          
## Log Likelihood           -167,058.100        
## Akaike Inf. Crit.         334,128.200        
## =============================================
## Note:             *p<0.1; **p<0.05; ***p<0.01
```
This table shows that there are more female, less black, and more people in other race in 1979. Also people are generally older in average in 1979. Although we shit of wage distribution could be caused by these demographic changs.

## (f) Reweight the distribution in 1997
To see the distriution is cause by the change of demographic in US or by the erosion of minimum wage, we can reweight the distribution in 1997 to the demographic distribution in 1979, i.e. a counterfactual 1997 distribution.
<img src="./figure/oaxaca/figureunnamed-chunk-9-1.png" style="display: block; margin: auto;" />
The 1997 distribution reweighted so that it has 1979 demographics is shown
in the figure. The distribution is much closer to the 1997 distribution than the 1979 distribution. That is, the changes in the wage distribution cannot only be explained by demographics, demographics increased inequality but only slightly.
