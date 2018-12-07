---
title: "Use of ggplot & RMarkdown"
author: "Eleni Domzaridou"
date: "Oct 2018"
output:
  word_document: default
  df_print: paged
  always_allow_html: yes
  chunk_output_type: console
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

```{r, out.width='25%', fig.align='center', fig.cap='...'}
# If you want to include a graphic that is not generated with R
#knitr::include_graphics('1.png')
```

## Introduction
The xls document of financial data can be found on this [link](https://www.google.co.uk/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=2ahUKEwi3x__kuaveAhXF6qQKHe8OBggQFjAAegQICRAC&url=http%3A%2F%2Fwww3.wabash.edu%2Feconometrics%2Feconometricsbook%2FChapters%2FCh03PivotTables%2FExcelFiles%2FEastNorthCentralFTWorkers.xls&usg=AOvVaw2rpiXMtxewvLn7R1nvur1l). The data came from full-time workers in East North Central USA from March 1999 CPS. We use the data to work with rmarkdown, ggplot and regression

# Pre-processing

```{r, echo=TRUE, warning=FALSE}

# read the file
myfd <- read.csv("D:/_MSc/myMSc/BOOKS & ARTICLES/MATHS & Bio Modelling/examples/findata.csv")

# remove unneeded columns
myfd <- myfd[,-c(10, 15, 16)]

# change columns' names
names(myfd) <- c("hours", "eduyears", "earnyear", "race", "sex", "earnweek", "state", "fumonths", "n_earnweek", "age", "edu", "n_race", "n_sex", "n_earnyear")

```

Start with data cleaning and general checkings:

calculate the missingness for either earnweek (character) using grep or by counting zeros in
weekly earnings variable. Here I show both ways:

```{r, echo=TRUE, warning=FALSE}

sum(myfd$n_earnweek == 0) 

length(grep("\\b0\\b", myfd$earnweek)) 
# (starts-ends with 0: use "\\b _pattern_ \\b")

```

The ~77% of weekly salary observations is equal to zero and moreover we don't really care as we can
calculate this salary based on annual salary. For annual earnings we have 316 values equal to zero (~0.05%),
which means, according to the dataset code file, that these are not be applicable values. 

```{r, echo=TRUE, warning=FALSE}

sum(myfd$n_earnyear == 0)

```


note: We can exclude these values from the dataset before creating working hours subcategories:

```{r, echo=TRUE, warning=FALSE}

myfd <- myfd[myfd$hours != 0,]

```

define categories for working hours:
1: [35-40]
2: [41-50] etc

```{r, echo=TRUE, warning=FALSE}

myfd$hour_cat <- 0
myfd$hour_cat <- ifelse(myfd$hours >= 35 & myfd$hours <= 40, 1, myfd$hour_cat)
myfd$hour_cat <- ifelse(myfd$hours > 40 & myfd$hours <= 50, 2, myfd$hour_cat)
myfd$hour_cat <- ifelse(myfd$hours > 50 & myfd$hours <= 60, 3, myfd$hour_cat)
myfd$hour_cat <- ifelse(myfd$hours > 60, 4, myfd$hour_cat)
myfd$hour_cat <- as.factor(myfd$hour_cat)
table(myfd$hour_cat)

```

```{r, echo=TRUE, warning=FALSE}

# devide the wage with 1,000 to produce better visualisations:
myfd$wage_in_thous <- myfd$n_earnyear/1000

# check the variable age:
summary(myfd$age)

# define 4 age categories:
myfd$age_cat <- 0
myfd$age_cat <- ifelse(myfd$age < 25, 1, myfd$age_cat)
myfd$age_cat <- ifelse(myfd$age >= 25 & myfd$age < 35, 2, myfd$age_cat)
myfd$age_cat <- ifelse(myfd$age >= 35 & myfd$age < 45, 3, myfd$age_cat)
myfd$age_cat <- ifelse(myfd$age >= 45 & myfd$age < 55, 4, myfd$age_cat)
myfd$age_cat <- ifelse(myfd$age >= 55, 5, myfd$age_cat)
myfd$age_cat <- as.factor(myfd$age_cat)
table(myfd$age_cat)

```
```{r, echo=TRUE, warning=FALSE}

edu_vector <- c("Less than 1st grade", "1st,2nd,3rd,or 4th grade", "5th or 6th grade", "7th and 8th grade",
  "9th grade", "10th grade", "11th grade", "12th grade no diploma", "High school graduate-high school diploma",
  "Some college but no degree", "Assc degree-occupation/vocation", "Assc degree-academic program",
  "Bachelor's degree (BA,AB,BS)", "Master's degree (MA,MS,MENG,MED,MSW,MBA)",
  "Professional school degree (MD,DDS,DVM)", "Doctorate degree (PHD,EDD)")


# #check the vector with the definition
# cbind(edu_vector, 31:46)
# 
# myfd$edu_cat <- 0
# 
# for(k in 1:length(myfd$edu)){
#   for(i in 31:46){
#     myfd$edu_cat <- ifelse(myfd$edu == i, edu_vector[i-30], myfd$edu_cat)
#   }
# }
# 
# myfd$edu_cat <- as.factor(myfd$edu_cat)
# 
# # final check
# table(myfd$edu_cat, myfd$edu)

```


We excluded 281 people with annual wage smaller than 100$. By excluding 72 (0.012%) people who had annual wage > 200.000$ we can take more comprehensive boxplots that represent better the variability between races and genders, without sacrificing power. We also keep only those who earn more than 4500$ annualy: 


```{r, echo=TRUE, warning=FALSE}

#temp <- myfd[myfd$wage_in_thous<200 & myfd$n_earnyear > 100,]

#temp <- temp[temp$n_earnyear>4500,] 

# save this new dataset:
#write.csv(temp, file = "mydata.csv")

temp <- read.csv("D:/_MSc/myMSc/BOOKS & ARTICLES/MATHS & Bio Modelling/examples/mydata.csv")

```

## Create some initial visualisations

The following boxplot shows ........

```{r, echo=TRUE, warning=FALSE}

library(ggplot2)


p <- ggplot(data = temp, aes(x=hour_cat, y=wage_in_thous, fill=factor(hour_cat))) + 
       geom_boxplot(outlier.alpha = 0.1, alpha=0.2) + 
       theme(axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank())

p <- p + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "lightblue", "pink"), 
                               name="Working \n Hours",
                               breaks=c("1", "2", "3", "4"),
                               labels=c("[35-40]", "[41-50]", "[51-60]", "[>61]"))

p <- p + labs(
  title = "Working hours and annual wage",
  x = "Working hours",
  y = "annual wage (thousand $)")

p

```

Here we can observe that women are better paid than men:

```{r, echo=TRUE, warning=FALSE}

p1 <- ggplot(data = temp, aes(x=factor(hour_cat), y=wage_in_thous, fill=sex)) +
       geom_boxplot(show.legend = TRUE, outlier.alpha = 0.1, alpha=0.2) +
       scale_x_discrete(labels = c("1"="[35-40]", "2"="[41-50]", "3"="[51-60]", "4"="[>61]"))

p1 <- p1 + scale_fill_manual(values=rep(c("violet", "steelblue1"), 4), 
                               name="Gender")

p1 <- p1 + labs(
  title = "Working hours and annual wage by gender",
  x = "Working hours",
  y = "annual wage (thousand $)")

p1
```

The boxplot that follows shows ...... :

```{r, echo=TRUE, warning=FALSE}

p2 <- ggplot(data = temp, aes(x=race, y=wage_in_thous, fill=race)) +
       geom_boxplot(show.legend = FALSE, outlier.alpha = 0.1, alpha=0.5)

p2 <- p2 + scale_fill_manual(values=rep(c("seashell2", "grey", "palegoldenrod", "white"), 4))

p2 <- p2 + labs(
  title = "Annual wage by race",
  x = "Race",
  y = "annual wage (thousand $)")

p2
```




```{r, echo=TRUE, warning=FALSE}

p3 <- ggplot(data = temp, aes(x=race, y=wage_in_thous, fill=factor(race))) +
       geom_boxplot(show.legend = FALSE, outlier.alpha = 0.1) +
       facet_wrap(~sex)

p3 <- p3 + scale_fill_manual(values=c("seashell2", "grey", "palegoldenrod", "white"))

p3 <- p3 + labs(
  title = "Annual wage by gender and race",
  x = "Race",
  y = "annual wage (thousand $)")

p3
```




.... 

```{r, echo=TRUE, warning=FALSE}

p5 <- ggplot(data = temp, aes(x=state, y=wage_in_thous, fill=sex)) +
       geom_boxplot(show.legend = TRUE, outlier.alpha = 0.1, alpha=0.2)

p5 <- p5 + scale_fill_manual(values=rep(c("violet", "steelblue1"), 4), 
                               name="Gender")

p5 <- p5 + labs(
  title = "Annual wage by gender in each state",
  x = "State",
  y = "annual wage (thousand $)")

p5
```



```{r, echo=TRUE, warning=FALSE}

p6 <- ggplot(data = temp, aes(x=factor(age_cat), y=wage_in_thous)) +
       geom_boxplot(show.legend = FALSE, outlier.alpha = 0.1, alpha=0.2) +
       geom_jitter(alpha=0.1, color="mediumorchid1") +
       scale_x_discrete(labels = c("1"="[< 25]", "2"="[25-34]", "3"="[35-44]", "4"="[45-55]", "5"="[> 55]"))
  
p6 <- p6 + labs(
  title = "Annual wage by age category",
  x = "age (years)",
  y = "annual wage (thousand $)")

p6

```


Create a horizontal boxplot to show the variability between education and annual earnings:

```{r fig.width=12, fig.height=8, echo=TRUE, warning=FALSE}

p7 <- ggplot(data = temp, aes(x=edu_cat, y=wage_in_thous, fill=factor(edu_cat))) +
       geom_boxplot(show.legend = FALSE, outlier.alpha = 0.1, alpha=0.2) +
       #geom_jitter(alpha=0.1, color="mediumorchid1", show.legend = NA) +
       scale_x_discrete(limits=edu_vector[1:16]) +
       theme(axis.text=element_text(size=12) ,axis.title=element_text(size=14)) +
       coord_flip()
  
p7 <- p7 + labs(
  title = "Annual wage by education",
  x = "education type",
  y = "annual wage (thousand $)")

p7

```

```{r, echo=TRUE, warning=FALSE}

p8 <- ggplot(data=temp, aes(age)) + 
      geom_histogram(aes(y = ..density..), 
                     breaks=seq(10, 100, by = 1), 
                    #col="purple", 
                     fill="blue", 
                     alpha = .2) + 
      geom_density(col = 2, fill = "red", alpha = 0.1) + 
      labs(title = "Histogram of Age") +
      labs(x = "age (years)", y = "density")

p8
```

```{r, echo=TRUE, warning=FALSE}

p9 <- ggplot(data=temp, aes(hours)) + 
      geom_histogram(aes(y = ..density..), 
                     breaks=seq(30, 144, by = 1), 
                     fill="blue", 
                     alpha = .2) + 
      geom_density(col = 2, fill = "red", alpha = 0.1) + 
      labs(title = "Histogram of working hours") +
      labs(x = "usual working hours", y = "density")

p9
```



```{r, echo=TRUE, warning=FALSE}

p9.1 <- ggplot(data=temp, aes(x=factor(fumonths), fill=factor(fumonths))) + 
      geom_bar(stat = "count", show.legend= FALSE, width = 0.8, colour="darksalmon") + 
      scale_fill_brewer(palette = "Pastel2") +
      labs(title = "Barplot of months of follow-up") +
      labs(x = "months", y = "number of employees")

p9.1
```
###### Note 1: If you do not convert fmonths into a factor, you cannot use different colors in the scale_fill_brewer
###### Note 2: colour="darksalmon" inside geom_bar defines the bars' border colour as darksalmon

```{r, echo=TRUE, warning=FALSE}

p10 <- ggplot(data=temp, aes(wage_in_thous)) + 
      geom_histogram(aes(y = ..density..), 
                     breaks=seq(0, 160, by = 1), 
                     fill="blue", 
                     alpha = .2) + 
      geom_density(col = 2, fill = "red", alpha = 0.1) + 
      labs(title = "Histogram of annual earnings") +
      labs(x = "annual earnings (thousant $)", y = "density")

p10
```



```{r, fig.width=5, fig.height=5, echo=TRUE, warning=FALSE}

p11 <- ggplot(data=temp, aes(x=sex, fill=sex, alpha=0.2)) + 
      geom_bar(stat = "count", show.legend= FALSE, width = 0.8) + 
      scale_fill_manual(values=c("violet", "steelblue1")) +
      labs(title = "Barplot of gender") +
      labs(x = "gender", y = "number of employees")

p11
```
Alternatively we can also use a pie:

```{r fig.width=5, fig.height=5 , echo=TRUE, warning=FALSE}

p11df <- data.frame(
  gender = c("Male", "Female"),
  percent = c(58.1, 41.9))


p11.2 <- ggplot(data=p11df, aes(x="", y=percent, fill=gender)) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values=c("violet", "steelblue1")) +
      coord_polar("y", start=0)

blank_theme <- #theme_minimal()+
  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  #panel.border = element_blank(),
  #panel.grid=element_blank(),
  axis.ticks = element_blank(),
  plot.title=element_text(size=12, face="bold"))

pie <- p11.2 + blank_theme + geom_text(aes(y = p11df$percent), 
                                       label = paste0(p11df$percent, sep="%"), 
                                       size=5, 
                                       position = position_stack(vjust = 0.5))
pie 

```


Here, we can define the p12df data.frame that keeps the count of edu_cat, otherwhise we will not be able to sort the barplot according to count:


```{r fig.width=12, fig.height=8 , echo=TRUE, warning=FALSE}

p12df <- dplyr::count(temp, edu_cat)
  
p12 <- ggplot(data=p12df, aes(x=reorder(edu_cat, n), y=n, fill=edu_cat, alpha=0.2)) + 
      geom_bar(stat = "identity", show.legend= FALSE, width = 0.7) +
      geom_text(aes(label=n, size=3), show.legend = FALSE, hjust=0) + 
      labs(title = "Barplot of education type") +
      labs(x = "education type", y = "number of employees") +
      theme(axis.text=element_text(size=12) ,axis.title=element_text(size=14)) +
      coord_flip()

p12
```


Use of tableone package to produce summary statistics stratified by gender:

```{r, echo=TRUE, warning=FALSE}

library(tableone)

#dput(names(temp))

myvars <- c("hours", "eduyears", "race", "state", "fumonths", 
            "n_earnweek", "age", "edu", "n_earnyear", "hour_cat", 
            "wage_in_thous", "age_cat", "edu_cat")

mytableOne <- CreateTableOne(vars = myvars, strata = c("sex"), data = temp)
#mytableOne

```
