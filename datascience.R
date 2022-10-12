# Title
# Data Science: Where in the World am I Going to Work?

# **Dedication:**   
# This project is dedicated to my two daughters.  Reflect on the important things to gain a better introspective and always move forward.

# **Introduction:**   
# I will analyze Data Science Job Salaries from Ruchi Bhatia's Kaggle(https://www.kaggle.com/datasets/ruchi798/data-science-job-salaries).  This data set contains salaries of jobs in the Data Science domain.  The dataset variable terminology is located in the Data Science Job Salaries Data Set Glossary and Terminology Section.   

## **Objective:**  
# Data Science is a diverse and rewarding career.  In this project, we will explore the data associated with the career field of Data Science to give you an insight into the different job positions that are available to you based on Salary, Cost of Living, Position, Country, and more! 
# The data has been downladed from the url $"https://www.kaggle.com/datasets/ruchi798/data-science-job-salaries"$ 

## **Data Installation:**  
# Upload following packages and libraries for data exploration. 

library(tidyverse)
library(caret)
library(data.table)
library(RColorBrewer)
library(rmarkdown)
library(dslabs)
library(gtable)
library(hexbin)
library(gt)
library(dplyr)
library(ggpmisc)
library(gridExtra)
library(janitor)
library(lubridate)
library(highcharter)
library(viridisLite)
library(broom)
library(scales)
library(xfun)
library(htmltools)
library(mime)
library(ggfortify)
library(gtsummary)
library(tinytex)
library(vroom)
library(curl)
library(gtools)
library(hrbrthemes)
library(viridis)
library(latexpdf)
library(kableExtra)
library(knitr)
library(remotes)
library(extrafont)

options(scipen = 999)
options(timeout = 320)
gc()

# **Data Analysis:**   
# First, upload the data set.


Tools <- xfun::pkg_load2(c("htmltools", "mime")) 
Download_Link <- xfun::embed_files(c('Data Analysts Files/Salariesdataanalyst.csv'))

Download_Link
if(interactive() ) htmltools::browsable(Download_Link)


All_Data <- read.csv(
  'Data Analysts Files/Salariesdataanalyst.csv')

colnames(All_Data)<- c('Analyst_Id', 'work_year', 'experience_level', 
                       'employment_type', 'job_title', 'salary', 
                       'salary_currency', 'salary_in_usd', 'employee_residence', 
                       'remote_ratio', 'company_location', 'company_size')

### **Dimensions and Summary**  
dim(All_Data)

# The dataset has 607 observations for 12 variables.  
summary(All_Data)
any(is.na(All_Data))

## ** Data Science Job Salaries Data Set Glossary and Terminology: **  
# 1. Analyst ID: Provides an unique ID number to each Data Analyst Job.

# 2. Work Year: The year the salary was paid.

# 3. Experience Level: 	The experience level in the job during the year with the following possible values: EN Entry-level / Junior,  MI Mid-level / Intermediate, SE Senior-level / Expert,  EX Executive-level / Director.

# 4. Employment Type: 	The type of employement for the role: PT Part-time, FT Full-time, CT Contract, FL Freelance.

# 5. Job Title: 	The role worked in during the year.

# 6. Salary: 		 The total gross salary amount paid.

# 7. Salary Currency: 	The currency of the salary paid as an ISO 4217 currency code.

# 8. Salary in USD:	The salary in USD (FX rate divided by avg. USD rate for the respective year via fxdata.foorilla.com).

# 9. Employee Residence: 		Employee's primary country of residence in during the work year as an ISO 3166 country code.

# 10. Remote Ratio: The overall amount of work done remotely, possible values are as follows: 0 No remote work (less than 20%), 50 Partially remote,  100 Fully remote (more than 80%).

# 11. Company Location: 		The country of the employer's main office or contracting branch as an ISO 3166 country code.

# 12. Company_Size:	The average number of people that worked for the company during the year: S means less than 50 employees (small). M means 50 to 250 employees (medium). L means  more than 250 employees (large).

#  Cost of living and the Pay.

# Average Salary for all positions 

Avg_Salary <- mean(All_Data$salary_in_usd)
Avg_Salary_USD<- scales::dollar(Avg_Salary)
Avg_Salary_USD

# all jobs making less than and more than the Average Salary
x <- All_Data$job_title[which(All_Data$salary_in_usd< 112298)]
x
table(All_Data$salary_in_usd)

H1 <- qplot(x)+
  coord_flip()+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  theme_classic() +
  labs(
    title = " Jobs < $112,298",
    subtitle = "Where in the World am I Going to Work?",
    caption = "1. $112,298 is the average of all positions.
    2. Portions of this data is from the Ref. Section.",
    x = "Job title",
    y = "Amount of Positions")+
  theme_bw()+
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0.5))

e <- All_Data$job_title[which(All_Data$salary_in_usd> 112298)]
e


L1 <- qplot(e)+
  coord_flip()+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  theme_classic() +
  labs(
    title = " Jobs > $112,298",
    subtitle = "Where in the World am I Going to Work?",
    caption = "1. $112,298 is the average of all  positions.
    2. Portions of this data is from the Ref. Section.",
    x = "Job title",
    y = "Amount of Positions")+
  theme_bw()+
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0.5))


grid.arrange(H1, L1, ncol = 2)

#  the highest and lowest pay jobs.

typeof(All_Data$salary_in_usd)
All_Data$salary_in_usd <- as.numeric(All_Data$salary_in_usd)
typeof(All_Data$salary_in_usd)
MaxNum <- max(All_Data$salary_in_usd)
MaxSal<- scales::dollar(MaxNum)
MaxSal
WagesDollars<- scales::dollar(All_Data$salary_in_usd)
mixedsort(WagesDollars)
MaxJT <- All_Data$job_title[which.max(All_Data$salary_in_usd)]
MaxEmployResid<-All_Data$employee_residence[which.max(All_Data$salary_in_usd)]
MaxCompLoc<-All_Data$company_location[which.max(All_Data$salary_in_usd)]
MaxExpLevel <-All_Data$experience_level[which.max(All_Data$salary_in_usd)]
MaxCompSize<-All_Data$company_size[which.max(All_Data$salary_in_usd)]
MaxWorkYear<-All_Data$work_year[which.max(All_Data$salary_in_usd)]

MaxJT

MaxSalaryResults <- tibble(MaxWorkYear, MaxJT, MaxSal, MaxExpLevel, MaxEmployResid, 
                           MaxCompLoc, MaxCompSize)

colnames(MaxSalaryResults)<- c("Year", "Job Title", "Max. Salary", "Exp. Level", 
                               "Employee Residence", "Company  Loc.", "Company Size")

MaxSalaryResults %>%
  gt() %>%
  tab_header(
    title = md("**Highest Paying Job**"),
    subtitle = md("Data Analyst Jobs 2020-2022")
  ) %>%
  cols_width(
    everything() ~ px(160)) %>%
  tab_source_note(
    source_note = md("Portions of this data is from the Reference Section.")
  ) %>%
  tab_footnote(
    footnote = md(" Salary is in USD")) %>%
  tab_footnote(
    footnote = md(" EX = Experience level is Executive-level")) %>%
  tab_footnote(
    footnote = md(" US = United States")) %>%
  tab_footnote(
    footnote = md(" L = Large Company, more than 250 employees")) %>%
  opt_table_font(
    font = "Times New Roman",
    weight = 300) %>%
  cols_align(
    align = "center",
    columns = everything())


# Least Paying Job
All_Data[which.min(All_Data$salary_in_usd), ]

MinNum <- min(All_Data$salary_in_usd)
MinSal<- scales::dollar(MinNum)
MinSal
JT <- All_Data$job_title[which.min(All_Data$salary_in_usd)]
EmployResid<-All_Data$employee_residence[which.min(All_Data$salary_in_usd)]
CompLoc<-All_Data$company_location[which.min(All_Data$salary_in_usd)]
ExpLevel <-All_Data$experience_level[which.min(All_Data$salary_in_usd)]
CompSize<-All_Data$company_size[which.min(All_Data$salary_in_usd)]
MinWorkYear<-All_Data$work_year[which.min(All_Data$salary_in_usd)]


LowestSalaryResults <- tibble(MinWorkYear, JT, MinSal, ExpLevel, EmployResid, CompLoc, CompSize)
colnames(LowestSalaryResults)<- c("Year", "Job Title", "Min. Salary", "Exp. Level", 
                                  "Employee Residence", "Company  Loc.", "Company Size")

LowestSalaryResults

LowestSalaryResults %>%
  gt() %>%
  tab_header(
    title = md("**Least Paying Job**"),
    subtitle = md("Data Analyst Jobs 2020-2022")
  ) %>%
  cols_width(
    everything() ~ px(160)) %>%
  tab_source_note(
    source_note = md("Portions of this data is from the Reference Section.")
  ) %>%
  tab_footnote(
    footnote = md(" Salary is in USD.")) %>%
  tab_footnote(
    footnote = md(" MI = Experience level is Mid-Level")) %>%
  tab_footnote(
    footnote = md(" MX = Mexico")) %>%
  tab_footnote(
    footnote = md(" S = Small Company, less than 50 employees")) %>%
  opt_table_font(
    font = "Times New Roman",
    weight = 300) %>%
  cols_align(
    align = "center",
    columns = everything())

61233/112298

#  Career Exploration

# plot all the jobs
ggplot(data.frame(All_Data), aes(x=job_title, fill = job_title)) +
  geom_bar()+
  labs(
    title =  "Data Science Occupations ",
    subtitle = "Data Science: Where in the World am I Going to Work?",
    caption = "Portions of this data is from the Reference Section.",
    x = "Job Title",
    y = "Total Number of Jobs Filled",
  )+
  theme_classic()+
  theme(
    legend.position = 'none',
    axis.text.x = element_text(angle = 70, vjust = 1, hjust = 1),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(face = "italic",hjust = 0.5))+
  scale_y_discrete(guide = guide_axis(check.overlap = TRUE)) +
  annotate("text", x = 1.3, y = 20, label = "20", color = "black")+
  annotate("text", x = 1.3, y = 40, label = "40", color = "black")+
  annotate("text", x = 1.3, y = 60, label = "60", color = "black")+
  annotate("text", x = 1.3, y = 80, label = "80", color = "black")+
  annotate("text", x = 1.3, y = 100, label = "100", color = "black")+
  annotate("text", x = 1.3, y = 120, label = "120", color = "black")+
  annotate("text", x = 1.3, y = 140, label = "140", color = "black")+
  coord_flip()

#Job with the most personnel
popjob <-All_Data$job_title
popjob
names(table(popjob))[as.vector(table(popjob))==max(table(popjob))]
max(table(popjob))

# Job with the least amount of personnel
notpopjob <-All_Data$job_title
notpopjob
names(table(notpopjob))[as.vector(table(notpopjob))==min(table(notpopjob))]
min(table(notpopjob))

# Plot the countries with the largest companies, Smallest and medium using a circle plot.
# Company size vs Pay

ggplot(All_Data, aes(y = company_size, x = salary_in_usd, size = salary_in_usd, fill = company_size)) +
  geom_point(alpha=0.7, shape=21, color="black")+
  scale_size(range = c(.1, 12), name = "Pay Scale ($)")+
  scale_fill_viridis(discrete = TRUE, guide = "none", option = "A") +
  theme_bw()+
  labs(
    title = " Data Science Companies Size and Salary in USD",
    subtitle = "Data Science: Where in the World am I Going to Work?",
    caption = "1. Salary in USD.
    2. S means less than 50 employees (small). 
    3. M means 50 to 250 employees (medium). 
    4. L means  more than 250 employees (large).
    5. Portions of this data is from the Reference Section.",
    x = "Salary in USD ($)",
    y = "Company Size",
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0.5))

# which company pays  more per position.
# Company Location and Size 
ggplot(All_Data, aes(x = company_location, y = company_size, fill = company_size))+
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(
    title = " Data Science Companies Location and Size",
    subtitle = "Data Science: Where in the World am I Going to Work?", 
    caption = "1. Company Location main office or contracting branch as an ISO 3166 country code.
    2. S means less than 50 employees (small). 
    3. M means 50 to 250 employees (medium). 
    4. L means  more than 250 employees (large).
    5. Portions of this data is from the Reference Section.",
    x = " Country",
    y = "Amount of Companies",
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0.5))+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  scale_y_discrete(guide = guide_axis(check.overlap = TRUE))+
  annotate("text", x = 1.3, y = 100, label = "50", color = "black")+
  annotate("text", x = 1.3, y = 200, label = "100", color = "black")+
  annotate("text", x = 1.3, y = 300, label = "150", color = "black")+
  annotate("text", x = 1.3, y = 400, label = "200", color = "black")+
  annotate("text", x = 1.3, y = 500, label = "250", color = "black")+
  annotate("text", x = 1.3, y = 600, label = "300", color = "black")+
  coord_flip()

JandS <- tibble(All_Data$job_title, All_Data$salary_in_usd, 
                All_Data$company_location)


colnames(JandS)<-c("Title", "SalaryUSD", "Company_Location" )

view(JandS)

#  which country has the ten highest-paying and lowest-paying jobs.
# Top 10 positions by Salary

Top10 <- with(JandS,  JandS[order(SalaryUSD) , ])
nrow(Top10)
L <- Top10[-c(1:597), ]
L
colnames(L)
nrow(L)

ggplot(data = L, mapping = aes(x = Title, y = SalaryUSD, fill = Company_Location)) +
  geom_bar(stat = "identity")+
  labs(
    title = " Top Ten Highest Paying Data Science Jobs",
    subtitle = "Data Science: Where in the World am I Going to Work?",
    caption = "1. Salary in USD: The salary in USD (FX rate divided by avg. USD rate for the respective year via fxdata.foorilla.com).
    2. Job Title: The role worked in during the year.
    3. Portions of this data is from the Reference Section.",
    y = "Salary in USD ($)",
    x = "Job Title",
  )+
  theme_classic()+
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0.5))+
  coord_flip()

# Bottom 10 positions by Salary

Bottom10 <- with(JandS,  JandS[order(SalaryUSD) , ])
nrow(Bottom10)
B <- Bottom10[-c(11:607), ]
B
colnames(B)
nrow(B)

ggplot(data = B, mapping = aes(x = Title, y = SalaryUSD, fill = Company_Location)) +
  geom_bar(stat = "identity")+
  labs(
    title = " Ten Lowest Paying Data Science Jobs (USD)",
    subtitle = "Data Science: Where in the World am I Going to Work?",
    caption = "1. Salary in USD.
    2. Job Title: The role worked in during the year.
    3. Company Location main office or contracting branch as an ISO 3166 country code.
    4. CH =  Switzerland.  IN = India.  IR = Iran.  
    5. MX = Mexico.  PK = Pakistan.  US = United States.  VN = Vietnam. 
    6. Portions of this data is from the Reference Section.",
    y = "Salary in USD ($)",
    x = "Job Title",
  )+
  theme_classic()+
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0.5))+
  coord_flip()

# which job will pay you the most for your experience level?  Let's explore it.
# Every Job pay based on Experience

ggplot(All_Data, aes(y = salary_in_usd, x =  job_title, fill = experience_level))+
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    title = " Data Science Jobs, Experience, and Salary ",
    subtitle = "Data Science: Where in the World am I Going to Work?",
    caption = "1. The Salary is in USD, k= x1,000.
    2. Experience Level: EN Entry-level / Junior,  
    3. MI Mid-level / Intermediate, 
    4. SE Senior-level / Expert,  
    5. EX Executive-level / Director.
    6. Portions of this data is from the Reference Section.",
    x = "Job Titles",
    y = "Salary in USD ($)",
  ) +
  theme_bw()+
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0.5))+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  scale_y_discrete(guide = guide_axis(check.overlap = TRUE))+
  annotate("text", x = 1.3, y = 1100000, label = "$450k", color = "black")+
  annotate("text", x = 1.3, y = 8000000, label = "$225k", color = "black")+
  annotate("text", x = 1.3, y = 14000000, label = "$75k", color = "black")+
  coord_flip()

# Based on Experience which job pays the most and what country 

PandS <- tibble(All_Data$job_title, All_Data$salary_in_usd, 
                All_Data$company_location, All_Data$experience_level)


colnames(PandS)<-c("Title", "SalaryUSD", "Company_Location", "Exp" )

EN <- with(PandS,  PandS[order(Exp == "EN",  SalaryUSD) , ])
Q <- EN[-c(1:606), ]
Q
MI <- with(PandS,  PandS[order(Exp == "MI",  SalaryUSD) , ])
R <- MI[-c(1:606), ]
R
SE <- with(PandS,  PandS[order(Exp == "SE",  SalaryUSD) , ])
S <- SE[-c(1:606), ]
S
EX <- with(PandS,  PandS[order(Exp == "EX",  SalaryUSD) , ])
T <- EX[-c(1:606), ]
T

# Experience Salary Location
ggplot(All_Data, aes(x = company_location, y = experience_level, size = salary_in_usd))+
  geom_point(aes(color = salary_in_usd), alpha=.8)+
  theme_classic() +
  labs(
    title = " Data Science Experience, Salary, and Location",
    subtitle = "Data Science: Where in the World am I Going to Work?",
    caption = "1. Company Location main office or contracting branch as an ISO 3166 country code.
2. Experience Level: EN Entry-level / Junior,  
    3. MI Mid-level / Intermediate, 
    4. SE Senior-level / Expert,  
    5. EX Executive-level / Director.
    6. Portions of this data is from the Reference Section.",
    x = "Country",
    y = "Experience Level",
  ) +
  theme_bw()+
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0.5))+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  guides(size = FALSE)+
  guides(color=guide_legend(title="Salary in USD"))

#For the Expert experience level, we can ascertain that it has fewer opportunities but higher pay based on the visualization.  Additionally, we can conclude that most countries have more Mid-level and Senior Level positions than any other position.  Now that we know our opportunities based on our experience level, let us explore remote job opportunities. 

# Career Location 

# Ten Best remote based on Salary 
BandS <- tibble(All_Data$job_title, All_Data$salary_in_usd, 
                All_Data$company_location, All_Data$experience_level, All_Data$remote_ratio, All_Data$employee_residence, All_Data$work_year)

colnames(BandS)<-c("Title", "SalaryUSD", "Company_Location", "Exp", "RemoteWork", "Employee Residence", "Year" )

Top10R <- with(BandS,  BandS[order(RemoteWork == "100",  SalaryUSD) , ])
nrow(Top10R)
Z <- Top10R[-c(1:597), ]
Z
colnames(Z)
nrow(Z)
Z%>%
  gt() %>%
  tab_header(
    title = md("**Ten Best Remote Paying Jobs**"),
    subtitle = md("Data Analyst Jobs 2020-2022")
  ) %>%
  cols_width(
    everything() ~ px(160)) %>%
  tab_source_note(
    source_note = md("Portions of this data is from the Reference Section.")
  ) %>%
  tab_footnote(
    footnote = md(" Salary is in USD.")) %>%
  tab_footnote(
    footnote = md(" Experience Level: The experience level in the job during the year with the following possible values: EN Entry-level / Junior,  MI Mid-level / Intermediate, SE Senior-level / Expert,  EX Executive-level / Director.
")) %>%
  tab_footnote(
    footnote = md("  Employee Residence: Employee's primary country of residence in during the work year as an ISO 3166 country code.
")) %>%
  tab_footnote(
    footnote = md(" Remote Ratio: The overall amount of work done remotely, possible values are as follows: 0 No remote work (less than 20%), 50 Partially remote,  100 Fully remote (more than 80%).
")) %>%
  opt_table_font(
    font = "Times New Roman",
    weight = 300) %>%
  cols_align(
    align = "center",
    columns = everything())

# Ten worst Remote based on Salary
YandS <- tibble(All_Data$job_title, All_Data$salary_in_usd, 
                All_Data$company_location, All_Data$experience_level, All_Data$remote_ratio, All_Data$employee_residence)

colnames(YandS)<-c("Title", "SalaryUSD", "Company_Location", "Exp", "RemoteWork", "Employee Residence" )


B10R <- with(YandS,  YandS[order(RemoteWork == "100") , ])

l <- YandS%>% filter(RemoteWork == "50")
l
B10R <- l[order(l$SalaryUSD), ]
B10R
nrow(B10R)
Y <- B10R[-c(11:99), ]
Y
colnames(Y)
nrow(Y)
Y%>%
  gt() %>%
  tab_header(
    title = md("**Ten Best Remote Paying Jobs**"),
    subtitle = md("Data Analyst Jobs 2020-2022")
  ) %>%
  cols_width(
    everything() ~ px(160)) %>%
  tab_source_note(
    source_note = md("Portions of this data is from the Reference Section.")
  ) %>%
  tab_footnote(
    footnote = md(" IN = India.   PK = Pakistan.  ES =  Spain.  TR = Turkey.   IT = Italy.   SI = Slovenia.  CO = Columbia")) %>%
  tab_footnote(
    footnote = md(" Experience Level: The experience level in the job during the year with the following possible values: EN Entry-level / Junior,  MI Mid-level / Intermediate, SE Senior-level / Expert,  EX Executive-level / Director.
")) %>%
  tab_footnote(
    footnote = md("  Employee Residence: Employee's primary country of residence in during the work year as an ISO 3166 country code.
")) %>%
  tab_footnote(
    footnote = md(" Remote Ratio: The overall amount of work done remotely, possible values are as follows: 0 No remote work (less than 20%), 50 Partially remote,  100 Fully remote (more than 80%).
")) %>%
  opt_table_font(
    font = "Times New Roman",
    weight = 300) %>%
  cols_align(
    align = "center",
    columns = everything())


# Ten Best Non Remote based on Salary
KandS <- tibble(All_Data$job_title, All_Data$salary_in_usd, 
                All_Data$company_location, All_Data$experience_level, All_Data$remote_ratio, All_Data$employee_residence)

colnames(KandS)<-c("Title", "SalaryUSD", "Company_Location", "Exp", "RemoteWork", "Employee Residence" )


B10NR <- with(KandS,  KandS[order(RemoteWork == "0") , ])

h <- KandS%>% filter(RemoteWork == "0")
h
B10NR <- h[order(-h$SalaryUSD), ]
B10NR
nrow(B10NR)
u <- B10NR[-c(11:127), ]
u
colnames(u)
nrow(u)
u%>%
  gt() %>%
  tab_header(
    title = md("**Ten Best Non Remote Paying Jobs**"),
    subtitle = md("Data Analyst Jobs 2020-2022")
  ) %>%
  cols_width(
    everything() ~ px(160)) %>%
  tab_source_note(
    source_note = md("Portions of this data is from the Reference Section.")
  ) %>%
  tab_footnote(
    footnote = md(" JP = Japan.   US = United States.")) %>%
  tab_footnote(
    footnote = md(" Experience Level: The experience level in the job during the year with the following possible values: EN Entry-level / Junior,  MI Mid-level / Intermediate, SE Senior-level / Expert,  EX Executive-level / Director.
")) %>%
  tab_footnote(
    footnote = md("  Employee Residence: Employee's primary country of residence in during the work year as an ISO 3166 country code.
")) %>%
  tab_footnote(
    footnote = md(" Remote Ratio: The overall amount of work done remotely, possible values are as follows: 0 No remote work (less than 20%), 50 Partially remote,  100 Fully remote (more than 80%).
")) %>%
  opt_table_font(
    font = "Times New Roman",
    weight = 300) %>%
  cols_align(
    align = "center",
    columns = everything())

# Ten worst Non Remote based on Salary
GandS <- tibble(All_Data$job_title, All_Data$salary_in_usd, 
                All_Data$company_location, All_Data$experience_level, All_Data$remote_ratio, All_Data$employee_residence)

colnames(GandS)<-c("Title", "SalaryUSD", "Company_Location", "Exp", "RemoteWork", "Employee Residence" )


B10RW <- with(GandS,  GandS[order(RemoteWork == "0") , ])

g <- GandS%>% filter(RemoteWork == "0")
g
B10RW <- g[order(g$SalaryUSD), ]
B10RW
nrow(B10RW)
v <- B10RW[-c(11:127), ]
v
colnames(v)
nrow(v)
v%>%
  gt() %>%
  tab_header(
    title = md("**Ten Worst Non Remote Paying Jobs**"),
    subtitle = md("Data Analyst Jobs 2020-2022")
  ) %>%
  cols_width(
    everything() ~ px(160)) %>%
  tab_source_note(
    source_note = md("Portions of this data is from the Reference Section.")
  ) %>%
  tab_footnote(
    footnote = md(" MX = Mexico.  VN = Vietnam. CH =  Switzerland.  IN = India. TR = Turkey.   BR = Brazil   MD = Modolva  HN = Honduras")) %>%
  tab_footnote(
    footnote = md(" Experience Level: The experience level in the job during the year with the following possible values: EN Entry-level / Junior,  MI Mid-level / Intermediate, SE Senior-level / Expert,  EX Executive-level / Director.
")) %>%
  tab_footnote(
    footnote = md("  Employee Residence: Employee's primary country of residence in during the work year as an ISO 3166 country code.
")) %>%
  tab_footnote(
    footnote = md(" Remote Ratio: The overall amount of work done remotely, possible values are as follows: 0 No remote work (less than 20%), 50 Partially remote,  100 Fully remote (more than 80%).
")) %>%
  opt_table_font(
    font = "Times New Roman",
    weight = 300) %>%
  cols_align(
    align = "center",
    columns = everything())

# Conclusion

#After exploring the different positions within the Data Science field, one may be able to make a sound decision on which position they would like to endeavor in,  the country that provides them the most opportunities based on pay and experience level. 

# Reference Section:

#1. Irizarry, R. A. (2022, July 7). Introduction to Data Science. HARVARD Data Science. Retrieved August 8, 2022, from Https://rafalab.github.io/dsbook/
  # This project utilized "Introduction to Data Science Data Analysis and Prediction Algorithms with R" by our course instructor Rafael A. Irizarry published 2022-07-07.

#2. Cost of living. Cost of Living. (n.d.). Retrieved October 11, 2022, from https://www.numbeo.com/cost-of-living/
# Numbeo is the world’s largest cost of living database. Numbeo is also a crowd-sourced global database of quality of life data: housing indicators, perceived crime rates, healthcare quality, transport quality, and other statistics.

#3. Banton, C. (2022, February 8). Cost of living. Investopedia. Retrieved October 11, 2022, from https://www.investopedia.com/terms/c/cost-of-living.asp 

#4. Cost of living. Cambridge Dictionary. (n.d.). Retrieved October 11, 2022, from https://dictionary.cambridge.org/dictionary/english/cost-of-living 

#5. Cost of living index by state 2022. (n.d.). Retrieved October 11, 2022, from https://worldpopulationreview.com/state-rankings/cost-of-living-index-by-state 

#6. Wikimedia Foundation. (2022, October 5). List of ISO 3166 country codes. Wikipedia. Retrieved October 11, 2022, from https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes 

#7. Statistics on remote workers that will surprise you (2022). Apollo Technical LLC. (2022, May 31). Retrieved October 11, 2022, from https://www.apollotechnical.com/statistics-on-remote-workers/ 

#8. Wise, J. (2022, July 28). Remote work statistics 2022: How many people work from home during Covid? EarthWeb. Retrieved October 11, 2022, from https://earthweb.com/remote-work-statistics/ 

