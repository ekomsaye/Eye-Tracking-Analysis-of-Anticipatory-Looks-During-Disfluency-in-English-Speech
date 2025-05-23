#Important libraries
library(tidyverse)
library(readxl)
library(summarytools)
library(kableExtra)
library(epiDisplay)
library(knitr)
library(janitor)
library(gtsummary)
library(flextable)
library(jtools)
library(GGally)
library(knitr)
library(lme4)

#set working directory
setwd("C:/Users/ekoms/Desktop/Upwork/Allia/Project 4")

#loading the desired data
Experiment1_data<-read_xlsx("Experiment1_updated.xlsx")
Experiment2_data<-read_xlsx("Experiment2_updated.xlsx")
Experiment3_data<-read_xlsx("Experiment3_updated.xlsx")

################################
#                              #
#Analysis of Experiment1 data  #
#                              #
################################

# Removing participants with low frequency
filtered_data<-Experiment1_data %>% filter(!(participant_number%in% c(3,17)))%>%
  mutate(lf_level=ifelse(looked_lf=="TRUE",1,0),
         fluency_level=ifelse(`fluency condition`=="disfluent",1,0))

#Getting the summary and structure of Experiment 1 data
str(filtered_data)
summary(filtered_data)

#Exploratory analysis

#Trial number
sd(filtered_data$trial_number)
summary(filtered_data$trial_number)

# A box plot for number of trials
boxplot(filtered_data$trial_number,col="blue",main="Number of Trials",
        xlab="Distribution",ylab="Trials")

#Fluency condition
My_data<- filtered_data %>% 
  count(`fluency condition`) %>% 
  mutate(perc=n/sum(n) * 100) %>% arrange(desc(`fluency condition`))
My_data %>% 
  ggplot()+
  aes(x=`fluency condition`,y = n)+
  geom_col(fill="green")+
  geom_text(aes(x=`fluency condition`,y=n
                ,label = paste0 (n," (", round (perc,1),"%)")
                ,vjust = -0.2 )) +
  theme_classic()+
  labs(x="Fluency Condition", y="Number",title = "Number of Fluency Conditions")

#looked lf
My_data<- filtered_data %>% 
  count(looked_lf) %>% 
  mutate(perc=n/sum(n) * 100) %>% arrange(desc(looked_lf))
My_data %>% 
  ggplot()+
  aes(x=2,y = perc,fill=looked_lf)+
  coord_polar("y",start=1)+
  geom_col()+
  geom_text(aes(label = paste0 (n," (", round (perc,1),"%)"))
            ,position=position_stack(vjust = 0.5 ))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size= 18)) +
  labs(x="looks to lf", y="Number",title = "Number of looks to L.F Object")

#looks to lf under dis-fluent condition
My_data<- filtered_data %>% 
  filter(`fluency condition`=="disfluent")%>%
  count(looked_lf) %>% 
  mutate(perc=n/sum(n) * 100) %>% arrange(desc(looked_lf))
My_data %>% 
  ggplot()+
  aes(x=2,y = perc,fill=looked_lf)+
  coord_polar("y",start=1)+
  geom_col()+
  geom_text(aes(label = paste0 (n," (", round (perc,1),"%)"))
            ,position=position_stack(vjust = 0.5 ))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size= 18)) +
  xlim(0.5,2.5)+
  labs(x="looks to lf", y="Number",title = "No.of looks to L.F Object on disfluent")

#looks to hf

My_data<- filtered_data %>% 
  count(looked_hf) %>% 
  mutate(perc=n/sum(n) * 100) %>% arrange(desc(looked_hf))

#looks to hf under disfluent condition
filtered_data%>%filter(`fluency condition`=="disfluent")%>%
tabyl(looked_hf)

#Linear time
sd(filtered_data$linear_time)
summary(filtered_data$linear_time)
boxplot((filtered_data$linear_time),col="yellow",main="Linear Time",
        xlab="Distribution",ylab="Time")

#Quadratic time
sd(filtered_data$quadratic_time)
summary(filtered_data$quadratic_time)
boxplot(filtered_data$quadratic_time,col="orange",main="Quadratic Time",
        xlab="Distribution",ylab="Time")


#Logistic regression model
cor(filtered_data$linear_time,filtered_data$quadratic_time)
plot(filtered_data$linear_time,filtered_data$quadratic_time,
     col="red",main="Scatter plot for linear time against quadratic time",
     xlab = "Linear time",ylab="Quadratic time",shape=3)

log_data<-Experiment1_data %>%
  mutate(lf_level=ifelse(looked_lf=="TRUE",1,0),
         fluency_level=ifelse(`fluency condition`=="disfluent",1,0)
         )

log_data$fluency_combined <- factor(
  ifelse(log_data$`fluency condition` == "fluent", "fluent", "disfluent"),
  levels = c("fluent", "disfluent")
)

model_disfluent <- glmer(lf_level ~ fluency_combined + sqrt(linear_time)+log(quadratic_time)+
                           (1 |`sentence template`) + (1 |participant_id)+ (1 | trial_number),
                         data = log_data, family = binomial(link = "logit"))
summary(model_disfluent)
model_fluent<-glmer(lf_level ~ `fluency condition` + sqrt(linear_time)+log(quadratic_time)+
                      (1 |`sentence template`) + (1 |participant_id)+ (1 | trial_number),
                    data = filtered_data, family = binomial(link = "logit"))
summary(model_fluent)

# exporting fit model
export_summs(model_disfluent, scale = FALSE, digits = 4, to.file = "docx", file.name = "fit regression_result.docx")
export_summs(model_fluent, scale = FALSE, digits = 4, to.file = "docx", file.name = "fit2 regression_result.docx")


#chi-square
table_1 =  filtered_data %>% select(`fluency condition`,looked_lf,looked_hf)%>%
  tbl_summary(by = `fluency condition`) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**fluency condition**") %>%
  modify_footnote(
    all_stat_cols() ~ "Median (IQR) or Frequency (%)"
  ) %>%
  modify_caption("**Chi-square**") %>%
  bold_labels()

table_1


################################
#                              #
#Analysis of Experiment2 data  #
#                              #
################################
# Removing participants with low frequency
filtered_data<-Experiment2_data %>% filter(!(participant_number%in% c(7,13,29)))%>%
  mutate(lf_level=ifelse(looked_lf=="TRUE",1,0),
         fluency_level=ifelse(`fluency condition`=="disfluent",1,0))

#Getting the summary and structure of Experiment 1 data
str(filtered_data)
summary(filtered_data)

#Exploratory analysis

#Trial number
sd(filtered_data$trial_number)
summary(filtered_data$trial_number)

# A box plot for number of trials
boxplot(filtered_data$trial_number,col="blue",main="Number of Trials",
        xlab="Distribution",ylab="Trials")

#Fluency condition
My_data<- filtered_data %>% 
  count(`fluency condition`) %>% 
  mutate(perc=n/sum(n) * 100) %>% arrange(desc(`fluency condition`))
My_data %>% 
  ggplot()+
  aes(x=`fluency condition`,y = n)+
  geom_col(fill="green")+
  geom_text(aes(x=`fluency condition`,y=n
                ,label = paste0 (n," (", round (perc,1),"%)")
                ,vjust = -0.2 )) +
  theme_classic()+
  labs(x="Fluency Condition", y="Number",title = "Number of Fluency Conditions")

#looked lf
My_data<- filtered_data %>% 
  count(looked_lf) %>% 
  mutate(perc=n/sum(n) * 100) %>% arrange(desc(looked_lf))
My_data %>% 
  ggplot()+
  aes(x=2,y = perc,fill=looked_lf)+
  coord_polar("y",start=1)+
  geom_col()+
  geom_text(aes(label = paste0 (n," (", round (perc,1),"%)"))
            ,position=position_stack(vjust = 0.5 ))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size= 18)) +
  labs(x="looks to lf", y="Number",title = "Number of looks to L.F Object")

#looks to lf under dis-fluent condition
My_data<- filtered_data %>% 
  filter(`fluency condition`=="disfluent")%>%
  count(looked_lf) %>% 
  mutate(perc=n/sum(n) * 100) %>% arrange(desc(looked_lf))
My_data %>% 
  ggplot()+
  aes(x=2,y = perc,fill=looked_lf)+
  coord_polar("y",start=1)+
  geom_col()+
  geom_text(aes(label = paste0 (n," (", round (perc,1),"%)"))
            ,position=position_stack(vjust = 0.5 ))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size= 18)) +
  xlim(0.5,2.5)+
  labs(x="looks to lf", y="Number",title = "No.of looks to L.F Object on disfluent")

#looks to hf

My_data<- filtered_data %>% 
  count(looked_hf) %>% 
  mutate(perc=n/sum(n) * 100) %>% arrange(desc(looked_hf))

#looks to hf under disfluent condition
filtered_data%>%filter(`fluency condition`=="disfluent")%>%
  tabyl(looked_hf)

#Linear time
sd(filtered_data$linear_time)
summary(filtered_data$linear_time)
boxplot((filtered_data$linear_time),col="yellow",main="Linear Time",
        xlab="Distribution",ylab="Time")

#Quadratic time
sd(filtered_data$quadratic_time)
summary(filtered_data$quadratic_time)
boxplot(filtered_data$quadratic_time,col="orange",main="Quadratic Time",
        xlab="Distribution",ylab="Time")


#Logistic regression model
cor(filtered_data$linear_time,filtered_data$quadratic_time)
plot(filtered_data$linear_time,filtered_data$quadratic_time,
     col="red",main="Scatter plot for linear time against quadratic time",
     xlab = "Linear time",ylab="Quadratic time",shape=3)

log_data<-Experiment1_data %>%
  mutate(lf_level=ifelse(looked_lf=="TRUE",1,0),
         fluency_level=ifelse(`fluency condition`=="disfluent",1,0)
  )

log_data$fluency_combined <- factor(
  ifelse(log_data$`fluency condition` == "fluent", "fluent", "disfluent"),
  levels = c("fluent", "disfluent")
)

model_disfluent <- glmer(lf_level ~ fluency_combined + sqrt(linear_time)+log(quadratic_time)+
                           (1 |`sentence template`) + (1 |participant_id)+ (1 | trial_number),
                         data = log_data, family = binomial(link = "logit"))
summary(model_disfluent)
model_fluent<-glmer(lf_level ~ `fluency condition` + sqrt(linear_time)+log(quadratic_time)+
                      (1 |`sentence template`) + (1 |participant_id)+ (1 | trial_number),
                    data = filtered_data, family = binomial(link = "logit"))
summary(model_fluent)

# exporting fit model
export_summs(model_disfluent, scale = FALSE, digits = 4, to.file = "docx", file.name = "fit regression_result.docx")
export_summs(model_fluent, scale = FALSE, digits = 4, to.file = "docx", file.name = "fit2 regression_result.docx")


#chi-square
table_1 =  filtered_data %>% select(`fluency condition`,looked_lf,looked_hf)%>%
  tbl_summary(by = `fluency condition`) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**fluency condition**") %>%
  modify_footnote(
    all_stat_cols() ~ "Median (IQR) or Frequency (%)"
  ) %>%
  modify_caption("**Chi-square**") %>%
  bold_labels()

table_1


################################
#                              #
#Analysis of Experiment3 data  #
#                              #
################################

# Removing participants with low frequency
filtered_data<-Experiment3_data %>% filter(!(participant_number%in% c(29,38,43,48)))%>%
  mutate(lf_level=ifelse(looked_lf=="TRUE",1,0),
         fluency_level=ifelse(`fluency condition`=="disfluent",1,0))

#Getting the summary and structure of Experiment 1 data
str(filtered_data3)
summary(filtered_data3)

#Exploratory analysis

#Trial number
sd(filtered_data$trial_number)
summary(filtered_data$trial_number)

# A box plot for number of trials
boxplot(filtered_data$trial_number,col="blue",main="Number of Trials",
        xlab="Distribution",ylab="Trials")

#Fluency condition
My_data<- filtered_data %>% 
  count(`fluency condition`) %>% 
  mutate(perc=n/sum(n) * 100) %>% arrange(desc(`fluency condition`))
My_data %>% 
  ggplot()+
  aes(x=`fluency condition`,y = n)+
  geom_col(fill="green")+
  geom_text(aes(x=`fluency condition`,y=n
                ,label = paste0 (n," (", round (perc,1),"%)")
                ,vjust = -0.2 )) +
  theme_classic()+
  labs(x="Fluency Condition", y="Number",title = "Number of Fluency Conditions")

#looked lf
My_data<- filtered_data %>% 
  count(looked_lf) %>% 
  mutate(perc=n/sum(n) * 100) %>% arrange(desc(looked_lf))
My_data %>% 
  ggplot()+
  aes(x=2,y = perc,fill=looked_lf)+
  coord_polar("y",start=1)+
  geom_col()+
  geom_text(aes(label = paste0 (n," (", round (perc,1),"%)"))
            ,position=position_stack(vjust = 0.5 ))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size= 18)) +
  labs(x="looks to lf", y="Number",title = "Number of looks to L.F Object")

#looks to lf under dis-fluent condition
My_data<- filtered_data %>% 
  filter(`fluency condition`=="disfluent")%>%
  count(looked_lf) %>% 
  mutate(perc=n/sum(n) * 100) %>% arrange(desc(looked_lf))
My_data %>% 
  ggplot()+
  aes(x=2,y = perc,fill=looked_lf)+
  coord_polar("y",start=1)+
  geom_col()+
  geom_text(aes(label = paste0 (n," (", round (perc,1),"%)"))
            ,position=position_stack(vjust = 0.5 ))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size= 18)) +
  xlim(0.5,2.5)+
  labs(x="looks to lf", y="Number",title = "No.of looks to L.F Object on disfluent")

#looks to hf

My_data<- filtered_data %>% 
  count(looked_hf) %>% 
  mutate(perc=n/sum(n) * 100) %>% arrange(desc(looked_hf))

#looks to hf under disfluent condition
filtered_data%>%filter(`fluency condition`=="disfluent")%>%
  tabyl(looked_hf)

#Linear time
sd(filtered_data$linear_time)
summary(filtered_data$linear_time)
boxplot((filtered_data$linear_time),col="yellow",main="Linear Time",
        xlab="Distribution",ylab="Time")

#Quadratic time
sd(filtered_data$quadratic_time)
summary(filtered_data$quadratic_time)
boxplot(filtered_data$quadratic_time,col="orange",main="Quadratic Time",
        xlab="Distribution",ylab="Time")


#Logistic regression model
cor(filtered_data$linear_time,filtered_data$quadratic_time)
plot(filtered_data$linear_time,filtered_data$quadratic_time,
     col="red",main="Scatter plot for linear time against quadratic time",
     xlab = "Linear time",ylab="Quadratic time",shape=3)

log_data<-Experiment1_data %>%
  mutate(lf_level=ifelse(looked_lf=="TRUE",1,0),
         fluency_level=ifelse(`fluency condition`=="disfluent",1,0)
  )

log_data$fluency_combined <- factor(
  ifelse(log_data$`fluency condition` == "fluent", "fluent", "disfluent"),
  levels = c("fluent", "disfluent")
)

model_disfluent <- glmer(lf_level ~ fluency_combined + sqrt(linear_time)+log(quadratic_time)+
                           (1 |`sentence template`) + (1 |participant_id)+ (1 | trial_number),
                         data = log_data, family = binomial(link = "logit"))
summary(model_disfluent)
model_fluent<-glmer(lf_level ~ `fluency condition` + sqrt(linear_time)+log(quadratic_time)+
                      (1 |`sentence template`) + (1 |participant_id)+ (1 | trial_number),
                    data = filtered_data, family = binomial(link = "logit"))
summary(model_fluent)

# exporting fit model
export_summs(model_disfluent, scale = FALSE, digits = 4, to.file = "docx", file.name = "fit regression_result.docx")
export_summs(model_fluent, scale = FALSE, digits = 4, to.file = "docx", file.name = "fit2 regression_result.docx")


#chi-square
table_1 =  filtered_data %>% select(`fluency condition`,looked_lf,looked_hf)%>%
  tbl_summary(by = `fluency condition`) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**fluency condition**") %>%
  modify_footnote(
    all_stat_cols() ~ "Median (IQR) or Frequency (%)"
  ) %>%
  modify_caption("**Chi-square**") %>%
  bold_labels()

table_1

