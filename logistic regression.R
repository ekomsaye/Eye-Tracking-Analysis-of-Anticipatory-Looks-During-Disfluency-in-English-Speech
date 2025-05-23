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
#loading the desired data
setwd("C:/Users/ekoms/Desktop/Upwork/Allia/Project 4")
Experiment1_data<-read_xlsx("Experiment1_updated.xlsx")
Experiment2_data<-read_xlsx("Experiment2_updated.xlsx")
Experiment3_data<-read_xlsx("Experiment3_updated.xlsx")

# Removing participants with low frequency
filtered_data1<-Experiment1_data %>% filter(!(participant_number%in% c(3,17)))%>%
  mutate(lf_level=ifelse(looked_lf=="TRUE",0,1),
         fluency_level=ifelse(`fluency condition`=="disfluent",1,0))
filtered_data3<-Experiment3_data %>% filter(!(participant_number%in% c(29,38,43,48)))%>%
  mutate(lf_level=ifelse(looked_lf=="TRUE",0,1),
         fluency_level=ifelse(`fluency condition`=="disfluent",0,1))

# Post Analysis
combined_data<-filtered_data1%>% rbind(filtered_data3)%>% mutate(Nativeness=c(rep(0,47865),rep(1,54552)))


# Simulated data (replace with your actual data)

# Fit GLMM fluent
model <- glmer(lf_level ~ `fluency condition` + sqrt(linear_time)+log(quadratic_time)+Nativeness + 
                 Nativeness*(sqrt(linear_time))+Nativeness*(log(quadratic_time))+
                 (1 |`sentence template`) + (1 |participant_id)+ (1 | trial_number),
               data = combined_data, family = binomial(link = "logit"))

# Interpret results, check p-values, etc.
summary(model)

#GLMM dis-fluent
log_data<-combined_data %>%
  mutate(lf_level=ifelse(looked_lf=="TRUE",0,1),
         fluency_level=ifelse(`fluency condition`=="disfluent",0,1)
  )

log_data$fluency_combined <- factor(
  ifelse(log_data$`fluency condition` == "fluent", "fluent", "disfluent"),
  levels = c("fluent", "disfluent")
)

model2 <- glmer(lf_level ~ fluency_combined + sqrt(linear_time)+log(quadratic_time)+Nativeness + 
                  Nativeness*(sqrt(linear_time))+Nativeness*(log(quadratic_time))+
                  (1 |`sentence template`) + (1 |participant_id)+ (1 | trial_number),
                data = log_data, family = binomial(link = "logit"))
summary(model2)


#comparing the logistic reg intercepts
# Extract intercept coefficients and standard errors
intercept_fluent <- c(-348.4,-0.07939,0.004944,-0.02294,0.00003285,0.07389,0.03936,-0.1072,-0.1273,0.00468)
intercept_disfluent <- c(-348.5,0.07939,0.004944,-0.02294,0.00003285,0.07389,0.03936,-0.1072,-0.1273,0.00468)

# Extract standard errors
se_fluent <- c(47.98,0.08317,0.004774,0.01721,0.000004536,0.04823,0.04312,0.08949,0.08365,0.00188)
se_disfluent <- c(47.98,0.08317,0.004774,0.01721,0.000004536,0.04823,0.04312,0.08949,0.08365,0.00188)

# Calculate the z-score
z_score <- (intercept_fluent - intercept_disfluent) / sqrt(se_fluent^2 + se_disfluent^2)

# Calculate the two-tailed p-value
p_value <- 2 * (1 - pnorm(abs(z_score)))
variables<-c("intercept","condition","linear time","quadratic time","participant ID",
             "sentence template D2","sentence template D3","sentence template F1","sentence template F2",
             "trial number")

# Create a data frame with model names and p-values
p_value_table <- data.frame(variables,intercept_fluent,intercept_disfluent,z_score,p_value)

# Print the table
kable(p_value_table, format = "markdown")
