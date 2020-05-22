---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Load data and get rid of non-complete data points

my_first_instrument_complete
2 complete 
0 is not complete
```{r}
library(prettyR)
library(descr)
library(ggplot2)
library(dplyr)
library(grid)
#TelehealthZoomclient_DATA_2020-05-20_1337
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/satisfaction")
tele_zoom_dat = read.csv("TelehealthZoomclient_DATA_2020-05-20_1337.csv", header = TRUE, na.strings = c(""))
tele_zoom_dat_complete  = subset(tele_zoom_dat, my_first_instrument_complete == 2)
dim(tele_zoom_dat_complete)
library(naniar)
## use pairwise
miss_var_summary(tele_zoom_dat_complete)

### After removing the incomplete only found one staff who took the survey on accident
tele_zoom_dat_complete = subset(tele_zoom_dat_complete, record_id != 37)
```
video_audio overall
In the Zoom session you just completed with your Centerstone provider, did you use audio only or audio and video?
1, Yes video and audio | 2, No audio only
```{r}

video_audio_overall_dat = tele_zoom_dat_complete


describe.factor(tele_zoom_dat_complete$video_audio)


video_audio_overall_dat = na.omit(data.frame(video_audio_overall = tele_zoom_dat_complete$video_audio))
n_video_audio_overall = dim(video_audio_overall_dat)[1]

video_audio_overall_dat$video_audio_overall = recode(video_audio_overall_dat$video_audio_overall, "1" = "Yes video and audio", "2" = "No audio only")



video_audio_overall_dat = video_audio_overall_dat %>% count(video_audio_overall)

video_audio_overall_dat$percent = as.numeric(video_audio_overall_dat$n / n_video_audio_overall)

video_audio_overall_dat$percent = round(video_audio_overall_dat$percent, 2)*100
video_audio_overall_dat$percent = paste0(video_audio_overall_dat$percent, "%")
title_video_audio_overalll = paste0("In the Zoom session you just completed with your Centerstone provider, \n did you use audio only or audio and video?", " ", "n=", n_video_audio_overall)
#reorder(gender_overall, -n)

plot_video_audio_overall = ggplot(video_audio_overall_dat, aes(x = reorder(video_audio_overall, -n),y =n, fill = video_audio_overall))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_video_audio_overalll, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,300))+
  geom_text(aes(label = percent), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")
plot_video_audio_overall

```
return_client overall
Are you a new or returning client?
1, New | 2, Returning
```{r}
return_client_overall_dat = tele_zoom_dat_complete


describe.factor(tele_zoom_dat_complete$return_client)


return_client_overall_dat = na.omit(data.frame(return_client_overall = tele_zoom_dat_complete$return_client))
n_return_client_overall = dim(return_client_overall_dat)[1]

return_client_overall_dat$return_client_overall = recode(return_client_overall_dat$return_client_overall, "1" = "New", "2" = "Returning")



return_client_overall_dat = return_client_overall_dat %>% count(return_client_overall)

return_client_overall_dat$percent = as.numeric(return_client_overall_dat$n / n_return_client_overall)

return_client_overall_dat$percent = round(return_client_overall_dat$percent, 2)*100
return_client_overall_dat$percent = paste0(return_client_overall_dat$percent, "%")
title_return_client_overalll = paste0("Are you a new or returning client?", " ", "n=", n_return_client_overall)

plot_return_client_overall = ggplot(return_client_overall_dat, aes(x = reorder(return_client_overall, -n),y =n, fill = return_client_overall))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_return_client_overalll, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,300))+
  geom_text(aes(label = percent), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")
plot_return_client_overall



```
instructions overall
Were the instructions for how to access your appointment online clear?
1, Very Clear | 2, Mostly Clear | 3, Somewhat Clear | 4, Not at all Clear | 5, Not Applicable

```{r}
instructions_overall_dat = tele_zoom_dat_complete

describe.factor(tele_zoom_dat_complete$instructions)


instructions_overall_dat = na.omit(data.frame(instructions_overall = tele_zoom_dat_complete$instructions))
n_instructions_overall = dim(instructions_overall_dat)[1]

instructions_overall_dat$instructions_overall = recode(instructions_overall_dat$instructions_overall, "1" = "Very Clear", "2" = "Mostly Clear", "3" = "Somewhat Clear", "4" = "Not at all Clear", "5" = "Not Applicable")

## relevel them
instructions_overall_dat$instructions_overall = as.factor(instructions_overall_dat$instructions_overall)
instructions_overall_dat$instructions_overall = factor(instructions_overall_dat$instructions_overall, levels = c("Very Clear","Mostly Clear", "Somewhat Clear", "Not at all Clear", "Not Applicable"))

instructions_overall_dat = instructions_overall_dat %>% count(instructions_overall)

instructions_overall_dat$percent = as.numeric(instructions_overall_dat$n / n_instructions_overall)



### Create extra annotation
greater_instructions_overall = paste0(round(sum(instructions_overall_dat$percent[1:2]),2)*100, "%")
greater_instructions_overall_text = paste0("% who found instructions very or mostly clear", " ", "=", " ", greater_instructions_overall)

grob <- grobTree(textGrob(greater_instructions_overall_text, x=0.05,  y=0.80, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))

##############

instructions_overall_dat$percent = round(instructions_overall_dat$percent, 2)*100
instructions_overall_dat$percent = paste0(instructions_overall_dat$percent, "%")
title_instructions_overalll = paste0("Were the instructions for how to access your appointment online clear?", " ", "n=", n_instructions_overall)



plot_instructions_overall = ggplot(instructions_overall_dat, aes(x = instructions_overall,y =n, fill = instructions_overall))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_instructions_overalll, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,210))+
  geom_text(aes(label = percent), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")+
  annotation_custom(grob)
plot_instructions_overall

```
quick overall
Were you satisfied with how quickly you got an appointment?
1, Very Satisfied | 2, Mostly Satisfied | 3, Somewhat Satisfied | 4, Not Satisfied | 5, Not Applicable

```{r}
quick_overall_dat = tele_zoom_dat_complete

describe.factor(tele_zoom_dat_complete$quick)


quick_overall_dat = na.omit(data.frame(quick_overall = tele_zoom_dat_complete$quick))
n_quick_overall = dim(quick_overall_dat)[1]

quick_overall_dat$quick_overall = recode(quick_overall_dat$quick_overall, "1" = "Very Satisfied", "2" = "Mostly Satisfied", "3" = "Somewhat Satisfied", "4" = "Not Satisfied", "5" = "Not Applicable")

## relevel them
quick_overall_dat$quick_overall = as.factor(quick_overall_dat$quick_overall)
quick_overall_dat$quick_overall = factor(quick_overall_dat$quick_overall, levels = c("Very Satisfied","Mostly Satisfied", "Somewhat Satisfied", "Not Satisfied", "Not Applicable"))

quick_overall_dat = quick_overall_dat %>% count(quick_overall)

quick_overall_dat$percent = as.numeric(quick_overall_dat$n / n_quick_overall)

greater_quick_overall = paste0(round(sum(quick_overall_dat$percent[1:2]),2)*100, "%")
greater_quick_overall_text = paste0("% who were very or mostly satisfied", " ", "=", " ", greater_quick_overall)

grob <- grobTree(textGrob(greater_quick_overall_text, x=0.05,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))


quick_overall_dat$percent = round(quick_overall_dat$percent, 2)*100
quick_overall_dat$percent = paste0(quick_overall_dat$percent, "%")
title_quick_overalll = paste0("Were you satisfied with how quickly you got an appointment?", " ", "n=", n_quick_overall)

plot_quick_overall = ggplot(quick_overall_dat, aes(x = quick_overall,y =n, fill = quick_overall))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_quick_overalll, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,300))+
  geom_text(aes(label = percent), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")+
  annotation_custom(grob)
plot_quick_overall
```
communicate overall
manage
recovery
1, Strongly disagree | 2, Disagree | 3, Undecided | 4, Agree | 5, Strongly agree
```{r}
### Client survey data
#clincian_survey_dat

sat_overall_dat = tele_zoom_dat_complete

### 6 means not applicible for substance so we are assuming they are 0 which means they do not agree or greater
sat_overall_dat = sat_overall_dat[,c(8,10,11)]
sat_overall_dat = na.omit(sat_overall_dat)
n_sat_overall_dat = dim(sat_overall_dat)[1]
sat_overall_dat = apply(sat_overall_dat, 2, function(x){ifelse(x > 3, 1, 0)})
sat_overall_dat = data.frame(sat_overall_dat)
sat_overall_dat = apply(sat_overall_dat, 2, sum)
percent_sat_overall = paste0(round(sat_overall_dat / n_sat_overall_dat,2)*100, "%")
sat_overall_dat = data.frame(sat_overall_dat, percent_sat_overall)
var_names_sat_overall = row.names(sat_overall_dat)
sat_overall_dat$var_names_sat_overall = var_names_sat_overall
rownames(sat_overall_dat) = NULL
## Get rid of total
title_sat_overall = paste0("Count and % agree or greater televideo satisfaction", " ", "n=", n_sat_overall_dat)
plot_telehealth_sat = ggplot(sat_overall_dat, aes(x = var_names_sat_overall,y = sat_overall_dat, fill = var_names_sat_overall))+
  geom_bar(stat = "identity")+
  labs(title=title_sat_overall, x ="Dimension of satisfaction", y = "Count of clients agree or greater")+
  scale_y_continuous(limits = c(0,200))+
  labs(fill = "")+
  geom_text(aes(label = percent_sat_overall), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")
plot_telehealth_sat

```
communicate overall
manage
recovery
1, Strongly disagree | 2, Disagree | 3, Undecided | 4, Agree | 5, Strongly agree
```{r}
### Client survey data
#clincian_survey_dat

sub_overall_dat = tele_zoom_dat_complete

sub_overall_dat = sub_overall_dat[,c(9)]
sub_overall_dat = na.omit(sub_overall_dat)
### Subset 6
sub_overall_dat = subset(sub_overall_dat, sub_overall_dat != 6)
sub_overall_dat
n_sub_overall_dat = length(sub_overall_dat)

sub_overall_dat = ifelse(sub_overall_dat > 3, 1, 0)
sub_overall_dat = sum(sub_overall_dat)
percent_sub_overall = paste0(round(sub_overall_dat / n_sub_overall_dat,2)*100, "%")
sub_overall_dat = data.frame(sub_overall_dat, percent_sub_overall)
sub_overall_dat$var_names_sub_overall = "substance"

## Get rid of total
title_sub_overall = paste0("Count and % agree or greater televideo satisfaction \n for substance use", " ", "n=", n_sub_overall_dat)
plot_telehealth_sub = ggplot(sub_overall_dat, aes(x = var_names_sub_overall,y = sub_overall_dat, fill = var_names_sub_overall))+
  geom_bar(stat = "identity")+
  labs(title=title_sub_overall, x ="Dimension of satisfaction", y = "Count of clients agree or greater")+
  scale_y_continuous(limits = c(0,50))+
  labs(fill = "")+
  geom_text(aes(label = percent_sub_overall), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")
plot_telehealth_sub

```
recommend
Would you recommend Centerstone's telehealth services to your Family and Friends?
1, Definitely | 2, Very Likely | 3, Somewhat Likely | 4, Unlikely | 5, Would not recommend
```{r}
recommend_overall_dat = na.omit(data.frame(recommend_overall = tele_zoom_dat_complete$recommend))
n_recommend_overall = dim(recommend_overall_dat)[1]

recommend_overall_dat$recommend_overall = recode(recommend_overall_dat$recommend_overall, "1" = "Definitely", "2" = "Very Likely", "3" = "Somewhat Likely", "4" = "Unlikely", "5" = "Would not recommend")

## relevel them
recommend_overall_dat$recommend_overall = as.factor(recommend_overall_dat$recommend_overall)
recommend_overall_dat$recommend_overall = factor(recommend_overall_dat$recommend_overall, levels = c("Definitely", "Very Likely", "Somewhat Likely", "Unlikely", "Would not recommend"))

recommend_overall_dat = recommend_overall_dat %>% count(recommend_overall)

recommend_overall_dat$percent = as.numeric(recommend_overall_dat$n / n_recommend_overall)

greater_recommend_overall = paste0(round(sum(recommend_overall_dat$percent[1:2]),2)*100, "%")
greater_recommend_overall_text = paste0("% who would definitely or are very likely to recommend Centerstone", " ", "=", " ", greater_recommend_overall)

grob <- grobTree(textGrob(greater_recommend_overall_text, x=0.05,  y=0.8, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))


recommend_overall_dat$percent = round(recommend_overall_dat$percent, 2)*100
recommend_overall_dat$percent = paste0(recommend_overall_dat$percent, "%")
title_recommend_overalll = paste0("Would you recommend Centerstone's telehealth services to your Family \n and Friends?", " ", "n=", n_recommend_overall)

plot_recommend_overall = ggplot(recommend_overall_dat, aes(x = recommend_overall,y =n, fill = recommend_overall))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_recommend_overalll, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,200))+
  geom_text(aes(label = percent), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")+
  annotation_custom(grob)
  
plot_recommend_overall
```
expectation
How would you rate your overall experience with telehealth at Centerstone?
1, 1 did not meet expectation | 2, 2 | 3, 3 | 4, 4 | 5, 5 | 6, 6 | 7, 7 | 8, 8 | 9, 9 | 10, 10 exceeds expectation
NPS is Promoter minus detracters

https://www.qualtrics.com/experience-management/customer/net-promoter-score/
https://www.netpromoter.com/know/
https://www.netpromotersystem.com/about/measuring-your-net-promoter-score/
https://www.surveymonkey.com/curiosity/what-is-a-good-net-promoter-score/
```{r}
expectation_overall_dat = na.omit(data.frame(expectation_overall = tele_zoom_dat_complete$expectation))
n_expectation_overall = dim(expectation_overall_dat)[1]

expectation_overall_dat$expectation_overall = ifelse(expectation_overall_dat$expectation_overall < 7, "Detractors", ifelse(expectation_overall_dat$expectation_overall < 9, "Passives", ifelse(expectation_overall_dat$expectation_overall >=9, "Promoters", "Wrong")))

describe.factor(expectation_overall_dat$expectation_overall)


## relevel them
expectation_overall_dat$expectation_overall = as.factor(expectation_overall_dat$expectation_overall)
expectation_overall_dat$expectation_overall = factor(expectation_overall_dat$expectation_overall, levels = c("Promoters", "Passives", "Detractors"))

expectation_overall_dat = expectation_overall_dat %>% count(expectation_overall)
expectation_overall_dat$percent = expectation_overall_dat$n  / n_expectation_overall
expectation_overall_dat$percent = round(expectation_overall_dat$percent, 2)*100
nps = expectation_overall_dat[1,3] - expectation_overall_dat[3,3]
nps = paste0("Net Promoter Score =", " ", nps)

grob <- grobTree(textGrob(nps, x=0.05,  y=0.8, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))

expectation_overall_dat$percent = paste0(expectation_overall_dat$percent, "%")
title_expectation_overalll = paste0("How would you rate your overall experience with telehealth \n at Centerstone?", " ", "n=", n_expectation_overall)

expectation_overall_dat


plot_expectation_overall = ggplot(expectation_overall_dat, aes(x = expectation_overall,y =n, fill = expectation_overall))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_expectation_overalll, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,200))+
  geom_text(aes(label = percent), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")+
  annotation_custom(grob)

plot_expectation_overall


```
prefer_service
In the future, how would you prefer to receive services from Centerstone? (select all that apply)
1, Telehealth video | 2, Telephone | 3, In-person
```{r}
prefer_service_overall_dat = na.omit(data.frame(televideo = tele_zoom_dat_complete$prefer_service___1, telephone = tele_zoom_dat_complete$prefer_service___2, in_person = tele_zoom_dat_complete$prefer_service___3))

n_prefer_service_overall = dim(prefer_service_overall_dat)[1]

prefer_service_overall_dat = apply(prefer_service_overall_dat, 2, sum)
percent_prefer_service_overall = paste0(round(prefer_service_overall_dat / n_prefer_service_overall,2)*100, "%")
prefer_service_overall_dat = data.frame(t(rbind(prefer_service_overall_dat, percent_prefer_service_overall)))
var_names =  rownames(prefer_service_overall_dat)
prefer_service_overall_dat = data.frame(var_names, prefer_service_overall_dat)
rownames(prefer_service_overall_dat) = NULL
prefer_service_overall_dat
colnames(prefer_service_overall_dat) = c("response_option", "count", "percent")
prefer_service_overall_dat

title_prefer_service_overall = paste0("In the future, how would you prefer to receive \n services from Centerstone? (select all that apply)", " ", "n=", n_prefer_service_overall)

write.csv(prefer_service_overall_dat, "prefer_service_overall_dat.csv", row.names = FALSE)
prefer_service_overall_dat = read.csv("prefer_service_overall_dat.csv", header = TRUE)
prefer_service_overall_dat

prefer_service_overall_dat$response_option = recode(prefer_service_overall_dat$response_option, "in_person" = "in person")


plot_prefer_service_overall = ggplot(prefer_service_overall_dat, aes(x = reorder(response_option, -count),y =count, fill = response_option))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_prefer_service_overall, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,200))+
  geom_text(aes(label = percent), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")

plot_prefer_service_overall


```
benefits
Code later
```{r}
benefits_complete_client_zoom = na.omit(tele_zoom_dat_complete$benefits)
length(benefits_complete_client_zoom)
write.csv(benefits_complete_client_zoom, "benefits_complete_client_zoom.csv", row.names = FALSE)
```
barriers
code later
```{r}

barriers_complete_client_zoom = na.omit(tele_zoom_dat_complete$barriers)
length(barriers_complete_client_zoom)
write.csv(barriers_complete_client_zoom, "barriers_complete_client_zoom.csv", row.names = FALSE)

```
state
Which state do you currently live in?
1, Indiana | 2, Florida | 3, Tennessee | 4, Illinois | 5, Another state
```{r}

state_overall_dat = na.omit(data.frame(state_overall = tele_zoom_dat_complete$state))
n_state_overall = dim(state_overall_dat)[1]

state_overall_dat$state_overall = recode(state_overall_dat$state_overall, "1" = "Indiana", "2" = "Florida", "3" = "Tennessee", "4" = "Illinois", "5" = "Another state")

## relevel them
state_overall_dat$state_overall = as.factor(state_overall_dat$state_overall)
state_overall_dat$state_overall = factor(state_overall_dat$state_overall, levels = c("Tennessee", "Indiana", "Illinois", "Florida", "Another state"))

state_overall_dat = state_overall_dat %>% count(state_overall)

state_overall_dat$percent = as.numeric(state_overall_dat$n / n_state_overall)

state_overall_dat$percent = round(state_overall_dat$percent, 2)*100
state_overall_dat$percent = paste0(state_overall_dat$percent, "%")
title_state_overalll = paste0("Which state do you currently live in?", " ", "n=", n_state_overall)

plot_state_overall = ggplot(state_overall_dat, aes(x = reorder(state_overall, -n),y =n, fill = state_overall))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_state_overalll, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,200))+
  geom_text(aes(label = percent), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")

plot_state_overall

```
age
Under 5 
5-14
15-24
25-34
35-44
45-54
55-64
65+
```{r}
age_overall = na.omit(tele_zoom_dat_complete$age)
n_age_overall = length(age_overall)
age_overall = ifelse(age_overall < 5, "Under 5", ifelse(age_overall < 15, "5-14", ifelse(age_overall < 25, "15-24", ifelse(age_overall < 35, "25-34", ifelse(age_overall < 45, "35-44", ifelse(age_overall < 55, "45-55", ifelse(age_overall < 65, "55-64", "65+")))))))
age_overall = describe.factor(age_overall)
age_overall = t(age_overall)
age_range = rownames(age_overall)
age_overall = data.frame(age_overall)
age_overall
age_overall = data.frame(age_range, age_overall)
age_overall$Percent= paste0(round(age_overall$Percent,0), "%")
age_overall
age_overall$age_range = factor(age_overall$age_range, levels = c("Under 5", "5-14", "15-24", "25-34", "35-44", "45-55", "55-64", "65+"))

title_age_overall = paste0("What is your age?", " ", "n=", n_age_overall)
plot_age_overall = ggplot(age_overall, aes(x = age_range,y =Count, fill = age_range))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_age_overall, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,100))+
  geom_text(aes(label = Percent), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")
plot_age_overall

```
race
What is your racial identity?
1, White | 2, Black or African American | 3, American Indian or Alaska Native | 4, Asian | 5, Native Hawaiian or Other Pacific Islander | 6, Multiracial | 7, Another racial identity | 8, Prefer not to respond
```{r}
race_overall_dat = na.omit(tele_zoom_dat_complete$race)
describe.factor(race_overall_dat)
n_race_overall = length(race_overall_dat)


race_overall_dat = recode(race_overall_dat, "1" = "White", "2" = "Black or African American", "3" = "American Indian or Alaska Native", "4" = "Asian", "5" = "Native Hawaiian or Other Pacific Islander", "6" = "Multiracial", "7" ="Another racial identity", "8" =  "Prefer not to respond")
race_overall_dat = data.frame(race_overall = race_overall_dat)

race_overall_dat = race_overall_dat %>% count(race_overall)

race_overall_dat$percent = as.numeric(race_overall_dat$n / n_race_overall)

race_overall_dat$percent = round(race_overall_dat$percent, 2)*100
race_overall_dat$percent = paste0(race_overall_dat$percent, "%")
title_race_overalll = paste0("What is your racial identity?", " ", "n=", n_race_overall)

plot_race_overall = ggplot(race_overall_dat, aes(x = reorder(race_overall, -n),y =n, fill = race_overall))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_race_overalll, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,200))+
  geom_text(aes(label = percent), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")

plot_race_overall

```
Gender
What is your gender identity?
1, Male | 2, Female | 3, Another gender identity | 4, Prefer not to respond
```{r}
gender_overall_dat = na.omit(tele_zoom_dat_complete$gender)
describe.factor(gender_overall_dat)
n_gender_overall = length(gender_overall_dat)


gender_overall_dat = recode(gender_overall_dat, "1" = "Male", "2" = "Female", "3" = "Another gender identity", "4" = "Prefer not to respond")
gender_overall_dat = data.frame(gender_overall = gender_overall_dat)

gender_overall_dat = gender_overall_dat %>% count(gender_overall)

gender_overall_dat$percent = as.numeric(gender_overall_dat$n / n_gender_overall)

gender_overall_dat$percent = round(gender_overall_dat$percent, 2)*100
gender_overall_dat$percent = paste0(gender_overall_dat$percent, "%")
title_gender_overalll = paste0("What is your gender identity?", " ", "n=", n_gender_overall)

plot_gender_overall = ggplot(gender_overall_dat, aes(x = reorder(gender_overall, -n),y =n, fill = gender_overall))+
  geom_bar(stat = "identity", position = "dodge2")+
  labs(title=title_gender_overalll, y = "Count", x = "Response option")+
  scale_y_continuous(limits = c(0,200))+
  geom_text(aes(label = percent), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position = "none")

plot_gender_overall

```







