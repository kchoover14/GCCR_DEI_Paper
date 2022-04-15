library(tidyverse); library(stringr)
library(ggplot2); library(ggpubr)

#read data for survey members
dei = read.csv("data-dei-survey2022-v2-clean.csv", stringsAsFactors = TRUE)
dei$TimeResearchActive_inYears <- factor(dei$TimeResearchActive_inYears, levels = c('0to4','5to9','10to14','15Plus'))

#demographics
table(dei$GCCR_Leader_Committee)
table(dei$InstitutionType)
table(dei$PrimaryField)
table(dei$TimeResearchActive_inYears)
table(dei$EnglishFirstLanguage)

#participation
#clean responses
dei <- dei %>% mutate(GCCR_Participation=str_replace(GCCR_Participation, 'Active participation \\(e\\.g\\.\\, GCCR committee or Leadership Team\\, research collaboration due to being a GCCR member\\)','Active'))
dei <- dei %>% mutate(GCCR_Participation=str_replace(GCCR_Participation, 'Passive participation \\(e\\.g\\.\\, checking Slack daily\\/weekly or reading the newsletter\\)','Passive'))
dei <- dei %>% mutate(GCCR_Participation=str_replace(GCCR_Participation, 'Passive\\, Active','Both'))
dei <- dei %>% mutate(Information_Source=str_replace(Information_Source, 'A barrier not listed \\(if wished, you can provide the barrier in the comments at the end\\)','Not_listed'))
table(dei$GCCR_Participation)
table(dei$Information_Source)

#collaboration
table(dei$Collaborate_General)
table(dei$Collaborate_Project)
table(dei$Collaborate_BestMethod)

#BARRIERS
#clean responses
dei <- dei %>% mutate(Experience_Barriers=str_replace(Experience_Barriers, 'A barrier not listed \\(if wished, you can provide the barrier in the comments at the end\\)','Not_listed'))
dei <- dei %>% mutate(Experience_Barriers=str_replace(Experience_Barriers, 'Time to participate','Time'))
dei <- dei %>% mutate(Experience_Barriers=str_replace(Experience_Barriers, 'I am not experiencing barriers to participation\\.','None_experienced'))
dei <- dei %>% mutate(Experience_Barriers=str_replace(Experience_Barriers, 'Disability \\(mental and\\/or physical\\)','Disability'))
dei <- dei %>% mutate(Experience_Barriers=str_replace(Experience_Barriers, 'Time zone differences','Time_Zone'))

dei <- dei %>% mutate(Observe_Barriers=str_replace(Observe_Barriers,'I have not observed others experiencing barriers to participation\\.','None_observed'))
dei <- dei %>% mutate(Observe_Barriers=str_replace(Observe_Barriers, 'A barrier not listed \\(if wished, you can provide the barrier in the comments at the end\\)','Not_listed'))
dei <- dei %>% mutate(Observe_Barriers=str_replace(Observe_Barriers, 'Time to participate','Time'))
dei <- dei %>% mutate(Observe_Barriers=str_replace(Observe_Barriers, 'Disability \\(mental and\\/or physical\\)','Disability'))
dei <- dei %>% mutate(Observe_Barriers=str_replace(Observe_Barriers, 'Time zone differences','Time_Zone'))

#remove implausible answer (no barrier but list time as a barrier) n=4
dei <- dei %>% mutate(Experience_Barriers=str_replace(Experience_Barriers, 'None_experienced\\, Time', 'Time'))

#percent not experiencing barrries
table(dei$Experience_Barriers) #n=44/102 not experiencing; Time 26, 8 language, 5 time zone
table(dei$Observe_Barriers) #n=65/102 not observing; Time 13, not listed 6, time zone 4

#experience barriers
dei.experience.barriers <- select(dei, Experience_Barriers)
dei.experience.barriers <- dei.experience.barriers %>% separate(Experience_Barriers, c('Var1', 'Var2', 'Var3'), sep=",")
table(dei.experience.barriers$Var1)

#observe barriers
dei.observe.barriers <- select(dei, Observe_Barriers)
dei.observe.barriers <- dei.observe.barriers %>% separate(Observe_Barriers, c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), sep=",")
table(dei.observe.barriers$Var1)

#COMMENTS
dei <- dei %>% tolower(dei$Comments) #not working




