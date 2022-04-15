library(tidyverse); library(stringr); library(janitor)

#read survey data
dei = read.csv("data-dei-survey2022-v1-raw.csv", stringsAsFactors = TRUE)

#tidy variable names
dei <- clean_names(dei)

#chang long games
dei <- rename(dei, Date = timestamp)
dei <- rename(dei, Country= in_which_country_do_you_engage_in_your_primary_professional_activity)
dei <- rename(dei, EnglishFirstLanguage= is_english_your_first_language)
dei <- rename(dei, InstitutionType= what_type_of_institution_are_you_affiliated_with_please_choose_the_closest_option_from_the_list_and_if_you_work_at_more_than_one_please_choose_the_option_that_describes_your_primary_affiliation_or_where_most_of_your_work_gets_done)
dei <- rename(dei, PrimaryField = what_is_your_primary_field_of_research_or_practice_please_choose_the_one_that_best_describes_your_research_or_practice)
dei <- rename(dei, TimeResearchActive_inYears= how_long_have_you_been_involved_in_research_practice_or_creative_activity)
dei <- rename(dei, GCCR_Leader_Committee = are_you_a_member_of_a_gccr_committee_this_includes_members_of_the_leadership_team)
dei <- rename(dei, GCCR_Participation= how_do_you_participate_in_the_gccr)
dei <- rename(dei, Information_Source= what_is_your_main_source_of_gccr_information_and_activities)
dei <- rename(dei, Collaborate_General= the_primary_people_i_interact_with_not_on_a_project_are)
dei <- rename(dei, Collaborate_Project= the_primary_people_i_collaborate_with_on_a_project_other_than_gccr_wide_papers_are)
dei <- rename(dei, Collaborate_BestMethod= what_have_you_found_to_be_the_single_best_way_to_form_or_join_a_new_collaboration_on_slack_via_the_gccr)
dei <- rename(dei, Experience_Barriers= if_you_are_experiencing_barriers_to_participation_in_the_gccr_which_of_the_following_apply)
dei <- rename(dei, Observe_Barriers= if_you_have_observed_others_experiencing_barriers_to_participation_in_the_gccr_which_of_the_following_apply)
dei <- rename(dei, Comments = please_share_any_additional_comments_on_diversity_equity_and_or_inclusivity_inclusion_in_the_gccr)

#clean country names to iso3
dei <- dei %>% mutate(Country=str_replace(Country, "Congo \\(DRC\\)", "Congo"))
dei <- dei %>% mutate(Country=str_replace(Country, "Democratic Republic of the Congo", "Congo"))
dei <- dei %>% mutate(Country=str_replace(Country, "Francr", "France"))
dei <- dei %>% mutate(Country=str_replace(Country, "france", "France"))
dei <- dei %>% mutate(Country=str_replace(Country, "FRance", "France"))
dei <- dei %>% mutate(Country=str_replace(Country, "FRANCE", "France"))
dei <- dei %>% mutate(Country=str_replace(Country, "GREECE", "Greece"))
dei <- dei %>% mutate(Country=str_replace(Country, "italy", "Italy"))
dei <- dei %>% mutate(Country=str_replace(Country, "Italy and US", "Italy"))
dei <- dei %>% mutate(Country=str_replace(Country, "ITALY", "Italy"))
dei <- dei %>% mutate(Country=str_replace(Country, "UK", "United Kingdom"))
dei <- dei %>% mutate(Country=str_replace(Country, "spain", "Spain"))
dei <- dei %>% mutate(Country=str_replace(Country, "Türkiye", "Turkey"))
dei <- dei %>% mutate(Country=str_replace(Country, "República Argentina", "Argentina"))
dei <- dei %>% mutate(Country=str_replace(Country, "US", "U.S."))
dei <- dei %>% mutate(Country=str_replace(Country, "U.S.A", "United States"))
dei <- dei %>% mutate(Country=str_replace(Country, "U.S.", "United States"))

#convert data and time to posixct class
dei$Date <- as.character(dei$Date)
dei$Date <- as.POSIXct(dei$Date, format="%m/%d/%Y %H:%M", tz="EST")

#clean variable response
dei <- dei %>% mutate(EnglishFirstLanguage=str_replace(EnglishFirstLanguage, 'Bilingual or multilingual from birth/early childhood','Bi_Multilingual'))

dei <- dei %>% mutate(InstitutionType=str_replace(InstitutionType, 'Non-Academic Institution \\(e\\.g\\.\\, not degree granting\\) or Research Institution','Non-Academic_Research'))

dei <- dei %>% mutate(PrimaryField=str_replace(PrimaryField, "Creative Activity \\(e\\.g\\., writing\\, art or cooking\\)","Creative"))
dei <- dei %>% mutate(PrimaryField=str_replace(PrimaryField, 'Innovation and\\/or Research and Development','Innovation'))
dei <- dei %>% mutate(PrimaryField=str_replace(PrimaryField, 'Life Sciences \\(including biochemistry\\, genetics/genomics\\)','LifeSciences'))
dei <- dei %>% mutate(PrimaryField=str_replace(PrimaryField, 'Social and Perceptual Sciences \\(including human behavior and culture\\)','SocialSciences'))

dei <- dei %>% mutate(TimeResearchActive_inYears=str_replace(TimeResearchActive_inYears, '0-4 years','0to4'))
dei <- dei %>% mutate(TimeResearchActive_inYears=str_replace(TimeResearchActive_inYears, '5-9 years','5to9'))
dei <- dei %>% mutate(TimeResearchActive_inYears=str_replace(TimeResearchActive_inYears, '10-14 years','10to14'))
dei <- dei %>% mutate(TimeResearchActive_inYears=str_replace(TimeResearchActive_inYears, '15\\+ years','15Plus'))

dei <- dei %>% mutate(GCCR_Participation=str_replace(GCCR_Participation, 'Passive\\, Active','Both'))

dei <- dei %>% mutate(Information_Source=str_replace_all(Information_Source, " ", "_"))

dei <- dei %>% mutate(Collaborate_General=str_replace(Collaborate_General, 'people I did know before becoming a GCCR member\\.','Knew'))
dei <- dei %>% mutate(Collaborate_General=str_replace(Collaborate_General, 'people I did not know before becoming a GCCR member\\.','Did_not_know'))
dei <- dei %>% mutate(Collaborate_General=str_replace(Collaborate_General, 'people I knew about before becoming a GCCR member \\(but did not talk to\\)\\.','Knew_About'))

dei <- dei %>% mutate(Collaborate_Project=str_replace(Collaborate_Project, 'people I did know before becoming a GCCR member\\.','Knew'))
dei <- dei %>% mutate(Collaborate_Project=str_replace(Collaborate_Project, 'people I did not know before becoming a GCCR member\\.','Did_not_know'))
dei <- dei %>% mutate(Collaborate_Project=str_replace(Collaborate_Project, 'people I knew about before becoming a GCCR member \\(but did not talk to\\)\\.','Knew_About'))

dei <- dei %>% mutate(Collaborate_BestMethod=str_replace(Collaborate_BestMethod, 'Being a project member on a new or existing GCCR data study','Project_Member'))
dei <- dei %>% mutate(Collaborate_BestMethod=str_replace(Collaborate_BestMethod, 'Contributing to GCCR-wide papers','GCCRwide_Paper'))
dei <- dei %>% mutate(Collaborate_BestMethod=str_replace(Collaborate_BestMethod, 'Direct contact via messaging','Messaging'))
dei <- dei %>% mutate(Collaborate_BestMethod=str_replace(Collaborate_BestMethod, 'Direct solicitation for specific expertise','Solicitation'))
dei <- dei %>% mutate(Collaborate_BestMethod=str_replace(Collaborate_BestMethod, 'I have not found a way to form or join a new collaboration on Slack','No_way'))
dei <- dei %>% mutate(Collaborate_BestMethod=str_replace(Collaborate_BestMethod, 'Meeting on a project idea page','Project_Page'))

#write cleaned data
write.csv(dei, "data-dei-survey2022-v2-clean.csv", row.names = FALSE)
rm(dei)