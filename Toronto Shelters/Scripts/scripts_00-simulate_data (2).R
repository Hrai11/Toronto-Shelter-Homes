#The required packages are:
library("dplyr")
library("opendatatoronto")
library("ggplot2")
#Looking at the dataset of interest:
my_data=search_packages("Toronto Shelter System Flow") %>% 
  list_package_resources() %>% 
  filter(name=="toronto-shelter-system-flow.csv") %>% 
  get_resource()
#Simulating the data:
#Simulating the X_id column:
set.seed(123456789)
X_id=seq(1,540)
date.mmm.yy.=rep(c(rep("Jan-18",7),rep("Feb-18",7),rep("Mar-18",7),rep("Apr-18",7),rep("May-18",7),
               rep("Jun-18",7),rep("Jul-18",7),rep("Aug-18",7),rep("Sep-18",7),rep("Oct-18",7),
               rep("Nov-18",7),rep("Dec-18",7)),6)
#Simulating the date.mmm.yy. column:
date.mmm.yy.=c()
for (my_year in seq(18,23)){
  if (my_year<21){
    for (my_month in c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")){
      date.mmm.yy.=c(date.mmm.yy.,rep(paste(my_month,"-",sep="",my_year),7))
    }
  }
  else{
    for (my_month in c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")){
      date.mmm.yy.=c(date.mmm.yy.,rep(paste(my_month,"-",sep="",my_year),8))
    }
  }
} 
#Simulating the population_group column:
starting_groups=c("All Population","Chronic","Refugees","Families","Youth","Single Adult",
                  "Non-refugees")
final_groups=c("All Population","Chronic","Refugees","Families","Youth","Single Adult",
               "Non-refugees","Indigenous")
population_group=c(rep(starting_groups,12*3),rep(final_groups,12*3))
simulated_data=data.frame(X_id,date.mmm.yy.,population_group)
#Simulating the returned_from housing column:
summary_data=my_data %>% 
  group_by(population_group) %>% 
  summarise(Mean=mean(returned_from_housing),
            Standard_Deviation=sd(returned_from_housing))
#Simulating the returned_from_housing column:
returned_from_housing=c()
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  returned_from_housing=round(c(returned_from_housing,all_populations,chronic,refugees,
                          families,youth,single_adult,non_refugees),0)
}
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  indigenous=rnorm(1,summary_data[summary_data$population_group=="Indigenous",]$Mean,
                   summary_data[summary_data$population_group=="Indigenous",]$Standard_Deviation)
  returned_from_housing=round(c(returned_from_housing,all_populations,chronic,refugees,
                          families,youth,single_adult,non_refugees,indigenous),0)
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing)
#Simulating the returned_to_shelter column:
summary_data=my_data %>% 
  group_by(population_group) %>% 
  summarise(Mean=mean(returned_to_shelter),
            Standard_Deviation=sd(returned_to_shelter))
returned_to_shelter=c()
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  returned_to_shelter=round(c(returned_to_shelter,all_populations,chronic,refugees,
                                families,youth,single_adult,non_refugees),0)
}
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  indigenous=rnorm(1,summary_data[summary_data$population_group=="Indigenous",]$Mean,
                   summary_data[summary_data$population_group=="Indigenous",]$Standard_Deviation)
  returned_to_shelter=round(c(returned_to_shelter,all_populations,chronic,refugees,
                                families,youth,single_adult,non_refugees,indigenous),0)
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing,
                          returned_to_shelter)
#Simulating the newly_identified column:
summary_data=my_data %>% 
  group_by(population_group) %>% 
  summarise(Mean=mean(newly_identified),
            Standard_Deviation=sd(newly_identified))
newly_identified=c()
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  newly_identified=round(c(newly_identified,all_populations,chronic,refugees,
                              families,youth,single_adult,non_refugees),0)
}
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  indigenous=rnorm(1,summary_data[summary_data$population_group=="Indigenous",]$Mean,
                   summary_data[summary_data$population_group=="Indigenous",]$Standard_Deviation)
  newly_identified=round(c(newly_identified,all_populations,chronic,refugees,
                              families,youth,single_adult,non_refugees,indigenous),0)
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing,
                          returned_to_shelter,newly_identified)
#Simulating the moved_to_housing column:
summary_data=my_data %>% 
  group_by(population_group) %>% 
  summarise(Mean=mean(moved_to_housing),
            Standard_Deviation=sd(moved_to_housing))
moved_to_housing=c()
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  moved_to_housing=round(c(moved_to_housing,all_populations,chronic,refugees,
                           families,youth,single_adult,non_refugees),0)
}
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  indigenous=rnorm(1,summary_data[summary_data$population_group=="Indigenous",]$Mean,
                   summary_data[summary_data$population_group=="Indigenous",]$Standard_Deviation)
  moved_to_housing=round(c(moved_to_housing,all_populations,chronic,refugees,
                           families,youth,single_adult,non_refugees,indigenous),0)
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing,
                          returned_to_shelter,newly_identified,moved_to_housing)
#Simulating the became_inactive column:
summary_data=my_data %>% 
  group_by(population_group) %>% 
  summarise(Mean=mean(became_inactive),
            Standard_Deviation=sd(became_inactive))
became_inactive=c()
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  became_inactive=round(c(became_inactive,all_populations,chronic,refugees,
                           families,youth,single_adult,non_refugees),0)
}
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  indigenous=rnorm(1,summary_data[summary_data$population_group=="Indigenous",]$Mean,
                   summary_data[summary_data$population_group=="Indigenous",]$Standard_Deviation)
  became_inactive=round(c(became_inactive,all_populations,chronic,refugees,
                           families,youth,single_adult,non_refugees,indigenous),0)
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing,
                          returned_to_shelter,newly_identified,moved_to_housing,
                          became_inactive)
#Simulating the actively_homeless column:
summary_data=my_data %>% 
  group_by(population_group) %>% 
  summarise(Mean=mean(actively_homeless),
            Standard_Deviation=sd(actively_homeless))
actively_homeless=c()
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  actively_homeless=round(c(actively_homeless,all_populations,chronic,refugees,
                          families,youth,single_adult,non_refugees),0)
}
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  indigenous=rnorm(1,summary_data[summary_data$population_group=="Indigenous",]$Mean,
                   summary_data[summary_data$population_group=="Indigenous",]$Standard_Deviation)
  actively_homeless=round(c(actively_homeless,all_populations,chronic,refugees,
                          families,youth,single_adult,non_refugees,indigenous),0)
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing,
                          returned_to_shelter,newly_identified,moved_to_housing,
                          became_inactive,actively_homeless)
#Simulating the ageunder16 column:
summary_data=my_data %>% 
  group_by(population_group) %>% 
  summarise(Mean=mean(ageunder16),
            Standard_Deviation=sd(ageunder16))
ageunder16=c()
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  ageunder16=round(c(ageunder16,all_populations,chronic,refugees,
                            families,youth,single_adult,non_refugees),0)
}
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  indigenous=rnorm(1,summary_data[summary_data$population_group=="Indigenous",]$Mean,
                   summary_data[summary_data$population_group=="Indigenous",]$Standard_Deviation)
  ageunder16=round(c(ageunder16,all_populations,chronic,refugees,
                            families,youth,single_adult,non_refugees,indigenous),0)
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing,
                          returned_to_shelter,newly_identified,moved_to_housing,
                          became_inactive,actively_homeless,ageunder16)
#Simulating the age16.24 column:
summary_data=my_data %>% 
  group_by(population_group) %>% 
  summarise(Mean=mean(age16.24),
            Standard_Deviation=sd(age16.24))
age16.24=c()
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  age16.24=round(c(age16.24,all_populations,chronic,refugees,
                     families,youth,single_adult,non_refugees),0)
}
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  indigenous=rnorm(1,summary_data[summary_data$population_group=="Indigenous",]$Mean,
                   summary_data[summary_data$population_group=="Indigenous",]$Standard_Deviation)
  age16.24=round(c(age16.24,all_populations,chronic,refugees,
                     families,youth,single_adult,non_refugees,indigenous),0)
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing,
                          returned_to_shelter,newly_identified,moved_to_housing,
                          became_inactive,actively_homeless,ageunder16,age16.24)
#Simulating the age25.44 column:
summary_data=my_data %>% 
  group_by(population_group) %>% 
  summarise(Mean=mean(age25.44),
            Standard_Deviation=sd(age25.44))
age25.44=c()
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  age25.44=round(c(age25.44,all_populations,chronic,refugees,
                   families,youth,single_adult,non_refugees),0)
}
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  indigenous=rnorm(1,summary_data[summary_data$population_group=="Indigenous",]$Mean,
                   summary_data[summary_data$population_group=="Indigenous",]$Standard_Deviation)
  age25.44=round(c(age25.44,all_populations,chronic,refugees,
                   families,youth,single_adult,non_refugees,indigenous),0)
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing,
                          returned_to_shelter,newly_identified,moved_to_housing,
                          became_inactive,actively_homeless,ageunder16,age16.24,
                          age25.44)
#Simulating the age45.64 column:
summary_data=my_data %>% 
  group_by(population_group) %>% 
  summarise(Mean=mean(age45.64),
            Standard_Deviation=sd(age45.64))
age45.64=c()
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  age45.64=round(c(age45.64,all_populations,chronic,refugees,
                   families,youth,single_adult,non_refugees),0)
}
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  indigenous=rnorm(1,summary_data[summary_data$population_group=="Indigenous",]$Mean,
                   summary_data[summary_data$population_group=="Indigenous",]$Standard_Deviation)
  age45.64=round(c(age45.64,all_populations,chronic,refugees,
                   families,youth,single_adult,non_refugees,indigenous),0)
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing,
                          returned_to_shelter,newly_identified,moved_to_housing,
                          became_inactive,actively_homeless,ageunder16,age16.24,
                          age25.44,age45.64)
#Simulating the age65over column:
summary_data=my_data %>% 
  group_by(population_group) %>% 
  summarise(Mean=mean(age65over),
            Standard_Deviation=sd(age65over))
age65over=c()
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  age65over=round(c(age65over,all_populations,chronic,refugees,
                   families,youth,single_adult,non_refugees),0)
}
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  indigenous=rnorm(1,summary_data[summary_data$population_group=="Indigenous",]$Mean,
                   summary_data[summary_data$population_group=="Indigenous",]$Standard_Deviation)
  age65over=round(c(age65over,all_populations,chronic,refugees,
                   families,youth,single_adult,non_refugees,indigenous),0)
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing,
                          returned_to_shelter,newly_identified,moved_to_housing,
                          became_inactive,actively_homeless,ageunder16,age16.24,
                          age25.44,age45.64,age65over)
#Simulating the gender_male column:
summary_data=my_data %>% 
  group_by(population_group) %>% 
  summarise(Mean=mean(gender_male),
            Standard_Deviation=sd(gender_male))
gender_male=c()
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  gender_male=round(c(gender_male,all_populations,chronic,refugees,
                    families,youth,single_adult,non_refugees),0)
}
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  indigenous=rnorm(1,summary_data[summary_data$population_group=="Indigenous",]$Mean,
                   summary_data[summary_data$population_group=="Indigenous",]$Standard_Deviation)
  gender_male=round(c(gender_male,all_populations,chronic,refugees,
                    families,youth,single_adult,non_refugees,indigenous),0)
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing,
                          returned_to_shelter,newly_identified,moved_to_housing,
                          became_inactive,actively_homeless,ageunder16,age16.24,
                          age25.44,age45.64,age65over,gender_male)
#Simulating the gender_female column:
summary_data=my_data %>% 
  group_by(population_group) %>% 
  summarise(Mean=mean(gender_female),
            Standard_Deviation=sd(gender_female))
gender_female=c()
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  gender_female=round(c(gender_female,all_populations,chronic,refugees,
                      families,youth,single_adult,non_refugees),0)
}
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  indigenous=rnorm(1,summary_data[summary_data$population_group=="Indigenous",]$Mean,
                   summary_data[summary_data$population_group=="Indigenous",]$Standard_Deviation)
  gender_female=round(c(gender_female,all_populations,chronic,refugees,
                      families,youth,single_adult,non_refugees,indigenous),0)
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing,
                          returned_to_shelter,newly_identified,moved_to_housing,
                          became_inactive,actively_homeless,ageunder16,age16.24,
                          age25.44,age45.64,age65over,gender_male,gender_female)
#Simulating the gender_transgender.non.binary_or_two_spirit column:
summary_data=my_data %>% 
  group_by(population_group) %>% 
  summarise(Mean=mean(gender_transgender.non.binary_or_two_spirit),
            Standard_Deviation=sd(gender_transgender.non.binary_or_two_spirit))
gender_transgender.non.binary_or_two_spirit=c()
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  gender_transgender.non.binary_or_two_spirit=round(c(gender_transgender.non.binary_or_two_spirit,
                                                      all_populations,chronic,refugees,
                                                      families,youth,single_adult,non_refugees),0)
}
for (i in 1:36){
  all_populations=rnorm(1,summary_data[summary_data$population_group=="All Population",]$Mean,
                        summary_data[summary_data$population_group=="All Population",]$Standard_Deviation)
  chronic=rnorm(1,summary_data[summary_data$population_group=="Chronic",]$Mean,
                summary_data[summary_data$population_group=="Chronic",]$Standard_Deviation)
  refugees=rnorm(1,summary_data[summary_data$population_group=="Refugees",]$Mean,
                 summary_data[summary_data$population_group=="Refugees",]$Standard_Deviation)
  families=rnorm(1,summary_data[summary_data$population_group=="Families",]$Mean,
                 summary_data[summary_data$population_group=="Families",]$Standard_Deviation)
  youth=rnorm(1,summary_data[summary_data$population_group=="Youth",]$Mean,
              summary_data[summary_data$population_group=="Youth",]$Standard_Deviation)
  single_adult=rnorm(1,summary_data[summary_data$population_group=="Single Adult",]$Mean,
                     summary_data[summary_data$population_group=="Single Adult",]$Standard_Deviation)
  non_refugees=rnorm(1,summary_data[summary_data$population_group=="Non-refugees",]$Mean,
                     summary_data[summary_data$population_group=="Non-refugees",]$Standard_Deviation)
  indigenous=rnorm(1,summary_data[summary_data$population_group=="Indigenous",]$Mean,
                   summary_data[summary_data$population_group=="Indigenous",]$Standard_Deviation)
  gender_transgender.non.binary_or_two_spirit=round(c(gender_transgender.non.binary_or_two_spirit,
                                                      all_populations,chronic,refugees,
                                                      families,youth,single_adult,non_refugees,indigenous),0)
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing,
                          returned_to_shelter,newly_identified,moved_to_housing,
                          became_inactive,actively_homeless,ageunder16,age16.24,
                          age25.44,age45.64,age65over,gender_male,gender_female,
                          gender_transgender.non.binary_or_two_spirit)
#Simulating the population_group_percentage column:
#Calculating the chronic percentage:
totals=simulated_data %>% 
  filter(population_group=="All Population") %>% 
  group_by(date.mmm.yy.) %>% 
  select(gender_male,gender_female) %>% 
  mutate(total=gender_male+gender_female)
chronic=simulated_data %>% 
  filter(population_group=="Chronic") %>% 
  group_by(date.mmm.yy.) %>% 
  select(gender_male,gender_female) %>% 
  mutate(total_chronic=gender_male+gender_female)
chronic_percentage=round(chronic$total_chronic/totals$total*100,1)
#Calculating the refugees percentage:
refugees=simulated_data %>% 
  filter(population_group=="Refugees") %>% 
  group_by(date.mmm.yy.) %>% 
  select(gender_male,gender_female) %>% 
  mutate(total_refugees=gender_male+gender_female)
refugees_percentage=round(refugees$total_refugees/totals$total*100,1)
#Calculating the refugees percentage:
non_refugees_percentage=100-refugees_percentage
#Calculating the families,youth, and single adult percentages:
families=simulated_data %>% 
  filter(population_group=="Families") %>% 
  group_by(date.mmm.yy.) %>% 
  select(gender_male,gender_female) %>% 
  mutate(total_families=gender_male+gender_female)
youth=simulated_data %>% 
  filter(population_group=="Youth") %>% 
  group_by(date.mmm.yy.) %>% 
  select(gender_male,gender_female) %>% 
  mutate(total_youth=gender_male+gender_female)
single_adult=simulated_data %>% 
  filter(population_group=="Single Adult") %>% 
  group_by(date.mmm.yy.) %>% 
  select(gender_male,gender_female) %>% 
  mutate(total_single_adult=gender_male+gender_female)
overall_total=families$total_families+youth$total_youth+single_adult$total_single_adult
families_percentage=round(families$total_families/overall_total*100,1)
youth_percentage=round(youth$total_youth/overall_total*100,1)
single_adult_percentage=100-families_percentage-youth_percentage
#Calculating the indigenous percentage:
indigenous=simulated_data[253:nrow(simulated_data),] %>% 
  filter(population_group=="Indigenous") %>% 
  group_by(date.mmm.yy.) %>% 
  select(gender_male,gender_female) %>% 
  mutate(total_indigenous=gender_male+gender_female)
indigenous_total=simulated_data[253:nrow(simulated_data),] %>% 
  filter(population_group=="All Population") %>% 
  group_by(date.mmm.yy.) %>% 
  select(gender_male,gender_female) %>% 
  mutate(total=gender_male+gender_female)
indigenous_percentage=round(indigenous$total_indigenous/indigenous_total$total*100,1)
population_group_percentage=c()
for (i in 1:36){
  population_group_percentage=c(population_group_percentage,100,chronic_percentage[i],
                                refugees_percentage[i],families_percentage[i],youth_percentage[i],
                                single_adult_percentage[i],non_refugees_percentage[i])
}
for (i in 1:36){
  population_group_percentage=c(population_group_percentage,100,chronic_percentage[i+36],
                                refugees_percentage[i+36],families_percentage[i+36],
                                youth_percentage[i+36],single_adult_percentage[+36],
                                non_refugees_percentage[i+36],indigenous_percentage[i])
}
simulated_data=data.frame(X_id,date.mmm.yy.,population_group,returned_from_housing,
                          returned_to_shelter,newly_identified,moved_to_housing,
                          became_inactive,actively_homeless,ageunder16,age16.24,
                          age25.44,age45.64,age65over,gender_male,gender_female,
                          gender_transgender.non.binary_or_two_spirit,population_group_percentage)
#Conducting the testing
#1 T-test: Is the average number of males in the shelter system equal to the average number of 
#females in the shelter system? 
males=simulated_data %>% 
  filter(population_group=="All Population") %>% 
  select(gender_male)
females=simulated_data %>% 
  filter(population_group=="All Population") %>% 
  select(gender_female)
t.test(males,females)
#The null hypothesis states that there is no difference between the average number of males
#and the average number of females in the shelter system. Since the p-value is less than the
#significance level of 0.05, we reject the null hypothesis and conclude that there is a
#significant difference between the average number of males and females in the shelter system.
#2 Chi-Square test: Are the proportions of families, youths and single adults equal in the 
#shelter system?
values=c(sum(families$total_families),sum(youth$total_youth),
         sum(single_adult$total_single_adult))
chisq.test(values, p = c(1/3, 1/3, 1/3))
#The null hypothesis states that there is no difference in proportions between the families,
#youths, and single adults in the shelter system. Since the p-value is less than the 
#significance level of 0.05, we reject the null hypothesis and conclude that the proportions of
#youths, families and single adults in the shelter system are not the same.

#Saving the simulated data:
write.csv(simulated_data, "simulated_data.csv", row.names=FALSE)






