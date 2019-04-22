library(ggplot2)
library(dplyr)
first.df = read.csv("reviews1.csv")
hotel_list = unique(first.df$hotel_name, incomparables = FALSE)
length(hotel_list)
first.df$hotel_name = as.character(first.df$hotel_name)
first.df$age_group = as.character(first.df$age_group)

p<-ggplot(data=first.df, aes(x=as.character(age_group),fill=age_group)) + 
  geom_bar() + xlab("Age group") + ylab("Count") + ggtitle("Number of reviews by age group") + 
  scale_fill_discrete(name = "Age Group")
p

first.df$traveled_as = as.character(first.df$traveled_as)
first.df$traveled_as[first.df$traveled_as == "Alleinreisend"] = "Traveling alone"
first.df$traveled_as[first.df$traveled_as == "Familie"] = "Family"
first.df$traveled_as[first.df$traveled_as == "Freunde"] = "Friends"
first.df$traveled_as[first.df$traveled_as == "Paar"] = "Couple"

p<-ggplot(data=first.df, aes(x=traveled_as,fill=factor(traveled_as))) +
  geom_bar() + scale_fill_discrete(name = "Traveled as") +
  xlab("Traveled as") + ylab("Count") + ggtitle("Reviewer traveled as")
p



first.df$reason_for_travel = as.character(first.df$reason_for_travel)
first.df$reason_for_travel[first.df$reason_for_travel == "Arbeit"] = "Job"
first.df$reason_for_travel[first.df$reason_for_travel == "Sonstige"] = "Other"
first.df$reason_for_travel[first.df$reason_for_travel == "Stadt"] = "City"
first.df$reason_for_travel[first.df$reason_for_travel == "Strand"] = "Beach"
first.df$reason_for_travel[first.df$reason_for_travel == "Wandern und Wellness"] = "Hiking and wellness"
first.df$reason_for_travel[first.df$reason_for_travel == "Arbeit"] = "Winter"



p = ggplot(data=first.df, aes(x=reason_for_travel,fill=factor(reason_for_travel))) + 
  geom_bar() + ylab("Count") + xlab("Reason for traveling") + 
  ggtitle("Reasons for travel") + scale_fill_discrete(name = "Reason for travel")
p 


first.df$children = as.character(first.df$children)
first.df$children[first.df$children == "Mehr als 4"] = "More than 4"
first.df$children[first.df$children == "Keine Kinder"] = "No children"




p = ggplot(data=first.df, aes(x=children,fill=factor(children))) + 
  geom_bar() + ylab("Count") + xlab("No. of children/s traveled together") + 
  ggtitle("No. of children/s") + scale_fill_discrete(name = "Children/s")
p 







#by age group
mean_overall_ratings_age = c()
median_overall_rating_age = c()
unique_val = unique(first.df$age_group, incomparables = FALSE)
for( value in unique_val){
  temp.df = first.df %>% filter(age_group == value)
  mean_overall_ratings_age = append(mean_overall_ratings_age,mean(temp.df$overall_rating))
  median_overall_rating_age = append(median_overall_rating_age,median(temp.df$overall_rating))
}

vline.age <- data.frame(z = mean_overall_ratings_age,age_group = unique_val)
vline.age.med = data.frame(z1 = median_overall_rating_age,age_group = unique_val)
p = ggplot(data=first.df, aes(x=overall_rating)) + geom_histogram() + geom_vline(aes(xintercept = z), vline.age,color="red")+ geom_vline(aes(xintercept = z1),vline.age.med,color="blue")+
  facet_wrap(~age_group) + xlab("Overall rating by age group") + ylab("Count") + ggtitle("Overall rating by age group and their mean and median")
p  

#by reason for travel
mean_overall_rating_reason = c()
median_overall_rating_reason = c()
unique_val_reason = unique(first.df$reason_for_travel, incomparables = FALSE)
for( value in unique_val_reason){
  temp.df = first.df %>% filter(reason_for_travel == value)
  mean_overall_rating_reason = append(mean_overall_rating_reason,mean(temp.df$overall_rating))
  median_overall_rating_reason = append(median_overall_rating_reason,median(temp.df$overall_rating))
}

vline.reason <- data.frame(z.reason = mean_overall_rating_reason,reason_for_travel = unique_val_reason)
vline.reason.med = data.frame(z1.reason = median_overall_rating_reason,reason_for_travel = unique_val_reason)
p = ggplot(data=first.df, aes(x=overall_rating)) + geom_histogram() + geom_vline(aes(xintercept = z.reason), vline.reason,color="red")+ geom_vline(aes(xintercept = z1.reason),vline.reason.med,color="blue")+
  facet_wrap(~reason_for_travel) + xlab("Overall rating by Reason for travel") + ylab("Count") + ggtitle("Overall rating by reason for travel and their mean and median")
p 


model1 = lm(hotel_rating~ location_and_surrounding_rating+room_rating+service_rating + gastronomy_rating+
              sports_and_entertainment_rating +factor(reason_for_travel)+factor(traveled_as)+factor(children)+factor(age_group),data = first.df)
print(summary(model1))
sink("summary.txt")
print(summary(model1))
sink()

model2 = lm(hotel_rating ~ factor(traveled_as) + factor(children) + room_rating+service_rating + gastronomy_rating+
              sports_and_entertainment_rating ,data = first.df)
summary(model2)

metrics2 = calc.relimp(model2,type=c("lmg"),rela=T)
metrics2

first.df$traveled_as[first.df$traveled_as == "Friends"] = "Not Family"
first.df$traveled_as[first.df$traveled_as == "Traveling alone"] = "Not Family"

first.df$children[first.df$children == "2"] = "Children"
first.df$children[first.df$children == "3"] = "Children"
first.df$children[first.df$children == "4"] = "Children"
first.df$children[first.df$children == "1"] = "Children"
first.df$children[first.df$children == "More than 4"] = "Children"

first.df$age_group[first.df$age_group == "19-25"] = "19-35"
first.df$age_group[first.df$age_group == "26-30"] = "19-35"
first.df$age_group[first.df$age_group == "31-35"] = "19-35"
first.df$age_group[first.df$age_group == "36-40"] = "36-55"
first.df$age_group[first.df$age_group == "41-45"] = "36-55"
first.df$age_group[first.df$age_group == "46-50"] = "36-55"
first.df$age_group[first.df$age_group == "51-55"] = "36-55"
first.df$age_group[first.df$age_group == "56-60"] = "56-71+"
first.df$age_group[first.df$age_group == "61-65"] = "56-71+"
first.df$age_group[first.df$age_group == "66-70"] = "56-71+"
first.df$age_group[first.df$age_group == "71+"] = "56-71+"

install.packages("relaimpo")
library(relaimpo)
metrics = calc.relimp(model1,type=c("lmg"),rela=T)
metrics


