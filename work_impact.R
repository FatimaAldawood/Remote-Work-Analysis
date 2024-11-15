# installing libraries
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(here)
library(skimr)
library(janitor)
library(dplyr)
#reading the csv file
setwd("C:/Users/YourUsername/Desktop/Remote_Work_Project")

remote_work <- read.csv("remote_work_cleaned.csv")

colnames(remote_work)



#plots

# plots of work location 
# Distribution of Work Locations of Employees

remote_work %>%
  count(Work_Location) %>%
  ggplot(aes(x = "", y = n, fill = Work_Location)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Work Locations of Employees", subtitle ="Distribution is fairly balanced with each category") +
  theme_void()

#The pie chart shows a balanced distribution of employees across work locations: **Remote Work** (33.4%), **On-Site Work** (32.2%), and **Hybrid Work** (34.4%).




# Distribution of Employees by Work Location and Gender

ggplot(remote_work, aes(x = Work_Location, fill = Gender)) +
  geom_bar(alpha = 0.5, position = "dodge") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Work Location", y = "Count", title = "Distribution of Employees by Work Location and Gender",
       subtitle = "On-site location have a higher proportion of male, 
while remote location have a higher proportion of  female")



#Distribution of Job Role by Work Location 

ggplot(remote_work, aes(x = Job_Role, fill = Work_Location)) +
  geom_bar(alpha = 0.5, position = "dodge") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels at 45 degrees
  ) +
  labs(x = "Job Role", y = "Count", title = "Distribution of Job Role by Work Location", subtitle = "Software engineers work remotely the most, 
followed by marketing  and data scientists")



#Gender Distribution by Role in Top Remote Work Positions
remote_work %>% 
  filter(Job_Role %in% c("Software Engineer", "Marketing", "Data Scientist")) %>% 
  ggplot(aes(Job_Role, fill = Gender)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  labs(x = "Job Role", y = "Count", title = "Gender Distribution by Role in Top Remote Work Positions", subtitle = "More males work in data engineering and marketing, 
while more females work in data science") +
  theme_bw() +
  theme (
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )


# stress levels
#stress levels vs Work location 

remote_work %>% arrange(Stress_Level) %>% 
  filter(Stress_Level == "High") %>% 
  ggplot(aes(x = Stress_Level, fill = Work_Location)) +
  geom_bar(alpha = 0.5, position = "dodge") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  labs(x = "Stress Level", y = "Count", title = "High Stress Level by Work Location", subtitle = "Remote work locations have the highest stress levels")



# pie chart of remote employees with hight stress level
# # selotion 
remote_work %>%
  filter(Work_Location  == "Remote") %>% 
  mutate(High_Stress = ifelse(Stress_Level == "High", "High Stress", "Not High Stress")) %>%
  count(High_Stress) %>%
  ggplot(aes(x = "", y = n, fill = High_Stress)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Remote Employees with High Stress Levels" , subtitle = "Over 50% of workers don't experience stress while working remotely") +
  theme_void()



# Satisfaction of Employees With Remote Work
# solutions 
remote_work %>%
  filter(Work_Location == "Remote") %>% 
  count(Satisfaction_with_Remote_Work) %>%
  ggplot(aes(x = "", y = n, fill = Satisfaction_with_Remote_Work)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Satisfaction of Employees With Remote Work ", subtitle = "Fairly balanced distribution of satisfaction,
neutrality, and dissatisfaction ") +
  theme_void()

# Hours Worked Per Week by Work Location
ggplot(remote_work, aes(x = Work_Location, y = Hours_Worked_Per_Week, fill = Work_Location)) +
  geom_boxplot(alpha = 0.5) +
  theme_bw() +
  theme (
    
  )
  labs(x = "Work Location", y = "Hours Worked Per Week", title = "Hours Worked Per Week by Work Location", subtitle = "Remote work shows excellent performance, 
with minimum hours surpassing those of other locations")


# Work-Life Balance Rating by Gender
remote_work %>% 
  filter(Work_Location == "Remote") %>% 
  ggplot(aes(x = Gender, y = Work_Life_Balance_Rating, fill = Gender)) +
  geom_boxplot(alpha = 0.5) +
  theme_bw() +
  labs(x = "Gender", y = "Work-Life Balance Rating ", title = "Remote Work-Life Balance Rating by Gender" ,subtitle = "Box Plot Indicates Remote Workers Have a Moderately Good 
Work-Life Balance, with Scores Centered Around 3")

# Work-Life Balance Rating By Job Role
remote_work %>% 
  filter(Work_Location == "Remote") %>% 
  ggplot(aes( y = Work_Life_Balance_Rating, fill = Job_Role)) +
  geom_boxplot(alpha = 0.5) +
  theme_bw() +
  theme (
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) +

  labs(x = "Gender", y = "Remote Work-Life Balance Rating", title = "Work-Life Balance Rating By Job Role" ,subtitle = "Software Engineers Experience the Best Work-Life Balance in Remote Work")


















