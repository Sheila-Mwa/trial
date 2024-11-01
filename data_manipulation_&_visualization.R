# loading the necessary package
library(tidyverse)

# manually creating a dataset 
clinical_data <- data.frame(
  patient_id = 1:16,
  age = c(34,45,29,51,42,37,48,26,53,39,32,44,55,30,49,38),
  gender = c("Male", "Female", "Female", "Male", "Male",
             "Female", "Male", "Female", "Male", "Female",
             "Male", "Female", "Male", "Female", "Male", "Female"),
  treatment_group = c("Treatment A", "Control", "Treatment B", "Treatment A", "Control",
                      "Treatment B", "Control", "Treatment A", "Treatment B", "Control",
                      "Treatment A", "Treatment B", "Control", "Treatment A", "Treatment B", "Control"),
  baseline_score = c(25, 30, 22, 28, 35,
                     27, 33, 21, 29, 32,
                     26, 24, 31, 23, 36, 20),
  followup_score = c(15, 28, 18, 20, 33,
                     22, 31, 19, 25, 30,
                     24, 20, 29, 22, 34, 19),
  adverse_events = c(1, 0, 2, 3, 0,
                     1, 2, 0, 2, 1,
                     1, 0, 2, 1, 3, 0)
)

# display the dataset
print(clinical_data)

View(clinical_data) 
head(clinical_data)

# write the patient_data table to a CSV file
write.csv(clinical_data, "clinical,csv", row.names = FALSE)


## Question 1
## Evaluate treatment effectiveness
## Investigate the effectiveness of the treatment by comparing the follow-up scores of patients in the treatment groups versus the control group
# cleaning and manipulating the dataset as needed
# using pivot_longer() to pivot the treatment_group column
clinical_data_long <- clinical_data %>% 
   pivot_longer(cols = baseline_score:followup_score, names_to = "score", values_to = "values")

# using summarise() and group_by() to calculate the mean followup-scores for each treatment group
clinical_data_mean <- clinical_data %>% 
  group_by(treatment_group) %>% 
  summarise(followup_mean = mean(followup_score))

head(clinical_data_mean)

# using aggregate() to calculate the mean of the followup-scores and setNames() to ename the columns
followup_score_aggregate <- setNames(
  aggregate(followup_score ~ treatment_group, data = clinical_data, FUN = mean), 
  c("treatment_group", "mean_followup_score")
)

head(followup_score_aggregate)

# plotting a barplot using geom_bar() to visualize the avg followup-scores by treatment_group
clinical_data_barplot <- followup_score_aggregate %>% 
  ggplot(aes(treatment_group, mean_followup_score, fill = treatment_group)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "The average followup-scores by treatment_group", 
       subtitle = "treatment_group vs mean", 
       x = "Treatment Group", 
       y = "Mean Followup Score") +
  theme_minimal() + # apply the minimal theme first
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "italic"),
    legend.position = "top",
    axis.text.x = element_text(angle = 45)
  ) 

clinical_data_barplot




## Question 2
## Analyze adverse events by treatment group
## Examine the relationship between treatment groups and the number of reported adverse events
# calculate summary statistics for adverse events
adverse_events_summary <- clinical_data %>% 
  group_by(treatment_group) %>% 
  summarise(
    mean_adverse_events = mean(adverse_events),
    median_adverse_events = median(adverse_events),
    min_adverse_events = min(adverse_events),
    max_adverse_events = max(adverse_events),
    sd_adverse_events = sd(adverse_events)
    )

head(adverse_events_summary)
str(adverse_events_summary)

# plotting a boxplot to visualize the distribution of adverse events across different treatment groups
clinical_data_boxplot <- clinical_data %>% 
  ggplot(aes(treatment_group, adverse_events, fill = treatment_group)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Adverse Events by Treatment Group",
    x = "Treatment Group",
    y = "Number of Adverse Events"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "italic"),
    legend.position = "right",
    axis.text.x = element_text(angle = 45)
  )

clinical_data_boxplot





## Question 3
## Age distribution of patients
## Analyze the age distribution of patients in the dataset
# clean and prepare the data
# arranging the dataset by age
arranged_data <- clinical_data %>% 
  group_by(age) %>% 
  arrange(age)

arranged_data

# plotting a histogram to visualize the age distribution of all patients
clinical_data_histogram <- arranged_data %>% 
  ggplot(aes(age)) +
  geom_histogram(binwidth = 10, color = "darkblue", fill = "skyblue") +
  labs(
    title = "Age Distribution of Patients",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "italic")
  )

clinical_data_histogram





## Question 4
## Gender differences in treatment response
## Investigate if there are any differences in treatment response based on gender
# calculate the mean change in score form baseline to followup for each gender
# create a new column name score_change
clinical_data_new <- clinical_data %>%
  mutate(score_change = followup_score - baseline_score)  # Calculate individual score change

head(clinical_data_new)

gender_score_change <- clinical_data_new %>%
  group_by(gender) %>%
  summarize(
    mean_score_change = mean(score_change)
  )

head(gender_score_change) 
head(clinical_data_new)

# plotting a scattterplot to visualize the change in scores by gender and treatment group
clinical_data_change <- clinical_data_new %>%
  ggplot(aes(x = gender, y = score_change, color = gender)) +
  geom_point(size = 3, alpha = 0.7) +
  facet_wrap(~ treatment_group) +
  scale_color_manual(values = c("darkorange", "navyblue")) +
  labs(
    title = "Score Change by Gender and Treatment Group",
    x = "Gender",
    y = "Score Change (Follow-up - Baseline)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "italic"),
    axis.text.x = element_text(angle = 45)
  )

clinical_data_change



## Question 5
## Effect of baseline score on treatment outcome
## Examine whether the baseline score affects the treatment outcome (i.e., the change in scores)
# creating a new variable that calculates the change in score (followup_score - baseline_score)
clinical_new_variable <- clinical_data %>% 
  mutate(changed_score = followup_score - baseline_score)

clinical_new_variable

# plotting a scatterplot to visualize the relationship between baseline scores and the change in scores
baseline_score_relationship <- clinical_new_variable %>% 
  ggplot(aes(baseline_score, changed_score)) +
  geom_point()

baseline_score_relationship
