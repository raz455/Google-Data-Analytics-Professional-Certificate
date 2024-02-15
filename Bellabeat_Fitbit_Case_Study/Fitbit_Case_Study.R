#Phase-3: Process
install.packages("tidyverse")
library(tidyverse)
install.packages("here")
library(here)
install.packages("skimr")
library(skimr)
install.packages("janitor")
library(janitor)
install.packages("lubridate")
library(lubridate)
install.packages("knitr")
library(knitr)
install.packages("kableExtra")
library(kableExtra)



daily_activity <- read_csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
daily_sleep <- read_csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
hourly_calories <- read_csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hourly_steps <- read_csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
hourly_intensities <- read_csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
heartrate_seconds <- read_csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
weight <- read_csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

#Getting an Overview of the Data

head(daily_activity)
head(hourly_calories)

#Inspecting the Structure of the Data
str(daily_activity)

str(daily_sleep)


#Cleaning the Data
#Checking for Unique User Id's in the Data Frames

n_distinct(daily_activity$Id)
n_distinct(daily_sleep$Id)
n_distinct(heartrate_seconds$Id)
n_distinct(hourly_calories$Id)
n_distinct(hourly_intensities$Id)
n_distinct(hourly_steps$Id)
n_distinct(weight$Id)


#Identifying Missing Values and Removing Duplicates

#Checking for duplicates in the data with the duplicated() function:

sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_steps))

#The daily_sleep data frame contains three duplicate entries, which will be eliminated in the subsequent step.
daily_activity <- daily_activity %>% 
  distinct() %>% 
  drop_na()
daily_sleep <- daily_sleep %>% 
  distinct() %>% 
  drop_na()
hourly_calories <- hourly_calories %>% 
  distinct() %>% 
  drop_na()
hourly_intensities <- hourly_intensities %>% 
  distinct() %>% 
  drop_na()
hourly_steps <- hourly_steps %>% 
  distinct() %>% 
  drop_na()


#Cleaning and Standardizing Column Name

daily_activity <- daily_activity %>% 
  clean_names() %>% 
  rename_with(tolower)
daily_sleep <- daily_sleep %>% 
  clean_names() %>% 
  rename_with(tolower)
hourly_calories <- hourly_calories %>% 
  clean_names() %>% 
  rename_with(tolower)
hourly_intensities <- hourly_intensities %>% 
  clean_names() %>% 
  rename_with(tolower)
hourly_steps <- hourly_steps %>% 
  clean_names() %>% 
  rename_with(tolower)

#Checking Cols

colnames(daily_activity)

#Standardizing Date and Time Formats

daily_activity <- daily_activity %>% 
  rename(date = activity_date) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))
daily_sleep <- daily_sleep %>% 
  rename(date = sleep_day) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))
hourly_calories <- hourly_calories %>% 
  rename(date_time = activity_hour) %>% 
  mutate(date_time = as.POSIXct(date_time, format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))
hourly_intensities <- hourly_intensities %>% 
  rename(date_time = activity_hour) %>% 
  mutate(date_time = as.POSIXct(date_time, format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))
hourly_steps<- hourly_steps %>% 
  rename(date_time = activity_hour) %>% 
  mutate(date_time = as.POSIXct(date_time, format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))


#Consequently, for the purpose of analysis, we will employ an 'outer_join' approach to retain all.
merged_daily_activity_sleep <- merge(daily_activity, daily_sleep, by = c ("id", "date"), all = TRUE)


#To evaluate the result of the merging process, we will inspect the combined dataset:
glimpse(merged_daily_activity_sleep)

#Phase-4: Summary Statistics Overview
daily_activity %>% 
  select(total_steps,
         total_distance,
         calories) %>% 
  summary()

#Summary of Active Minutes
daily_activity %>% 
  select(very_active_minutes,
         fairly_active_minutes,
         lightly_active_minutes,
         sedentary_minutes) %>% 
  summary()

#Summary of Daily Sleep
daily_sleep %>% 
  select(total_sleep_records,
         total_minutes_asleep,
         total_time_in_bed) %>% 
  summary()


#Summary of Hourly Calories
hourly_calories %>% 
  select(calories) %>% 
  summary()


#Phase-5: Share > Visualizations

#Let's dive into visualizing some essential explorations:
#How Does the Number of Steps Taken Daily Correlate with the Amount of Time Spent Sedentary?

ggplot(data = daily_activity, aes(x = total_steps, y = sedentary_minutes)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Total Steps vs. Sedentary Minutes",
       x = "Total Steps",
       y = "Sedentary Minutes")

#How does the Number of Steps Taken Correlate With the Total Calories Burned?

ggplot(data = daily_activity, aes(x = total_steps, y = calories)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Total Steps vs. Calories",
       x = "Total Steps",
       y = "Calories")

#How do the Minutes Spent Asleep Correlate With the Total Time Spent in Bed?

ggplot(data = daily_sleep, aes(x = total_minutes_asleep, y = total_time_in_bed)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Minutes Asleep vs. Time in Bed",
       x = "Minutes Asleep",
       y = "Time in Bed")

#How Does the Number of Steps Taken Correlate with Sleep?
ggplot(data = merged_daily_activity_sleep, aes(x = total_minutes_asleep, y = total_steps)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Minutes Asleep vs. Daily Activity",
       x = "Minutes Asleep",
       y = "Steps")

#What Patterns Emerge in Intensity Levels Over the Course of a Day?

# Calculate average intensity per hour from hourly_intensities data frame
average_intensities_per_hour <- hourly_intensities %>%
  # Convert date_time to POSIXct and extract the hour component
  mutate(hour = format(as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p"), "%H")) %>%
  # Group data by the extracted hour
  group_by(hour) %>%
  # Summarize to calculate the average intensities, removing NA values
  summarise(average_intensities = mean(total_intensity, na.rm = TRUE))

ggplot(data = average_intensities_per_hour, aes(x = hour, y = average_intensities, group = 1, color = average_intensities)) +
  geom_line() +
  scale_color_gradient(low = "red", high = "green") +
  labs(title = "Average Intensity Level Throughout the Day",
       x = "Hour of the Day",
       y = "Average Intensity") +
  theme_minimal()

#What Patterns Emerge in Physical Activity Levels Over the Course of a Day?

# Calculate average steps per hour from hourly_steps data frame
average_steps_per_hour <- hourly_steps %>%
  # Convert date_time to POSIXct and extract the hour component
  mutate(hour = format(as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p"), "%H")) %>%
  # Group data by the extracted hour
  group_by(hour) %>%
  # Summarize to calculate the average steps, removing NA values
  summarise(average_steps = mean(step_total, na.rm = TRUE))

ggplot(data = average_steps_per_hour, aes(x = hour, y = average_steps, group = 1, color = average_steps)) +
  geom_line() +
  scale_color_gradient(low = "red", high = "green") +
  labs(title = "Average Activity Level Throughout the Day",
       x = "Hour of the Day",
       y = "Average Steps") +
  theme_minimal()

#What Is the Variation in Activity Levels Across Different Weekdays?

# Create a summary of average steps by weekday
steps_by_weekday <- daily_activity %>%
  # Add a 'weekday' column, with weekdays ordered from Monday to Sunday
  mutate(weekday = factor(weekdays(date), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  # Group data by the 'weekday' column
  group_by(weekday) %>%
  # Calculate the mean of 'total_steps' for each weekday
  summarize(daily_steps = mean(total_steps, na.rm = TRUE)) # na.rm = TRUE removes NA values from the mean calculation

# Define the theme settings for the x-axis text in advance
x_text_theme <- element_text(angle = 45, vjust = 0.5, hjust = 1)

# Plot average daily steps for each weekday
ggplot(steps_by_weekday, aes(x = weekday, y = daily_steps)) +
  # Create purple bars for each weekday
  geom_col(fill = "purple") + 
  # Add a horizontal line at 10,000 steps as a reference
  geom_hline(yintercept = 10000, linetype = "dashed") + 
  # Add labels; leave x and y labels empty
  labs(title = "Daily Steps per Weekday", 
       x= "", 
       y = "",
       caption = "*The CDC recommends that adults aim for 10,000 steps per day") +
  # Apply the predefined theme to the x-axis text
  theme(axis.text.x = x_text_theme)
  
#What Is the Variation in Sleep Minutes Across Different Weekdays?
# Create a summary of average sleep by weekday
sleep_by_weekday <- daily_sleep %>%
  # Add a 'weekday' column, with weekdays ordered from Monday to Sunday
  mutate(weekday = factor(weekdays(date), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  # Group data by the 'weekday' column
  group_by(weekday) %>%
  # Calculate the mean of 'total_minutes_asleep' for each weekday
  summarize(daily_sleep_minutes = mean(total_minutes_asleep, na.rm = TRUE)) # na.rm = TRUE removes NA values from the mean calculation


# Plot average daily sleep for each weekday
ggplot(sleep_by_weekday, aes(x = weekday, y = daily_sleep_minutes)) +
  # Create purple bars for each weekday
  geom_col(fill = "purple") + 
  # Add a horizontal line at 420 minutes as a reference
  geom_hline(yintercept = 420, linetype = "dashed") + 
  # Add labels; leave x and y labels empty
  labs(title = "Daily Sleep Minutes per Weekday", 
       x= "", 
       y = "",
       caption = "*The CDC recommends that adults aim for at least 7 hours of sleep per day") +
  # Apply the predefined theme to the x-axis text
  theme(axis.text.x = x_text_theme)

#User Segmentation by Smart Device Usage
n_distinct(merged_daily_activity_sleep$date)

#Daytime Usage:

# Calculate the number of active days for each user and categorize them by usage
daytime_daily_use <- daily_activity %>%
  group_by(id) %>%
  # Summarize the total days used for each user
  summarize(days_used = n_distinct(date)) %>%
  # Categorize users based on the number of days they used the device
  mutate(user_type = case_when(
    days_used <= 10 ~ "low user",
    days_used <= 20 ~ "moderate user", 
    days_used <= 31 ~ "high user" 
  ))

# Display a glimpse of the resulting data frame
glimpse(daytime_daily_use)
  

# Calculate the percentage of total days used for each user type
daytime_daily_use_percent <- daytime_daily_use %>%
  # Count the number of users in each user_type
  count(user_type) %>%
  # Calculate the percentage of users in each user_type
  mutate(total_percent = n / sum(n)) %>%
  # Create a label for the percentage in a human-readable format
  mutate(labels = scales::percent(total_percent))

# Reorder the factor levels for user_type
daytime_daily_use_percent$user_type <- factor(daytime_daily_use_percent$user_type, levels = c("high user", "moderate user", "low user"))

# Display the first few rows of the resulting data frame
head(daytime_daily_use_percent)

#Nighttime Usage:
# Calculate the number of sleep records for each user and categorize them by usage
nighttime_daily_use <- daily_sleep %>%
  group_by(id) %>%
  # Summarize the total nights used for each user
  summarize(nights_used = n_distinct(date)) %>%
  # Categorize users based on the number of nights they used the device
  mutate(user_type = case_when(
    nights_used <= 10 ~ "low user",
    nights_used <= 20 ~ "moderate user", 
    nights_used <= 31 ~ "high user" 
  ))

# Display a glimpse of the resulting data frame
glimpse(nighttime_daily_use)


# Calculate the percentage of total nights used for each user type
nighttime_daily_use_percent <- nighttime_daily_use %>%
  # Count the number of users in each user_type
  count(user_type) %>%
  # Calculate the percentage of users in each user_type
  mutate(total_percent = n / sum(n)) %>%
  # Create a label for the percentage in a human-readable format
  mutate(labels = scales::percent(total_percent))

# Reorder the factor levels for user_type
nighttime_daily_use_percent$user_type <- factor(nighttime_daily_use_percent$user_type, levels = c("high user", "moderate user", "low user"))

# Display the first few rows of the resulting data frame
head(nighttime_daily_use_percent)


#Comparison of Daytime & Nighttime
# Create a pie chart to visualize user types by daily use percentage
daytime_daily_use_percent %>%
  ggplot(aes(x = "", y = total_percent, fill = user_type)) +
  # Draw the pie chart using bars with identity statistics
  geom_bar(stat = "identity", width = 1) +
  # Convert the bar chart to a pie chart
  coord_polar("y", start = 0) + 
  # Apply a minimalistic theme with no background, gridlines, or text
  theme_void() +
  # Manually set the colors and labels for the fill scale
  scale_fill_manual(values = c("high user" = "#F7CAC9", 
                               "moderate user" = "#92A8D1", 
                               "low user" = "#98DDDE"), 
                    labels = c("High user - 21 to 31 days",
                               "Moderate user - 11 to 20 days",
                               "Low user - 1 to 10 days")) +
  # Add a title to the plot
  labs(title = "Daily Daytime Use of Smart Device") +
  # Add text labels to the pie chart
  geom_text(aes(label = labels), position = position_stack(vjust = 0.8))


# Create a pie chart to visualize user types by daily use percentage
nighttime_daily_use_percent %>%
  ggplot(aes(x = "", y = total_percent, fill = user_type)) +
  # Draw the pie chart using bars with identity statistics
  geom_bar(stat = "identity", width = 1) +
  # Convert the bar chart to a pie chart
  coord_polar("y", start = 0) + 
  # Apply a minimalistic theme with no background, gridlines, or text
  theme_void() +
  # Manually set the colors and labels for the fill scale
  scale_fill_manual(values = c("high user" = "#F7CAC9", 
                               "moderate user" = "#92A8D1", 
                               "low user" = "#98DDDE"), 
                    labels = c("High user - 21 to 31 days",
                               "Moderate user - 11 to 20 days",
                               "Low user - 1 to 10 days")) +
  # Add a title to the plot
  labs(title = "Daily Nighttime Use of Smart Device") +
  # Add text labels to the pie chart
  geom_text(aes(label = labels), position = position_stack(vjust = 0.8))