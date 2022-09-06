library(tidyverse)
library(ggthemes)
library(hrbrthemes)
usa_uv <- read.csv('usa_uv.csv')
usa_uv_2 <- read.csv('usa_uv_2.csv')
usa_uv_3 <- read.csv('usa_uv_3.csv')

#combine tables

usa_uv_data <- bind_rows(usa_uv, usa_uv_2, usa_uv_3)

#drop extra tables
remove(usa_uv, usa_uv_2, usa_uv_3)

#drop url column
usa_uv_data <- subset(usa_uv_data, select = -url)

#check num of records per station
usa_uv_data %>%
  count(platform_name)

#check number of weather stations
print(n_distinct(usa_uv_data$platform_id))

#change BARROW to Barrow (AK)
usa_uv_data_cleaned <- usa_uv_data %>%
  mutate(platform_name=case_when(platform_name =='BARROW' ~ 'Barrow (AK)',
                                 TRUE ~ as.character(platform_name)))
#check change
usa_uv_data_cleaned %>%
  count(platform_name)

#check for unusually high UV
usa_uv_data_cleaned %>%
  filter(uv_index_hourly_average >10) %>%
  view()
#remove Mauna Loa  and Kilauea as they are volcanoes with unusual UV index
usa_uv_data_cleaned <- usa_uv_data_cleaned %>%
  filter(platform_id != 31, platform_id != 465)

#recheck unusual UV
usa_uv_data_cleaned %>%
  filter(uv_index_hourly_average >10) %>%
  view()

#extract y/m/d from date format given
usa_uv_data_cleaned <- usa_uv_data_cleaned %>%
  mutate(year = substr(usa_uv_data_cleaned$instance_datetime, 1,4)) %>%
  mutate(month = substr(usa_uv_data_cleaned$instance_datetime, 6,7)) %>%
  mutate(day = substr(usa_uv_data_cleaned$instance_datetime,9,10))

#Clean up by day
usa_uv_data_cleaned <- usa_uv_data_cleaned %>%
  group_by(platform_name, day, month, year)%>%
  summarise(daily_max = max(uv_index_daily_max))

#average daily maximum UV for each month
avg_peak_uv_month <- usa_uv_data_cleaned %>%
  group_by(month) %>%
  summarise(avg_peak_uv = round(mean(daily_max), digits = 2))

ggplot(data = avg_peak_uv_month, mapping = aes(x=month, y = avg_peak_uv))+
  geom_bar(stat = 'identity' , aes(fill= avg_peak_uv > 2))+
  theme_solarized() + theme(plot.title = element_text(face = "bold")) +labs(title = "Average daily naximum UV index by month",
    x = "Month", y = "Average daily maximum UV")+
  scale_fill_manual(values = c('#040054', '#A80000'), guide = 'none')+labs(subtitle = "A selection of US weather stations between 1991-2014")

#find mean daily max by latitude
max_daily_uv_by_latitude <- usa_uv_data %>%
  rename(latitude = Y) %>%
  group_by(latitude) %>%
  summarise(average_uv_index_daily_max = mean(uv_index_daily_max))

#scatter plot by latitude
ggplot(data = max_daily_uv_by_latitude, mapping= aes(x= latitude, y= average_uv_index_daily_max, color = average_uv_index_daily_max)) + 
  geom_point(color =  "#850000") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  theme_solarized() +
  theme(plot.title = element_text(hjust = 0.5)) +labs(title = "UV index by latitude", x = "Latitude",
    y = "Average maximum daily uv index")

#San Diego Only
sd_uv<- usa_uv_data_cleaned %>%
  filter(platform_name == 'San Diego (CA)')

#look at average daily max by year
sd_uv_yearly <- sd_uv %>%
  group_by(year,month) %>%
  summarise(avg_daily_max = round(mean(daily_max), digits = 2)) 

#remove 2008 and 1996 as they are incomplete
sd_uv_yearly <- sd_uv %>%
  filter(year!=2008, year!=1996) %>%
  group_by(year) %>%
  summarise(avg_daily_max = round(mean(daily_max), digits = 2)) 

#calculate percentage change
print(as_tibble(sd_uv_yearly))
sd_percent_change <- ((sd_uv_yearly[11,2] - sd_uv_yearly[1,2]) / abs(sd_uv_yearly[1,2])*100) %>%
  rename(percent_change = avg_daily_max)
print(as_tibble(sd_percent_change))

#line graph
ggplot(data= sd_uv_yearly, mapping = aes(x=year, y= avg_daily_max))+
  geom_line(aes(group=1, color='red')) +
  geom_smooth(method = 'lm', se = FALSE,linetype = "dashed",  aes(group = 1))+
  theme_solarized()+
  guides(color = 'none')+
  labs(title = 'Average daily maximum UV index by year in San Diego', y= 'average daily maximum uv')